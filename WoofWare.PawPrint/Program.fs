namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Program =
    /// Returns the pointer to the resulting array on the heap.
    let allocateArgs
        (args : string list)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let argsAllocations, state =
            (state, args)
            ||> Seq.mapFold (fun state arg ->
                IlMachineState.allocateManagedObject
                    (corelib.String
                     |> TypeInfo.mapGeneric (fun _ _ -> failwith<unit> "there are no generics here"))
                    (failwith "TODO: assert fields and populate")
                    state
            // TODO: set the char values in memory
            )

        let arrayAllocation, state =
            IlMachineState.allocateArray (fun () -> CliType.ObjectRef None) args.Length state

        let state =
            ((state, 0), argsAllocations)
            ||> Seq.fold (fun (state, i) arg ->
                let state =
                    IlMachineState.setArrayValue arrayAllocation (CliType.OfManagedObject arg) i state

                state, i + 1
            )
            |> fst

        arrayAllocation, state

    let rec pumpToReturn
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        impls
        (mainThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        match AbstractMachine.executeOneStep loggerFactory impls baseClassTypes state mainThread with
        | ExecutionResult.Terminated (state, terminatingThread) -> state, terminatingThread
        | ExecutionResult.Stepped (state', whatWeDid) ->

        match whatWeDid with
        | WhatWeDid.Executed ->
            logger.LogInformation $"Executed one step; active assembly: {state'.ActiveAssembly(mainThread).Name.Name}"
        | WhatWeDid.SuspendedForClassInit ->
            logger.LogInformation "Suspended execution of current method for class initialisation."
        | WhatWeDid.BlockedOnClassInit threadBlockingUs ->
            logger.LogInformation "Unable to execute because class has not yet initialised."

        pumpToReturn loggerFactory logger baseClassTypes impls mainThread state'

    /// Returns the abstract machine's state at the end of execution, together with the thread which
    /// caused execution to end.
    let run
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        impls
        (argv : string list)
        : IlMachineState * ThreadId
        =
        let logger = loggerFactory.CreateLogger "Program"

        let dumped = Assembly.read loggerFactory originalPath fileStream

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith "No entry point in input DLL"
            | Some d -> d

        let mainMethod = dumped.Methods.[entryPoint]

        if mainMethod.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let mainMethod =
            mainMethod
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "Refusing to execute generic main method")
            |> MethodInfo.mapMethodGenerics (fun _ -> failwith "Refusing to execute generic main method")

        let rec computeState (baseClassTypes : BaseClassTypes<DumpedAssembly> option) (state : IlMachineState) =
            // The thread's state is slightly fake: we will need to put arguments onto the stack before actually
            // executing the main method.
            // We construct the thread here before we are entirely ready, because we need a thread from which to
            // initialise the class containing the main method.
            // Once we've obtained e.g. the String and Array classes, we can populate the args array.
            match
                MethodState.Empty
                    (Option.toObj baseClassTypes)
                    state._LoadedAssemblies
                    dumped
                    // pretend there are no instructions, so we avoid preparing anything
                    { mainMethod with
                        Instructions = Some MethodInstructions.OnlyRet
                    }
                    None
                    (ImmutableArray.CreateRange [ CliType.ObjectRef None ])
                    None
            with
            | Ok meth -> IlMachineState.addThread meth dumped.Name state, baseClassTypes
            | Error requiresRefs ->
                let state =
                    (state, requiresRefs)
                    ||> List.fold (fun state ref ->
                        let handle, referencingAssy = ref.Handle
                        let referencingAssy = state.LoadedAssembly referencingAssy |> Option.get

                        let state, _, _ =
                            IlMachineState.loadAssembly loggerFactory referencingAssy handle state

                        state
                    )

                let corelib =
                    let coreLib =
                        state._LoadedAssemblies.Keys
                        |> Seq.tryFind (fun x -> x.StartsWith ("System.Private.CoreLib, ", StringComparison.Ordinal))

                    coreLib
                    |> Option.map (fun coreLib -> state._LoadedAssemblies.[coreLib] |> Corelib.getBaseTypes)

                computeState corelib state

        let (state, mainThread), baseClassTypes =
            IlMachineState.initial loggerFactory dotnetRuntimeDirs dumped
            |> computeState None

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineState.loadClass
                    loggerFactory
                    (Option.toObj baseClassTypes)
                    mainMethod.DeclaringType
                    mainThread
            with
            | StateLoadResult.NothingToDo ilMachineState -> ilMachineState
            | StateLoadResult.FirstLoadThis ilMachineState -> loadInitialState ilMachineState

        let state = loadInitialState state

        // Now that the object has been loaded, we can identify the critical types like `string` from System.Private.CoreLib.

        let baseClassTypes =
            match baseClassTypes with
            | None ->
                let coreLib =
                    state._LoadedAssemblies.Keys
                    |> Seq.find (fun x -> x.StartsWith ("System.Private.CoreLib, ", StringComparison.Ordinal))

                state._LoadedAssemblies.[coreLib] |> Corelib.getBaseTypes
            | Some c -> c

        let arrayAllocation, state =
            match mainMethod.Signature.ParameterTypes |> Seq.toList with
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                allocateArgs argv baseClassTypes state
            | _ -> failwith "Main method must take an array of strings; other signatures not yet implemented"

        match mainMethod.Signature.ReturnType with
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
        | _ -> failwith "Main method must return int32; other types not currently supported"

        // We might be in the middle of class construction. Pump the static constructors to completion.
        // We haven't yet entered the main method!

        let state, _ =
            pumpToReturn loggerFactory logger baseClassTypes impls mainThread state

        // Now that BCL initialisation has taken place and the user-code classes are constructed,
        // overwrite the main thread completely.
        let methodState =
            match
                MethodState.Empty
                    baseClassTypes
                    state._LoadedAssemblies
                    dumped
                    mainMethod
                    None
                    (ImmutableArray.Create (CliType.OfManagedObject arrayAllocation))
                    None
            with
            | Ok s -> s
            | Error _ -> failwith "TODO: I'd be surprised if this could ever happen in a valid program"

        let threadState =
            { state.ThreadState.[mainThread] with
                MethodStates = ImmutableArray.Create methodState
                ActiveMethodState = 0
            }

        let state, init =
            { state with
                ThreadState = state.ThreadState |> Map.add mainThread threadState
            }
            |> IlMachineState.ensureTypeInitialised
                loggerFactory
                baseClassTypes
                mainThread
                methodState.ExecutingMethod.DeclaringType

        match init with
        | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
        | WhatWeDid.BlockedOnClassInit _ -> failwith "logic error: surely this thread can't be blocked on class init"
        | WhatWeDid.Executed -> ()

        pumpToReturn loggerFactory logger baseClassTypes impls mainThread state
