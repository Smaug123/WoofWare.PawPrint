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
                IlMachineState.allocateManagedObject corelib.String (failwith "TODO: assert fields and populate") state
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

    /// Returns the abstract machine's state at the end of execution, together with the thread which
    /// caused execution to end.
    let run
        (loggerFactory : ILoggerFactory)
        (fileStream : Stream)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (argv : string list)
        : IlMachineState * ThreadId
        =
        let logger = loggerFactory.CreateLogger "Program"

        let dumped = Assembly.read loggerFactory fileStream

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith "No entry point in input DLL"
            | Some d -> d

        let mainMethod = dumped.Methods.[entryPoint]

        if mainMethod.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let state, mainThread =
            IlMachineState.initial dotnetRuntimeDirs dumped
            // The thread's state is slightly fake: we will need to put arguments onto the stack before actually
            // executing the main method.
            // We construct the thread here before we are entirely ready, because we need a thread from which to
            // initialise the class containing the main method.
            |> IlMachineState.addThread (MethodState.Empty mainMethod None) dumped.Name

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineState.loadClass
                    loggerFactory
                    (fst mainMethod.DeclaringType)
                    (snd mainMethod.DeclaringType)
                    mainThread
            with
            | StateLoadResult.NothingToDo ilMachineState -> ilMachineState
            | StateLoadResult.FirstLoadThis ilMachineState -> loadInitialState ilMachineState

        let state = loadInitialState state

        // Now that the object has been loaded, we can identify the String type from System.Private.CoreLib.

        let corelib =
            let coreLib =
                state._LoadedAssemblies.Keys
                |> Seq.find (fun x -> x.StartsWith ("System.Private.CoreLib, ", StringComparison.Ordinal))

            state._LoadedAssemblies.[coreLib]

        let baseClassTypes = Corelib.getBaseTypes corelib

        let arrayAllocation, state =
            match mainMethod.Signature.ParameterTypes |> Seq.toList with
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                allocateArgs argv baseClassTypes state
            | _ -> failwith "Main method must take an array of strings; other signatures not yet implemented"

        match mainMethod.Signature.ReturnType with
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
        | _ -> failwith "Main method must return int32; other types not currently supported"

        // TODO: now overwrite the main thread which we used for object initialisation. The below is not right.
        let state, mainThread =
            state
            |> IlMachineState.addThread
                { MethodState.Empty mainMethod None with
                    Arguments = ImmutableArray.Create (CliType.OfManagedObject arrayAllocation)
                }
                dumped.Name

        let rec go (state : IlMachineState) =
            match AbstractMachine.executeOneStep loggerFactory baseClassTypes state mainThread with
            | ExecutionResult.Terminated (state, terminatingThread) -> state, terminatingThread
            | ExecutionResult.Stepped (state', whatWeDid) ->

            match whatWeDid with
            | WhatWeDid.Executed -> logger.LogInformation "Executed one step."
            | WhatWeDid.SuspendedForClassInit ->
                logger.LogInformation "Suspended execution of current method for class initialisation."
            | WhatWeDid.BlockedOnClassInit threadBlockingUs ->
                logger.LogInformation "Unable to execute because class has not yet initialised."

            go state'

        go state
