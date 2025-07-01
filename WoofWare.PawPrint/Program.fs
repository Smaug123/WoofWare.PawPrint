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

        let mainMethodFromMetadata = dumped.Methods.[entryPoint]

        if mainMethodFromMetadata.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let state = IlMachineState.initial loggerFactory dotnetRuntimeDirs dumped

        // Find the core library by traversing the type hierarchy of the main method's declaring type
        // until we reach System.Object
        let rec findCoreLibraryAssemblyFromGeneric
            (state : IlMachineState)
            (currentType : TypeInfo<WoofWare.PawPrint.GenericParameter, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                let baseTypes = Corelib.getBaseTypes currentAssembly
                state, Some baseTypes
            | Some baseTypeInfo ->
                match baseTypeInfo with
                | BaseTypeInfo.TypeRef typeRefHandle ->
                    // Look up the TypeRef from the handle
                    let typeRef = currentAssembly.TypeRefs.[typeRefHandle]
                    // Resolve the type reference to find which assembly it's in
                    let rec go state =
                        match
                            Assembly.resolveTypeRef state._LoadedAssemblies currentAssembly typeRef ImmutableArray.Empty
                        with
                        | TypeResolutionResult.FirstLoadAssy assyRef ->
                            // Need to load this assembly first
                            let handle, definedIn = assyRef.Handle

                            let state, _, _ =
                                IlMachineState.loadAssembly
                                    loggerFactory
                                    state._LoadedAssemblies.[definedIn.FullName]
                                    handle
                                    state

                            go state
                        | TypeResolutionResult.Resolved (resolvedAssembly, resolvedType) ->
                            findCoreLibraryAssemblyFromResolved state resolvedType resolvedAssembly

                    go state
                | BaseTypeInfo.TypeDef typeDefHandle ->
                    // Base type is in the same assembly
                    let baseType = currentAssembly.TypeDefs.[typeDefHandle]
                    findCoreLibraryAssemblyFromGeneric state baseType currentAssembly
                | BaseTypeInfo.TypeSpec _ -> failwith "Type specs not yet supported in base type traversal"
                | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefHandle) ->
                    // Base type is in a foreign assembly
                    match state._LoadedAssemblies.TryGetValue assemblyName.FullName with
                    | true, foreignAssembly ->
                        let baseType = foreignAssembly.TypeDefs.[typeDefHandle]
                        findCoreLibraryAssemblyFromGeneric state baseType foreignAssembly
                    | false, _ -> failwith $"Foreign assembly {assemblyName.FullName} not loaded"

        and findCoreLibraryAssemblyFromResolved
            (state : IlMachineState)
            (currentType : TypeInfo<TypeDefn, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                let baseTypes = Corelib.getBaseTypes currentAssembly
                state, Some baseTypes
            | Some baseTypeInfo ->
                match baseTypeInfo with
                | BaseTypeInfo.TypeRef typeRefHandle ->
                    // Look up the TypeRef from the handle
                    let typeRef = currentAssembly.TypeRefs.[typeRefHandle]

                    let rec go state =
                        // Resolve the type reference to find which assembly it's in
                        match
                            Assembly.resolveTypeRef state._LoadedAssemblies currentAssembly typeRef ImmutableArray.Empty
                        with
                        | TypeResolutionResult.FirstLoadAssy assyRef ->
                            // Need to load this assembly first
                            let handle, definedIn = assyRef.Handle

                            let state, _, _ =
                                IlMachineState.loadAssembly
                                    loggerFactory
                                    state._LoadedAssemblies.[definedIn.FullName]
                                    handle
                                    state

                            go state
                        | TypeResolutionResult.Resolved (resolvedAssembly, resolvedType) ->
                            findCoreLibraryAssemblyFromResolved state resolvedType resolvedAssembly

                    go state
                | BaseTypeInfo.TypeDef typeDefHandle ->
                    // Base type is in the same assembly, but we need to convert back to generic form
                    let baseType = currentAssembly.TypeDefs.[typeDefHandle]
                    findCoreLibraryAssemblyFromGeneric state baseType currentAssembly
                | BaseTypeInfo.TypeSpec _ -> failwith "Type specs not yet supported in base type traversal"
                | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefHandle) ->
                    // Base type is in a foreign assembly
                    match state._LoadedAssemblies.TryGetValue assemblyName.FullName with
                    | true, foreignAssembly ->
                        let baseType = foreignAssembly.TypeDefs.[typeDefHandle]
                        findCoreLibraryAssemblyFromGeneric state baseType foreignAssembly
                    | false, _ -> failwith $"Foreign assembly {assemblyName.FullName} not loaded"

        let rec computeState (baseClassTypes : BaseClassTypes<DumpedAssembly> option) (state : IlMachineState) =
            match baseClassTypes with
            | Some baseTypes ->
                // We already have base class types, can directly create the concretized method
                // Use the original method from metadata, but convert FakeUnit to TypeDefn
                let rawMainMethod =
                    mainMethodFromMetadata
                    |> MethodInfo.mapTypeGenerics (fun i _ -> TypeDefn.GenericTypeParameter i)

                let state, concretizedMainMethod, _ =
                    IlMachineState.concretizeMethodWithTypeGenerics
                        loggerFactory
                        baseTypes
                        ImmutableArray.Empty // No type generics for main method's declaring type
                        { rawMainMethod with
                            Instructions = Some (MethodInstructions.onlyRet ())
                        }
                        None
                        state

                // Create the method state with the concretized method
                match
                    MethodState.Empty
                        state.ConcreteTypes
                        baseTypes
                        state._LoadedAssemblies
                        dumped
                        concretizedMainMethod
                        ImmutableArray.Empty
                        (ImmutableArray.CreateRange [ CliType.ObjectRef None ])
                        None
                with
                | Ok concretizedMeth -> IlMachineState.addThread concretizedMeth dumped.Name state, Some baseTypes
                | Error _ -> failwith "Unexpected failure creating method state with concretized method"
            | None ->
                // We need to discover the core library by traversing the type hierarchy
                let mainMethodType =
                    dumped.TypeDefs.[mainMethodFromMetadata.DeclaringType.Definition.Get]

                let state, baseTypes =
                    findCoreLibraryAssemblyFromGeneric state mainMethodType dumped

                computeState baseTypes state

        let (state, mainThread), baseClassTypes = state |> computeState None

        // Now that we have base class types, concretize the main method for use in the rest of the function
        let state, concretizedMainMethod, mainTypeHandle =
            match baseClassTypes with
            | Some baseTypes ->
                let rawMainMethod =
                    mainMethodFromMetadata
                    |> MethodInfo.mapTypeGenerics (fun i _ -> TypeDefn.GenericTypeParameter i)

                IlMachineState.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseTypes
                    ImmutableArray.Empty // No type generics for main method's declaring type
                    rawMainMethod
                    None
                    state
            | None -> failwith "Expected base class types to be available at this point"

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineState.loadClass loggerFactory (Option.toObj baseClassTypes) mainTypeHandle mainThread
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
            match mainMethodFromMetadata.Signature.ParameterTypes |> Seq.toList with
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                allocateArgs argv baseClassTypes state
            | _ -> failwith "Main method must take an array of strings; other signatures not yet implemented"

        match mainMethodFromMetadata.Signature.ReturnType with
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
        | _ -> failwith "Main method must return int32; other types not currently supported"

        // We might be in the middle of class construction. Pump the static constructors to completion.
        // We haven't yet entered the main method!

        let state, _ =
            pumpToReturn loggerFactory logger baseClassTypes impls mainThread state

        logger.LogInformation "Main method class now initialised"

        // Now that BCL initialisation has taken place and the user-code classes are constructed,
        // overwrite the main thread completely using the already-concretized method.
        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    dumped
                    concretizedMainMethod
                    ImmutableArray.Empty
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
            |> IlMachineState.ensureTypeInitialised loggerFactory baseClassTypes mainThread mainTypeHandle

        match init with
        | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
        | WhatWeDid.BlockedOnClassInit _ -> failwith "logic error: surely this thread can't be blocked on class init"
        | WhatWeDid.Executed -> ()

        pumpToReturn loggerFactory logger baseClassTypes impls mainThread state
