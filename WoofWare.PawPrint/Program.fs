namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Program =
    /// Returns the pointer to the resulting array on the heap.
    let allocateArgs
        (loggerFactory : ILoggerFactory)
        (args : string list)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' corelib state._LoadedAssemblies corelib.String
            |> IlMachineState.concretizeType
                loggerFactory
                corelib
                state
                corelib.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let argsAllocations, state =
            (state, args)
            ||> Seq.mapFold (fun state arg ->
                IlMachineState.allocateManagedObject stringType (failwith "TODO: assert fields and populate") state
            // TODO: set the char values in memory
            )

        let stringArrayType = ConcreteTypeHandle.OneDimArrayZero stringType

        let arrayAllocation, state =
            IlMachineState.allocateArray stringArrayType (fun () -> CliType.ObjectRef None) args.Length state

        let state =
            ((state, 0), argsAllocations)
            ||> Seq.fold (fun (state, i) arg ->
                let state =
                    IlMachineState.setArrayValue arrayAllocation (CliType.ofManagedObject arg) i state

                state, i + 1
            )
            |> fst

        arrayAllocation, state

    type PreparedProgram =
        {
            State : IlMachineState
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            EntryThread : ThreadId
            LastRan : ThreadId
        }

    type ProgramStartResult =
        | Ready of PreparedProgram
        | CompletedBeforeMain of RunOutcome

    type ProgramStepOutcome =
        | InstructionStepped of PreparedProgram * ranThread : ThreadId * whatWeDid : WhatWeDid
        | WorkerTerminated of PreparedProgram * terminatingThread : ThreadId
        | Completed of RunOutcome
        | Deadlocked of PreparedProgram * stuckThreads : string

    let private deadlockDescription (state : IlMachineState) : string =
        state.ThreadState
        |> Map.toSeq
        |> Seq.filter (fun (_, ts) -> ts.Status <> ThreadStatus.Terminated)
        |> Seq.map (fun (ThreadId i, ts) -> $"thread {i} in state {ts.Status}")
        |> String.concat "; "

    let private logStepOutcome
        (logger : ILogger)
        (state : IlMachineState)
        (thread : ThreadId)
        (whatWeDid : WhatWeDid)
        : unit
        =
        match whatWeDid with
        | WhatWeDid.Executed ->
            logger.LogTrace (
                "Executed one step; active assembly: {ActiveAssembly}",
                state.ActiveAssembly(thread).Name.Name
            )
        | WhatWeDid.SuspendedForClassInit ->
            logger.LogTrace "Suspended execution of current method for class initialisation."
        | WhatWeDid.BlockedOnClassInit _ -> logger.LogTrace "Unable to execute because class has not yet initialised."
        | WhatWeDid.ThrowingTypeInitializationException ->
            logger.LogTrace "TypeInitializationException dispatched due to failed .cctor."

    let stepPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        impls
        (prepared : PreparedProgram)
        : ProgramStepOutcome
        =
        // The pump reports NormalExit as soon as `EntryThread` Terminates, regardless
        // of whether other threads are still Runnable or Blocked. This matches both
        // use sites:
        //   * Pre-Main cctor pump: the synthetic onlyRet frame has returned, which
        //     means class initialisation is done. The entry thread isn't actually
        //     finished — Program.run is about to resurrect it with the real Main
        //     frame — so we deliberately do NOT mark it Terminated, because doing so
        //     would let a worker that joined the entry thread during a .cctor observe
        //     a false end-of-thread and proceed past the Join before Main has started.
        //   * Post-Main pump: when Main returns, we report NormalExit immediately
        //     rather than waiting for foreground threads. The test comparison oracles
        //     in WoofWare.PawPrint.Test just invoke `assy.EntryPoint.Invoke` via
        //     reflection, which also returns as soon as Main returns without waiting
        //     for foreground workers, so matching that behaviour keeps PawPrint and
        //     the oracle aligned. Environment.Exit from a worker still propagates as
        //     ProcessExit (handled below) before Main has a chance to return.
        match Scheduler.chooseNext prepared.LastRan prepared.State with
        | None ->
            // No Runnable threads and the entry thread didn't hit its ret. Every
            // remaining thread is blocked, so progress is impossible.
            ProgramStepOutcome.Deadlocked (prepared, deadlockDescription prepared.State)
        | Some nextThread ->
            match
                AbstractMachine.executeOneStep loggerFactory impls prepared.BaseClassTypes prepared.State nextThread
            with
            | ExecutionResult.Terminated (state, terminatingThread) ->
                if terminatingThread = prepared.EntryThread then
                    ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, prepared.EntryThread))
                else
                    let state = Scheduler.onThreadTerminated terminatingThread state

                    ProgramStepOutcome.WorkerTerminated (
                        { prepared with
                            State = state
                            LastRan = terminatingThread
                        },
                        terminatingThread
                    )
            | ExecutionResult.ProcessExit (state, exitingThread) ->
                ProgramStepOutcome.Completed (RunOutcome.ProcessExit (state, exitingThread))
            | ExecutionResult.UnhandledException (state, terminatingThread, exn) ->
                ProgramStepOutcome.Completed (RunOutcome.GuestUnhandledException (state, terminatingThread, exn))
            | ExecutionResult.Stepped (state, whatWeDid) ->
                logStepOutcome logger state nextThread whatWeDid

                let state = Scheduler.onStepOutcome nextThread whatWeDid state

                ProgramStepOutcome.InstructionStepped (
                    { prepared with
                        State = state
                        LastRan = nextThread
                    },
                    nextThread,
                    whatWeDid
                )

    let rec pumpPrepared
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        impls
        (prepared : PreparedProgram)
        : RunOutcome
        =
        match stepPrepared loggerFactory logger impls prepared with
        | ProgramStepOutcome.Completed outcome -> outcome
        | ProgramStepOutcome.Deadlocked (_, stuck) ->
            failwith $"Deadlock: no runnable threads and entry thread has not terminated. Stuck: {stuck}"
        | ProgramStepOutcome.InstructionStepped (prepared, _, _)
        | ProgramStepOutcome.WorkerTerminated (prepared, _) -> pumpPrepared loggerFactory logger impls prepared

    let pumpToReturn
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        impls
        (entryThread : ThreadId)
        (state : IlMachineState)
        : RunOutcome
        =
        let prepared =
            {
                State = state
                BaseClassTypes = baseClassTypes
                EntryThread = entryThread
                LastRan = entryThread
            }

        pumpPrepared loggerFactory logger impls prepared

    /// Reads the guest assembly and performs the one-time setup needed before Main is ready to schedule.
    let prepare
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        impls
        (argv : string list)
        : ProgramStartResult
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
        let rec handleBaseTypeInfo
            (state : IlMachineState)
            (baseTypeInfo : BaseTypeInfo)
            (currentAssembly : DumpedAssembly)
            (continueWithGeneric :
                IlMachineState
                    -> TypeInfo<GenericParamFromMetadata, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            (continueWithResolved :
                IlMachineState
                    -> TypeInfo<TypeDefn, TypeDefn>
                    -> DumpedAssembly
                    -> IlMachineState * BaseClassTypes<DumpedAssembly> option)
            : IlMachineState * BaseClassTypes<DumpedAssembly> option
            =
            match baseTypeInfo with
            | BaseTypeInfo.TypeRef typeRefHandle ->
                // Look up the TypeRef from the handle
                let typeRef = currentAssembly.TypeRefs.[typeRefHandle]

                let rec go state =
                    // Resolve the type reference to find which assembly it's in
                    match
                        Assembly.resolveTypeRef state._LoadedAssemblies currentAssembly ImmutableArray.Empty typeRef
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
                    | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                        continueWithResolved state resolvedType resolvedAssembly

                go state
            | BaseTypeInfo.TypeDef typeDefHandle ->
                // Base type is in the same assembly
                let baseType = currentAssembly.TypeDefs.[typeDefHandle]
                continueWithGeneric state baseType currentAssembly
            | BaseTypeInfo.TypeSpec _ -> failwith "Type specs not yet supported in base type traversal"
            | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefHandle) ->
                // Base type is in a foreign assembly
                match state._LoadedAssemblies.TryGetValue assemblyName.FullName with
                | true, foreignAssembly ->
                    let baseType = foreignAssembly.TypeDefs.[typeDefHandle]
                    continueWithGeneric state baseType foreignAssembly
                | false, _ -> failwith $"Foreign assembly {assemblyName.FullName} not loaded"

        let rec findCoreLibraryAssemblyFromGeneric
            (state : IlMachineState)
            (currentType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            (currentAssembly : DumpedAssembly)
            =
            match currentType.BaseType with
            | None ->
                // We've reached the root (System.Object), so this assembly contains the core library
                let baseTypes = Corelib.getBaseTypes currentAssembly
                state, Some baseTypes
            | Some baseTypeInfo ->
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

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
                handleBaseTypeInfo
                    state
                    baseTypeInfo
                    currentAssembly
                    findCoreLibraryAssemblyFromGeneric
                    findCoreLibraryAssemblyFromResolved

        let rec computeState (baseClassTypes : BaseClassTypes<DumpedAssembly> option) (state : IlMachineState) =
            match baseClassTypes with
            | Some baseTypes ->
                // We already have base class types, can directly create the concretized method
                // Use the original method from metadata, but convert FakeUnit to TypeDefn
                let rawMainMethod =
                    mainMethodFromMetadata
                    |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

                let state, concretizedMainMethod, _ =
                    ExecutionConcretization.concretizeMethodWithTypeGenerics
                        loggerFactory
                        baseTypes
                        ImmutableArray.Empty // No type generics for main method's declaring type
                        { rawMainMethod with
                            Instructions = Some (MethodInstructions.onlyRet ())
                        }
                        None
                        dumped.Name
                        ImmutableArray.Empty
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

        let baseClassTypes =
            match baseClassTypes with
            | Some c -> c
            | None -> failwith "Expected base class types to be available at this point"

        // Now that we have base class types, concretize the main method for use in the rest of the function
        let state, concretizedMainMethod, mainTypeHandle =
            let rawMainMethod =
                mainMethodFromMetadata
                |> MethodInfo.mapTypeGenerics (fun (i, _) -> TypeDefn.GenericTypeParameter i.SequenceNumber)

            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty // No type generics for main method's declaring type
                rawMainMethod
                None
                dumped.Name
                ImmutableArray.Empty
                state

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        let rec loadInitialState (state : IlMachineState) =
            match
                state
                |> IlMachineStateExecution.loadClass loggerFactory baseClassTypes mainTypeHandle mainThread
            with
            | StateLoadResult.NothingToDo ilMachineState -> ilMachineState
            | StateLoadResult.FirstLoadThis ilMachineState -> loadInitialState ilMachineState
            | StateLoadResult.ThrowingTypeInitializationException _ ->
                failwith "TypeInitializationException during initial class load of entry point type"

        let state = loadInitialState state

        let arrayAllocation, state =
            match mainMethodFromMetadata.Signature.ParameterTypes |> Seq.toList with
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                allocateArgs loggerFactory argv baseClassTypes state
            | _ -> failwith "Main method must take an array of strings; other signatures not yet implemented"

        match mainMethodFromMetadata.Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32) -> ()
        | _ -> failwith "Main method must return int32; other types not currently supported"

        // We might be in the middle of class construction. Pump the static constructors to completion.
        // We haven't yet entered the main method!

        match pumpToReturn loggerFactory logger baseClassTypes impls mainThread state with
        | RunOutcome.GuestUnhandledException _ as outcome ->
            // Either the entry thread's .cctor raised an unhandled exception, or a worker
            // spawned during cctor pumping did. In both cases the CLR would terminate the
            // process; propagate rather than collapsing to a host failwith that would
            // mask the guest-level diagnostic.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.ProcessExit _ as outcome ->
            // A worker started during cctor pumping called Environment.Exit; the process
            // has torn down. Propagate rather than pressing on into Main.
            ProgramStartResult.CompletedBeforeMain outcome
        | RunOutcome.NormalExit (state, _) ->

        logger.LogInformation "Main method class now initialised"

        // Now that BCL initialisation has taken place and the user-code classes are constructed,
        // overwrite the main thread completely using the already-concretized method. The entry
        // thread Terminated during the cctor pump (its onlyRet body hit `ret`); we're resurrecting
        // it to run Main, so restore Status to Runnable before the scheduler is asked to pick again.
        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    dumped
                    concretizedMainMethod
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ofManagedObject arrayAllocation))
                    None
            with
            | Ok s -> s
            | Error _ -> failwith "TODO: I'd be surprised if this could ever happen in a valid program"

        let threadState =
            state.ThreadState.[mainThread]
            |> ThreadState.replaceFrames methodState
            |> fun threadState ->
                { threadState with
                    Status = ThreadStatus.Runnable
                }

        let state, init =
            { state with
                ThreadState = state.ThreadState |> Map.add mainThread threadState
            }
            |> IlMachineStateExecution.ensureTypeInitialised loggerFactory baseClassTypes mainThread mainTypeHandle

        match init with
        | WhatWeDid.SuspendedForClassInit -> failwith "TODO: suspended for class init"
        | WhatWeDid.BlockedOnClassInit _ -> failwith "logic error: surely this thread can't be blocked on class init"
        | WhatWeDid.ThrowingTypeInitializationException ->
            failwith "TypeInitializationException during entry point type initialisation"
        | WhatWeDid.Executed -> ()

        ProgramStartResult.Ready
            {
                State = state
                BaseClassTypes = baseClassTypes
                EntryThread = mainThread
                LastRan = mainThread
            }

    /// Returns the outcome of the program run: normal exit or unhandled guest exception.
    let run
        (loggerFactory : ILoggerFactory)
        (originalPath : string option)
        (fileStream : Stream)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        impls
        (argv : string list)
        : RunOutcome
        =
        let logger = loggerFactory.CreateLogger "Program"

        match prepare loggerFactory originalPath fileStream dotnetRuntimeDirs impls argv with
        | ProgramStartResult.CompletedBeforeMain outcome -> outcome
        | ProgramStartResult.Ready prepared -> pumpPrepared loggerFactory logger impls prepared
