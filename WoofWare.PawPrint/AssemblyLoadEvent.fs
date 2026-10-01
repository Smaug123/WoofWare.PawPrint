namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// What the start of a thread's step did about the assemblies it has still to announce.
[<RequireQualifiedAccess>]
type AssemblyLoadAnnouncement =
    /// A frame announcing an assembly is now the thread's active frame, and pushing it is the whole
    /// of the step.
    | Pushed of IlMachineState
    /// Nothing is due, so the step runs as usual, against this state.
    | NothingDue of IlMachineState

/// <summary>
/// Raising <c>AppDomain.AssemblyLoad</c>. CoreCLR does it from <c>AppDomain::RaiseLoadingAssemblyEvent</c>
/// by calling CoreLib's <c>AssemblyLoadContext.OnAssemblyLoad(RuntimeAssembly)</c> on the thread
/// that loaded the assembly; see <c>PendingAssemblyLoads</c> for when PawPrint does.
/// </summary>
[<RequireQualifiedAccess>]
module AssemblyLoadEvent =
    let private assemblyLoadContext
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        NativeRuntimeTypeHelpers.findCorelibType baseClassTypes "System.Runtime.Loader" "AssemblyLoadContext"

    /// <summary>
    /// Whether <c>AssemblyLoadContext.AssemblyLoad</c>, the static field behind
    /// <c>AppDomain.AssemblyLoad</c>, holds a delegate. <c>RaiseLoadingAssemblyEvent</c> runs no
    /// managed code at all unless it does.
    /// </summary>
    let private hasSubscriber
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : bool
        =
        let alc = assemblyLoadContext baseClassTypes

        // Subscribing runs the event's `add` accessor, which concretises the type, so a type never
        // concretised has nothing subscribed. Checking first, rather than concretising here, keeps
        // a run that never subscribes from acquiring a concrete type it would not otherwise have.
        match AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes alc.Identity with
        | None -> false
        | Some handle ->

        let field =
            match alc.Fields |> List.filter (fun f -> f.Name = "AssemblyLoad" && f.IsStatic) with
            | [ field ] -> field
            | fields ->
                failwith
                    $"System.Runtime.Loader.AssemblyLoadContext has %d{fields.Length} static fields named AssemblyLoad, not the one CoreCLR's FIELD__ASSEMBLYLOADCONTEXT__ASSEMBLY_LOAD names"

        match
            IlMachineState.getStatic
                (StaticOwner.forField thread field)
                handle
                (ComparableFieldDefinitionHandle.Make field.Handle)
                state
        with
        | None
        | Some (CliType.ObjectRef None) -> false
        | Some (CliType.ObjectRef (Some _)) -> true
        | Some other ->
            failwith
                $"AssemblyLoadContext.AssemblyLoad holds %O{other}, which is not an object reference; the field is a delegate"

    /// Push a frame calling `AssemblyLoadContext.OnAssemblyLoad` with `definitionFullName`'s
    /// `RuntimeAssembly` on top of the thread's active frame, and return that frame. The caller
    /// resumes where it was once the frame returns, with its evaluation stack untouched.
    let private pushAnnouncement
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (definitionFullName : string)
        (state : IlMachineState)
        : IlMachineState * FrameId
        =
        let onAssemblyLoad =
            match
                (assemblyLoadContext baseClassTypes).Methods
                |> List.filter (fun m -> m.Name = "OnAssemblyLoad" && m.IsStatic && MethodInfo.arity m = 1)
            with
            | [ m ] ->
                m
                |> MethodInfo.mapTypeGenerics (fun _ ->
                    failwith<TypeDefn> "System.Runtime.Loader.AssemblyLoadContext was unexpectedly generic"
                )
            | methods ->
                failwith
                    $"System.Runtime.Loader.AssemblyLoadContext has %d{methods.Length} static one-argument methods named OnAssemblyLoad, not the one CoreCLR's METHOD__ASSEMBLYLOADCONTEXT__ON_ASSEMBLY_LOAD names"

        let state, concretized, _declaringType =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                onAssemblyLoad
                None
                None
                state

        // `pAssembly->GetExposedObject()`: the cached object every other route to this assembly
        // reports.
        let assembly, state =
            NativeRuntimeTypeHelpers.getOrAllocateRuntimeAssembly loggerFactory baseClassTypes definitionFullName state

        // The argument goes on the active frame's evaluation stack and the call pops it straight
        // off again, so that frame's stack is as it was.
        let state =
            IlMachineState.pushToEvalStack (CliType.ObjectRef (Some assembly)) thread state

        let state, commitment =
            IlMachineStateExecution.callMethodWithCommitment
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                IlMachineStateExecution.CallDispatch.Direct
                false
                false // the active frame did not call this, so it has no call to step past
                IlMachineStateExecution.CallSiteTransition.StaysCooperative
                concretized.Generics
                concretized
                thread
                state.ThreadState.[thread]
                None
                ReturnValueDisposition.Discard
                (ExceptionEscape.SwallowedByRuntime "AppDomain::RaiseLoadingAssemblyEvent")
                state

        match commitment with
        | IlMachineStateExecution.CallCommitment.Committed -> state, state.ThreadState.[thread].ActiveMethodState
        | IlMachineStateExecution.CallCommitment.Raised
        | IlMachineStateExecution.CallCommitment.Aborted _ ->
            failwith
                $"Entering AssemblyLoadContext.OnAssemblyLoad to announce %s{definitionFullName} did not push its frame (%O{commitment}); CoreCLR swallows anything raised there, which PawPrint does not yet model"

    let private withPending
        (thread : ThreadId)
        (pending : PendingAssemblyLoads)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState = state.ThreadState.[thread]

        { state with
            ThreadState =
                state.ThreadState
                |> Map.add
                    thread
                    { threadState with
                        PendingAssemblyLoads = pending
                    }
        }

    /// <summary>
    /// At the start of a step of <paramref name="thread"/>: if an assembly it loaded is due to be
    /// announced, push the frame announcing it. Loads passed over because nothing is subscribed any
    /// more are forgotten.
    /// </summary>
    let tryAnnounce
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : AssemblyLoadAnnouncement
        =
        let threadState = state.ThreadState.[thread]

        if PendingAssemblyLoads.isEmpty threadState.PendingAssemblyLoads then
            AssemblyLoadAnnouncement.NothingDue state
        else

        let isLive (frame : FrameId) : bool =
            ThreadState.tryGetFrame frame threadState |> Option.isSome

        let rec go (pending : PendingAssemblyLoads) : AssemblyLoadAnnouncement =
            match PendingAssemblyLoads.tryTake isLive pending with
            | None -> withPending thread pending state |> AssemblyLoadAnnouncement.NothingDue
            | Some taken ->
                // `RaiseLoadingAssemblyEvent` checks for a subscriber afresh for every load, and an
                // earlier handler may have unsubscribed.
                if not (hasSubscriber baseClassTypes thread state) then
                    go (PendingAssemblyLoads.skipped taken)
                else
                    let loadedBefore = state._LoadedAssemblies.DefinitionNamesInLoadOrder.Length

                    let state, frame =
                        pushAnnouncement loggerFactory baseClassTypes thread taken.DefinitionFullName state

                    // Nothing records what pushing the announcement loaded, so it must load nothing.
                    // Everything it touches is in corelib.
                    if state._LoadedAssemblies.DefinitionNamesInLoadOrder.Length <> loadedBefore then
                        failwith
                            $"logic error: pushing the announcement of %s{taken.DefinitionFullName} on thread %O{thread} loaded an assembly"

                    withPending thread (PendingAssemblyLoads.announcedBy frame taken) state
                    |> AssemblyLoadAnnouncement.Pushed

        go threadState.PendingAssemblyLoads

    /// <summary>
    /// A step of <paramref name="thread"/> began in <paramref name="before"/> and ended in
    /// <paramref name="result"/>. If the step loaded an assembly that is to be announced, discard
    /// the step, keeping only the assemblies it loaded, and push the first announcement instead:
    /// the step runs afresh once every announcement has returned, and finds its assemblies loaded.
    /// </summary>
    /// <remarks>
    /// This is how an assembly is announced before anything in it runs, as on CoreCLR. The step
    /// that needed the assembly may already have started a type initialiser in it (an
    /// <c>ldsfld</c> claims the type's initialisation and pushes its <c>.cctor</c>), and a handler
    /// announced over that would be let through to the type's statics as a recursive access by the
    /// initialising thread. Discarding the step discards the claim. The step is a pure function
    /// of <paramref name="before"/>, so running it again with its assemblies already loaded does
    /// what it would have done, less the loading; any effect it asked the driver to perform is
    /// discarded with it and asked for again.
    ///
    /// Sound only because loading an image is idempotent and writes nothing but the load context,
    /// which is all that survives the discard. A step that *creates* an assembly, as
    /// <c>AppDomain_CreateDynamicAssembly</c> does, would create it afresh when run again, so such
    /// a step must announce what it created itself rather than reach here.
    /// </remarks>
    let announceBeforeStep
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (before : IlMachineState)
        (result : ExecutionResult)
        : ExecutionResult
        =
        let after =
            match result with
            | ExecutionResult.Stepped (state, _, _)
            | ExecutionResult.Terminated (state, _)
            | ExecutionResult.ProcessExit (state, _)
            | ExecutionResult.Aborted (state, _, _)
            | ExecutionResult.SignalTerminated (state, _, _)
            | ExecutionResult.UnhandledException (state, _, _) -> state

        let loadedBefore = before._LoadedAssemblies.DefinitionNamesInLoadOrder.Length
        let loadOrder = after._LoadedAssemblies.DefinitionNamesInLoadOrder

        // `RaiseLoadingAssemblyEvent` returns early for corelib.
        let loaded =
            [ for i in loadedBefore .. loadOrder.Length - 1 -> loadOrder.[i] ]
            |> List.filter (fun name -> name <> baseClassTypes.Corelib.DefinitionFullName)

        if loaded.IsEmpty || not (hasSubscriber baseClassTypes thread before) then
            result
        else

        let rolledBack =
            let state = before.WithLoadContextOf after

            withPending
                thread
                (PendingAssemblyLoads.record loaded state.ThreadState.[thread].PendingAssemblyLoads)
                state

        match tryAnnounce loggerFactory baseClassTypes thread rolledBack with
        | AssemblyLoadAnnouncement.Pushed state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)
        | AssemblyLoadAnnouncement.NothingDue _ ->
            failwith
                $"logic error: thread %O{thread} loaded %A{loaded} with AppDomain.AssemblyLoad subscribed, and the step was discarded to announce them, but nothing was announced"
