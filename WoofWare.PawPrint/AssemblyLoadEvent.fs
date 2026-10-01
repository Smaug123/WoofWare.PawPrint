namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// Raising <c>AppDomain.AssemblyLoad</c>. CoreCLR does it from <c>AppDomain::RaiseLoadingAssemblyEvent</c>
/// by calling CoreLib's <c>AssemblyLoadContext.OnAssemblyLoad(RuntimeAssembly)</c> on the thread
/// that loaded the assembly, before anything in the assembly runs; see <c>announceBeforeStep</c>
/// for how PawPrint does the same.
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
    /// `RuntimeAssembly` on top of the thread's active frame. That frame resumes where it was once
    /// the new one returns, with its evaluation stack untouched.
    let private pushAnnouncement
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (definitionFullName : string)
        (state : IlMachineState)
        : IlMachineState
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
        | IlMachineStateExecution.CallCommitment.Committed -> state
        | IlMachineStateExecution.CallCommitment.Raised
        | IlMachineStateExecution.CallCommitment.Aborted _ ->
            failwith
                $"Entering AssemblyLoadContext.OnAssemblyLoad to announce %s{definitionFullName} did not push its frame (%O{commitment}); CoreCLR swallows anything raised there, which PawPrint does not yet model"

    /// <summary>
    /// A step of <paramref name="thread"/> began in <paramref name="before"/> and ended in
    /// <paramref name="result"/>. If the step loaded an assembly that is to be announced, discard
    /// the step and announce the first assembly it loaded instead, keeping that assembly loaded:
    /// once the announcement returns the step runs afresh from <paramref name="before"/>, finding
    /// that assembly already there.
    /// </summary>
    /// <remarks>
    /// This is how an assembly is announced before anything in it runs, as on CoreCLR.
    ///
    /// The step that needed the assembly may already have started a type initialiser in it (an
    /// <c>ldsfld</c> claims the type's initialisation and pushes its <c>.cctor</c>), and a handler
    /// announced over that would be let through to the type's statics as a recursive access by the
    /// initialising thread. Discarding the step discards the claim.
    ///
    /// Only the first assembly survives the discard, even if the step loaded more: had the rest
    /// survived too, the handler could run code in one of them before it was announced. Run again,
    /// the step loads the next one afresh and comes back here for it, unless a handler has needed
    /// it first and so had it announced inside itself, as CoreCLR does. Each pass leaves one more
    /// assembly loaded for good, so a step is discarded at most once per assembly it loads.
    ///
    /// The step is a pure function of <paramref name="before"/>, so running it again does what it
    /// would have done; any effect it asked the driver to perform is discarded with it and asked for
    /// again. That is sound only because loading an image is idempotent and writes nothing but the
    /// load context. A step that *creates* an assembly, as <c>AppDomain_CreateDynamicAssembly</c>
    /// does, would create it afresh when run again, so such a step must announce what it created
    /// itself rather than reach here.
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

        if loadOrder.Length < loadedBefore then
            failwith
                $"logic error: the load context held %d{loadedBefore} assemblies before a step of thread %O{thread} and %d{loadOrder.Length} after it; assemblies are never unloaded"

        // `RaiseLoadingAssemblyEvent` returns early for corelib.
        let firstToAnnounce =
            seq { loadedBefore .. loadOrder.Length - 1 }
            |> Seq.tryFind (fun i -> loadOrder.[i] <> baseClassTypes.Corelib.DefinitionFullName)

        match firstToAnnounce with
        | None -> result
        | Some _ when not (hasSubscriber baseClassTypes thread before) -> result
        | Some index ->

        let announced = loadOrder.[index]

        // Everything the step loaded up to and including `announced`, which is only ever corelib
        // besides it. Registered by definition identity alone: the step binds its references again
        // when it runs again.
        let state =
            seq { loadedBefore..index }
            |> Seq.fold
                (fun (state : IlMachineState) i ->
                    match after.LoadedAssembly loadOrder.[i] with
                    | Some assembly -> state.WithLoadedAssembly assembly
                    | None -> failwith $"logic error: %s{loadOrder.[i]} is in the load order but not the load context"
                )
                before

        let state = pushAnnouncement loggerFactory baseClassTypes thread announced state

        // Nothing would announce what pushing the announcement loaded, so it must load nothing.
        // Everything it touches is in corelib.
        if state._LoadedAssemblies.DefinitionNamesInLoadOrder.Length <> index + 1 then
            failwith $"logic error: pushing the announcement of %s{announced} on thread %O{thread} loaded an assembly"

        ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)
