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

    /// <summary>
    /// At the start of a step of <paramref name="thread"/>: if an assembly it loaded is due to be
    /// announced, push the frame announcing it. Loads passed over because nothing is subscribed are
    /// forgotten.
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

        let withPending (pending : PendingAssemblyLoads) (state : IlMachineState) : IlMachineState =
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

        let rec go (pending : PendingAssemblyLoads) : AssemblyLoadAnnouncement =
            match PendingAssemblyLoads.tryTake isLive pending with
            | None -> withPending pending state |> AssemblyLoadAnnouncement.NothingDue
            | Some taken ->
                // `RaiseLoadingAssemblyEvent` returns early for corelib, and checks for a subscriber
                // afresh for every load.
                if
                    taken.DefinitionFullName = baseClassTypes.Corelib.DefinitionFullName
                    || not (hasSubscriber baseClassTypes thread state)
                then
                    go (PendingAssemblyLoads.skipped taken)
                else
                    let state, frame =
                        pushAnnouncement loggerFactory baseClassTypes thread taken.DefinitionFullName state

                    withPending (PendingAssemblyLoads.announcedBy frame taken) state
                    |> AssemblyLoadAnnouncement.Pushed

        go threadState.PendingAssemblyLoads

    /// <summary>
    /// After a step of <paramref name="thread"/>: record what the step loaded, for announcing at
    /// the start of the thread's next step. <paramref name="loadedBefore"/> is how many assemblies
    /// the load context held when the step began.
    /// </summary>
    let recordLoads
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (loadedBefore : int)
        (result : ExecutionResult)
        : ExecutionResult
        =
        let loadedDuring (state : IlMachineState) : string list =
            let loadOrder = state._LoadedAssemblies.DefinitionNamesInLoadOrder

            if loadOrder.Length < loadedBefore then
                failwith
                    $"logic error: the load context held %d{loadedBefore} assemblies before a step of thread %O{thread} and %d{loadOrder.Length} after it; assemblies are never unloaded"

            [ for i in loadedBefore .. loadOrder.Length - 1 -> loadOrder.[i] ]

        match result with
        | ExecutionResult.Stepped (state, whatWeDid, effect) ->
            match loadedDuring state with
            | [] -> result
            | loaded ->
                let threadState = state.ThreadState.[thread]

                let state =
                    { state with
                        ThreadState =
                            state.ThreadState
                            |> Map.add
                                thread
                                { threadState with
                                    PendingAssemblyLoads =
                                        PendingAssemblyLoads.record loaded threadState.PendingAssemblyLoads
                                }
                    }

                ExecutionResult.Stepped (state, whatWeDid, effect)
        | ExecutionResult.Terminated (state, _) ->
            // The thread has no next step to announce in. Nothing observes the omission unless
            // something is subscribed.
            match loadedDuring state with
            | [] -> result
            | loaded when hasSubscriber baseClassTypes thread state ->
                failwith
                    $"Thread %O{thread} loaded %A{loaded} in the step that ended it, so there is no later step of it in which to raise AppDomain.AssemblyLoad, which has a subscriber. CoreCLR would raise it on that thread before the thread ended; PawPrint does not yet model that"
            | _ -> result
        | ExecutionResult.ProcessExit _
        | ExecutionResult.Aborted _
        | ExecutionResult.SignalTerminated _
        | ExecutionResult.UnhandledException _ -> result
