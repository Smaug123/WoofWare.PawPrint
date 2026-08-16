namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Result of a step of exception dispatch.
type ExceptionDispatchResult =
    /// Dispatch has parked the machine somewhere the exception's handling continues, and the
    /// interpreter should carry on stepping: a catch handler body, a `filter` body the first
    /// pass is evaluating, or a `finally`/`fault` clause the second pass is running.
    ///
    /// Not called "handler found": three of those four destinations are not
    /// handlers, and under two-pass dispatch a `filter` can be entered long before anything is
    /// known about whether the exception will be caught at all.
    | Dispatched of IlMachineState
    /// The exception is unhandled and the second pass has finished unwinding: no frame on the
    /// thread had a handler, and every `finally`/`fault` between the throw point and the
    /// outermost frame has now run.
    | ExceptionUnhandled of IlMachineState * CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

/// Exception handler dispatch that requires IlMachineState for type resolution.
[<RequireQualifiedAccess>]
module ExceptionDispatching =

    /// <summary>
    /// The type a <c>catch</c> clause of <paramref name="method"/> names. For a dynamic method's
    /// clause this is the handle resolved when the method was first prepared for execution, not
    /// whatever its scope holds at throw time.
    /// </summary>
    let private catchClauseType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        // Passed in rather than derived from `method` here, though it is exactly
        // `assemblyOfMethod state method`: that lookup is by `AssemblyName`, which rebuilds a full
        // name string, and this runs once per covering clause where the caller runs once per frame.
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        (catchType : ExceptionCatchType)
        : IlMachineState * ConcreteTypeHandle
        =
        match catchType with
        | ExceptionCatchType.FromMetadata catchTypeToken ->
            let state, catchTypeDefn, catchAssy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    typeGenerics
                    catchTypeToken

            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                catchAssy.Name
                typeGenerics
                methodGenerics
                catchTypeDefn
        | ExceptionCatchType.FromDynamicScope index ->
            // This arm is a lookup and not a resolution: a dynamic method's clause types were all
            // resolved when the method was first prepared for execution, which is where CoreCLR's
            // JIT resolves them (see DynamicMethodExecution.concretize). Resolving one here instead
            // would read the scope as it stands during the *throw*, and a guest that rewrote the
            // slot in between is measured not to be heard by real .NET.
            let handle =
                match method.SynthesisedKind with
                | Some (SynthesisedMethod.DynamicMethod handle) -> handle
                | _ ->
                    failwith
                        $"BUG: a catch clause of %s{method.Name} names DynamicScope entry %d{index}, but that method is not a dynamic method; only a body read off a DynamicResolver can carry such a clause"

            let definition =
                MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
                |> Option.defaultWith (fun () ->
                    failwith
                        $"exception dispatch through %s{method.Name}: %O{handle} is not registered in the method-handle registry"
                )

            let prepared =
                definition.GetPreparation ()
                |> Option.defaultWith (fun () ->
                    failwith
                        $"BUG: exception dispatch is examining a catch clause of %s{method.Name}, which has never been prepared for execution; a frame cannot exist for a method that was not prepared"
                )

            let handle =
                prepared.CatchTypes
                |> Map.tryFind index
                |> Option.defaultWith (fun () ->
                    failwith
                        $"BUG: a catch clause of %s{method.Name} names DynamicScope entry %d{index}, which was not resolved when the method was prepared; preparation resolves every clause of the body"
                )

            state, handle

    /// Check if an exception type matches a catch handler type.
    let private isExceptionAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        (catchType : ExceptionCatchType)
        : IlMachineState * bool
        =
        let state, catchTypeHandle =
            catchClauseType loggerFactory baseClassTypes state activeAssy typeGenerics methodGenerics method catchType

        IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state exceptionType catchTypeHandle

    let private exceptionFilterRegion (filterOffset : int) (handlerOffset : ExceptionOffset) : ExceptionFilterRegion =
        {
            FilterOffset = filterOffset
            HandlerOffset = handlerOffset
        }

    let private isSkippedFilter
        (skippedFilters : ExceptionFilterRegion list)
        (filterOffset : int)
        (handlerOffset : ExceptionOffset)
        : bool
        =
        let currentFilter = exceptionFilterRegion filterOffset handlerOffset
        skippedFilters |> List.contains currentFilter

    let internal exceptionObjectType
        (state : IlMachineState)
        (exceptionObject : ManagedHeapAddress)
        : ConcreteTypeHandle
        =
        ManagedHeap.getObjectConcreteType exceptionObject state.ManagedHeap

    let internal tryCurrentCatchException
        (methodState : MethodState)
        : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        match MethodInfo.tryIlBody methodState.ExecutingMethod with
        | None -> None
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.indexed
            |> Seq.choose (fun (regionIndex, region) ->
                match region with
                | ExceptionRegion.Catch (_, offset)
                | ExceptionRegion.Filter (_, offset) when
                    ExceptionHandling.isInHandlerBody methodState.IlOpIndex offset
                    ->
                    methodState.CatchExceptions
                    |> Map.tryFind offset
                    |> Option.map (fun exn -> regionIndex, offset, exn)
                | _ -> None
            )
            // Innermost handler wins: shortest handler body, then highest start offset, then metadata order.
            |> Seq.sortBy (fun (regionIndex, offset, _) -> offset.HandlerLength, -offset.HandlerOffset, regionIndex)
            |> Seq.tryHead
            |> Option.map (fun (_, _, exn) -> exn)

    /// The assembly whose metadata a method's own tokens are to be resolved against.
    let private assemblyOfMethod
        (state : IlMachineState)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        : DumpedAssembly
        =
        // Read from the method rather than from `state.ActiveAssembly`, which answers for
        // whichever frame the thread happens to be executing. Those coincided while handler
        // search only ever looked at the active frame; the first pass below searches frames the
        // thread is *not* in, so relying on the coincidence would resolve a `catch`'s type token
        // against the wrong assembly as soon as an exception crossed an assembly boundary.
        let name = method.DeclaringAssembly

        match state.LoadedAssembly name with
        | Some assy -> assy
        | None ->
            let available = state._LoadedAssemblies.DefinitionNames |> String.concat " ; "

            failwith
                $"Exception dispatch searching %s{method.Name} needs its declaring assembly %O{name}, which is not loaded; loaded assemblies are: %s{available}"

    /// Whether this frame is still waiting on its prologue, and so has executed nothing.
    ///
    /// Such a frame has no exception-handling regions in scope. The CLR emits the
    /// type-initialisation check outside the method's EH regions, so the
    /// `TypeInitializationException` it raises goes to the *caller* — measured on .NET 10, a
    /// method whose whole body is `try { … } catch (TypeInitializationException)` does not catch
    /// its own failure. PawPrint raises it with the frame already established, which is what lets
    /// the trace name the method, so without this the frame's own clauses would be candidates.
    ///
    /// The frame stays on the stack and in the trace either way; it is only its clauses that are
    /// out of scope, for both passes. A frame that never began has no `finally` to run either.
    let private hasNotStarted (frame : MethodState) : bool = frame.PendingTypeInit.IsSome

    /// The clause of `method` that accepts this exception at `currentPC`, if any: an assignable
    /// `catch`, or a `filter` whose body has not already run and rejected.
    ///
    /// `finally` and `fault` are invisible here: cleanup clauses are not candidates for
    /// *receiving* an exception — they run on the way to whichever clause does — so a first pass
    /// that could return one would be answering a different question from the one its callers
    /// ask (issue #865 is the failure mode).
    ///
    /// When several clauses cover `currentPC`, the innermost wins: smallest `try`, then metadata
    /// order, which ECMA-335 II.25.4.6 requires to list more deeply nested clauses first.
    let private findAcceptingClause
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentPC : int)
        (exceptionType : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        (skippedFilters : ExceptionFilterRegion list)
        : IlMachineState * WoofWare.PawPrint.ExceptionRegion option
        =
        match MethodInfo.tryIlBody method with
        | None -> state, None
        | Some instructions ->

        let activeAssy = assemblyOfMethod state method

        let covers (offset : ExceptionOffset) =
            currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength

        let state, matches =
            ((state, []), instructions.ExceptionRegions |> Seq.indexed)
            ||> Seq.fold (fun (state, acc) (regionIndex, region) ->
                match region with
                | ExceptionRegion.Catch (catchType, offset) ->
                    if covers offset then
                        let state, matches =
                            isExceptionAssignableTo
                                loggerFactory
                                baseClassTypes
                                state
                                activeAssy
                                method.DeclaringTypeGenerics
                                method.Generics
                                exceptionType
                                method
                                catchType

                        if matches then
                            state, (regionIndex, region) :: acc
                        else
                            state, acc
                    else
                        state, acc
                | ExceptionRegion.Filter (filterOffset, offset) ->
                    if covers offset && not (isSkippedFilter skippedFilters filterOffset offset) then
                        state, (regionIndex, region) :: acc
                    else
                        state, acc
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ -> state, acc
            )

        let result =
            matches
            |> List.sortBy (fun (regionIndex, region) -> (ExceptionHandling.regionOffset region).TryLength, regionIndex)
            |> List.tryHead
            |> Option.map snd

        state, result

    /// Enter a catch handler: set PC to the handler offset, clear eval stack, preserve any
    /// outer continuation frames, and push the exception object reference. Callers are
    /// responsible for stack hygiene: ordinary catches should enter with an empty stack,
    /// while catches nested inside filter/finally/fault evaluation must keep the outer frame
    /// for the eventual endfilter/endfinally.
    let enterCatchHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (offset : ExceptionOffset)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState
        =
        let newMethodState =
            methodState
            |> MethodState.setProgramCounter offset.HandlerOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearPendingPrefix
            |> MethodState.setCatchException offset cliException
            |> MethodState.pushToEvalStack' (EvalStackValue.ObjectRef cliException.ExceptionObject)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter a filter block: set PC to the filter offset, clear eval stack, push the exception
    /// object reference, and park the first-pass search so `endfilter` can resume it.
    let private enterFilterHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (search : ExceptionSearchState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (filterOffset : int)
        (handlerOffset : ExceptionOffset)
        : IlMachineState
        =
        let currentFilter = exceptionFilterRegion filterOffset handlerOffset

        let continuation : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                CurrentFilter = currentFilter
                Search = search
            }

        let newMethodState =
            methodState
            |> MethodState.setProgramCounter filterOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearPendingPrefix
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FilterHandler currentFilter)
                (ExceptionContinuation.ResumeAfterFilter continuation)
            |> MethodState.pushToEvalStack' (EvalStackValue.ObjectRef search.Exception.ExceptionObject)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter a finally handler: set PC to the handler offset, clear eval stack,
    /// park the second-pass unwind so `endfinally` can resume it.
    let enterFinallyHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (offset : ExceptionOffset)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState
        =
        let newMethodState =
            methodState
            |> MethodState.setProgramCounter offset.HandlerOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearPendingPrefix
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FinallyHandler offset)
                (ExceptionContinuation.PropagatingException unwind)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter a fault handler: set PC to the handler offset, clear eval stack,
    /// park the second-pass unwind so `endfinally` can resume it.
    let enterFaultHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (offset : ExceptionOffset)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState
        =
        let newMethodState =
            methodState
            |> MethodState.setProgramCounter offset.HandlerOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearPendingPrefix
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FaultHandler offset)
                (ExceptionContinuation.PropagatingException unwind)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter the cleanup clause `region` of the currently-active frame, parking `unwind` on it.
    let private enterCleanupHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (region : ExceptionRegion)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState
        =
        match region with
        | ExceptionRegion.Finally offset ->
            enterFinallyHandler currentThread methodState threadState state offset unwind
        | ExceptionRegion.Fault offset -> enterFaultHandler currentThread methodState threadState state offset unwind
        | ExceptionRegion.Catch _
        | ExceptionRegion.Filter _ ->
            failwith
                $"Logic error: the second pass of exception dispatch selected region %O{region} of %s{methodState.ExecutingMethod.Name} to run as cleanup, but only Finally and Fault are cleanup clauses"

    /// Write a raise's accumulated frames onto the exception object, into both of the sinks a
    /// guest can read them from: `_stackTraceString`, and the frozen `_stackTrace` token that
    /// `Exception.HasBeenThrown` and `ExceptionDispatchInfo` key off.
    let private projectStackTrace
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.setExceptionStackTraceString
            loggerFactory
            corelib
            cliException.ExceptionObject
            cliException.StackTrace
            state
        |> IlMachineState.recordThrownStackTrace
            loggerFactory
            corelib
            cliException.ExceptionObject
            cliException.StackTrace


    /// Mark the last frame of `frames` as the end of an earlier throw's trace. The empty list is a
    /// fixed point, which is CoreCLR's `numCurrentFrames > 0` guard (excep.cpp:3093) and is what
    /// gives an `ExceptionDispatchInfo` captured from a never-thrown exception a trace with no
    /// boundary in it.
    let private markLastFrameAsForeign
        (frames : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        =
        match List.rev frames with
        | [] -> []
        | last :: earlierReversed ->
            let last =
                { last with
                    IsLastFrameFromForeignExceptionStackTrace = true
                }

            List.rev (last :: earlierReversed)

    /// If `currentThread` has a pending foreign-raise flag, clear it and return the frames the
    /// exception already carried, with the last marked as the end of that earlier trace, so that
    /// the frame about to be appended continues them instead of starting a new trace. `None` means
    /// no flag was pending and the caller's own frames stand unchanged.
    ///
    /// This is `StackTraceInfo::AppendElement`'s read-and-reset (excep.cpp:3016-3017, 3087-3099),
    /// and the placement matters as much as the marking: CoreCLR consumes the flag when a frame is
    /// *appended*, not when a raise is *initiated*. A `rethrow` whose handler turns out to live in
    /// the same method appends nothing, so it leaves the flag pending for the next raise — measured
    /// on .NET 10, and covered by `sourcesPure/ForeignRaiseFlagSurvivesFramelessRethrow.cs`. Hence
    /// the two callers are PawPrint's two frame-append sites, not its two dispatch entry points.
    ///
    /// The read is unconditional at every append, exactly as CoreCLR's is, with no notion of "this
    /// raise's first append". Within one raise that is safe because its appends all happen in the
    /// first pass, before any cleanup clause runs: guest code that sets the flag from a `finally`
    /// cannot have it consumed by the raise it is unwinding, because that raise finished appending
    /// before the `finally` started. `sourcesPure/ForeignRaiseFlagSetInFinally.cs` and
    /// `ForeignRaiseFlagPendingBeforeCleanup.cs` differ only in *when* the flag is set and pin the
    /// two sides of that. The one place guest code does run between two appends of one raise is a
    /// `filter`, which is precisely where CoreCLR would also let a flag be consumed — so the
    /// unconditional read is what makes that case come out right rather than a gap in it.
    ///
    /// There is a third append site where cleanup *has* already run: the seed a wrapping boundary
    /// gives its synthesised wrapper (`deliverToTarget`), which the second pass reaches only after
    /// running the wrapping frame's cleanup. That is not an exception to the rule but a different
    /// raise — CoreCLR's own wrap is managed `throw new TargetInvocationException(e)`, whose first
    /// append performs the same unconditional read — so the answer is right for the same reason.
    /// `docs/plans/2026-08-11-two-pass-exception-dispatch.md` records why this answer is the
    /// faithful one.
    ///
    /// The frames come from the exception object rather than from any in-flight list, because
    /// CoreCLR re-reads `_stackTrace` at every append. That is observable: a nested throw of the
    /// *same* object from inside a catch updates the token while the outer handler's snapshot goes
    /// stale, and only the token still holds the nested boundary.
    ///
    /// `framesAlreadyPresent` is a thunk because only the flag-set path may evaluate it: the reader
    /// fails loudly on an exception address that is not a real heap object, which is exactly what
    /// the skeletal states in low-level dispatch tests use.
    let private consumeForeignExceptionRaise
        (currentThread : ThreadId)
        (framesAlreadyPresent :
            unit -> ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        (state : IlMachineState)
        : IlMachineState * ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list option
        =
        let threadState = state.ThreadState.[currentThread]

        if not threadState.IsRaisingForeignException then
            state, None
        else

        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add
                        currentThread
                        { threadState with
                            IsRaisingForeignException = false
                        }
            }

        state, framesAlreadyPresent () |> markLastFrameAsForeign |> Some

    /// True iff a frame boundary crossing into `caller` appends no stack-trace frame.
    ///
    /// A delegate's `Invoke` is a stub, not a managed method: real .NET has no frame for it, and
    /// an exception crossing a delegate call reports the target and then whoever called `Invoke`.
    /// PawPrint's ordinary delegate path gets that for free, because `dispatchDelegateInvoke`
    /// pops its synthetic frame before calling the target — so the frame is already gone by the
    /// time anything can throw. The exception is class initialisation, which runs
    /// *while* that frame is still active so the instruction can be retried after the `.cctor`
    /// returns; without this, a `.cctor` that throws would report a `System.Action.Invoke` frame
    /// that no real trace contains.
    ///
    /// Only `DelegateInvoke` is suppressed, not runtime-provided frames at large: an InternalCall
    /// or QCall *is* a managed method by name and real traces do show it.
    let private isDelegateInvokeStub (caller : MethodState) : bool =
        match caller.ExecutingMethod.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> true
        | _ -> false

    /// Append the caller's frame to a raise's trace as the first pass crosses into it, consuming
    /// any pending foreign-raise flag in the process.
    ///
    /// A suppressed delegate-`Invoke` stub appends nothing, so it consumes nothing either: real
    /// .NET has no `StackTraceElement` for that stub and hence no `AppendElement` call to read
    /// the flag, which stays pending for the next genuine frame the raise reaches.
    let private appendCallerFrame
        (currentThread : ThreadId)
        (caller : MethodState)
        (callSitePC : int)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        if isDelegateInvokeStub caller then
            state, cliException
        else

        let stackFrame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Method = caller.ExecutingMethod
                IlOffset = callSitePC
                // The frame being appended belongs to the raise in progress, never to an earlier
                // one — CoreCLR sets `stackTraceElem.flags = 0` here for the same reason
                // (excep.cpp:3045). A pending flag marks the frame *before* this one.
                IsLastFrameFromForeignExceptionStackTrace = false
            }

        let state, restoredFrames =
            state
            |> consumeForeignExceptionRaise currentThread (fun () -> cliException.StackTrace)

        let framesBefore = restoredFrames |> Option.defaultValue cliException.StackTrace

        state,
        { cliException with
            StackTrace = framesBefore @ [ stackFrame ]
        }

    /// The innermost `filter` clause of this frame that is currently being evaluated, if any.
    let private activeFilterOf
        (methodState : MethodState)
        : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        // Scanning the continuation stack rather than reading only its top is what lets the first
        // pass abandon an exception at a filter boundary: the first pass asks before running any
        // cleanup, so a cleanup scope belonging to a superseded raise can sit above the filter
        // that still owns the frame.
        methodState.ExceptionContinuations
        |> List.tryPick (fun frame ->
            match frame.Scope, frame.Continuation with
            | ExceptionContinuationScope.FilterHandler _, ExceptionContinuation.ResumeAfterFilter continuation ->
                Some continuation
            | _ -> None
        )

    /// What one run of the first pass did.
    type private FirstPassResult =
        /// The search reached a verdict. Frames may have been appended to the trace, but no
        /// frame was popped and no guest code ran.
        | SearchConcluded of
            ExceptionSearchState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
            ExceptionSearchOutcome
        /// The search met a `filter` and entered its body; the machine is parked there with the
        /// walk state on that frame's continuation, and `endfilter` resumes it.
        | SearchSuspendedInFilter

    /// Walk frames outward from `search.Frame` looking for a clause that accepts the exception,
    /// running each `filter` it meets in place — with the inner frames still live, which is what
    /// CoreCLR does and what makes a filter observe a `finally` it precedes as not-yet-run.
    let rec private firstPass
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (search : ExceptionSearchState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState * FirstPassResult
        =
        let threadState = state.ThreadState.[currentThread]
        let frame = ThreadState.getFrame search.Frame threadState

        let state, accepting =
            if hasNotStarted frame then
                state, None
            else

            findAcceptingClause
                loggerFactory
                corelib
                state
                search.SearchPC
                search.ExceptionType
                frame.ExecutingMethod
                search.SkippedFilters

        match accepting with
        | Some (ExceptionRegion.Catch _ as region) ->
            state, FirstPassResult.SearchConcluded (search, ExceptionSearchOutcome.CaughtAt (search.Frame, region))
        | Some (ExceptionRegion.Filter (filterOffset, handlerOffset)) ->
            // Guest code is about to read this exception, and CoreCLR has appended every frame
            // the search reached so far — measured on .NET 10: a `when` clause sees a trace
            // ending at its own frame. Project before entering, or a filter observes
            // `StackTrace == null` on an exception that has been thrown, and
            // `Exception.HasBeenThrown`, which keys off the frozen token, answers false.
            let state = projectStackTrace loggerFactory corelib search.Exception state

            // The filter runs in its own frame while every frame inner to it stays live: this is
            // a *search*, not an unwind, and the second pass still has those frames to walk.
            let threadState =
                state.ThreadState.[currentThread] |> ThreadState.setActiveFrame search.Frame

            let frame = ThreadState.getFrame search.Frame threadState

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread threadState
                }

            let state =
                enterFilterHandler currentThread frame threadState state search filterOffset handlerOffset

            state, FirstPassResult.SearchSuspendedInFilter
        | Some ((ExceptionRegion.Finally _ | ExceptionRegion.Fault _) as region) ->
            failwith
                $"Logic error: the first pass of exception dispatch selected cleanup region %O{region} of %s{frame.ExecutingMethod.Name} as an accepting clause; only Catch and Filter can accept"
        | None ->

        match activeFilterOf frame with
        | Some _ ->
            // No clause of this frame accepts, and the frame is mid-filter, so this is where the
            // exception leaves that filter. The CLR catches an exception that escapes a filter
            // at the filter boundary and reports the filter as false.
            state, FirstPassResult.SearchConcluded (search, ExceptionSearchOutcome.AbandonedAtFilter search.Frame)
        | None ->

        match frame.ReturnState with
        | None -> state, FirstPassResult.SearchConcluded (search, ExceptionSearchOutcome.NoHandler)
        | Some returnState ->

        if
            returnState.WasInitialisingType.IsSome
            || returnState.WrapExceptionInTargetInvocation
        then
            // Leaving this frame changes the exception's *type*, so every outer frame must be
            // searched against the wrapper instead. The walk cannot see past it; the second pass
            // unwinds to here, wraps, and starts a fresh first pass at the caller.
            state, FirstPassResult.SearchConcluded (search, ExceptionSearchOutcome.WrappedAt search.Frame)
        else

        let caller = ThreadState.getFrame returnState.JumpTo threadState

        let state, cliException =
            appendCallerFrame currentThread caller returnState.CallSiteIlOpIndex search.Exception state

        // Search the caller at the *call-site* PC, not at its resumed `IlOpIndex`: the latter has
        // already been advanced past the call/callvirt/newobj, which can place it outside the
        // protected region when the call is the last instruction in a `try`.
        let search =
            { search with
                Exception = cliException
                Frame = returnState.JumpTo
                SearchPC = returnState.CallSiteIlOpIndex
                SkippedFilters = []
            }

        firstPass loggerFactory corelib state currentThread search

    /// The IL offset inside the target frame at which unwinding stops. `None` means the
    /// exception is leaving that frame altogether, so every covering cleanup clause runs.
    let private unwindBoundaryIn (frame : MethodState) (outcome : ExceptionSearchOutcome) : int option =
        match outcome with
        | ExceptionSearchOutcome.CaughtAt (_, ExceptionRegion.Catch (_, offset))
        | ExceptionSearchOutcome.CaughtAt (_, ExceptionRegion.Filter (_, offset)) -> Some offset.HandlerOffset
        | ExceptionSearchOutcome.CaughtAt (_, ((ExceptionRegion.Finally _ | ExceptionRegion.Fault _) as region)) ->
            failwith
                $"Logic error: the first pass of exception dispatch concluded that cleanup region %O{region} of %s{frame.ExecutingMethod.Name} caught an exception; only Catch and Filter can catch"
        | ExceptionSearchOutcome.AbandonedAtFilter _ ->
            // The exception dies at the filter's boundary, so cleanup inside the filter body runs
            // and anything enclosing the filter clause does not.
            //
            // One shape this under-runs: a cleanup clause whose `try` begins at exactly
            // `FilterOffset` covers that offset and so is excluded, though it too is being
            // abandoned wholesale. It needs hand-written IL — C#'s `when` puts any `try` a filter
            // contains in a *callee* frame — and unlike the other filter-internal EH corners here
            // it fails quietly rather than loudly, because it is a clause not run rather than a
            // state the code cannot describe. Recorded rather than handled: what the CLR does with
            // it is unmeasured, and inventing an answer would be worse than naming the gap.
            match activeFilterOf frame with
            | Some continuation -> Some continuation.CurrentFilter.FilterOffset
            | None ->
                failwith
                    $"Logic error: the first pass of exception dispatch abandoned an exception at a filter of %s{frame.ExecutingMethod.Name}, but by the time the second pass reached that frame it had no filter under evaluation"
        | ExceptionSearchOutcome.WrappedAt _
        | ExceptionSearchOutcome.NoHandler -> None

    /// Pop this frame's exception continuations down to and including the innermost
    /// `FilterHandler`, which is the scope the abandoned exception is leaving, and return the
    /// search parked on it.
    ///
    /// Anything above it belongs to a raise that the abandoned one superseded — a `finally` of
    /// the filter body that was still running when this exception displaced it — and is
    /// discarded along with the frame's filter evaluation. Meeting a non-cleanup scope on the
    /// way is a logic error rather than a shape the CLR can produce.
    let rec private popToActiveFilter
        (methodState : MethodState)
        : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * MethodState
        =
        match MethodState.popExceptionContinuation methodState with
        | None, _ ->
            failwith
                $"Logic error: unwinding to the filter boundary of %s{methodState.ExecutingMethod.Name} found no exception continuation to reject"
        | Some {
                   Scope = ExceptionContinuationScope.FilterHandler currentFilter
                   Continuation = ExceptionContinuation.ResumeAfterFilter continuation
               },
          popped ->
            if currentFilter <> continuation.CurrentFilter then
                failwith
                    $"Logic error: filter continuation scope %O{currentFilter} of %s{methodState.ExecutingMethod.Name} does not match its continuation's filter %O{continuation.CurrentFilter}"

            continuation, popped
        | Some {
                   Scope = (ExceptionContinuationScope.FinallyHandler _ | ExceptionContinuationScope.FaultHandler _)
               },
          popped -> popToActiveFilter popped
        | Some frame, _ ->
            failwith
                $"Logic error: unwinding to the filter boundary of %s{methodState.ExecutingMethod.Name} met scope %O{frame.Scope} with continuation %O{frame.Continuation}, which is neither a filter nor cleanup"

    /// Interpose whichever synthesised wrappers this frame boundary carries, in the CLR's order:
    /// a throwing `.cctor` surfaces as `TypeInitializationException`, and an
    /// `Activator.CreateInstance<T>()` ctor is then additionally wrapped in
    /// `TargetInvocationException`.
    ///
    /// The two normally land on *different* boundaries — the `.cctor` frame and the constructor
    /// frame beneath it — because a type is initialised from its callee's prologue. One frame
    /// carrying both is nevertheless handled here rather than ruled out, since which flags a
    /// return state carries is not this function's to decide.
    ///
    /// Each wrapper is seeded with the frame that raises it, exactly as `throwExceptionObject`
    /// seeds an ordinary `throw`, because in CoreCLR that is literally what it is: the wrap is
    /// managed `throw new TargetInvocationException(e)` running in the catcher's frame. Measured
    /// on .NET 10, `Activator.CreateInstance<T>()` over a throwing `.cctor` gives the
    /// `TypeInitializationException` a trace ending at `RuntimeType.CreateInstanceOfT` and the
    /// `TargetInvocationException` one beginning there — the two meet at the wrapping frame.
    /// PawPrint inlines the `Activator` intrinsic and so has no `CreateInstanceOfT` frame of its
    /// own; the call site in the frame that invoked `Activator.CreateInstance<T>()` stands in for
    /// it, which is the substitution the outer `TargetInvocationException` already made.
    let private applyFrameWraps
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (returnState : MethodReturnState)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        : IlMachineState * CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * ConcreteTypeHandle
        =
        /// Give a freshly synthesised wrapper the one frame it is raised from. The caller frame is
        /// re-read from `state` at each use because a wrap allocates on the heap in between.
        let seedWrapper
            (state : IlMachineState)
            (wrapper : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : IlMachineState * CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            =
            let caller =
                ThreadState.getFrame returnState.JumpTo state.ThreadState.[currentThread]

            appendCallerFrame currentThread caller returnState.CallSiteIlOpIndex wrapper state

        // If this frame was running a .cctor, mark the type initialisation as failed and wrap the
        // exception in TypeInitializationException (CLR behaviour). Synthesize the TIE first so
        // we can cache it; repeated accesses rethrow the same instance (matching CLR identity
        // semantics).
        let state, cliException, exceptionType =
            match returnState.WasInitialisingType with
            | None -> state, cliException, exceptionType
            | Some finishedInitialising ->
                let typeFullName =
                    match AllConcreteTypes.lookup finishedInitialising state.ConcreteTypes with
                    | Some ct ->
                        let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
                        Assembly.fullName assy ct.Identity
                    | None ->
                        failwith
                            $"Logic error: failed to look up ConcreteType for initialising-type handle %O{finishedInitialising} when synthesising TypeInitializationException"

                // The exception being wrapped is *not* re-projected here.
                // `concludeFirstPass` has already
                // frozen its completed trace, so a write here could only be a no-op or a
                // regression: the second pass runs the wrapping frame's cleanup before arriving,
                // and guest code there may have moved the object's token on —
                // `RestoreDispatchState` assigns `_stackTrace` directly — which re-projecting the
                // search's older frames would clobber.
                let tieAddr, tieType, state =
                    IlMachineState.synthesizeTypeInitializationException
                        loggerFactory
                        corelib
                        typeFullName
                        cliException.ExceptionObject
                        state

                let state =
                    state.WithTypeFailedInit currentThread finishedInitialising tieAddr tieType

                let state, wrapped =
                    seedWrapper
                        state
                        {
                            ExceptionObject = tieAddr
                            StackTrace = []
                        }

                state, wrapped, tieType

        // If this frame was the ctor target of `Activator.CreateInstance<T>()` (or any other
        // CreateInstanceOfT-style invocation that opts in via `WrapExceptionInTargetInvocation`),
        // wrap the in-flight exception in a fresh `TargetInvocationException` whose
        // `_innerException` field points at the original. This mirrors CoreCLR's
        // `try { ctor } catch (Exception e) { throw new TargetInvocationException(e); }` wrap
        // around `cache.CallRefConstructor` in `RuntimeType.CreateInstanceOfT` without
        // synthesising an extra trampoline frame: the wrap only fires on unwind across this
        // frame's boundary, so a try/catch *inside* the ctor that handles the exception is
        // unaffected.
        if not returnState.WrapExceptionInTargetInvocation then
            state, cliException, exceptionType
        else

        // When both wraps fire on one boundary, `cliException` here is the
        // `TypeInitializationException` synthesised a few lines up. Its dispatch ends at this very
        // frame — it is caught and never propagates further — so freeze its trace now, for the
        // reason `concludeFirstPass` freezes a search that reached a handler. Doing it here rather
        // than at birth matches an ordinary raise, which is seeded by `throwExceptionObject` and
        // projected only once its search concludes.
        //
        // Only a wrapper *we* synthesised is projected. An original exception arriving here was
        // already frozen by `concludeFirstPass`, and re-projecting it could clobber a newer token
        // written by guest cleanup, as the comment on the first wrap explains.
        let state =
            match returnState.WasInitialisingType with
            | None -> state
            | Some _ -> projectStackTrace loggerFactory corelib cliException state

        let tieAddr, tieType, state =
            IlMachineState.synthesizeTargetInvocationException loggerFactory corelib cliException.ExceptionObject state

        let state, wrapped =
            seedWrapper
                state
                {
                    ExceptionObject = tieAddr
                    StackTrace = []
                }

        state, wrapped, tieType

    /// Run the first pass to a verdict, then the second pass to the frame it names.
    ///
    /// Splitting these is the whole of issue #865. CoreCLR appends every stack-trace frame and
    /// runs every `filter` before a single `finally` executes, so managed code running in a
    /// cleanup clause — or holding an exception the clause displaced — reads a *complete* trace.
    let rec private runFirstPass
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (search : ExceptionSearchState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : ExceptionDispatchResult
        =
        match firstPass loggerFactory corelib state currentThread search with
        | state, FirstPassResult.SearchSuspendedInFilter -> ExceptionDispatchResult.Dispatched state
        | state, FirstPassResult.SearchConcluded (search, outcome) ->
            concludeFirstPass loggerFactory corelib state currentThread search outcome

    /// Freeze the completed trace onto the exception object and begin the second pass.
    ///
    /// The projection happens for *every* outcome, not only when a handler was found. An
    /// exception that goes unhandled, one abandoned at a filter boundary, and one about to be
    /// swallowed by a synthesised wrapper have all propagated, and a guest holding any
    /// of them can read `StackTrace` afterwards.
    and private concludeFirstPass
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (search : ExceptionSearchState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (outcome : ExceptionSearchOutcome)
        : ExceptionDispatchResult
        =
        let state = projectStackTrace loggerFactory corelib search.Exception state

        let unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Exception = search.Exception
                ExceptionType = search.ExceptionType
                Frame = search.StartFrame
                PC = search.StartPC
                Target = outcome
            }

        secondPass loggerFactory corelib state currentThread unwind

    /// Unwind from the throw point to the frame the first pass named, running each `finally` and
    /// `fault` clause in between, and then deliver the exception to whatever the outcome says.
    and private secondPass
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : ExceptionDispatchResult
        =
        // The first pass runs filters in place, so it can leave the thread executing an outer
        // frame; the unwind starts back at the throw point regardless.
        let threadState =
            state.ThreadState.[currentThread] |> ThreadState.setActiveFrame unwind.Frame

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        let frame = ThreadState.getFrame unwind.Frame threadState

        let targetFrame =
            match unwind.Target with
            | ExceptionSearchOutcome.CaughtAt (frameId, _)
            | ExceptionSearchOutcome.AbandonedAtFilter frameId
            | ExceptionSearchOutcome.WrappedAt frameId -> Some frameId
            | ExceptionSearchOutcome.NoHandler -> None

        let atTarget = targetFrame = Some unwind.Frame

        let boundary =
            if atTarget then
                unwindBoundaryIn frame unwind.Target
            else
                None

        let regions =
            if hasNotStarted frame then
                // Nothing of this frame has run, so nothing of it needs cleaning up. Same scope
                // rule the first pass applied when declining to offer its `catch` clauses.
                Seq.empty
            else

            match MethodInfo.tryIlBody frame.ExecutingMethod with
            | None -> Seq.empty
            | Some instructions -> instructions.ExceptionRegions :> seq<_>

        match
            ExceptionHandling.cleanupRegionsBetween regions unwind.PC boundary
            |> List.tryHead
        with
        | Some region ->
            enterCleanupHandler currentThread frame threadState state region unwind
            |> ExceptionDispatchResult.Dispatched
        | None ->

        if atTarget then
            deliverToTarget loggerFactory corelib state currentThread frame threadState unwind
        else

        match frame.ReturnState with
        | None ->
            match unwind.Target with
            | ExceptionSearchOutcome.NoHandler ->
                // The outermost frame, with nothing left to run. It is not popped:
                // the thread is terminating and its final frames stay for the report.
                ExceptionDispatchResult.ExceptionUnhandled (state, unwind.Exception)
            | other ->
                failwith
                    $"Logic error: the second pass of exception dispatch ran out of frames in %s{frame.ExecutingMethod.Name} while unwinding towards %O{other}"
        | Some returnState ->

        let threadState =
            threadState
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame unwind.Frame

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        secondPass
            loggerFactory
            corelib
            state
            currentThread
            { unwind with
                Frame = returnState.JumpTo
                PC = returnState.CallSiteIlOpIndex
            }

    /// The second pass has unwound to the frame the first pass named, and every cleanup clause
    /// between the throw point and here has run. Hand the exception over.
    and private deliverToTarget
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (frame : MethodState)
        (threadState : ThreadState)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : ExceptionDispatchResult
        =
        match unwind.Target with
        | ExceptionSearchOutcome.NoHandler ->
            failwith
                "Logic error: the second pass of exception dispatch reached a target frame for an outcome that names none (NoHandler)"
        | ExceptionSearchOutcome.CaughtAt (_, region) ->
            let offset =
                match region with
                | ExceptionRegion.Catch (_, offset)
                | ExceptionRegion.Filter (_, offset) -> offset
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ ->
                    failwith
                        $"Logic error: the first pass of exception dispatch concluded that cleanup region %O{region} of %s{frame.ExecutingMethod.Name} caught an exception; only Catch and Filter can catch"

            // A `Filter`'s body already ran, back in the first pass, and its continuation was
            // popped by the `endfilter` that accepted; either way what is entered here is the
            // handler body.
            enterCatchHandler currentThread frame threadState state offset unwind.Exception
            |> ExceptionDispatchResult.Dispatched
        | ExceptionSearchOutcome.AbandonedAtFilter _ ->
            // This exception's dispatch ends here — the CLR reports the filter as false and
            // discards it — and the search for the *original* exception, parked on the filter's
            // continuation, resumes with this filter struck off.
            let continuation, methodState = popToActiveFilter frame

            let threadState =
                ThreadState.setFrame unwind.Frame (MethodState.clearEvalStack methodState) threadState

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add currentThread threadState
                }

            let outer = continuation.Search

            runFirstPass
                loggerFactory
                corelib
                state
                currentThread
                { outer with
                    SkippedFilters = continuation.CurrentFilter :: outer.SkippedFilters
                }
        | ExceptionSearchOutcome.WrappedAt _ ->

        let returnState =
            match frame.ReturnState with
            | Some returnState -> returnState
            | None ->
                failwith
                    $"Logic error: the first pass of exception dispatch found a wrapping boundary on %s{frame.ExecutingMethod.Name}, which has no caller to wrap towards"

        let state, wrapped, wrappedType =
            applyFrameWraps loggerFactory corelib state currentThread returnState unwind.Exception unwind.ExceptionType

        let threadState =
            state.ThreadState.[currentThread]
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame unwind.Frame

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        // `applyFrameWraps` has already seeded the outermost wrapper with the caller's call site,
        // which is where its raise begins; the search for it starts there too.
        runFirstPass
            loggerFactory
            corelib
            state
            currentThread
            {
                Exception = wrapped
                ExceptionType = wrappedType
                StartFrame = returnState.JumpTo
                StartPC = returnState.CallSiteIlOpIndex
                Frame = returnState.JumpTo
                SearchPC = returnState.CallSiteIlOpIndex
                SkippedFilters = []
            }

    /// Dispatch an exception that has been thrown or is being propagated, starting the handler
    /// search in the thread's active frame at its current program counter.
    let dispatchException
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        : ExceptionDispatchResult
        =
        let threadState = state.ThreadState.[currentThread]
        let frameId = threadState.ActiveMethodState
        let searchPC = threadState.MethodState.IlOpIndex

        runFirstPass
            loggerFactory
            corelib
            state
            currentThread
            {
                Exception = cliException
                ExceptionType = exceptionType
                StartFrame = frameId
                StartPC = searchPC
                Frame = frameId
                SearchPC = searchPC
                SkippedFilters = []
            }

    /// Resume a first-pass search that suspended to evaluate a filter, now that `endfilter` has
    /// said whether the filter accepted. The caller has already popped the filter's continuation.
    let resumeSearchAfterFilter
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (continuation : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (accepted : bool)
        : ExceptionDispatchResult
        =
        let search = continuation.Search
        let activeFrame = state.ThreadState.[currentThread].ActiveMethodState

        if activeFrame <> search.Frame then
            failwith
                $"Logic error: endfilter for filter %O{continuation.CurrentFilter} resumed a search parked on frame %O{search.Frame} while the thread was executing frame %O{activeFrame}"

        if accepted then
            let region =
                ExceptionRegion.Filter (
                    continuation.CurrentFilter.FilterOffset,
                    continuation.CurrentFilter.HandlerOffset
                )

            concludeFirstPass
                loggerFactory
                corelib
                state
                currentThread
                search
                (ExceptionSearchOutcome.CaughtAt (search.Frame, region))
        else
            runFirstPass
                loggerFactory
                corelib
                state
                currentThread
                { search with
                    SkippedFilters = continuation.CurrentFilter :: search.SkippedFilters
                }

    /// Resume a second-pass unwind that suspended to run a `finally` or `fault` clause, now that
    /// its `endfinally` has been reached. The caller has already popped the clause's continuation.
    let resumeUnwindAfterCleanup
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (unwind : ExceptionUnwindState<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : ExceptionDispatchResult
        =
        let threadState = state.ThreadState.[currentThread]

        if threadState.ActiveMethodState <> unwind.Frame then
            failwith
                $"Logic error: endfinally resumed an unwind parked on frame %O{unwind.Frame} while the thread was executing frame %O{threadState.ActiveMethodState}"

        // The resume PC is the frame's *live* program counter — the `endfinally` itself — rather
        // than the PC the unwind was parked with. That is what terminates a tower of nested
        // cleanup clauses: the just-run clause's `try` does not cover its own handler body, while
        // a `try` enclosing it still does, so re-searching from here finds the next clause out
        // and nothing else.
        secondPass
            loggerFactory
            corelib
            state
            currentThread
            { unwind with
                PC = threadState.MethodState.IlOpIndex
            }

    /// Initiate exception dispatch for an exception object already on the heap.
    /// Builds the initial stack trace frame and dispatches.
    let throwExceptionObject
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (exceptionAddr : ManagedHeapAddress)
        (exceptionType : ConcreteTypeHandle)
        : ExceptionDispatchResult
        =
        let threadState = state.ThreadState.[currentThread]
        let currentMethodState = threadState.MethodState

        let stackFrame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Method = currentMethodState.ExecutingMethod
                IlOffset = currentMethodState.IlOpIndex
                IsLastFrameFromForeignExceptionStackTrace = false
            }

        // `Exception.PrepareForForeignExceptionRaise` has just told us that this throw is
        // re-raising an exception whose trace was captured earlier, so the frames behind its
        // `_stackTrace` must survive rather than be replaced. CoreCLR splits this across two
        // readers of the same flag — `IL_Throw` (jithelpers.cpp:814) declines to clear
        // `_stackTrace`, and the next `StackTraceInfo::AppendElement` (excep.cpp:3087) marks the
        // last frame already present — but PawPrint has no clear-at-throw step to decline, and
        // appends its first frame right here, so both land in one place.
        //
        // The other half of `IL_Throw`'s flag branch, `SetStackTraceString(NULL)`, needs nothing
        // here: `RestoreDispatchState` assigns `_stackTraceString = null` itself in managed code
        // (Exception.CoreCLR.cs:141), which PawPrint interprets like any other store. CoreCLR's
        // native null exists for flag-setters that bypass `RestoreDispatchState` — only
        // `IL_ThrowExact` (jithelpers.cpp:937), a JIT helper with no managed caller in CoreLib.
        //
        // The frames come from the exception's *own* token, not from anything the flag carries:
        // `RestoreDispatchState` (Exception.CoreCLR.cs:140) has already written the captured token
        // into `_stackTrace`, and if some other exception were raised while the flag was set,
        // CoreCLR would likewise splice whatever that one's `_stackTrace` held.
        //
        // Consumed here for every raise reaching this function, not only for the `throw` opcode,
        // matching the unconditional reset at excep.cpp:3017: the flag belongs to the thread's
        // next dispatch, whatever raises it. `rethrow` does not come through here — it appends no
        // frame of its own — and so carries the question forward instead.
        let state, restoredFrames =
            state
            |> consumeForeignExceptionRaise
                currentThread
                (fun () -> IlMachineState.frozenStackTraceFrames corelib exceptionAddr state)

        let restoredFrames = restoredFrames |> Option.defaultValue []

        let cliException =
            {
                ExceptionObject = exceptionAddr
                StackTrace = restoredFrames @ [ stackFrame ]
            }

        dispatchException loggerFactory corelib state currentThread cliException exceptionType

    /// Return the HResult that the real CLR would set for a runtime-synthesised exception of the
    /// given type.  The real CLR calls the default constructor (which sets the subclass-specific
    /// HResult) and then overwrites it with the mapped value from EEException::GetHR(); for the
    /// common exception types these are identical.  Unknown types fall back to COR_E_EXCEPTION.
    let private hresultForExceptionType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : int
        =
        let id = exceptionTypeInfo.Identity

        if id = baseClassTypes.NullReferenceException.Identity then
            ExceptionHResults.lookup "System.NullReferenceException"
        elif id = baseClassTypes.IndexOutOfRangeException.Identity then
            ExceptionHResults.lookup "System.IndexOutOfRangeException"
        elif id = baseClassTypes.DivideByZeroException.Identity then
            ExceptionHResults.lookup "System.DivideByZeroException"
        elif id = baseClassTypes.OverflowException.Identity then
            ExceptionHResults.lookup "System.OverflowException"
        elif id = baseClassTypes.InvalidCastException.Identity then
            ExceptionHResults.lookup "System.InvalidCastException"
        elif id = baseClassTypes.ArithmeticException.Identity then
            ExceptionHResults.lookup "System.ArithmeticException"
        elif id = baseClassTypes.StackOverflowException.Identity then
            ExceptionHResults.lookup "System.StackOverflowException"
        elif id = baseClassTypes.OutOfMemoryException.Identity then
            ExceptionHResults.lookup "System.OutOfMemoryException"
        elif id = baseClassTypes.TypeInitializationException.Identity then
            ExceptionHResults.lookup "System.TypeInitializationException"
        elif id = baseClassTypes.TypeLoadException.Identity then
            ExceptionHResults.lookup "System.TypeLoadException"
        elif id = baseClassTypes.MissingFieldException.Identity then
            ExceptionHResults.lookup "System.MissingFieldException"
        elif id = baseClassTypes.MissingMethodException.Identity then
            ExceptionHResults.lookup "System.MissingMethodException"
        elif id = baseClassTypes.ArgumentException.Identity then
            ExceptionHResults.lookup "System.ArgumentException"
        elif id = baseClassTypes.ArgumentNullException.Identity then
            ExceptionHResults.lookup "System.ArgumentNullException"
        elif id = baseClassTypes.NotSupportedException.Identity then
            ExceptionHResults.lookup "System.NotSupportedException"
        elif id = baseClassTypes.DuplicateWaitObjectException.Identity then
            ExceptionHResults.lookup "System.DuplicateWaitObjectException"
        elif id = baseClassTypes.InvalidProgramException.Identity then
            ExceptionHResults.lookup "System.InvalidProgramException"
        elif id = baseClassTypes.BadImageFormatException.Identity then
            ExceptionHResults.lookup "System.BadImageFormatException"
        elif id = baseClassTypes.ArgumentOutOfRangeException.Identity then
            ExceptionHResults.lookup "System.ArgumentOutOfRangeException"
        else
            ExceptionHResults.corEException

    /// Allocate a zero-initialised exception of the given type on the managed heap and set its
    /// _HResult field to the correct value.  The constructor is NOT run; the caller is
    /// responsible for pushing a ctor frame (see IlMachineStateExecution.raiseRuntimeException).
    ///
    /// This is the allocation half of the CLR's EEException::CreateThrowable.
    /// See the corresponding CLR source:
    /// https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
    let allocateRuntimeException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        if not exceptionTypeInfo.Generics.IsEmpty then
            failwith
                $"allocateRuntimeException: exception type %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %d{exceptionTypeInfo.Generics.Length} generic parameter(s), but this helper only supports non-generic exception types"

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies exceptionTypeInfo

        let state, exnHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                exceptionTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (exceptionTypeInfo.Identity, stk))

        let state, fields =
            IlMachineState.buildInstanceStorage loggerFactory baseClassTypes state exnHandle

        let addr, state = IlMachineState.allocateManagedObject exnHandle fields state

        // Pre-set _HResult to the correct value for this exception type.  The ctor will
        // overwrite this (base Exception() sets COR_E_EXCEPTION, then the subclass ctor
        // sets its own value), but we pre-set it as a safety net for partial ctor execution
        // and for synthesizeTypeInitializationException which bypasses the ctor.
        //
        // The real CLR additionally calls SetHResult(GetHR()) *after* the ctor returns;
        // that post-ctor overwrite is performed by overwriteHResultPostCtor, called from
        // the Ret handler's DispatchException path in NullaryIlOp.fs.
        let hresult = hresultForExceptionType baseClassTypes exceptionTypeInfo

        let hresultField =
            FieldIdentity.requiredNonGenericInstanceFieldId state.ConcreteTypes baseClassTypes.Exception "_HResult"

        let state =
            IlMachineState.setInstanceFieldById addr hresultField (CliType.Numeric (CliNumericType.Int32 hresult)) state

        addr, exnHandle, state

    /// Overwrite _HResult on a runtime-synthesised exception after its constructor has run.
    /// This mirrors the CLR's EEException::CreateThrowable which calls SetHResult(GetHR())
    /// after CallDefaultConstructor.
    /// See: https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L999-L1000
    let overwriteHResultPostCtor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exnAddr : ManagedHeapAddress)
        (exnType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState
        =
        let ct =
            AllConcreteTypes.lookup exnType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith "overwriteHResultPostCtor: ConcreteTypeHandle not found in AllConcreteTypes"
            )

        let typeInfo =
            (state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName)
                .TypeDefs.[ct.Identity.TypeDefinition.Get]

        let hresult = hresultForExceptionType baseClassTypes typeInfo

        let hresultField =
            FieldIdentity.requiredNonGenericInstanceFieldId state.ConcreteTypes baseClassTypes.Exception "_HResult"

        IlMachineState.setInstanceFieldById exnAddr hresultField (CliType.Numeric (CliNumericType.Int32 hresult)) state
