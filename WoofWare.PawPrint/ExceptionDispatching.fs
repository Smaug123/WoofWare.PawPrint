namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Result of attempting to dispatch an exception to a handler.
type ExceptionDispatchResult =
    /// A handler was found and entered; the machine state is positioned at the handler entry.
    | HandlerFound of IlMachineState
    /// The exception is unhandled; no handler was found in any frame.
    | ExceptionUnhandled of IlMachineState * CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

/// Exception handler dispatch that requires IlMachineState for type resolution.
[<RequireQualifiedAccess>]
module ExceptionDispatching =

    /// Check if an exception type matches a catch handler type.
    let private isExceptionAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        (catchTypeToken : MetadataToken)
        : IlMachineState * bool
        =
        let state, catchTypeDefn, catchAssy =
            IlMachineState.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                typeGenerics
                catchTypeToken

        let state, catchTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                catchAssy.Name
                typeGenerics
                methodGenerics
                catchTypeDefn

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

    let private exceptionRegionOffset (region : ExceptionRegion) : ExceptionOffset =
        match region with
        | ExceptionRegion.Catch (_, offset)
        | ExceptionRegion.Filter (_, offset)
        | ExceptionRegion.Finally offset
        | ExceptionRegion.Fault offset -> offset

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

    /// Find the first matching exception handler for the given exception at the given PC.
    /// Also returns whether this is a cleanup block (finally/fault) rather than e.g. a catch.
    let private findExceptionHandlerSkippingFilters
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (currentPC : int)
        (exceptionType : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        (skippedFilters : ExceptionFilterRegion list)
        : IlMachineState * (WoofWare.PawPrint.ExceptionRegion * bool) option
        =
        match MethodInfo.tryIlBody method with
        | None -> state, None
        | Some instructions ->

        let state, matches =
            ((state, []), instructions.ExceptionRegions |> Seq.indexed)
            ||> Seq.fold (fun (state, acc) (regionIndex, region) ->
                match region with
                | ExceptionRegion.Catch (typeToken, offset) ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        let state, matches =
                            isExceptionAssignableTo
                                loggerFactory
                                baseClassTypes
                                state
                                activeAssy
                                method.DeclaringType.Generics
                                method.Generics
                                exceptionType
                                typeToken

                        if matches then
                            state, (regionIndex, region, false) :: acc
                        else
                            state, acc
                    else
                        state, acc
                | ExceptionRegion.Filter (filterOffset, offset) ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        if isSkippedFilter skippedFilters filterOffset offset then
                            state, acc
                        else
                            state, (regionIndex, region, false) :: acc
                    else
                        state, acc
                | ExceptionRegion.Finally offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (regionIndex, region, true) :: acc
                    else
                        state, acc
                | ExceptionRegion.Fault offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (regionIndex, region, true) :: acc
                    else
                        state, acc
            )

        // When multiple regions match (e.g. a catch and a finally for the same try block),
        // pick the innermost (smallest TryLength) handler. Among equal-sized try regions, preserve
        // metadata order for catch/filter clauses, and prefer those clauses over cleanup handlers.
        let result =
            match matches |> List.rev with
            | [] -> None
            | [ (_, region, isCleanup) ] -> Some (region, isCleanup)
            | multiple ->
                multiple
                |> List.sortBy (fun (regionIndex, region, _isCleanup) ->
                    let offset = exceptionRegionOffset region

                    let clauseGroupOrder =
                        match region with
                        | ExceptionRegion.Catch _
                        | ExceptionRegion.Filter _ -> 0
                        | ExceptionRegion.Finally _
                        | ExceptionRegion.Fault _ -> 1

                    (offset.TryLength, clauseGroupOrder, regionIndex)
                )
                |> List.head
                |> (fun (_, region, isCleanup) -> region, isCleanup)
                |> Some

        state, result

    /// Find the first matching exception handler for the given exception at the given PC.
    /// Also returns whether this is a cleanup block (finally/fault) rather than e.g. a catch.
    let findExceptionHandler
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (currentPC : int)
        (exceptionType : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, 'methodVar>)
        : IlMachineState * (WoofWare.PawPrint.ExceptionRegion * bool) option
        =
        findExceptionHandlerSkippingFilters
            loggerFactory
            baseClassTypes
            state
            activeAssy
            currentPC
            exceptionType
            method
            []

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
    /// object reference, and remember how to continue the handler search when `endfilter` returns.
    let private enterFilterHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (searchPC : int)
        (skippedFilters : ExceptionFilterRegion list)
        (filterOffset : int)
        (handlerOffset : ExceptionOffset)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState
        =
        let currentFilter = exceptionFilterRegion filterOffset handlerOffset

        let continuation : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                CurrentFilter = currentFilter
                SkippedFilters = skippedFilters
                SearchPC = searchPC
                CliException = cliException
            }

        let newMethodState =
            methodState
            |> MethodState.setProgramCounter filterOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearPendingPrefix
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FilterHandler currentFilter)
                (ExceptionContinuation.ResumeAfterFilter continuation)
            |> MethodState.pushToEvalStack' (EvalStackValue.ObjectRef cliException.ExceptionObject)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter a finally handler: set PC to the handler offset, clear eval stack,
    /// set exception continuation to propagate the exception after the finally completes.
    let enterFinallyHandler
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
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FinallyHandler offset)
                (ExceptionContinuation.PropagatingException cliException)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    /// Enter a fault handler: set PC to the handler offset, clear eval stack,
    /// set exception continuation to propagate the exception after the fault completes.
    let enterFaultHandler
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
            |> MethodState.pushExceptionContinuation
                (ExceptionContinuationScope.FaultHandler offset)
                (ExceptionContinuation.PropagatingException cliException)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

    let private enterHandlerAtSearchPC
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (searchPC : int)
        (skippedFilters : ExceptionFilterRegion list)
        (handler : ExceptionRegion)
        : IlMachineState
        =
        match handler with
        | ExceptionRegion.Catch (_, offset) ->
            enterCatchHandler currentThread methodState threadState state offset cliException
        | ExceptionRegion.Finally offset ->
            enterFinallyHandler currentThread methodState threadState state offset cliException
        | ExceptionRegion.Fault offset ->
            enterFaultHandler currentThread methodState threadState state offset cliException
        | ExceptionRegion.Filter (filterOffset, offset) ->
            enterFilterHandler
                currentThread
                methodState
                threadState
                state
                searchPC
                skippedFilters
                filterOffset
                offset
                cliException

    /// Given a matched handler from findExceptionHandler, enter the handler. Returns the updated state.
    let enterHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (handler : ExceptionRegion)
        : IlMachineState
        =
        enterHandlerAtSearchPC currentThread methodState threadState state cliException methodState.IlOpIndex [] handler

    let private prepareRejectedFilterSearch
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (continuation : ExceptionFilterContinuation<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState *
          MethodState *
          ThreadState *
          CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle *
          int *
          ExceptionFilterRegion list
        =
        let popped, methodState = MethodState.popExceptionContinuation methodState

        match popped with
        | Some {
                   Scope = ExceptionContinuationScope.FilterHandler currentFilter
                   Continuation = ExceptionContinuation.ResumeAfterFilter popped
               } when
            currentFilter = continuation.CurrentFilter
            && popped.CurrentFilter = continuation.CurrentFilter
            ->
            ()
        | Some frame ->
            failwith
                $"Expected to reject active filter %O{continuation.CurrentFilter}, but top exception continuation was scope %O{frame.Scope} with continuation %O{frame.Continuation}"
        | None ->
            failwith $"Expected to reject active filter %O{continuation.CurrentFilter}, but no continuation was active"

        let newMethodState = methodState |> MethodState.clearEvalStack

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread newThreadState
            }

        let skippedFilters = continuation.CurrentFilter :: continuation.SkippedFilters

        let exceptionType =
            exceptionObjectType state continuation.CliException.ExceptionObject

        state,
        newMethodState,
        newThreadState,
        continuation.CliException,
        exceptionType,
        continuation.SearchPC,
        skippedFilters

    let private tryFindAndEnterHandlerAtSearchPC
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        (searchPC : int)
        (skippedFilters : ExceptionFilterRegion list)
        : IlMachineState * IlMachineState option
        =
        let activeAssy = state.ActiveAssembly currentThread

        let state, handlerResult =
            findExceptionHandlerSkippingFilters
                loggerFactory
                corelib
                state
                activeAssy
                searchPC
                exceptionType
                methodState.ExecutingMethod
                skippedFilters

        match handlerResult with
        | Some (handler, _isFinally) ->
            // `_isFinally` is ignored, so this fires on cleanup handlers too, and there
            // `cliException.StackTrace` holds only the frames unwound so far: PawPrint
            // interleaves handler search with cleanup rather than completing a first pass
            // first, as CoreCLR does. Managed code running in a `finally` therefore sees a
            // truncated trace — measured, and pre-dating the frozen-trace token: the same
            // partial list already reached `_stackTraceString` here. Issue #865 tracks giving
            // dispatch a real two-pass structure, which is what fixes both sinks at once.
            //
            // Recording the partial trace is nonetheless right, rather than skipping the write
            // for cleanup handlers: `Exception.HasBeenThrown` keys off `_stackTrace` being
            // non-null, and the exception genuinely has been thrown by this point. Skipping
            // would trade an incomplete trace for a wrong answer to a different question.
            let state =
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

            state,
            enterHandlerAtSearchPC
                currentThread
                methodState
                threadState
                state
                cliException
                searchPC
                skippedFilters
                handler
            |> Some
        | None -> state, None

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
    /// Appending is not enough on its own, though, because CoreCLR appends every frame in pass one,
    /// *before* running any cleanup clause. Guest code that sets the flag from a `finally` therefore
    /// cannot have it consumed by the raise it is unwinding — that raise's appends already happened.
    /// PawPrint has no pass one; it interleaves search with cleanup, so what it has instead is
    /// `CliException.MayConsumeForeignRaise`, set when a raise begins and carried through every
    /// suspension. A flag that predates the raise is consumed at its first append; one set by the
    /// raise's own cleanup is not. `sourcesPure/ForeignRaiseFlagSetInFinally.cs` covers the second,
    /// and `ForeignRaiseFlagPendingBeforeCleanup.cs` the first — they differ only in *when* the
    /// flag is set, so a rule that looked at the resume site rather than the raise gets one of
    /// them wrong whichever way it decides.
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

    /// Unwind the call stack looking for an exception handler. Pops frames until a handler is found
    /// (catch or cleanup), entering it; or until no frames remain, in which case the exception is unhandled.
    ///
    let rec unwindToCallerAndSearch
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        : ExceptionDispatchResult
        =
        let threadState = state.ThreadState.[currentThread]
        let unwoundFrameId = threadState.ActiveMethodState
        let currentMethodState = threadState.MethodState

        match currentMethodState.ReturnState with
        | None ->
            let state =
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

            ExceptionDispatchResult.ExceptionUnhandled (state, cliException)
        | Some returnState ->

        // If this frame was running a .cctor, mark the type initialisation as failed
        // and wrap the exception in TypeInitializationException (CLR behaviour).
        // Synthesize the TIE first so we can cache it; repeated accesses rethrow the
        // same instance (matching CLR identity semantics).
        let state, cliException, exceptionType =
            match returnState.WasInitialisingType with
            | None -> state, cliException, exceptionType
            | Some finishedInitialising ->
                // Per CLR spec, a throwing .cctor surfaces to managed code as
                // TypeInitializationException wrapping the original exception.
                let typeFullName =
                    match AllConcreteTypes.lookup finishedInitialising state.ConcreteTypes with
                    | Some ct ->
                        let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
                        Assembly.fullName assy ct.Identity
                    | None ->
                        failwith
                            $"Logic error: failed to look up ConcreteType for initialising-type handle %O{finishedInitialising} when synthesising TypeInitializationException"

                let state =
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

                let tieAddr, tieType, state =
                    IlMachineState.synthesizeTypeInitializationException
                        loggerFactory
                        corelib
                        typeFullName
                        cliException.ExceptionObject
                        state

                let state =
                    state.WithTypeFailedInit currentThread finishedInitialising tieAddr tieType

                let wrappedCliException =
                    {
                        ExceptionObject = tieAddr
                        StackTrace = []
                        // The raise carries on, so its answer to the foreign-raise question does
                        // too: wrapping swaps the object, not the raise.
                        MayConsumeForeignRaise = cliException.MayConsumeForeignRaise
                    }

                state, wrappedCliException, tieType

        // If this frame was the ctor target of `Activator.CreateInstance<T>()` (or any other
        // CreateInstanceOfT-style invocation that opts in via `WrapExceptionInTargetInvocation`),
        // wrap the in-flight exception in a fresh `TargetInvocationException` whose
        // `_innerException` field points at the original. This mirrors CoreCLR's
        // `try { ctor } catch (Exception e) { throw new TargetInvocationException(e); }` wrap
        // around `cache.CallRefConstructor` in `RuntimeType.CreateInstanceOfT` without
        // synthesising an extra trampoline frame: the wrap only fires on unwind across this
        // frame's boundary, so a try/catch *inside* the ctor that handles the exception is
        // unaffected.
        let state, cliException, exceptionType =
            if not returnState.WrapExceptionInTargetInvocation then
                state, cliException, exceptionType
            else
                let state =
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

                let tieAddr, tieType, state =
                    IlMachineState.synthesizeTargetInvocationException
                        loggerFactory
                        corelib
                        cliException.ExceptionObject
                        state

                let wrappedCliException =
                    {
                        ExceptionObject = tieAddr
                        StackTrace = []
                        // The raise carries on, so its answer to the foreign-raise question does
                        // too: wrapping swaps the object, not the raise.
                        MayConsumeForeignRaise = cliException.MayConsumeForeignRaise
                    }

                state, wrappedCliException, tieType

        // Pop to caller frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadState

        let threadState =
            threadState
            |> ThreadState.setActiveFrame returnState.JumpTo
            |> ThreadState.removeFrame unwoundFrameId

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        // Search for a handler in the caller's method at the *call-site* PC (before
        // advanceProgramCounter).  The caller frame's IlOpIndex has already been advanced
        // past the call/callvirt/newobj, which can place it outside the protected region
        // when the call is the last instruction in a try block.
        let callSitePC = returnState.CallSiteIlOpIndex

        let stackFrame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Method = callerFrame.ExecutingMethod
                IlOffset = callSitePC
                // The frame being appended belongs to the raise in progress, never to an earlier
                // one — CoreCLR sets `stackTraceElem.flags = 0` here for the same reason
                // (excep.cpp:3045). A pending flag marks the frame *before* this one, below.
                IsLastFrameFromForeignExceptionStackTrace = false
            }

        // A delegate's `Invoke` is a stub, not a managed method: real .NET has no frame for it,
        // and an exception crossing a delegate call reports the target and then whoever called
        // `Invoke`. PawPrint's ordinary delegate path gets that for free, because
        // `dispatchDelegateInvoke` pops its synthetic frame before calling the target — so the
        // frame is already gone by the time anything can throw. The exception is class
        // initialisation, which deliberately runs *while* that frame is still active so the
        // instruction can be retried after the `.cctor` returns; without this, a `.cctor` that
        // throws would report a `System.Action.Invoke` frame that no real trace contains.
        //
        // Only `DelegateInvoke` is suppressed, not runtime-provided frames at large: an
        // InternalCall or QCall *is* a managed method by name and real traces do show it.
        let isDelegateInvokeStub =
            match callerFrame.ExecutingMethod.Body with
            | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> true
            | _ -> false

        // This is one of PawPrint's two frame-append sites, so a flag left pending by
        // `Exception.PrepareForForeignExceptionRaise` is consumed here (the other site is the
        // throw-site frame in `throwExceptionObject`). In practice only a `rethrow` reaches this
        // with a flag still set: a `throw` consumes it when it seeds its first frame.
        //
        // A suppressed delegate-`Invoke` stub appends nothing, so it consumes nothing either: real
        // .NET has no `StackTraceElement` for that stub and hence no `AppendElement` call to read
        // the flag, which stays pending for the next genuine frame the raise reaches.
        //
        // The frames to mark are the ones the raise is already carrying: a `rethrow` read them out
        // of `_stackTrace` when it began, and a `throw` seeded them at its own throw site. That is
        // deliberately not a re-read of the token here, even though here is where CoreCLR reads it
        // — because here can be separated from the raise's initiation by guest cleanup code, and a
        // `finally` that throws the same exception again moves the token on. CoreCLR never faces
        // the question: pass one appends every frame before any cleanup clause runs, so the frames
        // it marks are the ones the raise began with, which is what this carries.
        //
        // The two answers cannot be told apart today. Reaching a case where they differ needs a
        // raise inside that `finally`, and such a raise also steals the flag before this one can
        // spend it — `sourcesPure/ForeignRaiseFlagNotStolenByCleanup.cs`, parked on issue #865. So
        // no test pins this line; it is written this way because it is the same fact as
        // `MayConsumeForeignRaise`, that a suspended raise comes back with the state it left with.
        let state, restoredFrames =
            if isDelegateInvokeStub || not cliException.MayConsumeForeignRaise then
                state, None
            else

            state
            |> consumeForeignExceptionRaise currentThread (fun () -> cliException.StackTrace)

        let framesBefore = restoredFrames |> Option.defaultValue cliException.StackTrace

        // `threadState` is a *value* captured before the consume, and the handler-entry path below
        // takes it as a parameter and writes it back into the state map. Left stale, it would
        // resurrect the flag we just cleared — with the visible effect that the boundary appears
        // here and the flag is *also* still there for the next raise to spend. Re-read it, which
        // is exact: the consume changes nothing else, and the frame bookkeeping above is already
        // in `state`.
        let threadState = state.ThreadState.[currentThread]

        let cliExceptionAtCallSite =
            if isDelegateInvokeStub then
                // Nothing appended, so nothing decided: the question stays open for the next frame.
                cliException
            else
                { cliException with
                    StackTrace = framesBefore @ [ stackFrame ]
                    // Decided, one way or the other, at this raise's first appended frame; the rest of
                    // the unwind must not ask again.
                    MayConsumeForeignRaise = false
                }

        match callerFrame.ExceptionContinuation with
        | Some (ExceptionContinuation.ResumeAfterFilter continuation) ->
            match
                tryFindAndEnterHandlerAtSearchPC
                    loggerFactory
                    corelib
                    state
                    currentThread
                    callerFrame
                    threadState
                    cliExceptionAtCallSite
                    exceptionType
                    callSitePC
                    []
            with
            | _state, Some state ->
                // A local handler at the call site runs inside the active filter evaluation.
                // Keep ResumeAfterFilter; the filter's eventual endfilter will pop it.
                ExceptionDispatchResult.HandlerFound state
            | state, None ->

                // An exception escaping a callee invoked by a filter rejects the filter and discards
                // the escaping exception. We deliberately do not append a frame here: handler search
                // is resuming for the original exception, whose stack already records the original
                // throw path. The filter-body exception is only the reason this filter returned false.
                let threadState = state.ThreadState.[currentThread]
                let callerFrame = ThreadState.getFrame threadState.ActiveMethodState threadState

                let state, callerFrame, threadState, cliException, exceptionType, searchPC, skippedFilters =
                    prepareRejectedFilterSearch currentThread callerFrame threadState state continuation

                match
                    tryFindAndEnterHandlerAtSearchPC
                        loggerFactory
                        corelib
                        state
                        currentThread
                        callerFrame
                        threadState
                        cliException
                        exceptionType
                        searchPC
                        skippedFilters
                with
                | _state, Some state -> ExceptionDispatchResult.HandlerFound state
                | state, None ->
                    unwindToCallerAndSearch loggerFactory corelib state currentThread cliException exceptionType

        | _ ->

            match
                tryFindAndEnterHandlerAtSearchPC
                    loggerFactory
                    corelib
                    state
                    currentThread
                    callerFrame
                    threadState
                    cliExceptionAtCallSite
                    exceptionType
                    callSitePC
                    []
            with
            | _state, Some state -> ExceptionDispatchResult.HandlerFound state
            | state, None ->
                // No handler in this frame either; continue unwinding
                unwindToCallerAndSearch loggerFactory corelib state currentThread cliExceptionAtCallSite exceptionType


    let dispatchExceptionFromSearchPC
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        (searchPC : int)
        (skippedFilters : ExceptionFilterRegion list)
        : ExceptionDispatchResult
        =
        let threadState = state.ThreadState.[currentThread]
        let currentMethodState = threadState.MethodState

        match
            tryFindAndEnterHandlerAtSearchPC
                loggerFactory
                corelib
                state
                currentThread
                currentMethodState
                threadState
                cliException
                exceptionType
                searchPC
                skippedFilters
        with
        | _state, Some state ->
            // A local handler at searchPC runs inside the active filter evaluation, if any.
            // Keep ResumeAfterFilter; the filter's eventual endfilter will pop it.
            ExceptionDispatchResult.HandlerFound state
        | state, None ->
            let threadState = state.ThreadState.[currentThread]
            let currentMethodState = threadState.MethodState

            match currentMethodState.ExceptionContinuation with
            | Some (ExceptionContinuation.ResumeAfterFilter continuation) ->
                let state, currentMethodState, threadState, cliException, exceptionType, searchPC, skippedFilters =
                    prepareRejectedFilterSearch currentThread currentMethodState threadState state continuation

                match
                    tryFindAndEnterHandlerAtSearchPC
                        loggerFactory
                        corelib
                        state
                        currentThread
                        currentMethodState
                        threadState
                        cliException
                        exceptionType
                        searchPC
                        skippedFilters
                with
                | _state, Some state -> ExceptionDispatchResult.HandlerFound state
                | state, None ->
                    unwindToCallerAndSearch loggerFactory corelib state currentThread cliException exceptionType

            | _ -> unwindToCallerAndSearch loggerFactory corelib state currentThread cliException exceptionType


    /// Dispatch an exception that has been thrown or is being propagated. Searches for a handler
    /// in the current method; if found, enters it; otherwise unwinds to the caller.
    /// Returns the updated state with the thread positioned at the handler entry point,
    /// or ExceptionUnhandled if no handler exists in any frame.
    let dispatchException
        (loggerFactory : ILoggerFactory)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentThread : ThreadId)
        (cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (exceptionType : ConcreteTypeHandle)
        : ExceptionDispatchResult
        =
        let currentMethodState = state.ThreadState.[currentThread].MethodState

        dispatchExceptionFromSearchPC
            loggerFactory
            corelib
            state
            currentThread
            cliException
            exceptionType
            currentMethodState.IlOpIndex
            []

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
                // Whatever the flag had to say has been said, right here at this raise's first
                // appended frame, so the unwind below must not ask again.
                MayConsumeForeignRaise = false
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

        let state, allFields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state exnHandle

        let fields =
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                exnHandle
                exceptionTypeInfo.Layout
                (CharSetMetadata.ofTypeAttributes exceptionTypeInfo.TypeAttributes)
                allFields

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
