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

    let private isInHandlerBody (pc : int) (offset : ExceptionOffset) : bool =
        pc >= offset.HandlerOffset && pc < offset.HandlerOffset + offset.HandlerLength

    let internal tryCurrentCatchException
        (methodState : MethodState)
        : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        match methodState.ExecutingMethod.Instructions with
        | None -> None
        | Some instructions ->
            instructions.ExceptionRegions
            |> Seq.indexed
            |> Seq.choose (fun (regionIndex, region) ->
                match region with
                | ExceptionRegion.Catch (_, offset)
                | ExceptionRegion.Filter (_, offset) when isInHandlerBody methodState.IlOpIndex offset ->
                    methodState.CatchExceptions
                    |> Map.tryFind offset
                    |> Option.map (fun exn -> regionIndex, offset, exn)
                | _ -> None
            )
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
        match method.Instructions with
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

    /// Enter a catch handler: set PC to the handler offset, clear eval stack and exception continuation,
    /// push the exception object reference.
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
            |> MethodState.clearExceptionContinuation
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
            |> MethodState.setExceptionContinuation (ExceptionContinuation.ResumeAfterFilter continuation)
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
            |> MethodState.setExceptionContinuation (ExceptionContinuation.PropagatingException cliException)

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
            |> MethodState.setExceptionContinuation (ExceptionContinuation.PropagatingException cliException)

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
        let newMethodState =
            methodState
            |> MethodState.clearEvalStack
            |> MethodState.clearExceptionContinuation

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

    /// Unwind the call stack looking for an exception handler. Pops frames until a handler is found
    /// (catch or cleanup), entering it; or until no frames remain, in which case the exception is unhandled.
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
        let currentMethodState = threadState.MethodState

        match currentMethodState.ReturnState with
        | None -> ExceptionDispatchResult.ExceptionUnhandled (state, cliException)
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
                        let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
                        Assembly.fullName assy ct.Identity
                    | None ->
                        failwith
                            $"Logic error: failed to look up ConcreteType for initialising-type handle %O{finishedInitialising} when synthesising TypeInitializationException"

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
                    { cliException with
                        ExceptionObject = tieAddr
                    }

                state, wrappedCliException, tieType

        // Pop to caller frame
        let callerFrame = ThreadState.getFrame returnState.JumpTo threadState

        let threadState =
            { threadState with
                ActiveMethodState = returnState.JumpTo
                ActiveAssembly = callerFrame.ExecutingMethod.DeclaringType.Assembly
            }

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add currentThread threadState
            }

        match callerFrame.ExceptionContinuation with
        | Some (ExceptionContinuation.ResumeAfterFilter continuation) ->
            // An exception escaping a callee invoked by a filter rejects the filter and discards
            // the escaping exception. We deliberately do not append a frame here: handler search
            // is resuming for the original exception, whose stack already records the original
            // throw path. The filter-body exception is only the reason this filter returned false.
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

        // Search for a handler in the caller's method at the *call-site* PC (before
        // advanceProgramCounter).  The caller frame's IlOpIndex has already been advanced
        // past the call/callvirt/newobj, which can place it outside the protected region
        // when the call is the last instruction in a try block.
        let callSitePC = returnState.CallSiteIlOpIndex
        // Record the caller frame in the stack trace at its call-site PC.
        let stackFrame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                Method = callerFrame.ExecutingMethod
                IlOffset = callSitePC
            }

        let cliException =
            { cliException with
                StackTrace = cliException.StackTrace @ [ stackFrame ]
            }

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
                callSitePC
                []
        with
        | _state, Some state -> ExceptionDispatchResult.HandlerFound state
        | state, None ->
            // No handler in this frame either; continue unwinding
            unwindToCallerAndSearch loggerFactory corelib state currentThread cliException exceptionType

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

        match currentMethodState.ExceptionContinuation with
        | Some (ExceptionContinuation.ResumeAfterFilter continuation) ->
            // TODO: handwritten IL can put a try/catch inside a filter body. In that case a direct
            // throw from the filter should first search handlers at the filter PC, and only reject
            // the filter if none match. Supporting that correctly requires nested exception
            // continuations: entering the local handler must not discard this ResumeAfterFilter.
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
        | _ ->
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
            }

        let cliException =
            {
                ExceptionObject = exceptionAddr
                StackTrace = [ stackFrame ]
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
        else
            ExceptionHResults.corEException

    /// Allocate a zero-initialised exception of the given type on the managed heap and set its
    /// _HResult field to the correct value.  The constructor is NOT run; the caller is
    /// responsible for pushing a ctor frame (see IlMachineStateExecution.raiseManagedException).
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
            CliValueType.OfFields baseClassTypes state.ConcreteTypes exnHandle exceptionTypeInfo.Layout allFields

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

        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let heapObj =
            AllocatedNonArrayObject.SetField "_HResult" (CliType.Numeric (CliNumericType.Int32 hresult)) heapObj

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

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
            state._LoadedAssemblies.[ct.Identity.AssemblyFullName].TypeDefs.[ct.Identity.TypeDefinition.Get]

        let hresult = hresultForExceptionType baseClassTypes typeInfo

        let heapObj = ManagedHeap.get exnAddr state.ManagedHeap

        let heapObj =
            AllocatedNonArrayObject.SetField "_HResult" (CliType.Numeric (CliNumericType.Int32 hresult)) heapObj

        { state with
            ManagedHeap = ManagedHeap.set exnAddr heapObj state.ManagedHeap
        }
