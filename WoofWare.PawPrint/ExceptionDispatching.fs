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

    /// Find the first matching exception handler for the given exception at the given PC.
    /// Also returns `isFinally : bool`: whether this is a `finally` block (as opposed to e.g. a `catch`).
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
        match method.Instructions with
        | None -> state, None
        | Some instructions ->

        let state, matches =
            ((state, []), instructions.ExceptionRegions)
            ||> Seq.fold (fun (state, acc) region ->
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
                            state, (region, false) :: acc
                        else
                            state, acc
                    else
                        state, acc
                | ExceptionRegion.Filter (_filterOffset, offset) ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        failwith "TODO: filter needs to be evaluated"
                    else
                        state, acc
                | ExceptionRegion.Finally offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (region, true) :: acc
                    else
                        state, acc
                | ExceptionRegion.Fault offset ->
                    if currentPC >= offset.TryOffset && currentPC < offset.TryOffset + offset.TryLength then
                        state, (region, true) :: acc
                    else
                        state, acc
            )

        // When multiple regions match (e.g. a catch and a finally for the same try block),
        // pick the innermost (smallest TryLength) handler. Among equal-sized try regions,
        // prefer catch over finally/fault per ECMA-335 §I.12.4.2.7.
        let result =
            match matches |> List.rev with
            | [] -> None
            | [ x ] -> Some x
            | multiple ->
                multiple
                |> List.sortBy (fun (region, _isFinally) ->
                    let offset =
                        match region with
                        | ExceptionRegion.Catch (_, o) -> o
                        | ExceptionRegion.Filter (_, o) -> o
                        | ExceptionRegion.Finally o -> o
                        | ExceptionRegion.Fault o -> o

                    // Sort by try length ascending (innermost first), then catch before finally/fault
                    let kindOrder =
                        match region with
                        | ExceptionRegion.Catch _ -> 0
                        | ExceptionRegion.Filter _ -> 1
                        | ExceptionRegion.Finally _ -> 2
                        | ExceptionRegion.Fault _ -> 3

                    (offset.TryLength, kindOrder)
                )
                |> List.head
                |> Some

        state, result

    /// Enter a catch handler: set PC to the handler offset, clear eval stack and exception continuation,
    /// push the exception object reference.
    let enterCatchHandler
        (currentThread : ThreadId)
        (methodState : MethodState)
        (threadState : ThreadState)
        (state : IlMachineState)
        (offset : ExceptionOffset)
        (exceptionObjectAddr : ManagedHeapAddress)
        : IlMachineState
        =
        let newMethodState =
            methodState
            |> MethodState.setProgramCounter offset.HandlerOffset
            |> MethodState.clearEvalStack
            |> MethodState.clearExceptionContinuation
            |> MethodState.pushToEvalStack' (EvalStackValue.ObjectRef exceptionObjectAddr)

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
            |> MethodState.setExceptionContinuation (ExceptionContinuation.PropagatingException cliException)

        let newThreadState =
            ThreadState.setFrame threadState.ActiveMethodState newMethodState threadState

        { state with
            ThreadState = state.ThreadState |> Map.add currentThread newThreadState
        }

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
        match handler with
        | ExceptionRegion.Catch (_, offset) ->
            enterCatchHandler currentThread methodState threadState state offset cliException.ExceptionObject
        | ExceptionRegion.Finally offset ->
            enterFinallyHandler currentThread methodState threadState state offset cliException
        | _ -> failwith "TODO: Filter and Fault handlers not yet implemented"

    /// Unwind the call stack looking for an exception handler. Pops frames until a handler is found
    /// (catch or finally), entering it; or until no frames remain, in which case the exception is unhandled.
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
                let tieAddr, tieType, state =
                    IlMachineState.synthesizeTypeInitializationException
                        loggerFactory
                        corelib
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

        // Search for a handler in the caller's method at the *call-site* PC (before
        // advanceProgramCounter).  The caller frame's IlOpIndex has already been advanced
        // past the call/callvirt/newobj, which can place it outside the protected region
        // when the call is the last instruction in a try block.
        let callSitePC = returnState.CallSiteIlOpIndex
        let activeAssy = state.ActiveAssembly currentThread

        let state, handlerResult =
            findExceptionHandler
                loggerFactory
                corelib
                state
                activeAssy
                callSitePC
                exceptionType
                callerFrame.ExecutingMethod

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

        match handlerResult with
        | Some (handler, _isFinally) ->
            enterHandler currentThread callerFrame threadState state cliException handler
            |> ExceptionDispatchResult.HandlerFound
        | None ->
            // No handler in this frame either; continue unwinding
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
        let threadState = state.ThreadState.[currentThread]
        let currentMethodState = threadState.MethodState

        let activeAssy = state.ActiveAssembly currentThread

        let state, handlerResult =
            findExceptionHandler
                loggerFactory
                corelib
                state
                activeAssy
                currentMethodState.IlOpIndex
                exceptionType
                currentMethodState.ExecutingMethod

        match handlerResult with
        | Some (handler, _isFinally) ->
            enterHandler currentThread currentMethodState threadState state cliException handler
            |> ExceptionDispatchResult.HandlerFound
        | None -> unwindToCallerAndSearch loggerFactory corelib state currentThread cliException exceptionType

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
