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
            0x80004003 // E_POINTER
        elif id = baseClassTypes.IndexOutOfRangeException.Identity then
            int 0x80131508u // COR_E_INDEXOUTOFRANGE
        elif id = baseClassTypes.DivideByZeroException.Identity then
            0x80020012 // COR_E_DIVIDEBYZERO
        elif id = baseClassTypes.OverflowException.Identity then
            int 0x80131516u // COR_E_OVERFLOW
        elif id = baseClassTypes.InvalidCastException.Identity then
            0x80004002 // COR_E_INVALIDCAST
        elif id = baseClassTypes.ArithmeticException.Identity then
            0x80070216 // COR_E_ARITHMETIC
        elif id = baseClassTypes.StackOverflowException.Identity then
            int 0x800703E9u // COR_E_STACKOVERFLOW
        elif id = baseClassTypes.OutOfMemoryException.Identity then
            0x8007000E // COR_E_OUTOFMEMORY
        elif id = baseClassTypes.TypeInitializationException.Identity then
            int 0x80131534u // COR_E_TYPEINITIALIZATION
        elif id = baseClassTypes.TypeLoadException.Identity then
            int 0x80131522u // COR_E_TYPELOAD
        elif id = baseClassTypes.MissingFieldException.Identity then
            int 0x80131511u // COR_E_MISSINGFIELD
        elif id = baseClassTypes.MissingMethodException.Identity then
            int 0x80131513u // COR_E_MISSINGMETHOD
        else
            int 0x80131500u // COR_E_EXCEPTION (base Exception default)

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

        let rec collectAllInstanceFields
            (state : IlMachineState)
            (concreteType : ConcreteTypeHandle)
            : IlMachineState * CliField list
            =
            let ct =
                AllConcreteTypes.lookup concreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith "allocateRuntimeException: ConcreteTypeHandle not found in AllConcreteTypes"
                )

            let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
            let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

            let state, ownFields =
                let instanceFields =
                    typeInfo.Fields
                    |> List.filter (fun field ->
                        not (field.Attributes.HasFlag System.Reflection.FieldAttributes.Static)
                    )

                ((state, []), instanceFields)
                ||> List.fold (fun (state, fields) field ->
                    let state, zero, fieldTypeHandle =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            assy
                            field.Signature
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            state

                    let cliField : CliField =
                        {
                            Name = field.Name
                            Contents = zero
                            Offset = field.Offset
                            Type = fieldTypeHandle
                        }

                    state, cliField :: fields
                )

            let ownFields = List.rev ownFields

            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            match baseHandle with
            | None -> state, ownFields
            | Some parentHandle ->
                let state, baseFields = collectAllInstanceFields state parentHandle
                state, baseFields @ ownFields

        let state, allFields = collectAllInstanceFields state exnHandle
        let fields = CliValueType.OfFields exceptionTypeInfo.Layout allFields

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
