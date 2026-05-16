namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        impls
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        let dispatchNative () =
            let targetAssy =
                state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly
                |> Option.get

            let targetType =
                targetAssy.TypeDefs.[instruction.ExecutingMethod.DeclaringType.Definition.Get]

            let nativeContext =
                {
                    LoggerFactory = loggerFactory
                    Implementations = impls
                    BaseClassTypes = baseClassTypes
                    Thread = thread
                    State = state
                    Instruction = instruction
                    TargetAssembly = targetAssy
                    TargetType = targetType
                }

            let outcome =
                match NativeDispatch.tryExecute nativeContext with
                | Some result -> result
                | None -> NativeDispatch.failUnimplemented nativeContext

            match outcome with
            | NativeHandlerResult.Completed (state, effect) ->
                // Native handler ran to completion. Pop the native frame and surface
                // WhatWeDid.Executed; this is the common case.
                match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
                | ReturnFrameResult.NormalReturn state -> ExecutionResult.Stepped (state, WhatWeDid.Executed, effect)
                | result -> failwith $"unexpected ReturnFrameResult from extern method return: %A{result}"
            | NativeHandlerResult.PushedManagedCallee (state, effect) ->
                // The handler pushed a managed callee on top of itself for re-entry: leave
                // the native frame on the stack so the dispatch loop runs the callee, then
                // re-enters this native method on a future step.
                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForManagedCall, effect)
            | NativeHandlerResult.RaiseException (state, exnType, effect) ->
                // The handler wants to raise `exnType`. Allocate the exception, call its
                // parameterless ctor, and arm dispatch-on-return; leave the native frame
                // on the stack so exception dispatch can unwind through it on the ctor's
                // `Ret`. The handler is never re-entered. We surface
                // SuspendedForManagedCall because, from the Scheduler's point of view, a
                // managed callee (the ctor) has been pushed on top of the native frame.
                let state, _whatWeDid =
                    IlMachineStateExecution.raiseRuntimeException loggerFactory baseClassTypes exnType thread state

                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForManagedCall, effect)
            | NativeHandlerResult.SuspendedForClassInit (state, effect) ->
                // A cctor was pushed; the native frame must stay on the stack so the dispatch
                // loop runs the cctor first, then re-enters this native method on the next step.
                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForClassInit, effect)
            | NativeHandlerResult.BlockedOnClassInit (state, blockedBy, effect) ->
                // Another thread owns this type's .cctor lock; the native frame must persist
                // until that thread finishes, then we re-enter.
                ExecutionResult.Stepped (state, WhatWeDid.BlockedOnClassInit blockedBy, effect)
            | NativeHandlerResult.ThrowingTypeInitializationException (state, effect) ->
                // A sub-call's exception has already unwound past this native frame to the
                // matching handler; returnStackFrame would pop the wrong frame.
                ExecutionResult.Stepped (state, WhatWeDid.ThrowingTypeInitializationException, effect)
            | NativeHandlerResult.Terminating executionResult ->
                // The handler delegated to an ExternImpl that produced a terminating
                // outcome (ProcessExit, FailFast, Terminated, UnhandledException). Surface
                // it verbatim — frame management is irrelevant because the run is over.
                executionResult

        let dispatchDelegateCtor () =
            IlMachineState.executeDelegateConstructor baseClassTypes instruction state
            // can't advance the program counter here - there's no IL instructions executing!
            |> IlMachineState.returnStackFrame loggerFactory baseClassTypes thread
            |> function
                | ReturnFrameResult.NormalReturn state -> (state, WhatWeDid.Executed) |> ExecutionResult.stepped
                | result -> failwith $"unexpected ReturnFrameResult from delegate constructor: %A{result}"

        let dispatchDelegateInvoke () =
            // We've been instructed to run a delegate.
            let delegateToRunAddr =
                match instruction.Arguments.[0] with
                | CliType.ObjectRef (Some addr) -> addr
                | _ -> failwith "expected a managed object ref to delegate"

            let delegateToRun = state.ManagedHeap.NonArrayObjects.[delegateToRunAddr]

            let delegateTypeHandle =
                AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.DelegateType

            let delegateFieldId (fieldName : string) : FieldId =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.DelegateType fieldName
                |> FieldIdentity.fieldId delegateTypeHandle

            let target =
                match
                    delegateToRun
                    |> AllocatedNonArrayObject.DereferenceFieldById (delegateFieldId "_target")
                with
                | CliType.ObjectRef addr -> addr
                | x -> failwith $"TODO: delegate target wasn't an object ref: %O{x}"

            let methodPtr =
                // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                match
                    delegateToRun
                    |> AllocatedNonArrayObject.DereferenceFieldById (delegateFieldId "_methodPtr")
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

            let methodGenerics = instruction.ExecutingMethod.Generics

            // Preserve the original call-site offset from the callvirt Invoke that
            // created this delegate frame.  After returnStackFrame the caller's
            // IlOpIndex has already been advanced, so we must carry the original
            // call-site through to the delegate target's MethodReturnState.
            let originalCallSitePC =
                instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

            // When we return, we need to go back up the stack
            match state |> IlMachineState.returnFromSyntheticStackFrame thread with
            | ReturnFrameResult.NoFrameToReturn -> failwith "unexpectedly nowhere to return from delegate"
            | ReturnFrameResult.DispatchException _ -> failwith "unexpected exception dispatch from delegate frame pop"
            | ReturnFrameResult.NormalReturn state ->

            // Rebuild the stack in normal instance-call shape: `this` below the real arguments.
            // Push `target` first (if instance method) so it ends up at the bottom.
            let state =
                match target with
                | None -> state
                | Some target -> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some target)) thread state

            // Push the real invoke parameters, skipping instruction.Arguments.[0] which is the
            // delegate object itself (not needed by the target method).
            let state =
                let mutable s = state

                for i = 1 to instruction.Arguments.Length - 1 do
                    s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                s

            // Don't advance the program counter again on return; that was already done by the Callvirt that
            // caused this delegate to be invoked.
            let currentThreadState = state.ThreadState.[thread]

            let state =
                IlMachineStateExecution.callMethod
                    loggerFactory
                    baseClassTypes
                    None
                    None
                    false
                    false
                    false
                    methodGenerics
                    methodPtr
                    thread
                    currentThreadState
                    originalCallSitePC
                    false
                    false // wrapExceptionInTargetInvocation
                    state

            ExecutionResult.stepped (state, WhatWeDid.Executed)

        match instruction.ExecutingMethod.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor -> dispatchDelegateCtor ()
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> dispatchDelegateInvoke ()
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, targetName)) ->
            let nameStr =
                match targetName with
                | Some n -> $"\"{n}\""
                | None -> "<attributed method name>"

            failwith
                $"TODO: dispatch [UnsafeAccessor] is unimplemented for {instruction.ExecutingMethod.DeclaringType.Name}::{instruction.ExecutingMethod.Name} (kind={kind}, target={nameStr})"
        | MethodBody.RuntimeProvided (RuntimeBehaviour.Unrecognised name) ->
            failwith
                $"BUG: reached executeOneStep for {instruction.ExecutingMethod.DeclaringType.Name}::{instruction.ExecutingMethod.Name} which is runtime-provided but unclassified ({name}); add explicit handling"
        | MethodBody.Abstract ->
            failwith
                $"BUG: reached executeOneStep for abstract method {instruction.ExecutingMethod.DeclaringType.Name}::{instruction.ExecutingMethod.Name}; virtual dispatch should have resolved to a concrete override"
        | MethodBody.InternalCall
        | MethodBody.PInvoke -> dispatchNative ()
        | MethodBody.Il instructions ->

        match instructions.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ ->
            failwith
                $"Wanted to execute a nonexistent instruction in {instruction.ExecutingMethod.DeclaringType.Name}.{instruction.ExecutingMethod.Name}"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly instruction.ExecutingMethod.DeclaringType.Assembly with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue instruction.ExecutingMethod.DeclaringType.Definition.Get with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogTrace (
            "Executing one step (index {ExecutingIlOpIndex}, max {MaxIlOpIndex}, in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            (Map.maxKeyValue instructions.Locations |> fst),
            executingInType,
            instruction.ExecutingMethod.Name,
            executingInstruction
        )

        match instructions.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> NullaryIlOp.execute loggerFactory baseClassTypes state thread op
        | IlOp.UnaryConst unaryConstIlOp ->
            UnaryConstIlOp.execute state thread unaryConstIlOp |> ExecutionResult.stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.stepped
        | IlOp.Switch immutableArray -> SwitchIlOp.execute state thread immutableArray |> ExecutionResult.stepped
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            UnaryStringTokenIlOp.execute loggerFactory baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.stepped
