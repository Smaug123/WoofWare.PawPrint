namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    /// `executeOneStep` runs once per interpreted IL instruction, and `CreateLogger` is not free
    /// at that rate: the `Type` overload formats a display name on every call, and an
    /// `ILoggerFactory` is under no obligation to return a cached instance (the one in
    /// `WoofWare.PawPrint.Test` allocates a fresh logger per call). Ask each factory once.
    /// Keyed weakly so that disposing a factory still lets it be collected.
    let private loggerCache = ConditionalWeakTable<ILoggerFactory, ILogger> ()

    let private logger (loggerFactory : ILoggerFactory) : ILogger =
        loggerCache.GetValue (loggerFactory, fun f -> f.CreateLogger typeof<Dummy>.DeclaringType)

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = logger loggerFactory
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
            | NativeHandlerResult.Yielded (state, reportsSwitch, effect) ->
                // Native handler ran to completion AND requested a scheduler yield. Frame
                // management is identical to Completed (pop the native frame); the
                // distinction is carried in `WhatWeDid.VoluntaryYield` for the Scheduler,
                // which is where the yield is actually acted on. Note the frame pop happens
                // *before* the Scheduler sees the outcome, which is what puts any optimistic
                // return value the handler pushed onto the caller's eval stack in time for
                // `Scheduler.onStepOutcome` to rewrite it.
                match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
                | ReturnFrameResult.NormalReturn state ->
                    ExecutionResult.Stepped (state, WhatWeDid.VoluntaryYield reportsSwitch, effect)
                | result -> failwith $"unexpected ReturnFrameResult from yielding extern method return: %A{result}"
            | NativeHandlerResult.PushedManagedCallee (state, effect) ->
                // The handler pushed a managed callee on top of itself for re-entry: leave
                // the native frame on the stack so the dispatch loop runs the callee, then
                // re-enters this native method on a future step.
                ExecutionResult.Stepped (state, WhatWeDid.SuspendedForManagedCall, effect)
            | NativeHandlerResult.RaiseException (state, exnType, message, effect) ->
                // The handler wants to raise `exnType`. Allocate the exception, call its
                // parameterless ctor (overwriting `_message` afterwards if the handler
                // supplied one), and arm dispatch-on-return; leave the native frame
                // on the stack so exception dispatch can unwind through it on the ctor's
                // `Ret`. The handler is never re-entered. We surface
                // SuspendedForManagedCall because, from the Scheduler's point of view, a
                // managed callee (the ctor) has been pushed on top of the native frame.
                let state, _whatWeDid =
                    IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                        loggerFactory
                        baseClassTypes
                        exnType
                        message
                        thread
                        state

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
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) ->
                    FunctionPointerTarget.requireManaged "delegate invocation" target
                | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

            let methodGenerics = instruction.ExecutingMethod.Generics

            // Preserve the original call-site offset from the callvirt Invoke that
            // created this delegate frame.  After returnStackFrame the caller's
            // IlOpIndex has already been advanced, so we must carry the original
            // call-site through to the delegate target's MethodReturnState.
            let originalCallSitePC =
                instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

            // Calling a method runs its declaring type's initialiser first, and invoking a
            // delegate is a call like any other: every sibling call op does this
            // (`UnaryMetadataCallOps.executeCall`, `executeCallvirt`, `executeCalli`,
            // `UnaryMetadataObjectOps`' `newobj`), and only this path was missing it.
            //
            // The rule is not narrowed to static targets. ECMA-335 II.10.5.3.1 also triggers on
            // the first invocation of an instance method of a *value type*, which is the one
            // receiver whose existence does not already imply initialisation — for a class,
            // holding an instance means `base..ctor()` ran up the chain. Measured against real
            // .NET 10: a delegate over a struct's instance method leaves the initialiser
            // unrun at construction and runs it at invocation, exactly as a static target does.
            // `sourcesPure/DelegateToValueTypeInstanceMethodRunsCctor.cs` pins that.
            //
            // Deferring to invocation rather than running it at `ldftn` is likewise measured, not
            // assumed: taking a function pointer is not a use of the type, and CoreCLR's
            // `comdelegate.cpp` contains no class-init call at all.
            let declaringTypeHandle =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    methodPtr.DeclaringType.Identity
                    methodPtr.DeclaringType.Generics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"delegate invocation: declaring type %s{methodPtr.DeclaringType.Namespace}.%s{methodPtr.DeclaringType.Name} of the target method is not registered in AllConcreteTypes"
                )

            // For a *synthesised* method the declaring type is the subject rather than the owner,
            // so "calling a member initialises its type" does not follow; `initialisesDeclaringType`
            // is the single place that question is answered. Same gate as `calli`.
            let classInitialisation =
                let required =
                    match methodPtr with
                    | MethodInfo.Metadata _ -> true
                    | MethodInfo.Synthesised (_, kind) -> SynthesisedMethod.initialisesDeclaringType kind

                if required then
                    IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state
                else
                    StateLoadResult.NothingToDo state

            // This runs *before* the synthetic frame is popped, which is what makes the retry
            // free. Everything above is a pure read — `instruction.Arguments`, the heap, the
            // concrete-type table — so on the suspension paths below we leave this frame exactly
            // as we found it, the initialiser is pushed on top of it, and when that returns the
            // dispatch loop re-enters `dispatchDelegateInvoke` from the top and recomputes the
            // same values. There is nothing to save and restore, unlike `calli`, which must push
            // its function pointer back because it had to pop it before the arguments.
            //
            // Re-entering a runtime-provided frame this way is the same mechanism `dispatchNative`
            // above relies on for `NativeHandlerResult.SuspendedForClassInit`.
            match classInitialisation with
            | StateLoadResult.FirstLoadThis state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForClassInit)
            | StateLoadResult.ThrowingTypeInitializationException state ->
                ExecutionResult.stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            | StateLoadResult.Blocked (state, blockedBy) ->
                ExecutionResult.stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            | StateLoadResult.NothingToDo state ->

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

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    false
                    false
                    false
                    methodGenerics
                    methodPtr
                    thread
                    currentThreadState
                    originalCallSitePC
                    ConstructedObjectDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            // The class initialisation above covers the *target's declaring type*, but the target
            // itself can still ask to suspend from inside the call: `Activator.CreateInstance<T>()`
            // is serviced as an intrinsic and suspends to run `T`'s initialiser, and
            // `Func<Foo> f = Activator.CreateInstance<Foo>;` is legal C#. By this point our
            // synthetic frame is gone, so unlike a call opcode there is nothing left to re-execute
            // and no program counter to leave unadvanced — the suspension would be silently
            // dropped and the activator would never run. Refuse loudly instead of corrupting the
            // frame; `calli` handles the same situation by restoring its stack and retrying, which
            // is only open to it because its frame survives.
            match commitment with
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | IlMachineStateExecution.CallCommitment.SuspendedForClassInit ->
                failwith
                    $"TODO: delegate invocation of %s{methodPtr.DeclaringType.Namespace}.%s{methodPtr.DeclaringType.Name}::%s{methodPtr.Name} suspended for class initialisation after the delegate's synthetic frame was popped; there is no frame left to re-enter, so the call would be silently dropped"

        match instruction.ExecutingMethod.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor -> dispatchDelegateCtor ()
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> dispatchDelegateInvoke ()
        | MethodBody.RuntimeProvided RuntimeBehaviour.StructMarshalStub ->
            StructMarshalStub.executeStubCall loggerFactory baseClassTypes thread instruction state
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

        // Everything this message needs stays behind the level check, because it runs once per
        // interpreted IL instruction: the assembly lookup is keyed by `AssemblyName` (whose
        // `FullName` recomputes a public key token), `Map.maxKeyValue` walks the instruction
        // map, and the parameterised `LogTrace` overload boxes each argument into an `obj[]`
        // before any provider gets to decide whether it wants the message.
        if logger.IsEnabled LogLevel.Trace then
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

        // `executingInstruction` is the value `TryGetValue` above already produced for this
        // index; re-indexing `Locations` would be a second lookup for the same key.
        match executingInstruction with
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
