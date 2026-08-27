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

    /// Held rather than written inline at the `GetValue` call below: the conversion from an F#
    /// lambda to this delegate type allocates, so inline it would be a fresh delegate on every
    /// interpreted instruction to serve a table that answers from cache after the first.
    let private createLogger =
        ConditionalWeakTable<ILoggerFactory, ILogger>.CreateValueCallback (fun f ->
            f.CreateLogger typeof<Dummy>.DeclaringType
        )

    let private logger (loggerFactory : ILoggerFactory) : ILogger =
        loggerCache.GetValue (loggerFactory, createLogger)

    /// Run the active frame's prologue, if it still has one: the type-initialisation check the
    /// CLR performs on entry to a method, before any of its instructions.
    ///
    /// Returns `Choice2Of2` when the check has finished and the frame may execute, and
    /// `Choice1Of2` when it has not — the initialiser is now the active frame, the thread is
    /// parked on another thread's, or a cached failure has been raised. The flag survives the
    /// first two, so re-entering this frame asks again; the third clears it, because the
    /// type is now `Failed` and asking again would raise a second time at every step if the
    /// frame's own handler caught the first.
    let private runPendingTypeInit
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        : Choice<ExecutionResult, IlMachineState>
        =
        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes ty thread state with
        | StateLoadResult.NothingToDo state ->
            state
            |> IlMachineState.mapFrame
                thread
                state.ThreadState.[thread].ActiveMethodState
                MethodState.clearPendingTypeInit
            |> Choice2Of2
        | StateLoadResult.FirstLoadThis state ->
            ExecutionResult.stepped (state, WhatWeDid.SuspendedForClassInit) |> Choice1Of2
        | StateLoadResult.Blocked (state, blockedBy) ->
            ExecutionResult.stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            |> Choice1Of2
        | StateLoadResult.ThrowingTypeInitializationException state ->
            // The dispatch above already ran against this frame, so the exception's trace names
            // it. Clear the flag on whichever frame is still ours to clear: dispatch may have
            // unwound us entirely, in which case there is nothing to do.
            let state =
                match state.ThreadState |> Map.tryFind thread with
                | None -> state
                | Some threadState ->
                    match threadState.MethodStates |> Map.tryFind threadState.ActiveMethodState with
                    | None -> state
                    | Some _ ->
                        state
                        |> IlMachineState.mapFrame thread threadState.ActiveMethodState MethodState.clearPendingTypeInit

            ExecutionResult.stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            |> Choice1Of2

    /// The active frame's prologue has run; execute one of its instructions.
    let private executeOneStepInitialised
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        (logger : ILogger)
        : ExecutionResult
        =
        let instruction = state.ThreadState.[thread].MethodState

        let dispatchNative () =
            let nativeImport = instruction.ExecutingMethod.TryNativeImport

            // The P/Invoke stub's pre-call clear. Runs on every entry, including a re-entry after
            // the handler parked or pushed a managed callee: errno is per-thread, so the only
            // thread that could have written this slot in between is the one that was parked, and
            // clearing again is therefore exactly equivalent to having cleared once.
            let state = NativeDispatch.clearLastError nativeImport thread state

            let targetAssy =
                state.LoadedAssembly instruction.ExecutingMethod.DeclaringAssemblyFullName
                |> Option.get

            let targetType =
                targetAssy.TypeDefs.[instruction.ExecutingMethod.RequiredDeclaringType.Definition.Get]

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
                let state = NativeDispatch.captureLastError nativeImport thread state

                match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
                | ReturnFrameResult.NormalReturn state -> ExecutionResult.Stepped (state, WhatWeDid.Executed, effect)
                | result -> failwith $"unexpected ReturnFrameResult from extern method return: %A{result}"
            | NativeHandlerResult.Yielded (state, reportsSwitch, effect) ->
                // Native handler ran to completion AND requested a scheduler yield. Frame
                // management is identical to Completed (pop the native frame); the
                // distinction is carried in `WhatWeDid.VoluntaryYield` for the Scheduler,
                // which is where the yield is actually acted on. The frame pop happens
                // *before* the Scheduler sees the outcome, which is what puts any optimistic
                // return value the handler pushed onto the caller's eval stack in time for
                // `Scheduler.onStepOutcome` to rewrite it.
                let state = NativeDispatch.captureLastError nativeImport thread state

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
            | NativeHandlerResult.BlockedRetainingFrame (state, effect) ->
                // The handler parked its own thread and wants re-entering from the top when it
                // wakes, so the native frame stays on the stack and the caller's program
                // counter is left naming the call. `Executed` is what the Scheduler is told
                // because a step really was retired — the handler ran, and blocking is what it
                // did — and the transition it would otherwise apply here has already been
                // performed by the handler.
                ExecutionResult.Stepped (state, WhatWeDid.Executed, effect)
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

            let delegateToRun = ManagedHeap.get delegateToRunAddr state.ManagedHeap

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

            let methodPtrTarget =
                // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                match
                    delegateToRun
                    |> AllocatedNonArrayObject.DereferenceFieldById (delegateFieldId "_methodPtr")
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) -> target
                | d -> failwith $"unexpectedly not a method pointer in delegate invocation: {d}"

            // A method minted by `Reflection.Emit` has no `MethodInfo` sitting in the pointer: it
            // has no MethodDef row for one to be read from, so `Delegate_BindToMethodInfo` stored
            // its registry handle instead. Build the method here, at the moment of invocation --
            // see `DynamicMethodExecution.concretize` for why not earlier, and for what the first
            // such invocation latches.
            let state, methodPtr =
                match methodPtrTarget with
                | FunctionPointerTarget.Managed methodPtr -> state, methodPtr |> Ok
                | FunctionPointerTarget.Dynamic handle ->
                    DynamicMethodExecution.concretize loggerFactory baseClassTypes "delegate invocation" handle state
                | FunctionPointerTarget.RuntimeAllocator ->
                    FunctionPointerTarget.requireManaged "delegate invocation" methodPtrTarget
                    |> fun m -> state, Ok m

            let methodGenerics = instruction.ExecutingMethod.Generics

            // Preserve the original call-site offset from the callvirt Invoke that
            // created this delegate frame.  After returnStackFrame the caller's
            // IlOpIndex has already been advanced, so we must carry the original
            // call-site through to the delegate target's MethodReturnState.
            let originalCallSitePC =
                instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

            // Invoking a delegate triggers the target's declaring-type initialiser, and the rule
            // is not narrowed to static targets: ECMA-335 II.10.5.3.1 also triggers on the first
            // invocation of an instance method of a *value type*, the one receiver whose
            // existence does not already imply initialisation (for a class, holding an instance
            // means `base..ctor()` ran up the chain). Measured against real .NET 10: a delegate
            // over a struct's instance method leaves the initialiser unrun at construction and
            // runs it at invocation, exactly as a static target does.
            // `sourcesPure/DelegateToValueTypeInstanceMethodRunsCctor.cs` pins that.
            //
            // The initialiser runs at invocation, not at `ldftn`: taking a function pointer is
            // not a use of the type, and CoreCLR's `comdelegate.cpp` contains no class-init call
            // at all.
            //
            // No class-initialisation check here: the target's frame carries it and runs it as its
            // own prologue, which is what lets this synthetic frame be popped unconditionally
            // below. Running the check while the stub frame was still up would put a
            // `System.Action.Invoke` stub frame into a failing `.cctor`'s stack trace;
            // `DelegateCctorFailureTraceHasNoStubFrame.cs` pins its absence.

            // When we return, we need to go back up the stack
            match state |> IlMachineState.returnFromSyntheticStackFrame thread with
            | ReturnFrameResult.NoFrameToReturn -> failwith "unexpectedly nowhere to return from delegate"
            | ReturnFrameResult.DispatchException _ -> failwith "unexpected exception dispatch from delegate frame pop"
            | ReturnFrameResult.NormalReturn state ->

            // Both failures below happen *after* the delegate's synthetic `Invoke` frame is popped,
            // and with the caller's program counter put back to its call site: the `callvirt Invoke`
            // advanced past it, and exception dispatch reads that offset both to decide which of the
            // caller's `try` regions cover the throw and to name the frame.
            //
            // Popping first costs a frame in the guest's trace, which real .NET shows and PawPrint
            // does not: both failures happen while CoreCLR is preparing to enter the target, so the
            // target is on its stack. That is a deliberate trade rather than an oversight — leaving
            // the stub frame up instead puts a `System.Action.Invoke` frame in the trace that real
            // .NET never shows, whose absence `sourcesPure/DelegateCctorFailureTraceHasNoStubFrame.cs`
            // pins. See docs/divergences.md, "A delegate invocation that fails before entering its
            // target names no frame for it", for what closing it would take: neither failure has a
            // frame available to name, so the fix is to push one that has executed nothing, and the
            // existing machinery for that (`MethodState.PendingTypeInit`) carries a type to
            // initialise and would run its `.cctor`.
            let raiseFromPoppedStub
                (exceptionType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                (message : string option)
                (state : IlMachineState)
                : ExecutionResult
                =
                let state =
                    match originalCallSitePC with
                    | None -> state
                    | Some pc ->
                        let threadState = state.ThreadState.[thread]

                        state
                        |> IlMachineState.mapFrame
                            thread
                            threadState.ActiveMethodState
                            (MethodState.setProgramCounter pc)

                let state, _whatWeDid =
                    IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                        loggerFactory
                        baseClassTypes
                        exceptionType
                        message
                        thread
                        state

                ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)

            match methodPtr with
            | Error (exceptionType, why) ->
                // The target could not be compiled: measured on real .NET as an
                // `InvalidProgramException` raised by the *first invocation*, not by
                // `CreateDelegate`. Raised after the stub frame is popped, for the reason the
                // paragraph above gives — a stub frame still on the stack lands in the guest's
                // stack trace, which is what `DelegateCctorFailureTraceHasNoStubFrame.cs` pins for
                // the sibling failure — and with the caller's PC put back to its call site, since
                // the `callvirt Invoke` advanced past it and dispatch reads that offset both to
                // decide which of the caller's `try` regions cover the throw and to name the frame.
                //
                // Residual divergence, measured: real .NET's trace has a frame for the dynamic
                // method itself above the caller's, because the failure happens as the JIT compiles
                // that method. PawPrint has no frame to name, having refused to build one. Closing
                // that means pushing the frame anyway and failing in its prologue, as a failed
                // `.cctor` does — `MethodState.PendingTypeInit` and `hasNotStarted` are the
                // existing machinery for "a frame that is on the stack and has executed nothing,
                // whose own clauses are therefore out of scope", which is exactly this situation.
                logger.LogWarning ("delegate invocation refused a dynamic target: {Reason}", why)

                raiseFromPoppedStub exceptionType (DynamicScopeOperand.clrMessageFor baseClassTypes exceptionType) state
            | Ok methodPtr ->

            // An abstract target has no body to run. Reachable only through
            // `Delegate.CreateDelegate` closed over a *null* receiver: a non-null receiver's runtime
            // type is necessarily a subclass of the abstract declaring type, so binding virtualises
            // to a concrete override, and the open shape is refused by
            // `Delegate_BindToMethodInfo` (see `sourcesPure/DelegateBindOpenVirtual.cs`).
            //
            // Real .NET builds that delegate and fails only here, with a catchable
            // `BadImageFormatException` whose HResult is `COR_E_BADIMAGEFORMAT`. The message is
            // measured rather than derived, because it is the CLR's HRESULT text and not the
            // parameterless constructor's — which is a different string with no HRESULT in it. Its
            // prose is localisable, so only the numeral is a machine-independent fact and only the
            // numeral is asserted; this reproduces the invariant-culture wording.
            //
            // Both an abstract class's method and an interface's behave identically, which
            // `sourcesPure/DelegateToAbstractMethodOverNull.cs` pins.
            match methodPtr.Body with
            | MethodBody.Abstract ->
                raiseFromPoppedStub
                    baseClassTypes.BadImageFormatException
                    (Some "An attempt was made to load a program with an incorrect format.\n (0x8007000B)")
                    state
            | MethodBody.Il _
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _ ->

            // Rebuild the stack in normal instance-call shape: the bound argument below the real
            // ones, so it ends up at the bottom.
            //
            // Whether there *is* a bound argument comes from the arity, not from whether `_target`
            // happens to be null. The two differ for a delegate closed over `null` — legal, and
            // what `CreateDelegate(t, null)` produces for a static target one argument wider than
            // `Invoke` (`NativeDelegate.isCompatible` classifies it `Closed` on arity for exactly
            // this reason). Reading null as "nothing to push" would then hand the callee one
            // argument too few: measured on real .NET, a `(string, int) -> int` closed over null
            // and invoked with 7 receives `(null, 7)` and returns accordingly, so the null is a
            // value that is passed, not an absence.
            //
            // `Invoke` supplies `instruction.Arguments.Length - 1` (its own `this` is index 0), so
            // the callee taking one more than that is precisely the closed case.
            let suppliedArgs = instruction.Arguments.Length - 1
            let calleeArgs = MethodInfo.arity methodPtr + (if methodPtr.IsStatic then 0 else 1)

            let state =
                if calleeArgs = suppliedArgs then
                    // Open: `Invoke` supplies everything, and nothing was bound.
                    state
                elif calleeArgs = suppliedArgs + 1 then
                    IlMachineState.pushToEvalStack (CliType.ObjectRef target) thread state
                else
                    failwith
                        $"delegate invocation: %O{methodPtr} takes %d{calleeArgs} argument(s) but Invoke supplied %d{suppliedArgs}; binding should have refused this pairing"

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
                    IlMachineStateExecution.CallSiteTransition.StaysCooperative
                    methodGenerics
                    methodPtr
                    thread
                    currentThreadState
                    originalCallSitePC
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            // A suspension here would be unrecoverable — the delegate's synthetic frame is
            // already popped, so there is nothing to re-execute — but none can occur: a callee's
            // type initialiser runs in the callee frame's own prologue.
            match commitment with
            | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                ExecutionResult.stepped (state, WhatWeDid.Aborted fatal)
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised -> ExecutionResult.stepped (state, WhatWeDid.Executed)

        match instruction.ExecutingMethod.Body with
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor -> dispatchDelegateCtor ()
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke -> dispatchDelegateInvoke ()
        | MethodBody.RuntimeProvided RuntimeBehaviour.StructMarshalStub ->
            StructMarshalStub.executeStubCall loggerFactory baseClassTypes thread instruction state
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, targetName, hasTypeNameOverrides)) ->
            UnsafeAccessorDispatch.execute
                loggerFactory
                baseClassTypes
                thread
                instruction
                kind
                targetName
                hasTypeNameOverrides
                state
        | MethodBody.RuntimeProvided (RuntimeBehaviour.Unrecognised name) ->
            failwith
                $"BUG: reached executeOneStep for {MethodOwner.describe instruction.ExecutingMethod.Owner}::{instruction.ExecutingMethod.Name} which is runtime-provided but unclassified ({name}); add explicit handling"
        | MethodBody.Abstract ->
            failwith
                $"BUG: reached executeOneStep for abstract method {MethodOwner.describe instruction.ExecutingMethod.Owner}::{instruction.ExecutingMethod.Name}; virtual dispatch should have resolved to a concrete override, and a delegate over an abstract target raises BadImageFormatException before calling it"
        | MethodBody.InternalCall
        | MethodBody.PInvoke -> dispatchNative ()
        | MethodBody.Il instructions ->

        match instructions.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ ->
            failwith
                $"Wanted to execute a nonexistent instruction in {MethodOwner.describe instruction.ExecutingMethod.Owner}.{instruction.ExecutingMethod.Name}"
        | true, executingInstruction ->

        // Everything this message needs stays behind the level check, because it runs once per
        // interpreted IL instruction: the assembly lookup is keyed by `AssemblyName` (whose
        // `FullName` recomputes a public key token), `Map.maxKeyValue` walks the instruction
        // map, and the parameterised `LogTrace` overload boxes each argument into an `obj[]`
        // before any provider gets to decide whether it wants the message.
        if logger.IsEnabled LogLevel.Trace then
            // One lookup serves both the type name and the source location. The assembly is
            // resolved from the executing method's *declaring type*, which is what makes it the
            // assembly whose metadata handles that method's `TryResolveMethodSource` indexes.
            let declaringAssembly =
                state.LoadedAssembly instruction.ExecutingMethod.DeclaringAssemblyFullName

            let executingInType =
                match declaringAssembly, instruction.ExecutingMethod.TryDeclaringType with
                | None, _ -> "<unloaded assembly>"
                // A method minted by `Reflection.Emit` has no declaring type to name. This runs
                // once per interpreted instruction of such a body, so it must not be the partial
                // accessor: crashing exactly when someone turns tracing on to debug a dynamic
                // method would be the worst possible place for it. `describe` renders the owner in
                // a form no type could be confused with.
                | Some _, None -> MethodOwner.describe instruction.ExecutingMethod.Owner
                | Some assy, Some declaringType ->
                    match assy.TypeDefs.TryGetValue declaringType.Definition.Get with
                    | true, v -> v.Name
                    | false, _ -> "<unrecognised type>"

            let maxIlOpIndex = Map.maxKeyValue instructions.Locations |> fst

            // The raw program counter, with none of the stepping-back `GuestLocation` does. That
            // exists because a *stuck* thread has already advanced past the call it is parked in;
            // here the instruction is the one about to run, so the offset is exactly the one to
            // attribute.
            let source =
                declaringAssembly
                |> Option.bind (fun assy ->
                    assy.TryResolveMethodSource instruction.ExecutingMethod instruction.IlOpIndex
                )

            // Two templates rather than one carrying a "no source" sentinel: absence is then the
            // absence of the fields, which a structured consumer can filter on, and `SourceLine`
            // stays a number. The framework assemblies ship no symbols, so most instructions
            // would otherwise carry a dangling `at <no source>`.
            match source with
            | None ->
                logger.LogTrace (
                    "Executing one step (index {ExecutingIlOpIndex}, max {MaxIlOpIndex}, in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
                    instruction.IlOpIndex,
                    maxIlOpIndex,
                    executingInType,
                    instruction.ExecutingMethod.Name,
                    executingInstruction
                )
            | Some source ->
                logger.LogTrace (
                    "Executing one step (index {ExecutingIlOpIndex}, max {MaxIlOpIndex}, in method {ExecutingMethodType}.{ExecutingMethodName} at {SourceFile}:{SourceLine}): {ExecutingIlOp}",
                    instruction.IlOpIndex,
                    maxIlOpIndex,
                    executingInType,
                    instruction.ExecutingMethod.Name,
                    source.DocumentPath,
                    source.StartLine,
                    executingInstruction
                )

        match executingInstruction with
        | IlOp.Nullary op -> NullaryIlOp.execute loggerFactory baseClassTypes state thread op
        | IlOp.UnaryConst unaryConstIlOp ->
            UnaryConstIlOp.execute baseClassTypes state thread unaryConstIlOp
            |> ExecutionResult.stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.stepped
        | IlOp.Switch immutableArray -> SwitchIlOp.execute state thread immutableArray |> ExecutionResult.stepped
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            UnaryStringTokenIlOp.execute loggerFactory baseClassTypes unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.stepped

    /// Convert a step that tore the process down into the terminating outcome.
    ///
    /// This is the one translation the step protocol cannot perform for itself: `WhatWeDid` is
    /// reported per-thread and so carries no `ThreadId`, while `ExecutionResult.Aborted` names the
    /// thread that gave up. Applied at the single exit from `executeOneStep`, so nothing
    /// downstream — the scheduler in particular — ever observes `WhatWeDid.Aborted`; a step that
    /// aborted is not a step that retired, and `Scheduler.onStepOutcome` has no answer for one.
    let internal surfaceAbort (thread : ThreadId) (result : ExecutionResult) : ExecutionResult =
        match result with
        | ExecutionResult.Stepped (state, WhatWeDid.Aborted fatal, StepEffect.NoEffect) ->
            ExecutionResult.Aborted (state, thread, fatal)
        | ExecutionResult.Stepped (_, WhatWeDid.Aborted fatal, effect) ->
            // An aborting step did not finish whatever it was describing, so an effect here would
            // be a write the driver is being asked to perform on behalf of a step that never
            // completed. No producer does this today; fail loudly rather than silently choosing
            // between dropping the effect and performing it.
            failwith
                $"logic error: thread %O{thread} aborted (%O{fatal.Code}) while also requesting the step effect %O{effect}; an aborting step must not emit one"
        | _ -> result

    /// Execute one step of the given thread: its active frame's prologue if it still has one, and
    /// otherwise one IL instruction.
    ///
    /// A prologue that finishes does not consume the step. The check is bookkeeping the CLR emits
    /// into the callee's entry rather than an instruction the guest wrote, so charging virtual
    /// time for it would make a call cost more the first time a type is touched.
    let executeOneStep
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = logger loggerFactory

        match state.ThreadState.[thread].MethodState.PendingTypeInit with
        | None ->
            executeOneStepInitialised loggerFactory baseClassTypes state thread logger
            |> surfaceAbort thread
        | Some ty ->

        match runPendingTypeInit loggerFactory baseClassTypes state thread ty with
        | Choice1Of2 result -> result |> surfaceAbort thread
        | Choice2Of2 state ->
            executeOneStepInitialised loggerFactory baseClassTypes state thread logger
            |> surfaceAbort thread
