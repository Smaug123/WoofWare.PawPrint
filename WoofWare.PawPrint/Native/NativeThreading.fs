namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeThreading =
    let private objectOwnFieldId
        (state : IlMachineState)
        (heapObj : AllocatedNonArrayObject)
        (fieldName : string)
        : FieldId
        =
        IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType fieldName

    let private delegateFieldId
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (fieldName : string)
        : FieldId
        =
        FieldIdentity.requiredNonGenericInstanceFieldId state.ConcreteTypes baseClassTypes.DelegateType fieldName

    /// Recover the heap address stored inside a `ThreadHandle` QCall argument.
    /// `ThreadHandle` is `internal readonly struct ThreadHandle { IntPtr _ptr }`
    /// in CoreCLR; the QCall marshaller may flatten it to a bare `nativeint`
    /// or pass the wrapping struct cell. In both cases the underlying value is
    /// the `_DONT_USE_InternalThread` we wrote in `initializeThreadObject`,
    /// which is the heap address of the Thread object reinterpreted as a
    /// `nativeint`.
    let private threadAddrFromThreadHandle
        (state : IlMachineState)
        (operation : string)
        (handleArg : CliType)
        : ManagedHeapAddress
        =
        match handleArg |> CliType.unwrapPrimitiveLike with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
            ManagedHeapAddress (int addrInt)
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_ptr"

            match CliValueType.DereferenceFieldById ptrField vt |> CliType.unwrapPrimitiveLike with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
                ManagedHeapAddress (int addrInt)
            | other -> failwith $"%s{operation}: expected Verbatim nativeint inside ThreadHandle._ptr, got %O{other}"
        | other -> failwith $"%s{operation}: unexpected shape for ThreadHandle argument: %O{other}"

    /// Reverse-lookup the interpreter `ThreadId` that owns the given Thread
    /// heap object. Distinguishes "guest handed a wild pointer" (interpreter
    /// bug — no heap object exists at the address at all) from "Thread object
    /// was never Start()ed" (guest bug that the real CLR would surface as
    /// `ThreadStateException` once exception synthesis lands here). Both
    /// surface as `failwith` for now because the interpreter cannot yet
    /// raise managed exceptions on behalf of native helpers.
    let private threadIdFromThreadAddr
        (state : IlMachineState)
        (operation : string)
        (threadAddr : ManagedHeapAddress)
        : ThreadId
        =
        state.ManagedThreadObjects
        |> Map.toSeq
        |> Seq.tryPick (fun (tid, addr) -> if addr = threadAddr then Some tid else None)
        |> Option.defaultWith (fun () ->
            match state.ManagedHeap.NonArrayObjects |> Map.tryFind threadAddr with
            | Some _ ->
                failwith
                    $"%s{operation}: Thread object at {threadAddr} was never Start()ed. The real CLR raises ThreadStateException here; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
            | None ->
                failwith
                    $"%s{operation}: no heap object at {threadAddr} (interpreter bug: stale or invalid Thread reference)."
        )

    /// Core of the Thread.Join semantics shared between the pre-.NET 10 InternalCall and the
    /// .NET 10 ThreadNative_Join QCall. Returns the post-call state (with any necessary scheduler
    /// block applied) and the bool result the caller should push as Join's return value.
    let private executeJoinCore
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (threadAddr : ManagedHeapAddress)
        (timeout : int)
        : IlMachineState * bool
        =
        // `timeout` follows Thread.Join semantics: -1 (Timeout.Infinite) blocks
        // until the target terminates, 0 is a non-blocking poll. Any other value
        // is a finite wait, which PawPrint cannot honour because the scheduler
        // doesn't model wall-clock time — a guest that relies on a Join(100) to
        // fall through after a timeout would instead block forever here. Fail
        // loud rather than silently diverging from guest semantics; once a
        // virtual-clock story lands, replace this with the real finite-wait
        // implementation. The CLR also rejects timeout < -1 with
        // ArgumentOutOfRangeException; we can't synthesise that yet, so the
        // same failwith covers it.
        match timeout with
        | -1
        | 0 -> ()
        | other ->
            failwith
                $"Thread.Join: millisecondsTimeout=%d{other} is not supported. Only -1 (Timeout.Infinite) and 0 (non-blocking poll) are implemented; finite timeouts require a virtual clock PawPrint does not yet model. Negative values other than -1 would raise ArgumentOutOfRangeException in the real CLR, which PawPrint doesn't synthesise yet."

        let targetThreadId = threadIdFromThreadAddr state "Thread.Join" threadAddr

        // Self-join is an immediate deadlock: blocking ourselves on ourselves means
        // no thread will ever wake us. The real CLR also hangs, but in PawPrint this
        // would surface much later as a generic "no runnable threads" failure far
        // from the actual Join call; report it at the cause site.
        if targetThreadId = ctx.Thread then
            failwith
                $"Thread.Join: thread {ctx.Thread} is attempting to join itself, which would deadlock. The real CLR also hangs on self-join; PawPrint reports this at the call site rather than as a downstream deadlock."

        let targetState =
            state.ThreadState
            |> Map.tryFind targetThreadId
            |> Option.defaultWith (fun () ->
                failwith $"Thread.Join: target ThreadId {targetThreadId} has no ThreadState"
            )

        let targetTerminated = targetState.Status = ThreadStatus.Terminated

        match timeout with
        | 0 -> state, targetTerminated
        | _ ->
            // The bool result is true regardless of whether we end up blocking: the
            // -1 timeout is "wait forever", so the only way control flows past the
            // join is via target termination, which always yields `true`. The
            // caller's IL slot for Join's return is filled before we (possibly)
            // block, so when the scheduler later flips us back to Runnable the
            // pushed value is already sitting as Join's return value.
            let state =
                if targetTerminated then
                    state
                else
                    Scheduler.blockOnJoin ctx.Thread targetThreadId state

            state, true

    /// Sets up the managed thread ID, priority, and native handle sentinel on the Thread object,
    /// and pre-allocates a `NotStarted` interpreter `ThreadState` bound to the Thread heap
    /// address. Backs the Initialize InternalCall in pre-.NET 10 BCLs and the
    /// ThreadNative_Initialize QCall in .NET 10+.
    ///
    /// Pre-allocating the `ThreadState` here (rather than at `StartInternal` time) is what
    /// lets the `IsBackground` QCalls — which the thread-pool worker setup invokes between
    /// the constructor and `Start` — find a per-thread record to store their value on. The
    /// scheduler ignores `NotStarted` threads, so the slot stays inert until `StartInternal`
    /// populates a bottom frame and flips the status to `Runnable`.
    let private initializeThreadObject
        (threadAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let managedThreadId = state.NextManagedThreadId
        let threadPriorityNormal = 2
        let (ManagedHeapAddress addrInt) = threadAddr

        let threadObj = ManagedHeap.get threadAddr state.ManagedHeap

        let updatedObj =
            threadObj
            |> AllocatedNonArrayObject.SetFieldById
                (objectOwnFieldId state threadObj "_managedThreadId")
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> AllocatedNonArrayObject.SetFieldById
                (objectOwnFieldId state threadObj "_priority")
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> AllocatedNonArrayObject.SetFieldById
                (objectOwnFieldId state threadObj "_DONT_USE_InternalThread")
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set threadAddr updatedObj state.ManagedHeap
                NextManagedThreadId = state.NextManagedThreadId + 1
            }

        IlMachineState.allocateUnstartedThread threadAddr state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ThreadNative_GetCurrentThread",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "GetCurrentThread",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            // .NET 10 QCall: writes the calling thread's managed Thread object into *thread.
            let operation = "ThreadNative_GetCurrentThread"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let threadOut =
                NativeCall.objectHandleOnStackTarget operation state "thread" instruction.Arguments.[0]

            let addr, state =
                IlMachineState.getOrAllocateManagedThreadObject ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    threadOut
                    (CliType.ObjectRef (Some addr))

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "ThreadNative_Initialize",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "Initialize",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            // .NET 10 QCall replacing the parameterless Thread.Initialize InternalCall. The Thread
            // reference comes in through ObjectHandleOnStack rather than as `this`.
            let operation = "ThreadNative_Initialize"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let threadPtr =
                NativeCall.objectHandleOnStackTarget operation state "thread" instruction.Arguments.[0]

            let threadValue = IlMachineState.readManagedByref ctx.BaseClassTypes state threadPtr

            let threadAddr =
                match threadValue with
                | CliType.ObjectRef (Some a) -> a
                | CliType.ObjectRef None ->
                    failwith $"%s{operation}: ObjectHandleOnStack pointed to a null Thread reference"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            let state, _newThreadId = initializeThreadObject threadAddr state
            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "ThreadNative_Join",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          _,
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            objectHandleGenerics.IsEmpty
            ->
            // .NET 10 QCall replacing the Thread.Join(int) InternalCall. Argument 0 is
            // ObjectHandleOnStack pointing at the Thread reference (this is the LibraryImport
            // marshalling shape; the reference assembly's `Join(int)` synthesises the handle
            // on the caller's stack). Argument 1 is the timeout. The QCall returns a 32-bit
            // BOOL where non-zero means "the join completed".
            let operation = "ThreadNative_Join"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let threadPtr =
                NativeCall.objectHandleOnStackTarget operation state "thread" instruction.Arguments.[0]

            let threadValue = IlMachineState.readManagedByref ctx.BaseClassTypes state threadPtr

            let threadAddr =
                match threadValue with
                | CliType.ObjectRef (Some a) -> a
                | CliType.ObjectRef None ->
                    failwith $"%s{operation}: ObjectHandleOnStack pointed to a null Thread reference"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 timeout, got %O{other}"

            let state, result = executeJoinCore ctx state threadAddr timeout

            let resultInt = if result then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 resultInt)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "ThreadNative_SetIsBackground",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "SetIsBackground",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Threading",
                                              "ThreadHandle",
                                              threadHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics) ],
          MethodReturnType.Void when threadHandleGenerics.IsEmpty && boolGenerics.IsEmpty ->
            // .NET 10 QCall backing `Thread.IsBackground = value`. We don't yet model the
            // "process terminates when the last foreground thread exits" semantics, so the
            // flag is stored on `ThreadState.IsBackground` purely so the paired getter
            // round-trips and guest code that reads `Thread.IsBackground` after writing it
            // sees its own value. The real CLR also raises `ThreadStateException` when the
            // target is `_isDead`; PawPrint can't synthesise managed exceptions from QCalls
            // yet, so a Set against a terminated thread would silently succeed here. That
            // mirrors the existing gap in `executeJoinCore` rather than introducing a new
            // one — a guest that depends on the throw will need exception synthesis first.
            let operation = "ThreadNative_SetIsBackground"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let threadAddr =
                threadAddrFromThreadHandle state operation instruction.Arguments.[0]

            let targetThreadId = threadIdFromThreadAddr state operation threadAddr

            // Interop.BOOL is int32-backed: FALSE=0, TRUE=1. The IL marshaller flattens the
            // enum to its underlying value before the QCall, so unwrap the primitive and
            // treat any non-zero as truthy (matches `result != 0` in the BCL).
            let value =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLikeDeep with
                | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
                | other -> failwith $"%s{operation}: expected Interop.BOOL as Int32, got %O{other}"

            let updatedThreadState =
                state.ThreadState
                |> Map.tryFind targetThreadId
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: target ThreadId {targetThreadId} has no ThreadState"
                )
                |> fun ts ->
                    { ts with
                        IsBackground = value
                    }

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add targetThreadId updatedThreadState
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "ThreadNative_GetIsBackground",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "GetIsBackground",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Threading",
                                              "ThreadHandle",
                                              threadHandleGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics)) when
            threadHandleGenerics.IsEmpty && boolGenerics.IsEmpty
            ->
            // .NET 10 QCall backing the `Thread.IsBackground` getter. Returns Interop.BOOL
            // (int32-backed: TRUE=1, FALSE=0); we push 0/1 directly because the IL caller
            // reinterprets the return on the stack via `(int)result != 0`.
            let operation = "ThreadNative_GetIsBackground"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let threadAddr =
                threadAddrFromThreadHandle state operation instruction.Arguments.[0]

            let targetThreadId = threadIdFromThreadAddr state operation threadAddr

            let isBackground =
                state.ThreadState
                |> Map.tryFind targetThreadId
                |> Option.map (fun ts -> ts.IsBackground)
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: target ThreadId {targetThreadId} has no ThreadState"
                )

            let resultInt = if isBackground then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 resultInt)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "GetCurrentThreadNative",
          [],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Threading",
                                                                      "Thread",
                                                                      threadGenerics)) when threadGenerics.IsEmpty ->
            let addr, state =
                IlMachineState.getOrAllocateManagedThreadObject ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib", "System.Threading", "Thread", "Initialize", [], MethodReturnType.Void ->
            // Pre-.NET 10 InternalCall backing `new Thread(...)` constructor. .NET 10 routes the
            // same logic through the ThreadNative_Initialize QCall above.
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let thisRef, state = IlMachineState.popEvalStack ctx.Thread state

            let threadAddr =
                match thisRef with
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"Thread.Initialize: expected ObjectRef for 'this', got %O{other}"

            let state, _newThreadId = initializeThreadObject threadAddr state
            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib", "System.Threading", "Thread", "StartInternal", _, MethodReturnType.Void ->
            // StartInternal (ThreadHandle t, int stackSize, int priority, Interop.BOOL isThreadPool, char* pThreadName) -> void
            // We don't yet model stack size / priority / thread-pool / native name; we recover the
            // Thread heap object from the handle and spawn a new interpreter thread that begins
            // executing the user-supplied delegate directly, bypassing the BCL StartCallback
            // path (which otherwise pulls in ExecutionContext/culture/autorelease machinery).
            let threadAddr =
                threadAddrFromThreadHandle state "Thread.StartInternal" instruction.Arguments.[0]

            // The Thread heap object is always bound to a `NotStarted` interpreter
            // ThreadId from `Thread.Initialize`; recover that slot here and fill in
            // its bottom frame below. Double-Start detection rides on the slot's
            // status: anything other than `NotStarted` means `Start` has already
            // succeeded (Runnable / blocked / Terminated) — the real runtime nulls
            // `_startHelper` on a successful Start so the second call would observe
            // `ThreadStateException`, and `startUnstartedThread` surfaces the same
            // condition via its status assert. When exception synthesis lands,
            // replace that loud failure with the ThreadStateException raise plus
            // the `_startHelper` nulling.
            let newThreadId = threadIdFromThreadAddr state "Thread.StartInternal" threadAddr

            let threadObj = ManagedHeap.get threadAddr state.ManagedHeap

            let startHelperAddr =
                match
                    AllocatedNonArrayObject.DereferenceFieldById
                        (objectOwnFieldId state threadObj "_startHelper")
                        threadObj
                with
                | CliType.ObjectRef (Some a) -> a
                | other ->
                    failwith $"Thread.StartInternal: expected non-null _startHelper on Thread object, got %O{other}"

            let startHelperObj = ManagedHeap.get startHelperAddr state.ManagedHeap

            let delegateAddr =
                match
                    AllocatedNonArrayObject.DereferenceFieldById
                        (objectOwnFieldId state startHelperObj "_start")
                        startHelperObj
                with
                | CliType.ObjectRef (Some a) -> a
                | other ->
                    failwith $"Thread.StartInternal: expected non-null StartHelper._start delegate, got %O{other}"

            let delegateObj = ManagedHeap.get delegateAddr state.ManagedHeap

            let target =
                match
                    AllocatedNonArrayObject.DereferenceFieldById
                        (delegateFieldId ctx.BaseClassTypes state "_target")
                        delegateObj
                with
                | CliType.ObjectRef addr -> addr
                | other -> failwith $"Thread.StartInternal: expected ObjectRef for delegate _target, got %O{other}"

            let targetMethod =
                // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                match
                    AllocatedNonArrayObject.DereferenceFieldById
                        (delegateFieldId ctx.BaseClassTypes state "_methodPtr")
                        delegateObj
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                | other ->
                    failwith $"Thread.StartInternal: expected FunctionPointer in delegate _methodPtr, got %O{other}"

            let containingAssembly =
                state.LoadedAssembly targetMethod.DeclaringType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"Thread.StartInternal: assembly {targetMethod.DeclaringType.Assembly.Name} not loaded"
                )

            let thisArgs =
                if targetMethod.IsStatic then
                    System.Collections.Immutable.ImmutableArray.Empty
                else
                    match target with
                    | Some t ->
                        // For delegates bound to value-type instance methods, the receiver
                        // must be a managed pointer into the boxed heap object's value
                        // data, matching `callMethod`'s coercion in IlMachineStateExecution.
                        let declaringTypeDef =
                            containingAssembly.TypeDefs.[targetMethod.DeclaringType.Definition.Get]

                        let receiver =
                            if
                                DumpedAssembly.isValueType ctx.BaseClassTypes state._LoadedAssemblies declaringTypeDef
                            then
                                CliType.RuntimePointer (
                                    CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.HeapValue t, []))
                                )
                            else
                                CliType.ObjectRef (Some t)

                        System.Collections.Immutable.ImmutableArray.Create receiver
                    | None -> failwith "Thread.StartInternal: instance-method delegate has null _target"

            // ParameterizedThreadStart passes StartHelper._startArg as the single
            // declared parameter; plain ThreadStart takes none. `this` is not counted
            // in Signature.ParameterTypes.
            let args =
                match targetMethod.Signature.ParameterTypes.Length with
                | 0 -> thisArgs
                | 1 ->
                    let startArg =
                        AllocatedNonArrayObject.DereferenceFieldById
                            (objectOwnFieldId state startHelperObj "_startArg")
                            startHelperObj

                    thisArgs.Add startArg
                | other ->
                    failwith
                        $"Thread.StartInternal: target method %s{targetMethod.Name} declares %d{other} parameters; only ThreadStart/ParameterizedThreadStart are supported"

            let newMethodState =
                match
                    MethodState.Empty
                        state.ConcreteTypes
                        ctx.BaseClassTypes
                        state._LoadedAssemblies
                        containingAssembly
                        targetMethod
                        targetMethod.Generics
                        args
                        None
                with
                | Ok ms -> ms
                | Error _ -> failwith "Thread.StartInternal: failed to build MethodState for thread delegate target"

            // The ThreadId slot was minted at `Thread.Initialize` time and bound to
            // `threadAddr` in `ManagedThreadObjects`; promote it from `NotStarted`
            // to `Runnable` and install the worker's bottom frame in one step.
            // Status / frame transitions go through `startUnstartedThread` so the
            // double-Start guard (status must be `NotStarted`) lives next to the
            // mutation it protects.
            let state = IlMachineState.startUnstartedThread newThreadId newMethodState state

            // ECMA-335: a type's .cctor must run before any of its static methods
            // or before the first instance is touched. For delegates bound to a
            // method on a not-yet-initialised type, the normal call path would
            // trigger initialisation, but we bypass that by building the worker's
            // initial frame directly. Route the worker through
            // ensureTypeInitialised so all four cctor states are handled: already
            // initialised (no-op), fresh load (cctor frame pushed on the worker,
            // runs before the target method), another thread is mid-init (worker
            // marked BlockedOnClassInit so the scheduler stalls it), or the cctor
            // already failed (cached TypeInitializationException dispatched onto
            // the worker's frames).
            let declaringTypeHandle =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    targetMethod.DeclaringType.Identity
                    targetMethod.DeclaringType.Generics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Thread.StartInternal: declaring type %s{targetMethod.DeclaringType.Name} of delegate target is not registered in ConcreteTypes"
                )

            let state, workerInitOutcome =
                IlMachineStateExecution.ensureTypeInitialised
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    newThreadId
                    declaringTypeHandle
                    state

            // The worker's bottom frame is the target method itself, not a
            // `call` of the target. That matters for BlockedOnClassInit: the
            // speculative wake in Scheduler.onStepOutcome would flip the worker
            // back to Runnable on the blocker's next step, but unlike every
            // other call site we can't re-run ensureTypeInitialised when the
            // worker resumes — it would just start executing the target's
            // first IL op before the cctor has actually finished. Fail loud
            // for now; every other cross-thread-InProgress path in the
            // interpreter also fails loud (see loadClass and UnaryMetadataIlOp
            // Call/Newobj). Fixing this properly requires either a synthetic
            // caller frame that issues the call or first-class class-init
            // re-entry, both of which are out of scope for this change.
            match workerInitOutcome with
            | WhatWeDid.BlockedOnClassInit _ ->
                failwith
                    $"Thread.StartInternal: target type %s{targetMethod.DeclaringType.Name} is being initialised on another thread. Cross-thread class-init synchronisation for workers is not yet implemented."
            | _ -> ()

            let state = Scheduler.onWorkerSpawned newThreadId workerInitOutcome state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "Join",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // Pre-.NET 10 InternalCall path for `bool Thread.Join(int millisecondsTimeout)`.
            // The deployed CoreLib stamps this method as InternalCall; the managed body we see
            // in source lives only in the reference assembly. `this` is arg 0, the timeout is
            // arg 1. .NET 10 routes the same logic through the ThreadNative_Join QCall above.
            let thisArg = instruction.Arguments.[0]

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"Thread.Join: expected int32 timeout, got %O{other}"

            let threadAddr =
                match thisArg with
                | CliType.ObjectRef (Some a) -> a
                | other -> failwith $"Thread.Join: expected non-null Thread `this`, got %O{other}"

            let state, result = executeJoinCore ctx state threadAddr timeout

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some
        | _ -> None
