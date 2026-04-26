namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeThreading =
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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib", "System.Threading", "Thread", "Initialize", [], MethodReturnType.Void ->
            // InternalCall backing `new Thread(...)` constructor. Sets up the managed
            // thread ID, priority, and native handle sentinel on the Thread object.
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let thisRef, state = IlMachineState.popEvalStack ctx.Thread state

            let threadAddr =
                match thisRef with
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"Thread.Initialize: expected ObjectRef for 'this', got %O{other}"

            let managedThreadId = state.NextManagedThreadId
            let threadPriorityNormal = 2
            let (ManagedHeapAddress addrInt) = threadAddr

            let updatedObj =
                ManagedHeap.get threadAddr state.ManagedHeap
                |> AllocatedNonArrayObject.SetField
                    "_managedThreadId"
                    (CliType.Numeric (CliNumericType.Int32 managedThreadId))
                |> AllocatedNonArrayObject.SetField
                    "_priority"
                    (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
                |> AllocatedNonArrayObject.SetField
                    "_DONT_USE_InternalThread"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

            let state =
                { state with
                    ManagedHeap = ManagedHeap.set threadAddr updatedObj state.ManagedHeap
                    NextManagedThreadId = state.NextManagedThreadId + 1
                }

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib", "System.Threading", "Thread", "StartInternal", _, MethodReturnType.Void ->
            // StartInternal (ThreadHandle t, int stackSize, int priority, Interop.BOOL isThreadPool, char* pThreadName) -> void
            // We don't yet model stack size / priority / thread-pool / native name; we recover the
            // Thread heap object from the handle and spawn a new interpreter thread that begins
            // executing the user-supplied delegate directly, bypassing the BCL StartCallback
            // path (which otherwise pulls in ExecutionContext/culture/autorelease machinery).
            let threadHandleArg = instruction.Arguments.[0]

            let threadAddr =
                match threadHandleArg |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
                    ManagedHeapAddress (int addrInt)
                | CliType.ValueType vt ->
                    match CliValueType.DereferenceField "_ptr" vt |> CliType.unwrapPrimitiveLike with
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim addrInt)) ->
                        ManagedHeapAddress (int addrInt)
                    | other ->
                        failwith
                            $"Thread.StartInternal: expected Verbatim nativeint inside ThreadHandle._ptr, got %O{other}"
                | other -> failwith $"Thread.StartInternal: unexpected shape for ThreadHandle argument: %O{other}"

            // Double-Start detection: if this Thread heap object is already bound to
            // an interpreter thread, the guest has called Start() twice. The real
            // runtime nulls out _startHelper on a successful Start so the second
            // call throws ThreadStateException; we can't synthesise that exception
            // yet, so fail the interpreter loudly instead of silently spawning a
            // second worker. When exception synthesis lands, replace this failwith
            // with the ThreadStateException raise and the _startHelper nulling.
            if state.ManagedThreadObjects |> Map.exists (fun _ addr -> addr = threadAddr) then
                failwith
                    $"Thread.StartInternal: Thread object at {threadAddr} has already been started; the guest would observe ThreadStateException, which is not yet synthesised. Double-Start is a guest bug."

            let threadObj = ManagedHeap.get threadAddr state.ManagedHeap

            let startHelperAddr =
                match AllocatedNonArrayObject.DereferenceField "_startHelper" threadObj with
                | CliType.ObjectRef (Some a) -> a
                | other ->
                    failwith $"Thread.StartInternal: expected non-null _startHelper on Thread object, got %O{other}"

            let startHelperObj = ManagedHeap.get startHelperAddr state.ManagedHeap

            let delegateAddr =
                match AllocatedNonArrayObject.DereferenceField "_start" startHelperObj with
                | CliType.ObjectRef (Some a) -> a
                | other ->
                    failwith $"Thread.StartInternal: expected non-null StartHelper._start delegate, got %O{other}"

            let delegateObj = ManagedHeap.get delegateAddr state.ManagedHeap

            let target =
                match AllocatedNonArrayObject.DereferenceField "_target" delegateObj with
                | CliType.ObjectRef addr -> addr
                | other -> failwith $"Thread.StartInternal: expected ObjectRef for delegate _target, got %O{other}"

            let targetMethod =
                // Delegate._methodPtr is typed IntPtr (primitive-like); unwrap to the inner NativeInt.
                match
                    AllocatedNonArrayObject.DereferenceField "_methodPtr" delegateObj
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
                    let startArg = AllocatedNonArrayObject.DereferenceField "_startArg" startHelperObj
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

            let state, newThreadId =
                IlMachineState.addThread newMethodState targetMethod.DeclaringType.Assembly state

            // Link the fresh ThreadId to the pre-existing Thread heap object so that
            // Thread.CurrentThread on the new thread returns the original Thread reference.
            let state =
                { state with
                    ManagedThreadObjects = state.ManagedThreadObjects |> Map.add newThreadId threadAddr
                }

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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "Join",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            // public bool Thread.Join(int millisecondsTimeout) — shipped as an InternalCall in
            // the deployed CoreLib (the managed body we see in source exists only in the
            // reference assembly). `this` is arg 0, the timeout is arg 1.
            let thisArg = instruction.Arguments.[0]

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"Thread.Join: expected int32 timeout, got %O{other}"

            let threadAddr =
                match thisArg with
                | CliType.ObjectRef (Some a) -> a
                | other -> failwith $"Thread.Join: expected non-null Thread `this`, got %O{other}"

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

            let targetThreadId =
                state.ManagedThreadObjects
                |> Map.toSeq
                |> Seq.tryPick (fun (tid, addr) -> if addr = threadAddr then Some tid else None)
                |> Option.defaultWith (fun () ->
                    // Distinguish "guest called Join on a Thread it never Start()ed"
                    // (real CLR would raise ThreadStateException) from "the interpreter's
                    // ManagedThreadObjects bookkeeping is out of sync with a live thread".
                    // Presence of a heap object at `threadAddr` means the guest legitimately
                    // allocated a Thread; absence means we've been handed a wild pointer
                    // and the bug is inside PawPrint.
                    match state.ManagedHeap.NonArrayObjects |> Map.tryFind threadAddr with
                    | Some _ ->
                        failwith
                            $"Thread.Join: Thread object at {threadAddr} was never Start()ed. The real CLR raises ThreadStateException here; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
                    | None ->
                        failwith
                            $"Thread.Join: no heap object at {threadAddr} (interpreter bug: stale or invalid Thread reference handed to Join)."
                )

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
            | 0 ->
                let state =
                    IlMachineState.pushToEvalStack (CliType.ofBool targetTerminated) ctx.Thread state

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | _ ->
                // Push `true` onto the caller's eval stack before (possibly) blocking.
                // This push persists across the block: the IP has already advanced past
                // the Join call by the time we return Stepped, so when the scheduler
                // eventually flips us back to Runnable the `true` is already sitting as
                // Join's return value and control flows straight past the call site.
                let state = IlMachineState.pushToEvalStack (CliType.ofBool true) ctx.Thread state

                let state =
                    if targetTerminated then
                        state
                    else
                        Scheduler.blockOnJoin ctx.Thread targetThreadId state

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
