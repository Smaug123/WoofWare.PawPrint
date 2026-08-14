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
            match ManagedHeap.tryGet threadAddr state.ManagedHeap with
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
    ///
    /// `timeout` follows Thread.Join semantics: `-1` (Timeout.Infinite)
    /// blocks until the target terminates; `0` is a non-blocking poll;
    /// any other positive value is a finite wait expressed in
    /// milliseconds. Negative values other than `-1` are rejected by the
    /// BCL with `ArgumentOutOfRangeException` before reaching us; we
    /// `failwith` because we cannot yet synthesise that exception, so
    /// any reach here means the wrapper was bypassed.
    ///
    /// Finite-timeout handling mirrors the optimistic-push-then-rewrite
    /// pattern used by `Monitor.Wait` / `LowLevelMonitor.TimedWait` /
    /// `WaitHandle.WaitOne`: the caller pushes `true`/`Int32 1`
    /// immediately after `executeJoinCore` returns, before the next
    /// scheduler step. If the target terminates first,
    /// `Scheduler.onThreadTerminated` flips the joiner back to Runnable
    /// and the pushed `true` is consumed as Join's return value. If the
    /// deadline fires first, `Scheduler.fireJoinTimeout` rewrites the
    /// slot to `Int32 0` so Join returns `false`. Both code paths
    /// (QCall: pushes `CliType.Numeric (Int32 1/0)`; InternalCall:
    /// pushes `CliType.ofBool true/false`) project to
    /// `EvalStackValue.Int32 (Int32Source.Verbatim 1)/0`, so a single Int32-rewrite suffices
    /// in `fireJoinTimeout`.
    let private executeJoinCore
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (threadAddr : ManagedHeapAddress)
        (timeout : int)
        : IlMachineState * bool
        =
        let deadlineTicks : int64 option option =
            // `None` = caller passed `0` (non-blocking poll, no block at all).
            // `Some None` = caller passed `-1` (infinite wait, block with no deadline).
            // `Some (Some ms)` = caller passed `> 0` (finite timeout, block with deadline).
            // Wrapping in an outer Option keeps the "did we even enter the blocking
            // path?" question structurally distinct from the deadline value itself.
            if timeout = 0 then
                None
            elif timeout = System.Threading.Timeout.Infinite then
                Some None
            elif timeout < 0 then
                // `< -1` is rejected by the BCL wrappers (`Thread.Join(int)`)
                // before reaching us, so reaching here means the wrapper was
                // bypassed. A silent treat-as-infinite or treat-as-zero would
                // turn a guest bug into a different bug elsewhere.
                failwith
                    $"Thread.Join: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's Thread.Join(int) validates this argument before the call, so reaching here means the wrapper was bypassed."
            else
                // `timeout > 0` is the finite-wait case. `int64` keeps the addition
                // safe against an `Int32.MaxValue` timeout against a long-running
                // virtual clock.
                Some (
                    Some (
                        state.Kernel.VirtualClockTicks
                        + int64 timeout * EmulatedKernel.ticksPerMillisecond
                    )
                )

        let targetThreadId = threadIdFromThreadAddr state "Thread.Join" threadAddr

        // Infinite self-join is an unresolvable deadlock: blocking ourselves on
        // ourselves means no thread will ever wake us, and unlike the finite case
        // there is no deadline that can let us out. The real CLR also hangs; in
        // PawPrint this would surface much later as a generic "no runnable
        // threads" failure far from the actual Join call, so report it at the
        // cause site.
        //
        // Finite (`> 0`) self-joins are *not* a deadlock under PawPrint's
        // deadline machinery — they park on `BlockedOnJoin (self, Some d)` and
        // get woken by `fireJoinTimeout` when the virtual clock reaches `d`,
        // returning `false`. This matches CoreCLR: `Thread.CurrentThread.Join(50)`
        // waits 50 ms and returns false. The non-blocking poll (`timeout = 0`)
        // is also fine for self: `targetTerminated` is false for a running self,
        // so we return false immediately without any status transition.
        match deadlineTicks with
        | Some None when targetThreadId = ctx.Thread ->
            failwith
                $"Thread.Join: thread {ctx.Thread} is attempting to join itself with an infinite timeout, which would deadlock. The real CLR also hangs on infinite self-join; PawPrint reports this at the call site rather than as a downstream deadlock. Use Thread.Join(int) with a finite timeout (or 0) if you want the call to return."
        | _ -> ()

        let targetState =
            state.ThreadState
            |> Map.tryFind targetThreadId
            |> Option.defaultWith (fun () ->
                failwith $"Thread.Join: target ThreadId {targetThreadId} has no ThreadState"
            )

        // A constructed-but-never-Start()ed Thread used to be unreachable here:
        // `ManagedThreadObjects` had no entry, so `threadIdFromThreadAddr` would
        // fail. Pre-allocating the NotStarted slot at Initialize time means the
        // lookup now succeeds, so we have to reject the case explicitly. The
        // real CLR raises ThreadStateException; PawPrint can't synthesise that
        // yet, so fail loud at the call site rather than silently returning
        // false (timeout=0) or blocking forever on a thread that will never run.
        match targetState.Status with
        | ThreadStatus.NotStarted ->
            failwith
                $"Thread.Join: target ThreadId {targetThreadId} has never been Start()ed. The real CLR raises ThreadStateException here; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
        | _ -> ()

        let targetTerminated = targetState.Status = ThreadStatus.Terminated

        match deadlineTicks with
        | None ->
            // timeout = 0: non-blocking poll. Result is whether the target is
            // already terminated; no status transition.
            state, targetTerminated
        | Some maybeDeadline ->
            // The bool result is the optimistic `true`: the only way control
            // flows past the Join via target termination yields `true`, and a
            // finite-deadline expiry will be rewritten to `false` by
            // `Scheduler.fireJoinTimeout` (which pops the optimistic slot and
            // pushes `Int32 0`). The caller pushes this value synchronously
            // after we return — before the next scheduler step — so the
            // optimistic slot is in place by the time any deadline could fire.
            let state =
                if targetTerminated then
                    state
                else
                    Scheduler.blockOnJoin ctx.Thread targetThreadId maybeDeadline state

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

        let state =
            state
            |> IlMachineState.setOwnInstanceField
                threadAddr
                "_managedThreadId"
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> IlMachineState.setOwnInstanceField
                threadAddr
                "_priority"
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> IlMachineState.setOwnInstanceField
                threadAddr
                "_DONT_USE_InternalThread"
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                NextManagedThreadId = state.NextManagedThreadId + 1
            }

        IlMachineState.allocateUnstartedThread threadAddr state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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

            NativeHandlerResult.completed state |> Some
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
            NativeHandlerResult.completed state |> Some
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

            NativeHandlerResult.completed state |> Some
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
            // sees its own value.
            //
            // The real CLR raises `ThreadStateException` when the target is dead — the BCL
            // does this in managed code via the `_isDead` check on `Thread.IsBackground`
            // before reaching the QCall. PawPrint doesn't currently write `_isDead = true`
            // when a thread terminates (a separate piece of work — also needed by Priority
            // and the other `_isDead`-guarded properties), so the BCL check passes and we
            // reach this handler on a Terminated thread. Reject that here rather than
            // silently storing an unobservable flag, mirroring the executeJoinCore guard.
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

            let targetState =
                state.ThreadState
                |> Map.tryFind targetThreadId
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: target ThreadId {targetThreadId} has no ThreadState"
                )

            match targetState.Status with
            | ThreadStatus.Terminated ->
                failwith
                    $"%s{operation}: target ThreadId {targetThreadId} has terminated. The real CLR raises ThreadStateException via the BCL's `_isDead` check; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
            | _ -> ()

            let updatedThreadState =
                { targetState with
                    IsBackground = value
                }

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add targetThreadId updatedThreadState
                }

            NativeHandlerResult.completed state |> Some
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
            // reinterprets the return on the stack via `(int)result != 0`. Symmetric with
            // the setter: the BCL's managed `_isDead` check should reject reads against
            // terminated threads before the QCall fires, but PawPrint doesn't yet flip
            // `_isDead`, so we guard the case here rather than handing back a stale flag.
            let operation = "ThreadNative_GetIsBackground"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let threadAddr =
                threadAddrFromThreadHandle state operation instruction.Arguments.[0]

            let targetThreadId = threadIdFromThreadAddr state operation threadAddr

            let targetState =
                state.ThreadState
                |> Map.tryFind targetThreadId
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: target ThreadId {targetThreadId} has no ThreadState"
                )

            match targetState.Status with
            | ThreadStatus.Terminated ->
                failwith
                    $"%s{operation}: target ThreadId {targetThreadId} has terminated. The real CLR raises ThreadStateException via the BCL's `_isDead` check; PawPrint doesn't synthesise that yet, so this is a guest bug we can't currently report structurally."
            | _ -> ()

            let resultInt = if targetState.IsBackground then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 resultInt)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "ThreadNative_InformThreadNameChange",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          // The C# source uses LibraryImport with StringMarshalling.Utf16 on a non-blittable
          // `string?` parameter, so Roslyn emits a marshalling stub whose synthesised name
          // (`<InformThreadNameChange>g____PInvoke|N_M`) carries source-generator counters
          // not stable across runtime versions. Discard the IL method name and validate the
          // signature shape on the parameter-types pattern below; the QCall entry-point
          // tuple element is already an exact match. Same approach as Environment_FailFast.
          _,
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Threading",
                                              "ThreadHandle",
                                              threadHandleGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void when threadHandleGenerics.IsEmpty ->
            // .NET 10 QCall fired by the `Thread.Name` setter after the BCL has already
            // written the canonical name into the managed `Thread._name` field. In CoreCLR
            // this hook tells the OS / debugger about the new thread name
            // (SetThreadDescription on Windows, pthread_setname_np on Linux). PawPrint has
            // no OS threads to name, so we simply record the value on `ThreadState.Name`
            // as a diagnostic mirror — guest reads of `Thread.Name` go through `_name` and
            // never consult our mirror, so the mirror cannot mislead guest code.
            //
            // The BCL passes (handle, char* name, int32 length). When the guest clears the
            // name (`Thread.Name = null`), the call arrives with a null pointer and
            // length=0; we translate that to `None`. Otherwise length is the count of
            // UTF-16 code units pointed to by `name` (no null terminator required), which
            // we read with `readLengthPrefixedUtf16` and store as `Some s`.
            let operation = "ThreadNative_InformThreadNameChange"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let threadAddr =
                threadAddrFromThreadHandle state operation instruction.Arguments.[0]

            let targetThreadId = threadIdFromThreadAddr state operation threadAddr

            let namePtr =
                NativeCall.managedPointerOfPointerArgument operation "name" instruction.Arguments.[1]

            let nameLength =
                match instruction.Arguments.[2] |> CliType.unwrapPrimitiveLikeDeep with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 length, got %O{other}"

            let newName : string option =
                // Disambiguate `Thread.Name = null` from `Thread.Name = ""` by inspecting
                // the pointer alongside the length: clearing the name passes a null
                // pointer with len=0, but assigning `""` passes a non-null pointer
                // (from `String.Empty.GetPinnableReference()`) also with len=0. The
                // canonical `_name` field stores the two states distinctly, so the
                // diagnostic mirror must do the same. Anything else delegates to
                // `readLengthPrefixedUtf16`, which yields `""` for non-null + len=0
                // and fails loudly on null + len>0.
                match namePtr with
                | ManagedPointerSource.Null when nameLength = 0 -> None
                | _ ->
                    NativeCall.readLengthPrefixedUtf16 operation ctx.BaseClassTypes state namePtr nameLength
                    |> Some

            let targetState =
                state.ThreadState
                |> Map.tryFind targetThreadId
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: target ThreadId {targetThreadId} has no ThreadState"
                )

            // Unlike SetIsBackground, `Thread.Name`'s managed setter has no `_isDead`
            // check and CoreCLR's native InformThreadNameChange just skips the OS-level
            // naming when the handle is invalid (it does not throw). So setting or
            // clearing `Thread.Name` on a terminated thread is valid guest behaviour —
            // we record the requested value unconditionally, matching the BCL's
            // unconditional update of the canonical `_name` field that runs before
            // this QCall fires.
            let updatedThreadState =
                { targetState with
                    Name = newName
                }

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add targetThreadId updatedThreadState
                }

            NativeHandlerResult.completed state |> Some
        | "ThreadNative_YieldThread",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          // LibraryImport's source generator synthesises the stub name as
          // `<YieldInternal>g____PInvoke|N_M` with SDK-version-unstable counters,
          // so we discard the IL method name and rely on the entry-point and
          // signature shape (zero parameters, Interop.BOOL return) for matching.
          // Same precedent as `ThreadNative_InformThreadNameChange` and
          // `Environment_FailFast`.
          _,
          [],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "", "BOOL", boolGenerics)) when
            boolGenerics.IsEmpty
            ->
            // .NET 10 QCall backing `Thread.Yield()`:
            //   public static bool Yield() => YieldInternal() != Interop.BOOL.FALSE;
            // CoreCLR's native implementation calls `__SwitchToThread(0, ...)` and
            // returns whether the OS actually switched away from the caller.
            //
            // Return-value contract. We push `Interop.BOOL.FALSE` (Int32 0)
            // unconditionally. A truthful TRUE would require evidence that the
            // scheduler is honouring the yield by running a different thread
            // before this caller observes the return — and PawPrint's current
            // `Scheduler.chooseNext` is *permitted* to schedule another thread
            // under round-robin but is not *required* to as a consequence of
            // VoluntaryYield (its signature is `(lastRan, state)` and it doesn't
            // see the previous outcome). Claiming TRUE based on a runnable-set
            // scan would therefore lie under that contract. Returning FALSE is
            // honest: PawPrint has not yet modelled a guaranteed successful
            // switch. The BCL's common callers (`SpinWait.SpinOnceCore`,
            // `LowLevelSpinWaiter.SpinWaitForCondition`) discard this boolean
            // and escalate via `Thread.Sleep` anyway, so the choice does not
            // affect forward progress on the canonical Task.Run spin path.
            //
            // Why `NativeHandlerResult.yielded` rather than `completed`. The
            // yield *intent* — that the guest asked the scheduler to consider
            // running someone else — is preserved as `WhatWeDid.VoluntaryYield`,
            // a distinct signal at the driver/scheduler boundary even though
            // today's policy treats it identically to `Executed`. A future
            // fuzz/pruning harness can branch on the variant (e.g. to weight
            // voluntary interleavings differently or to constrain the next-
            // thread choice) without revisiting the QCall surface.
            let operation = "ThreadNative_YieldThread"

            if instruction.Arguments.Length <> 0 then
                failwith $"%s{operation}: expected zero native arguments, got %d{instruction.Arguments.Length}"

            // Optimistic `Interop.BOOL.FALSE`. We cannot know here whether the switch will
            // happen: that is the scheduler's decision, taken in `Scheduler.onStepOutcome`
            // when it sees our `WhatWeDid.VoluntaryYield true` and either charges a yield debt
            // or declines. It rewrites this slot to TRUE iff a switch is now guaranteed, the
            // same optimistic-push-then-rewrite contract `Scheduler.fireJoinTimeout` uses for
            // `Thread.Join`'s return value.
            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) ctx.Thread state

            NativeHandlerResult.yielded true state |> Some
        | "ThreadNative_Sleep",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "SleepInternal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // .NET 10 CoreCLR routes `Thread.Sleep(int)` to `SleepInternal(int)`
            // (a `[LibraryImport]` partial in `Thread.CoreCLR.cs`), which the
            // runtime resolves as the `ThreadNative_Sleep` QCall
            // (`comsynchronizable.cpp`). The BCL pre-validates the timeout
            // (rejects values < -1 with `ArgumentOutOfRangeException`) and
            // translates `Timeout.Infinite` (-1) and finite millisecond
            // timeouts directly without any wide-string / handle marshalling.
            // `Thread.Sleep(TimeSpan)` reaches the same partial via
            // `WaitHandle.ToTimeoutMilliseconds`, so this single arm covers
            // both overloads.
            //
            // PawPrint's deterministic scheduler advances `VirtualClockTicks`
            // one tick at a time; the actual wait is implemented by parking
            // the thread in `BlockedOnSleep` with an absolute deadline
            // (or `None` for `Timeout.Infinite`) and letting
            // `Program.fireExpiredDeadlines` route through
            // `Scheduler.fireSleepTimeout` once the virtual clock crosses
            // the deadline. `Thread.Sleep(0)` is a no-op (BCL uses it as a
            // yield hint; we have no preemption to invoke).
            let operation = "ThreadNative_Sleep"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let millisecondsTimeout =
                match instruction.Arguments.[0] |> CliType.unwrapPrimitiveLikeDeep with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 millisecondsTimeout, got %O{other}"

            if millisecondsTimeout = 0 then
                // `Thread.Sleep(0)` does not park: CoreCLR's `SleepEx(0, ...)` relinquishes
                // the remainder of the caller's time slice to a ready thread of equal
                // priority and returns immediately, without waiting for the clock. So it
                // costs no virtual time — but it *is* a yield, and the BCL treats it as one:
                // `SpinWait.SpinOnceCore` uses it as one of its backoff rungs, alternating it
                // with `Thread.Yield()` for the first 20 iterations.
                //
                // Reporting it as a yield rather than as `completed` is what lets
                // `Scheduler.onStepOutcome` charge the caller a yield debt. `reportsSwitch`
                // is `false`: unlike `Thread.Yield()`, `Thread.Sleep(int)` returns `void`,
                // so there is no eval-stack slot for the scheduler to rewrite with the
                // outcome, and the guest cannot observe whether the switch happened.
                NativeHandlerResult.yielded false state |> Some
            else

            let state =
                if millisecondsTimeout = System.Threading.Timeout.Infinite then
                    Scheduler.blockOnSleep ctx.Thread None state
                elif millisecondsTimeout < 0 then
                    failwith
                        $"%s{operation}: negative timeout %d{millisecondsTimeout} ms is not Infinite (-1); the BCL Thread.Sleep call site is required to pre-validate this. Reaching here means the validation was bypassed (e.g. by a synthesised IL call) — bug in the caller."
                else
                    let deadline =
                        state.Kernel.VirtualClockTicks
                        + int64 millisecondsTimeout * EmulatedKernel.ticksPerMillisecond

                    Scheduler.blockOnSleep ctx.Thread (Some deadline) state

            NativeHandlerResult.completed state |> Some
        | "ThreadNative_SpinWait",
          "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          // Two distinct managed methods share this one QCall entry point:
          // `SpinWaitInternal` (the `[SuppressGCTransition]` fast path used
          // for `iterations < SpinWaitCoopThreshold`) and
          // `LongSpinWaitInternal` (the ordinary GC-transitioning P/Invoke
          // used above that threshold, reached via the `LongSpinWait`
          // no-inline wrapper). Both declare
          // `[LibraryImport(RuntimeHelpers.QCall, EntryPoint =
          // "ThreadNative_SpinWait")]` in `Thread.CoreCLR.cs`, so which
          // managed name we'd see here depends on which of the two the
          // caller's iteration count selected. Match on the entry point and
          // signature shape instead of the name, same approach as
          // `ThreadNative_YieldThread` and `ThreadNative_InformThreadNameChange`.
          _,
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // .NET 10 QCall backing `Thread.SpinWait(int)`. CoreCLR's native
            // side (`comsynchronizable.cpp`) is:
            //   if (iterations <= 0) return;
            //   YieldProcessorNormalized(iterations);
            // `YieldProcessorNormalized` issues `iterations *
            // yieldsPerNormalizedYield` PAUSE/YIELD instructions on the
            // calling CPU and returns — it never calls `SwitchToThread`,
            // `Sleep`, or anything else that could change which OS thread is
            // running. It writes to no managed or native memory the guest
            // can observe, throws nothing, and its only effect is consuming
            // wall-clock time proportional to `iterations`.
            //
            // Elapsed time *is* observable to a PawPrint guest — that is what
            // `EmulatedKernel.VirtualClockTicks` is, and `Environment.TickCount64`
            // and `DateTime.UtcNow` both read it. But the driver loop advances
            // it as a function of scheduler ticks (`InstructionCostTicks` of
            // virtual time per instruction retired), never as a function of what a given
            // instruction physically costs. So this handler doing no work does
            // not freeze the guest's clock: the `call` still retires, the clock
            // still moves, and a guest delay loop such as
            // `while (TickCount64 - start < N) Thread.SpinWait(k);` terminates,
            // after about `N * ticksPerMillisecond / instructionCostTicks`
            // iterations.
            //
            // What is deliberately not modelled is the *proportionality* to
            // `iterations`: `SpinWait(1)` and `SpinWait(10_000_000)` each cost
            // the handful of ticks their call sequence retires, where real
            // hardware puts them ~7 orders of magnitude apart. Restoring that
            // proportionality was considered and rejected twice over. Scaled
            // consistently with the rest of the clock (a real spin iteration is
            // some tens of instruction-times, and an instruction bills 1 ms
            // here) `SpinWait(10_000_000)` would have to jump the clock by
            // days of virtual time, firing every outstanding timeout in the
            // process — strictly worse for guest fidelity than under-charging.
            // And it would make a native handler a second writer of
            // `VirtualClockTicks`, which `EmulatedKernel` documents as
            // scheduler-only; the property that two threads reading on the same
            // tick observe the same value is stated there and relied on by the
            // `SystemNative_GetLowResolutionTimestamp` and
            // `SystemNative_GetSystemTimeAsTicks` handlers.
            //
            // The residual divergence is therefore that spinning is *dearer*
            // here than on real hardware rather than free, so a guest running a
            // spin-then-block primitive against a timeout exhausts its spin
            // budget in very few iterations and falls back to the blocking path
            // sooner than it would natively (measured: a 100 ms budget admits
            // 2 `SpinWait.SpinOnce()` calls). That is the safe direction — the
            // blocking path is the one with the real synchronisation in it —
            // but it does mean PawPrint under-exercises guest spin paths.
            //
            // Note also that every CoreLib caller of `Thread.SpinWait`
            // (`SpinWait.SpinOnceCore`, `LowLevelSpinWaiter.Wait`,
            // `ReaderWriterLockSlim`, `PortableThreadPool.WorkerThread`) is
            // guarded by `!Environment.IsSingleProcessor`, and
            // `EmulatedKernel.defaultProcessorCount` is 1. So at the default
            // kernel config no BCL path reaches here at all; this arm is
            // exercised by direct `Thread.SpinWait` calls from guest code, and
            // by those BCL paths only once a host raises
            // `KernelConfig.ProcessorCount` above 1.
            //
            // `NativeHandlerResult.completed`, not `.yielded`. Contrast
            // directly with the `ThreadNative_YieldThread` arm just above:
            // that one *is* the guest asking the scheduler to consider
            // running a different thread, so it reports `VoluntaryYield` to
            // preserve that intent for the scheduler/fuzzer boundary.
            // `Thread.SpinWait` is the opposite move by design — its doc
            // comment recommends it specifically as the alternative to a
            // real yield, for callers who expect the awaited condition to
            // change very soon and would rather keep this thread hot than
            // pay a context-switch. Reporting `VoluntaryYield` here would
            // assert a yield intent the guest explicitly did not express,
            // corrupting that signal for any future interleaving-aware
            // consumer of `WhatWeDid`.
            //
            // We still validate the argument shape (and read the value) so a
            // marshalling regression here fails loudly rather than silently
            // matching a different overload; the value itself is otherwise
            // unused, matching the real QCall's behaviour of accepting (and
            // silently no-op-ing on) non-positive counts.
            let operation = "ThreadNative_SpinWait"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            match instruction.Arguments.[0] |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.Numeric (CliNumericType.Int32 _) -> ()
            | other -> failwith $"%s{operation}: expected int32 iterations, got %O{other}"

            NativeHandlerResult.completed state |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
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

            NativeHandlerResult.completed state |> Some
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
            NativeHandlerResult.completed state |> Some
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
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) ->
                    FunctionPointerTarget.requireManaged "Thread.StartInternal" target
                | other ->
                    failwith $"Thread.StartInternal: expected FunctionPointer in delegate _methodPtr, got %O{other}"

            let containingAssembly =
                state.LoadedAssembly targetMethod.DeclaringAssembly
                |> Option.defaultWith (fun () ->
                    failwith $"Thread.StartInternal: assembly {targetMethod.DeclaringAssembly.Name} not loaded"
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
                            containingAssembly.TypeDefs.[targetMethod.RequiredDeclaringType.Definition.Get]

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
                    targetMethod.RequiredDeclaringType.Identity
                    targetMethod.DeclaringTypeGenerics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Thread.StartInternal: declaring type %s{MethodOwner.describe targetMethod.Owner} of delegate target is not registered in ConcreteTypes"
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
                    $"Thread.StartInternal: target type %s{MethodOwner.describe targetMethod.Owner} is being initialised on another thread. Cross-thread class-init synchronisation for workers is not yet implemented."
            | _ -> ()

            let state = Scheduler.onWorkerSpawned newThreadId workerInitOutcome state

            NativeHandlerResult.completed state |> Some
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

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Thread",
          "get_OptimalMaxSpinWaitsPerSpinIteration",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // InternalCall backing the `internal static int
            // Thread.OptimalMaxSpinWaitsPerSpinIteration { get; }` property
            // (`[MethodImpl(MethodImplOptions.InternalCall)]` in
            // `Thread.CoreCLR.cs`, `ThreadNative::GetOptimalMaxSpinWaitsPerSpinIteration`
            // in `comsynchronizable.cpp`). Real CoreCLR answers this from a
            // background measurement of the host CPU's `YieldProcessor()`
            // latency (`YieldProcessorNormalization`) — see the extensive
            // rationale on `EmulatedKernel.OptimalMaxSpinWaitsPerSpinIteration`.
            let value = state.Kernel.OptimalMaxSpinWaitsPerSpinIteration

            if value < 1 || value > EmulatedKernel.maxOptimalMaxSpinWaitsPerSpinIteration then
                // A kernel built by record-copy can bypass
                // `EmulatedKernel.withOptimalMaxSpinWaitsPerSpinIteration`;
                // re-assert here so the guest never observes a value the real
                // property could not have produced (mirrors the
                // `Environment.GetProcessorCount` guard in
                // `NativeEnvironment.fs`).
                failwith
                    $"Thread.get_OptimalMaxSpinWaitsPerSpinIteration: kernel OptimalMaxSpinWaitsPerSpinIteration is %d{value}, which is outside the legal range [1, %d{EmulatedKernel.maxOptimalMaxSpinWaitsPerSpinIteration}]"

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 value)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None
