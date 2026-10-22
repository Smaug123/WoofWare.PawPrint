namespace WoofWare.PawPrint

open System.Buffers.Binary
open WoofWare.PosixKernel

/// Native handler for the three `SystemNative_LowLevelFutex_*` entry points `libSystem.Native`
/// exports on Linux, through which .NET 11's `LowLevelThreadBlocker` blocks and wakes a thread
/// (and so `LowLevelLock` and `LowLevelLifoSemaphore`). Each is one `futex(2)` call
/// (`pal_threading.c`), made here through the kernel's `UnixFutex.futex`.
///
/// The kernel keys a futex by the word's address, which PawPrint gives only native-heap
/// memory (`NativeMemoryPool.tryAddressOf`); `LowLevelThreadBlocker` keeps its word in a block
/// from `NativeMemory.AlignedAlloc`. A word anywhere else is refused.
[<RequireQualifiedAccess>]
module NativeLowLevelFutex =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.TryNativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    /// The futex word `int* address` names: the address the kernel keys it by, and how the
    /// kernel finds the memory there.
    let private futexWord
        (operation : string)
        (address : CliType)
        (state : IlMachineState)
        : uint64 * UserBuffer * ManagedPointerSource option
        =
        let pointer = NativeSystemNative.bufferPointerArgument operation "address" address

        match pointer with
        | BufferPointer.RawAddress raw -> raw, UserBuffer.Unmapped raw, None
        | BufferPointer.Storage _ ->
            let source =
                match BufferPointer.dereferenceable pointer with
                | Some source -> source
                | None ->
                    failwith
                        $"%s{operation}: %O{pointer} names storage but has no pointer to it (this is an interpreter bug)"

            match ManagedPointerSource.tryNativeMemoryByte source with
            | None ->
                failwith
                    $"%s{operation}: the futex word %O{source} is not in native-heap memory, which is the only memory PawPrint gives an address, and the kernel keys a futex by its word's address. CoreLib keeps every futex word in a block from NativeMemory.AlignedAlloc."
            | Some (block, offset) ->

            match NativeMemoryPool.tryAddressOf block offset (IlMachineState.getNativeMemoryPool state) with
            | Some address -> address, UserBuffer.Mapped, Some source
            | None ->
                failwith
                    $"%s{operation}: the futex word is %d{offset} bytes into %O{block}, which is freed or does not extend that far, so it has no address"
        | BufferPointer.Symbolic _
        | BufferPointer.Unstatable _ -> failwith $"%s{operation}: %s{BufferPointer.unmodelledHint pointer}"

    /// `UnixFutex.futex`, refusing loudly under a kernel of the Darwin flavour, which has no
    /// `futex(2)`: only a Linux CoreLib calls these entry points.
    let private futex
        (operation : string)
        (caller : Caller<ThreadId>)
        (address : uint64)
        (op : int)
        (value : int)
        (word : UserBuffer)
        (timeout : TimespecArgument)
        (system : UnixSystem<ThreadId, NativeSignalHandler>)
        =
        try
            UnixFutex.futex caller address op value word timeout 0u system
        with :? System.PlatformNotSupportedException as e ->
            KernelRefusal.failAdvising
                operation
                "Only a Linux CoreLib makes this call, so run it under a Linux KernelConfig.UnixPlatform."
                e

    /// What the shim's single `futex(2)` call did, once the kernel has answered: what is left
    /// to do is the shim's own.
    [<RequireQualifiedAccess>]
    type private WaitEnd =
        /// The call returned 0: a wake took the waiter.
        | Woken
        /// The call failed with `error`, which errno now holds.
        | Failed of error : UnixError

    let private withLocals (ctx : NativeCallContext) (locals : NativeLocals option) (state : IlMachineState) =
        state
        |> IlMachineState.mapFrame
            ctx.Thread
            state.ThreadState.[ctx.Thread].ActiveMethodState
            (MethodState.withNativeLocals locals)

    /// `syscall(SYS_futex, address, FUTEX_WAIT_PRIVATE, comparand, timeout, NULL, 0)`, made by
    /// a native frame that answers `answer` once the call returns.
    ///
    /// A first entry makes the call, and either answers at once or parks re-entrantly, as
    /// `SystemNative_Poll` does: the frame stays, so a wake re-enters this handler, which
    /// finishes the call from the task's park record. The shim makes the call once, with no
    /// loop on `EINTR`, so a call a signal interrupted is ended by the leader's return to user
    /// mode (`NativeLocals.ReturnedToUser`), as `sigsuspend` is: it fails with `EINTR`, or it is
    /// made again from its arguments, which reads the word again as Linux's restart does.
    let private wait
        (ctx : NativeCallContext)
        (operation : string)
        (timeout : TimespecArgument)
        (answer : WaitEnd -> IlMachineState -> NativeHandlerResult)
        : NativeHandlerResult
        =
        let state = ctx.State

        let restarted =
            match ctx.Instruction.NativeLocals with
            | None -> Ok false
            | Some (NativeLocals.ReturnedToUser InterruptedCall.Restarts) -> Ok true
            | Some (NativeLocals.ReturnedToUser InterruptedCall.FailsWithEintr) -> Error ()
            | Some NativeLocals.AwaitingReturnToUser ->
                failwith
                    $"%s{operation}: thread %O{ctx.Thread} re-entered its interrupted futex wait before it returned to user mode, which ends the call (this is an interpreter bug)."
            | Some (NativeLocals.AcceptAddressLength _)
            | Some (NativeLocals.PollEntries _)
            | Some NativeLocals.SocketEventWaitLoop ->
                failwith
                    $"%s{operation}: thread %O{ctx.Thread}'s futex frame keeps another call's locals (this is an interpreter bug)."

        match restarted with
        | Error () ->
            withLocals ctx None state
            |> NativeSystemNative.withErrnoOnly ctx UnixError.EINTR
            |> answer (WaitEnd.Failed UnixError.EINTR)
        | Ok restarted ->

        let state = if restarted then withLocals ctx None state else state
        let caller = state.Kernel.Caller ctx.Thread

        let outcome, system =
            match UnixThreadState.parkedIn (EmulatedKernel.taskOf ctx.Thread state.Kernel.Tasks) with
            | Some (ParkedSyscall.Futex _) -> UnixFutex.finishFutexWait caller state.Kernel.System
            | Some other ->
                // Unreachable: a task parked in another syscall is not running IL. Refused
                // rather than treated as a first entry, which would park over the stale record
                // and destroy the evidence.
                failwith
                    $"%s{operation}: thread %O{ctx.Thread} entered the call while its task is parked in %A{other}. A task blocks in one syscall at a time, so that call's completion failed to clear its record (this is an interpreter bug)."
            | None ->
                let address, word, source = futexWord operation ctx.Instruction.Arguments.[0] state
                let comparand = NativeCall.int32Argument operation ctx.Instruction.Arguments.[1]

                match
                    futex operation caller address UnixFutex.FutexWaitPrivate comparand word timeout state.Kernel.System
                with
                | CallProgress.Answered answered -> answered
                | CallProgress.NeedsCopyIn paused ->
                    let source =
                        match source with
                        | Some source -> source
                        | None ->
                            failwith
                                $"%s{operation}: the kernel asked to read the futex word at 0x%x{address}, which names no storage, though it answers EFAULT for such a word before reading it (this is a bug in the kernel library)."

                    // The read and the kernel's decision to sleep are one step, as the kernel
                    // requires: no other thread runs inside a native handler.
                    let bytes = NativeSystemNative.readBytesThrough ctx operation source 4 state
                    UnixFutex.futexWithWord (BinaryPrimitives.ReadUInt32LittleEndian (bytes.AsSpan ())) paused

        match outcome with
        | SyscallOutcome.WouldBlock _ ->
            NativeSystemNative.withAnswered system state
            |> Scheduler.parkInSyscall ctx.Thread
            |> NativeHandlerResult.blockedRetainingFrame
        | SyscallOutcome.Answered (SyscallAnswer.Completed _) ->
            NativeSystemNative.withAnswered system state |> answer WaitEnd.Woken
        | SyscallOutcome.Answered (SyscallAnswer.Failed error) ->
            NativeSystemNative.withErrno ctx error system state
            |> answer (WaitEnd.Failed error)
        | SyscallOutcome.Interrupted ->
            // Only the leader takes a signal (`SignalDispatch.poll`), so only the leader
            // returns to user mode, which ends the call.
            if ctx.Thread <> state.Kernel.Leader then
                failwith
                    $"%s{operation}: a signal interrupted thread %O{ctx.Thread}'s futex wait, but PawPrint returns only the leader %O{state.Kernel.Leader} to user mode. Nothing PawPrint answers directs a signal at any other thread (this is an interpreter bug)."

            NativeSystemNative.withAnswered system state
            |> withLocals ctx (Some NativeLocals.AwaitingReturnToUser)
            |> NativeHandlerResult.reenterRetainingFrame

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            trySystemNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some "SystemNative_LowLevelFutex_WaitOnAddress",
          [ ConcretePointer _ ; ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // `void SystemNative_LowLevelFutex_WaitOnAddress(int32_t* address, int32_t comparand)`:
            // `syscall(SYS_futex, address, FUTEX_WAIT_PRIVATE, comparand, NULL, NULL, 0)`, its
            // answer ignored.
            wait
                ctx
                "SystemNative_LowLevelFutex_WaitOnAddress"
                TimespecArgument.Null
                (fun _ state -> NativeHandlerResult.completed state)
            |> Some

        | Some "SystemNative_LowLevelFutex_WaitOnAddressTimeout",
          [ ConcretePointer _
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_LowLevelFutex_WaitOnAddressTimeout(int32_t* address,
            // int32_t comparand, int32_t timeoutMilliseconds)`: the same call with a relative
            // timeout the shim builds on its own stack, from the milliseconds cast to
            // `uint32_t` (asserted non-negative only in a debug build). It answers 0 for a
            // timeout and 1 for anything else, an early or spurious wake included.
            let operation = "SystemNative_LowLevelFutex_WaitOnAddressTimeout"

            let milliseconds =
                uint32 (NativeCall.int32Argument operation instruction.Arguments.[2])

            let timeout =
                TimespecArgument.Readable
                    {
                        Seconds = int64 (milliseconds / 1000u)
                        Nanoseconds = int64 (milliseconds % 1000u) * 1_000_000L
                    }

            let answer (ended : WaitEnd) (state : IlMachineState) : NativeHandlerResult =
                let timedOut =
                    match ended with
                    | WaitEnd.Failed UnixError.ETIMEDOUT -> true
                    | WaitEnd.Failed _
                    | WaitEnd.Woken -> false

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (if timedOut then 0 else 1)))
                    ctx.Thread
                |> NativeHandlerResult.completed

            wait ctx operation timeout answer |> Some

        | Some "SystemNative_LowLevelFutex_WakeByAddressSingle", [ ConcretePointer _ ], MethodReturnType.Void ->
            // `void SystemNative_LowLevelFutex_WakeByAddressSingle(int32_t* address)`:
            // `syscall(SYS_futex, address, FUTEX_WAKE_PRIVATE, 1, NULL, NULL, 0)`, its answer
            // ignored. A wake reads no memory.
            let operation = "SystemNative_LowLevelFutex_WakeByAddressSingle"
            let address, word, _ = futexWord operation instruction.Arguments.[0] state

            match
                futex
                    operation
                    (state.Kernel.Caller ctx.Thread)
                    address
                    UnixFutex.FutexWakePrivate
                    1
                    word
                    TimespecArgument.Null
                    state.Kernel.System
            with
            | CallProgress.Answered (SyscallOutcome.Answered (SyscallAnswer.Completed _), system) ->
                NativeSystemNative.withAnswered system state
                |> NativeHandlerResult.completed
                |> Some
            | CallProgress.Answered (SyscallOutcome.Answered (SyscallAnswer.Failed error), system) ->
                NativeSystemNative.withErrno ctx error system state
                |> NativeHandlerResult.completed
                |> Some
            | other ->
                failwith
                    $"%s{operation}: a futex wake answered %A{other}, but a wake neither reads memory nor sleeps (this is a bug in the kernel library)."

        | _ -> None
