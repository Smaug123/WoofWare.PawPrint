namespace WoofWare.PawPrint

/// Native handler for the seven `SystemNative_LowLevelMonitor_*` entry points
/// exposed by `libSystem.Native`. The managed counterparts live in
/// `System.Threading.LowLevelMonitor.Unix` and back `LowLevelLock`,
/// `LowLevelLifoSemaphore`, `PortableThreadPool`, etc. Real CoreCLR sits on
/// `pthread_mutex_t` + `pthread_cond_t`; we route through the deterministic
/// state machine in `LowLevelMonitor.fs`.
[<RequireQualifiedAccess>]
module NativeLowLevelMonitor =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.NativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    /// Decode the `IntPtr monitor` argument shared by every entry point except
    /// Create. The guest only ever obtains this value from Create, so a foreign
    /// IntPtr (e.g. an EventPipe handle, a GC handle, a `Verbatim` scratch
    /// value) is unambiguously a guest bug. Null is rejected too: the managed
    /// wrappers wrap `Initialize` such that a `IntPtr.Zero _nativeMonitor`
    /// never reaches these P/Invokes in the normal path.
    let private monitorOfArgument (operation : string) (arg : CliType) : LowLevelMonitorId =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.LowLevelMonitorPtr id)) -> id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
            failwith
                $"%s{operation}: monitor argument was IntPtr.Zero, but LowLevelMonitor invariants require a non-null handle (the wrapper would have thrown OOM at Initialize)."
        | other -> failwith $"%s{operation}: expected LowLevelMonitor handle, got %O{other}"

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            trySystemNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some "SystemNative_LowLevelMonitor_Create",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            // Mint a fresh tagged monitor handle. Counter starts at 1 so the
            // guest's `if (_nativeMonitor == IntPtr.Zero) throw new OOM()` check
            // never fires for a successful Create.
            let id, state = LowLevelMonitor.create state

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.LowLevelMonitorPtr id))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | Some "SystemNative_LowLevelMonitor_Destroy",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Destroy"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let state = LowLevelMonitor.destroy id state
            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_Acquire",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Acquire"
            let id = monitorOfArgument operation instruction.Arguments.[0]

            // Both the fast path (uncontended) and the contended path return
            // void to the guest; the difference is just whether the caller's
            // status is now `BlockedOnMonitorAcquire`. Mirror Thread.Join's
            // posture: we always return Stepped/Executed — the scheduler will
            // simply not pick this thread again until it's woken via Release
            // or Signal_Release.
            let state =
                match LowLevelMonitor.acquire ctx.Thread id state with
                | LowLevelMonitor.AcquireOutcome.Acquired state
                | LowLevelMonitor.AcquireOutcome.Blocked state -> state

            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_Release",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Release"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let state = LowLevelMonitor.release ctx.Thread id state
            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_Wait",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Wait"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let state = LowLevelMonitor.wait ctx.Thread id None state
            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_TimedWait",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // The managed BCL marshals `bool` ↔ `Int32`: non-zero means
            // signalled, zero means timed out. The wait *always* parks
            // (no fast path — TimedWait is a condvar primitive, not a
            // lock try), so we always push the optimistic `1` (signalled)
            // at park time; if the deadline fires first,
            // `LowLevelMonitor.fireTimeout` rewrites it to `0` on the
            // way out. The IL site advances in both shapes; the
            // resumption past the call site happens only after the
            // monitor has been reacquired, mirroring
            // `pthread_cond_timedwait`'s contract.
            let operation = "SystemNative_LowLevelMonitor_TimedWait"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let timeout = NativeCall.int32Argument operation instruction.Arguments.[1]

            let deadlineMs =
                if timeout = System.Threading.Timeout.Infinite then
                    // The managed `LowLevelMonitor.Wait(int)` wrapper
                    // routes `-1 → Wait()` rather than calling TimedWait,
                    // so this branch is defensive: an infinite-timeout
                    // TimedWait still works (signal-only wake, no
                    // deadline firing), even if no current caller takes
                    // this path.
                    None
                elif timeout < 0 then
                    // `< -1` is rejected by the BCL wrapper before the
                    // QCall; reaching here means the wrapper was bypassed
                    // and the caller meant something we cannot infer. A
                    // silent treat-as-infinite or treat-as-zero would
                    // turn a guest bug into a different bug elsewhere.
                    failwith
                        $"%s{operation}: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's LowLevelMonitor.Wait(int) validates this argument before the QCall, so reaching here means the wrapper was bypassed."
                else
                    // `timeout = 0` is legal (the BCL `LowLevelLifoSemaphore`
                    // uses it as a "park then immediately timeout" probe).
                    // Recording the deadline as the current clock value
                    // means the next driver tick's `fireExpiredDeadlines`
                    // pass will pull the thread out — observably an
                    // immediate timeout against signal-poll-then-park.
                    // `int64` keeps the addition safe for `Int32.MaxValue`
                    // timeouts against a long-running clock.
                    Some (state.Kernel.VirtualClockMs + int64 timeout)

            // Push the optimistic `Int32 1` (signalled) onto the calling
            // thread's eval stack *before* parking. Park flips the
            // thread's status; the IL site advances past TimedWait when
            // the native handler returns `Stepped/Executed`, so the
            // pushed value sits on the parked thread's frame stack until
            // it's eventually woken — at which point either the value
            // is correct as-is (signal-wake) or it was rewritten to `0`
            // by `fireTimeout` (deadline-wake).
            let state =
                state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread

            let state = LowLevelMonitor.wait ctx.Thread id deadlineMs state
            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_Signal_Release",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Signal_Release"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let state = LowLevelMonitor.signalRelease ctx.Thread id state
            NativeHandlerResult.completed state |> Some

        | _ -> None
