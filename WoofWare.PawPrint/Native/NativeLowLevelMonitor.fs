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
            let state = LowLevelMonitor.wait ctx.Thread id state
            NativeHandlerResult.completed state |> Some

        | Some "SystemNative_LowLevelMonitor_TimedWait",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // PawPrint has no virtual clock, so we can't compute "did the
            // wait time out before being signalled?" deterministically.
            // Treating TimedWait as Wait (always block until signalled) would
            // turn finite-timeout code into a quiet deadlock when no signal
            // ever arrives; treating it as immediate timeout would break
            // LowLevelLifoSemaphore's flow-control invariants. Fail loud so
            // the missing primitive surfaces at the call site.
            let operation = "SystemNative_LowLevelMonitor_TimedWait"
            let _ = monitorOfArgument operation instruction.Arguments.[0]

            let timeout = NativeCall.int32Argument operation instruction.Arguments.[1]

            failwith
                $"%s{operation}: timed wait (%d{timeout} ms) is not yet implemented; PawPrint has no virtual clock to compute timeout deterministically. Guest code that depends on timed waits must be lifted onto a deterministic clock abstraction first."

        | Some "SystemNative_LowLevelMonitor_Signal_Release",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Void ->
            let operation = "SystemNative_LowLevelMonitor_Signal_Release"
            let id = monitorOfArgument operation instruction.Arguments.[0]
            let state = LowLevelMonitor.signalRelease ctx.Thread id state
            NativeHandlerResult.completed state |> Some

        | _ -> None
