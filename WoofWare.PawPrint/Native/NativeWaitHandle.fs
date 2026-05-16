namespace WoofWare.PawPrint

/// Native handler for the Win32-shaped wait-handle QCalls reachable from
/// CoreCLR-on-Unix: `CreateSemaphoreExW`, `ReleaseSemaphore`,
/// `CloseHandle`, `WaitHandle_WaitOneCore`, and
/// `WaitHandle_WaitOnePrioritized`. On .NET 10 the BCL compiles
/// `Semaphore.Windows.cs` regardless of host, with `Libraries.Kernel32`
/// rebound to `RuntimeHelpers.QCall`, so every Kernel32 LibraryImport
/// routes to the runtime as a QCall whose entry point uses the Win32
/// wide-string name. `LowLevelLifoSemaphore.Unix.cs` independently
/// imports `WaitHandle_WaitOnePrioritized` (the PAL-prioritized waiter
/// that `PortableThreadPool` workers park on). PawPrint catches each
/// entry point here and forwards it to the deterministic state machine
/// in `WaitHandle.fs`.
///
/// This first slice supports the semaphore variant only; events,
/// mutexes, multi-handle waits, and non-zero finite timeouts are out
/// of scope pending future PRs and a virtual clock. Zero-timeout waits
/// (the deterministic non-blocking probe `WaitOne(0)` emits) are
/// handled inline — no clock is needed.
[<RequireQualifiedAccess>]
module NativeWaitHandle =

    /// `ERROR_TOO_MANY_POSTS = 0x12A`. The Win32 contract returned by
    /// `ReleaseSemaphore` when the release would breach the configured
    /// maximum. `Semaphore.ReleaseCore` checks the BOOL return and
    /// throws `SemaphoreFullException` without inspecting the error
    /// code, but the source-generator wrapper still propagates the last
    /// error into `LastPInvokeError`, so we set it for fidelity with
    /// any guest that reads `Marshal.GetLastPInvokeError` after the
    /// throw.
    let private errorTooManyPosts : int = 298

    /// `WAIT_OBJECT_0 = 0`. The Win32 return code for a successful wait
    /// — the wait acquired its target object. Matches
    /// `WaitHandle.WaitSuccess` in the BCL.
    let private waitObjectZero : int = 0

    /// `WAIT_TIMEOUT = 0x102 = 258`. The Win32 return code for a wait
    /// that did not acquire its target before the timeout expired.
    /// `WaitHandle.WaitOne(int)` checks the return against
    /// `WaitHandle.WaitTimeout = 0x102` to decide whether to return
    /// `true` / `false` to the guest.
    let private waitTimeout : int = 0x102

    /// Mutate the kernel's `LastSystemError` slot. The
    /// LibraryImport-generated stub for a `SetLastError = true` import
    /// reads this immediately after the QCall returns and copies it
    /// into `LastPInvokeError`, so writing here is sufficient to make
    /// the error visible to `Marshal.GetLastPInvokeError`.
    let private withLastSystemError (error : int) (state : IlMachineState) : IlMachineState =
        state.MapKernel (fun kernel ->
            { kernel with
                LastSystemError = error
            }
        )

    /// Decode the `IntPtr handle` argument that every wait-handle entry
    /// point except `CreateSemaphoreExW` carries. The guest only ever
    /// obtains this value from a Create QCall, so a foreign IntPtr
    /// (e.g. a LowLevelMonitor handle, a GcHandle, a `Verbatim` scratch
    /// value) is unambiguously a guest bug. Null is rejected too: the
    /// managed wrappers null-check before reaching the P/Invoke in
    /// every normal path.
    let private waitHandleOfArgument (operation : string) (arg : CliType) : WaitHandleId =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.WaitHandlePtr id)) -> id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
            failwith
                $"%s{operation}: handle argument was IntPtr.Zero, but WaitHandle invariants require a non-null handle (the BCL wrapper would have thrown ObjectDisposedException or set SafeHandle.IsInvalid before reaching the runtime)."
        | other -> failwith $"%s{operation}: expected WaitHandle handle, got %O{other}"

    /// Decode an IntPtr argument that should be either `IntPtr.Zero`
    /// (CoreLib passes 0 for `lpSecurityAttributes`) or fail loud.
    let private requireNullIntPtr (operation : string) (argName : string) (arg : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> ()
        | other ->
            failwith
                $"%s{operation}: expected %s{argName} to be IntPtr.Zero (CoreLib does not pass security attributes through this QCall), got %O{other}"

    /// Decode an Int32 argument representing a Win32 BOOL flag. The
    /// LibraryImport source generator marshals `[MarshalAs(UnmanagedType
    /// .Bool)] bool` as 4-byte BOOL in the QCall signature; both 0
    /// (FALSE) and any non-zero value (TRUE) are accepted.
    let private boolOfBoolArgument (operation : string) (argName : string) (arg : CliType) : bool =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
        | other -> failwith $"%s{operation}: expected %s{argName} to be Int32 BOOL, got %O{other}"

    /// Decode the UTF-16 `name` pointer for `CreateSemaphoreExW`. CoreLib
    /// passes `null` for unnamed semaphores; named semaphores are out of
    /// scope until the named-handle registry lands, so a non-null name
    /// pointer fails loud.
    let private requireNullName (operation : string) (arg : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> ()
        | other ->
            failwith
                $"%s{operation}: named wait handles are not yet supported; expected a null name pointer, got %O{other}"

    /// Drive the timeout dispatch shared by `WaitHandle_WaitOneCore`
    /// and `WaitHandle_WaitOnePrioritized`: timeout = -1 (INFINITE)
    /// blocks via `blockingWait`; timeout = 0 (the non-blocking probe
    /// `WaitOne(0)` issues) takes the deterministic try-and-return path
    /// via `tryWaitOne`; any other finite timeout currently fails loud
    /// (no virtual clock — same blocker as
    /// `SystemNative_LowLevelMonitor_TimedWait`). Returns the new
    /// `IlMachineState` and the Int32 return value the guest sees.
    ///
    /// The two callers differ in `blockingWait`: `WaitOneCore` uses
    /// `WaitHandle.waitOne` (FIFO tail insertion);
    /// `WaitOnePrioritized` uses `WaitHandle.waitOnePrioritized`
    /// (LIFO head insertion). Both share the zero-timeout `tryWaitOne`
    /// because priority only matters when the wait actually parks.
    let private dispatchWait
        (operation : string)
        (id : WaitHandleId)
        (timeout : int)
        (blockingWait : IlMachineState -> WaitHandle.WaitOutcome)
        (state : IlMachineState)
        : IlMachineState * int
        =
        if timeout = System.Threading.Timeout.Infinite then
            // Both fast (count > 0) and slow (parked) blocking paths
            // return WAIT_OBJECT_0 to the guest; the IL site advances
            // in both cases, and the scheduler simply will not pick a
            // parked thread again until a subsequent `releaseSemaphore`
            // wakes it. This mirrors
            // `SystemNative_LowLevelMonitor_Acquire`'s posture.
            let state =
                match blockingWait state with
                | WaitHandle.WaitOutcome.Acquired state
                | WaitHandle.WaitOutcome.Blocked state -> state

            state, waitObjectZero
        elif timeout = 0 then
            // Zero-timeout: try the fast path, then return immediately.
            // CoreCLR returns `WAIT_OBJECT_0` if the handle was signalled,
            // else `WAIT_TIMEOUT`; the caller is never enqueued. Treating
            // a zero-timeout as an unimplemented finite timeout would
            // crash on a deterministic non-blocking probe (e.g. the
            // `WaitOne(0)` poll pattern), which is the wrong envelope.
            match WaitHandle.tryWaitOne id state with
            | WaitHandle.TryWaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.TryWaitOutcome.TimedOut state -> state, waitTimeout
        else
            // No virtual clock yet. Same envelope as `Monitor_Wait` /
            // `SystemNative_LowLevelMonitor_TimedWait`: silently treating
            // a finite timeout as Infinite would mask guest bugs that
            // depend on timeout-based wakeups; treating it as immediate
            // timeout would break the higher-level fairness contract.
            failwith
                $"%s{operation}: finite timeout (%d{timeout} ms) is not yet implemented; PawPrint has no virtual clock. Guest code that depends on timed waits must be lifted onto a deterministic clock abstraction first."

    /// Decode an optional `int*` out-pointer that may be `IntPtr.Zero`.
    /// `Some ptr` means we must write to `*ptr`; `None` means the
    /// caller passed null and the write is skipped (matching the Win32
    /// `lpPreviousCount` contract).
    let private optionalIntPointer
        (operation : string)
        (argName : string)
        (arg : CliType)
        : ManagedPointerSource option
        =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> Some ptr
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) -> Some ptr
        | other -> failwith $"%s{operation}: expected %s{argName} to be a managed int pointer or null, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "CreateSemaphoreExW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "CreateSemaphoreExW"
            // CoreLib's `CreateSemaphoreCore` passes `lpSecurityAttributes = 0`,
            // `flags = 0`, and `AccessRights` for `dwDesiredAccess`. We refuse a
            // non-null security-attributes pointer (out of scope) but ignore
            // `flags` / `dwDesiredAccess` — deviation from the documented
            // defaults would still produce a working handle on real Win32,
            // and modelling fidelity here would just paint over a guest bug
            // that should surface elsewhere.
            requireNullIntPtr operation "lpSecurityAttributes" instruction.Arguments.[0]

            let initialCount = NativeCall.int32Argument operation instruction.Arguments.[1]

            let maximumCount = NativeCall.int32Argument operation instruction.Arguments.[2]

            requireNullName operation instruction.Arguments.[3]

            let id, state = WaitHandle.createSemaphore initialCount maximumCount state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.WaitHandlePtr id)) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | "ReleaseSemaphore",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "ReleaseSemaphore"
            let id = waitHandleOfArgument operation instruction.Arguments.[0]

            let releaseCount = NativeCall.int32Argument operation instruction.Arguments.[1]

            let previousCountPtr =
                optionalIntPointer operation "lpPreviousCount" instruction.Arguments.[2]

            let outcome, state = WaitHandle.releaseSemaphore id releaseCount state

            match outcome with
            | Ok previousCount ->
                let state =
                    match previousCountPtr with
                    | None -> state
                    | Some ptr ->
                        IlMachineState.writeManagedByrefWithBase
                            ctx.BaseClassTypes
                            state
                            ptr
                            (CliType.Numeric (CliNumericType.Int32 previousCount))

                state
                |> withLastSystemError 0
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
                |> Some

            | Error (WaitHandle.ReleaseFailure.WouldExceedMaximum _) ->
                state
                |> withLastSystemError errorTooManyPosts
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
                |> Some

        | "CloseHandle",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CloseHandle"
            let id = waitHandleOfArgument operation instruction.Arguments.[0]
            let state = WaitHandle.close id state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | "WaitHandle_WaitOneCore",
          "System.Private.CoreLib",
          "WaitHandle",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "WaitHandle_WaitOneCore"
            let id = waitHandleOfArgument operation instruction.Arguments.[0]

            let timeout = NativeCall.int32Argument operation instruction.Arguments.[1]
            // `useTrivialWaits` only affects async-context teardown ordering
            // (whether `Wait` may run APC-style callbacks); PawPrint does not
            // model that ordering, so we decode the argument for shape
            // validation but otherwise ignore it.
            let _useTrivialWaits =
                boolOfBoolArgument operation "useTrivialWaits" instruction.Arguments.[2]

            let state, ret =
                dispatchWait operation id timeout (WaitHandle.waitOne ctx.Thread id) state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 ret) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | "WaitHandle_WaitOnePrioritized",
          "System.Private.CoreLib",
          "LowLevelLifoSemaphore",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `LowLevelLifoSemaphore.WaitCore` (PortableThreadPool's worker
            // park primitive on Unix) imports this 2-arg variant of the
            // waiter. The "Prioritized" tag corresponds to
            // `PAL_WaitForSingleObjectPrioritized`'s LIFO release policy:
            // new waiters are registered at the head of the wait queue
            // and a later `Release` wakes the most recent one first.
            // This is load-bearing for `LowLevelLifoSemaphore` (and hence
            // `PortableThreadPool`) — that's exactly why it has its own
            // entry point separate from `WaitOneCore`.
            let operation = "WaitHandle_WaitOnePrioritized"
            let id = waitHandleOfArgument operation instruction.Arguments.[0]

            let timeout = NativeCall.int32Argument operation instruction.Arguments.[1]

            let state, ret =
                dispatchWait operation id timeout (WaitHandle.waitOnePrioritized ctx.Thread id) state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 ret) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | _ -> None
