namespace WoofWare.PawPrint

/// Native handler for the Win32-shaped wait-handle QCalls reachable from
/// CoreCLR-on-Unix: `CreateSemaphoreExW`, `ReleaseSemaphore`,
/// `CloseHandle`, `WaitHandle_WaitOneCore`,
/// `WaitHandle_WaitOnePrioritized`, `PAL_CreateMutexW`, and
/// `ReleaseMutex`. On .NET 10 the BCL compiles `Semaphore.Windows.cs`
/// and `Mutex.CoreCLR.Unix.cs` regardless of host, with
/// `Libraries.Kernel32` rebound to `RuntimeHelpers.QCall`, so every
/// Kernel32 LibraryImport routes to the runtime as a QCall whose entry
/// point uses the Win32 wide-string name. `LowLevelLifoSemaphore.Unix
/// .cs` independently imports `WaitHandle_WaitOnePrioritized` (the
/// PAL-prioritized waiter that `PortableThreadPool` workers park on);
/// `Mutex.CoreCLR.Unix.cs` imports `PAL_CreateMutexW` directly. PawPrint
/// catches each entry point here and forwards it to the deterministic
/// state machine in `WaitHandle.fs`.
///
/// Semaphore and mutex variants are supported today; events,
/// multi-handle waits, named handles (`PAL_OpenMutexW`), and non-zero
/// finite timeouts are out of scope pending future PRs and a virtual
/// clock. Zero-timeout waits (the deterministic non-blocking probe
/// `WaitOne(0)` emits) are handled inline — no clock is needed.
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

    /// `ERROR_NOT_OWNER = 0x120 = 288`. The Win32 contract returned by
    /// `ReleaseMutex` when the calling thread does not own the mutex
    /// (either the mutex is free, or another thread holds it). The
    /// BCL's `Mutex.ReleaseMutex` checks the BOOL return and throws
    /// `ApplicationException` (today: `SynchronizationLockException`)
    /// without inspecting the error code, but the source-generator
    /// wrapper still propagates the last error into
    /// `LastPInvokeError`, so we set it for fidelity with any guest
    /// that reads `Marshal.GetLastPInvokeError` after the throw.
    let private errorNotOwner : int = 288

    /// `WAIT_OBJECT_0 = 0`. The Win32 return code for a successful wait
    /// — the wait acquired its target object. Matches
    /// `WaitHandle.WaitSuccess` in the BCL.
    let private waitObjectZero : int = 0

    /// `WAIT_ABANDONED = 0x80 = 128`. The Win32 return code for a wait
    /// that acquired its target mutex, but the previous owner had
    /// terminated without calling `ReleaseMutex`. The BCL's
    /// `Mutex.WaitOneNoCheck` translates this into
    /// `AbandonedMutexException`.
    let private waitAbandoned : int = 0x80

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
    /// point except the Create QCalls carries. The guest only ever
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

    /// Decode the `IntPtr handle` argument and additionally require
    /// that it refer to a Semaphore (rather than a Mutex or any future
    /// kind). Used by `WaitHandle_WaitOnePrioritized`: the
    /// PAL-prioritized waiter is the LowLevelLifoSemaphore park
    /// primitive and is only ever called against semaphore handles;
    /// passing a mutex handle would be a guest bug. Fail loud rather
    /// than fall through to a kind-generic probe.
    let private semaphoreHandleOfArgument (operation : string) (arg : CliType) (state : IlMachineState) : WaitHandleId =
        let id = waitHandleOfArgument operation arg

        match Map.tryFind id state.Kernel.WaitHandles with
        | Some (WaitHandleState.Semaphore _) -> id
        | Some (WaitHandleState.Mutex _) ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Mutex, but this entry point is only legal against a Semaphore (the BCL's LowLevelLifoSemaphore is the sole caller). This is a guest bug."
        | None ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is not registered (use-after-free on a closed handle, or never created)."

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

    /// Decode the UTF-16 `name` pointer for `PAL_CreateMutexW`. Unlike
    /// `Semaphore.Windows.cs`, which throws `PlatformNotSupportedException`
    /// before ever passing a non-null name to the PAL, `Mutex.CoreCLR
    /// .Unix.cs` treats `string.IsNullOrEmpty(name)` as unnamed at the
    /// options layer but still flows the original `name` string through
    /// the LibraryImport marshaller. An empty string therefore arrives
    /// at this QCall as a non-null UTF-16 pointer to a single NUL char;
    /// CoreLib still considers the call unnamed. Accept null or empty
    /// UTF-16 here, and fail loud for any non-empty name — named
    /// mutexes are out of scope until the named-handle registry lands.
    let private requireUnnamedMutex
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : CliType)
        : unit
        =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> ()
        | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) ->
            let name = NativeCall.readNullTerminatedUtf16 operation baseClassTypes state ptr

            if name <> "" then
                failwith
                    $"%s{operation}: named mutexes are not yet supported; expected an unnamed handle (null or empty name) but got '%s{name}'"
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) ->
            let name = NativeCall.readNullTerminatedUtf16 operation baseClassTypes state ptr

            if name <> "" then
                failwith
                    $"%s{operation}: named mutexes are not yet supported; expected an unnamed handle (null or empty name) but got '%s{name}'"
        | other ->
            failwith
                $"%s{operation}: named mutexes are not yet supported; expected a null or empty UTF-16 name pointer, got %O{other}"

    /// Decode an unused `byte*` argument (e.g. `PAL_CreateMutexW`'s
    /// `systemCallErrors` out-buffer). The buffer is only written on the
    /// PAL failure paths we don't take; on success the BCL ignores its
    /// contents. We accept any managed/native pointer (the CoreLib
    /// caller is a `stackalloc byte[256]` whose representation depends
    /// on the frame's local layout) without inspecting them.
    let private ignoreBytePointer (operation : string) (argName : string) (arg : CliType) : unit =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt _)
        | CliType.RuntimePointer _ -> ()
        | other -> failwith $"%s{operation}: expected %s{argName} to be a pointer-shaped argument, got %O{other}"

    /// Drive the timeout dispatch shared by `WaitHandle_WaitOneCore`
    /// and `WaitHandle_WaitOnePrioritized`: timeout = -1 (INFINITE)
    /// blocks via `blockingWait`; timeout = 0 (the non-blocking probe
    /// `WaitOne(0)` issues) takes the deterministic try-and-return path
    /// via `tryWait`; any other finite timeout currently fails loud
    /// (no virtual clock — same blocker as
    /// `SystemNative_LowLevelMonitor_TimedWait`). Returns the new
    /// `IlMachineState` and the Int32 return value the guest sees.
    ///
    /// `blockingWait` and `tryWait` are passed in separately rather
    /// than derived from a shared kind-generic primitive so that the
    /// prioritized handler can route to semaphore-only variants and a
    /// mutex handle reaching the prioritized entry point fails loud at
    /// the decoder level rather than accidentally succeeding via a
    /// kind-generic probe.
    let private dispatchWait
        (operation : string)
        (timeout : int)
        (blockingWait : IlMachineState -> WaitHandle.WaitOutcome)
        (tryWait : IlMachineState -> WaitHandle.TryWaitOutcome)
        (state : IlMachineState)
        : IlMachineState * int
        =
        if timeout = System.Threading.Timeout.Infinite then
            // Both fast (count > 0 / mutex free / re-entrant) and slow
            // (parked) blocking paths advance the IL site; the
            // scheduler will not pick a parked thread again until a
            // subsequent wake. `Acquired` returns `WAIT_OBJECT_0`;
            // `AcquiredAbandoned` returns `WAIT_ABANDONED`; `Blocked`
            // pushes `WAIT_OBJECT_0` at park time (see the
            // abandoned-mutex-propagation note in `WaitHandle.fs`).
            match blockingWait state with
            | WaitHandle.WaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.WaitOutcome.AcquiredAbandoned state -> state, waitAbandoned
            | WaitHandle.WaitOutcome.Blocked state -> state, waitObjectZero
        elif timeout = 0 then
            // Zero-timeout: try the fast path, then return immediately.
            // CoreCLR returns `WAIT_OBJECT_0` / `WAIT_ABANDONED` if the
            // handle was signalled, else `WAIT_TIMEOUT`; the caller is
            // never enqueued. Treating a zero-timeout as an
            // unimplemented finite timeout would crash on a
            // deterministic non-blocking probe (e.g. the `WaitOne(0)`
            // poll pattern), which is the wrong envelope.
            match tryWait state with
            | WaitHandle.TryWaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.TryWaitOutcome.AcquiredAbandoned state -> state, waitAbandoned
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
                dispatchWait
                    operation
                    timeout
                    (WaitHandle.waitOne ctx.Thread id)
                    (WaitHandle.tryWaitOne ctx.Thread id)
                    state

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
            // Enforce the semaphore-only contract at the decoder layer so
            // a mutex handle reaching this entry point is a clean guest-
            // bug failure rather than an accidental success via the
            // kind-generic primitive.
            let id = semaphoreHandleOfArgument operation instruction.Arguments.[0] state

            let timeout = NativeCall.int32Argument operation instruction.Arguments.[1]

            let state, ret =
                dispatchWait
                    operation
                    timeout
                    (WaitHandle.waitOnePrioritized ctx.Thread id)
                    (WaitHandle.tryWaitOneSemaphore id)
                    state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 ret) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | "PAL_CreateMutexW",
          "System.Private.CoreLib",
          "Mutex",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns _ ->
            // `Mutex.CoreCLR.Unix.cs` declares this as a `partial
            // SafeWaitHandle` return; the LibraryImport source generator
            // marshals the SafeWaitHandle as an `IntPtr` over the wire,
            // but the QCall return type recorded in the method's
            // signature is the SafeWaitHandle reference type (the
            // marshalling is applied by the generated stub on either
            // side of the call). We don't constrain the return type
            // here beyond "the method has a return" because the
            // declared return is a reference type in IL terms, not the
            // primitive `IntPtr` the semaphore Create QCall uses.
            let operation = "PAL_CreateMutexW"

            let initialOwner =
                boolOfBoolArgument operation "initialOwner" instruction.Arguments.[0]

            requireUnnamedMutex operation ctx.BaseClassTypes state instruction.Arguments.[1]
            // `currentUserOnly` flags whether the named-handle backing
            // store should be filtered to the current user. We don't
            // support named handles, so its value is moot — decode for
            // shape validation but ignore.
            let _currentUserOnly =
                boolOfBoolArgument operation "currentUserOnly" instruction.Arguments.[2]

            ignoreBytePointer operation "systemCallErrors" instruction.Arguments.[3]
            // `systemCallErrorsBufferSize` is only consulted on the
            // failure paths we don't take. CoreLib hardcodes 256; we
            // accept whatever's there.
            let _bufSize = NativeCall.int32Argument operation instruction.Arguments.[4]

            let id, state = WaitHandle.createMutex initialOwner ctx.Thread state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.WaitHandlePtr id)) ctx.Thread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.stepped
            |> Some

        | "ReleaseMutex",
          "System.Private.CoreLib",
          "Kernel32",
          [ _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // The single argument is declared as `SafeWaitHandle` in
            // CoreLib's `Interop.Mutex.cs`, but the LibraryImport stub
            // marshals it as an `IntPtr` at the QCall boundary. Decode
            // through `waitHandleOfArgument`, which already accepts the
            // `NativeIntSource.WaitHandlePtr` representation produced
            // by `PAL_CreateMutexW`.
            let operation = "ReleaseMutex"
            let id = waitHandleOfArgument operation instruction.Arguments.[0]
            let outcome, state = WaitHandle.releaseMutex ctx.Thread id state

            match outcome with
            | Ok () ->
                state
                |> withLastSystemError 0
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
                |> Some
            | Error WaitHandle.ReleaseMutexFailure.NotOwner ->
                state
                |> withLastSystemError errorNotOwner
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.stepped
                |> Some

        | _ -> None
