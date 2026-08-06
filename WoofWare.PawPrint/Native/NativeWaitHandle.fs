namespace WoofWare.PawPrint

/// Native handler for the Win32-shaped wait-handle QCalls reachable from
/// CoreCLR-on-Unix: `CreateSemaphoreExW`, `ReleaseSemaphore`,
/// `CloseHandle`, `WaitHandle_WaitOneCore`,
/// `WaitHandle_WaitOnePrioritized`, `PAL_CreateMutexW`, `ReleaseMutex`,
/// `CreateEventExW`, `SetEvent`, and `ResetEvent`. On .NET 10 the BCL
/// compiles `Semaphore.Windows.cs`, `Mutex.CoreCLR.Unix.cs`, and
/// `EventWaitHandle.Windows.cs` regardless of host, with
/// `Libraries.Kernel32` rebound to `RuntimeHelpers.QCall`, so every
/// Kernel32 LibraryImport routes to the runtime as a QCall whose entry
/// point uses the Win32 wide-string name. `LowLevelLifoSemaphore.Unix
/// .cs` independently imports `WaitHandle_WaitOnePrioritized` (the
/// PAL-prioritized waiter that `PortableThreadPool` workers park on);
/// `Mutex.CoreCLR.Unix.cs` imports `PAL_CreateMutexW` directly. PawPrint
/// catches each entry point here and forwards it to the deterministic
/// state machine in `WaitHandle.fs`.
///
/// Semaphore, mutex, and event variants are supported today;
/// multi-handle waits, named handles (`PAL_OpenMutexW`, `OpenEventW`),
/// and non-zero finite timeouts are out of scope pending future PRs and
/// a virtual clock. Zero-timeout waits (the deterministic non-blocking
/// probe `WaitOne(0)` emits) are handled inline — no clock is needed.
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

    /// `ERROR_INVALID_PARAMETER = 87`. The Win32 error the PAL sets when a
    /// wait-all names the same handle twice (`wait.cpp`'s duplicate scan).
    /// The QCall returns `WAIT_FAILED` alongside it.
    let private errorInvalidParameter : int = 87

    /// The Win32 wait return codes live on `WaitHandle` rather than here:
    /// they are part of the wait-handle contract that the state machine
    /// itself has to speak (a multi-wait's return value is materialised at
    /// wake time, inside the state machine), so a second copy here would be
    /// two definitions of one thing.
    let private waitObjectZero : int = WaitHandle.waitObjectZero
    let private waitAbandoned : int = WaitHandle.waitAbandoned
    let private waitTimeout : int = WaitHandle.waitTimeout
    let private waitFailed : int = WaitHandle.waitFailed

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
    /// that it refer to an Event (rather than a Semaphore, a Mutex, or
    /// any future kind). Used by `SetEvent` and `ResetEvent`: these are
    /// only legal against an event handle; routing a foreign-kind handle
    /// through them is a guest bug that should fail loud at the decoder
    /// rather than fall through to a kind-generic probe.
    let private eventHandleOfArgument (operation : string) (arg : CliType) (state : IlMachineState) : WaitHandleId =
        let id = waitHandleOfArgument operation arg

        match Map.tryFind id state.Kernel.WaitHandles with
        | Some (WaitHandleState.Event _) -> id
        | Some (WaitHandleState.Semaphore _) ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Semaphore, but this entry point is only legal against an Event. This is a guest bug — the BCL only calls this through EventWaitHandle."
        | Some (WaitHandleState.Mutex _) ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is a Mutex, but this entry point is only legal against an Event. This is a guest bug — the BCL only calls this through EventWaitHandle."
        | None ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is not registered (use-after-free on a closed handle, or never created)."

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
        | Some (WaitHandleState.Event _) ->
            failwith
                $"%s{operation}: WaitHandle %O{id} is an Event, but this entry point is only legal against a Semaphore (the BCL's LowLevelLifoSemaphore is the sole caller). This is a guest bug."
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

    /// Decode a UInt32 argument. PawPrint models a CLI UInt32 as a
    /// signed Int32 cell while preserving the low 32 bits (see
    /// `NativeCall.cliUInt32`); we reverse that here.
    let private uint32OfArgument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> uint32 i
        | other -> failwith $"%s{operation}: expected %s{argName} to be UInt32, got %O{other}"

    /// `CREATE_EVENT_MANUAL_RESET = 0x1` — when set on `CreateEventExW`'s
    /// `flags`, the new event is `Manual`; cleared means `Auto`. See
    /// `Interop.EventWaitHandle.cs`.
    let private createEventManualReset : uint32 = 0x1u

    /// `CREATE_EVENT_INITIAL_SET = 0x2` — when set, the new event is
    /// created in the signalled state. See `Interop.EventWaitHandle.cs`.
    let private createEventInitialSet : uint32 = 0x2u

    /// Parse `CreateEventExW`'s `flags` argument into the two documented
    /// bits (`CREATE_EVENT_MANUAL_RESET`, `CREATE_EVENT_INITIAL_SET`).
    /// Any unknown bit fails loud — a guest passing a flag CoreLib does
    /// not produce is using an unsupported Win32 extension we have not
    /// modelled. The two known bits compose freely.
    let private parseCreateEventFlags (operation : string) (flags : uint32) : bool * EventResetMode =
        let known = createEventManualReset ||| createEventInitialSet
        let unknown = flags &&& ~~~known

        if unknown <> 0u then
            failwith
                $"%s{operation}: unrecognised CreateEventExW flag bits: 0x%x{unknown} (known bits: CREATE_EVENT_MANUAL_RESET=0x1, CREATE_EVENT_INITIAL_SET=0x2)"

        let initialState = (flags &&& createEventInitialSet) <> 0u

        let mode =
            if (flags &&& createEventManualReset) <> 0u then
                EventResetMode.Manual
            else
                EventResetMode.Auto

        initialState, mode

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
    /// and `WaitHandle_WaitOnePrioritized`. Three cases:
    ///
    ///  - `timeout = -1` (INFINITE): pass `None` for the deadline, so a
    ///    slow-path park records no deadline and the driver loop never
    ///    fires a timeout.
    ///  - `timeout = 0`: the non-blocking probe `WaitOne(0)` issues —
    ///    routed through `tryWait`, which never parks and returns
    ///    `WAIT_OBJECT_0` / `WAIT_ABANDONED` / `WAIT_TIMEOUT` inline.
    ///  - `timeout > 0`: compute an absolute deadline as `VirtualClockMs
    ///    + timeout` and thread it through `blockingWait`. The fast
    ///    paths ignore the deadline; the slow path records it on the
    ///    parked thread's `BlockedOnWaitHandle` status, and the driver
    ///    loop's deadline-firing pass picks it up.
    ///
    /// Returns the new `IlMachineState` and the Int32 return value the
    /// guest sees. Note that the `Blocked` outcome pushes `WAIT_OBJECT_0`
    /// at park time even for a finite-timeout wait — the deadline-fire
    /// path rewrites the slot to `WAIT_TIMEOUT` when it expires.
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
        (blockingWait : int64 option -> IlMachineState -> WaitHandle.WaitOutcome)
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
            match blockingWait None state with
            | WaitHandle.WaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.WaitOutcome.AcquiredAbandoned state -> state, waitAbandoned
            | WaitHandle.WaitOutcome.Blocked state -> state, waitObjectZero
        elif timeout = 0 then
            // Zero-timeout: try the fast path, then return immediately.
            // CoreCLR returns `WAIT_OBJECT_0` / `WAIT_ABANDONED` if the
            // handle was signalled, else `WAIT_TIMEOUT`; the caller is
            // never enqueued. Routing through `tryWait` rather than the
            // finite-deadline path keeps the deterministic non-blocking
            // probe entirely off the scheduler's deadline radar.
            match tryWait state with
            | WaitHandle.TryWaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.TryWaitOutcome.AcquiredAbandoned state -> state, waitAbandoned
            | WaitHandle.TryWaitOutcome.TimedOut state -> state, waitTimeout
        elif timeout < 0 then
            // The BCL's `WaitHandle.WaitOne(int)` wrapper validates that
            // the only legal negative value is `-1 = INFINITE`; reaching
            // the QCall with any other negative value means the wrapper
            // was bypassed (guest bug). Fail loud rather than treating
            // it as a 0 timeout (silent bug-masking) or as INFINITE
            // (different deadlock surface).
            failwith
                $"%s{operation}: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's WaitHandle.WaitOne(int) validates this argument before the QCall, so reaching here means the wrapper was bypassed."
        else
            // Finite positive timeout: compute an absolute deadline
            // against the virtual clock. `VirtualClockMs` advances 1 ms
            // per scheduler tick (per `Program.stepPrepared`), and the
            // driver loop fires `WaitHandle.fireTimeout` when the clock
            // reaches or passes a parked thread's deadline; if no other
            // thread is Runnable, the driver also jumps the clock to
            // the nearest pending deadline so the wait can resolve.
            // `int64` keeps the addition safe even for
            // `Int32.MaxValue` timeouts against a long-running clock.
            let deadlineMs = state.Kernel.VirtualClockMs + int64 timeout

            match blockingWait (Some deadlineMs) state with
            | WaitHandle.WaitOutcome.Acquired state -> state, waitObjectZero
            | WaitHandle.WaitOutcome.AcquiredAbandoned state -> state, waitAbandoned
            | WaitHandle.WaitOutcome.Blocked state -> state, waitObjectZero

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

    /// The integer a satisfied multi-handle wait reports to the guest.
    ///
    /// A wait-any adds the index of the handle that satisfied it, so
    /// `WaitHandle.WaitAny` can return it and `WaitMultiple` can recover which
    /// handle was abandoned from the `[WaitAbandoned, WaitAbandoned + count)`
    /// range. A wait-all adds nothing: the OS reports only *that* a mutex was
    /// abandoned, not which, and the BCL throws a bare
    /// `AbandonedMutexException` for it.
    let private multiWaitResult (waitAll : bool) (index : int) (abandoned : bool) : int =
        let baseCode =
            if abandoned then
                WaitHandle.waitAbandoned
            else
                WaitHandle.waitObjectZero

        if waitAll then baseCode else baseCode + index

    /// `MAXIMUM_WAIT_OBJECTS`. The PAL rejects a multi-wait naming more than
    /// this many handles with `ERROR_INVALID_PARAMETER`; the BCL's
    /// `WaitHandle.MaxWaitHandles` check throws `NotSupportedException`
    /// first, so reaching the runtime over the limit means the wrapper was
    /// bypassed.
    let private maximumWaitObjects : int = 64

    /// The `System.IntPtr` concrete type, needed to step a byte view along
    /// the guest's handle array in pointer-sized strides.
    let private requiredIntPtrConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.IntPtr.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.IntPtr is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.IntPtr handle %O{handle} not found")

    /// Read `count` handles from the guest's `IntPtr*`.
    ///
    /// The array is a `stackalloc IntPtr[n]` that `WaitHandle
    /// .ObtainSafeWaitHandles` filled with `DangerousGetHandle()` results,
    /// flattened to a pointer by the LibraryImport stub's
    /// `GetPinnableReference` + `conv.u`. Each element is therefore a
    /// `WaitHandlePtr`-tagged native int, which has no bit pattern: the read
    /// has to come back as a whole typed cell or the identity is lost.
    /// `readManagedByrefBytesAs` with an `IntPtr` template does exactly that
    /// — its cell-aligned fast path (`readStackMemoryBytesAs`) returns a
    /// non-byte-addressable cell as-is when a same-shaped cell starts at the
    /// offset — which is why this steps a byte view rather than reinterpreting
    /// the buffer.
    let private readWaitHandleArray
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (count : int)
        : WaitHandleId list
        =
        let intPtrConcreteType = requiredIntPtrConcreteType operation baseClassTypes state
        let stride = 8

        [ 0 .. count - 1 ]
        |> List.map (fun index ->
            let elementPtr =
                ManagedPointerByteView.addByteOffset baseClassTypes state intPtrConcreteType (index * stride) ptr

            let element =
                IlMachineState.readManagedByrefBytesAs
                    baseClassTypes
                    state
                    elementPtr
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

            waitHandleOfArgument $"%s{operation} (handle #%d{index})" element
        )

    /// Decode the `IntPtr*` handle-array argument. The BCL never passes null
    /// here (an empty array throws `ArgumentException` in `WaitMultiple`
    /// before the QCall), so a null pointer means the wrapper was bypassed.
    let private handleArrayPointer (operation : string) (arg : CliType) : ManagedPointerSource =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
            failwith
                $"%s{operation}: handle array pointer was null; WaitHandle.WaitMultiple rejects an empty array before the QCall, so reaching here means the wrapper was bypassed."
        | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) -> ptr
        | other -> failwith $"%s{operation}: expected a managed IntPtr* handle array, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
            |> NativeHandlerResult.completed
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
                |> NativeHandlerResult.completed
                |> Some

            | Error (WaitHandle.ReleaseFailure.WouldExceedMaximum _) ->
                state
                |> withLastSystemError errorTooManyPosts
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                |> NativeHandlerResult.completed
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
            |> NativeHandlerResult.completed
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
            |> NativeHandlerResult.completed
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
            |> NativeHandlerResult.completed
            |> Some

        | "WaitHandle_WaitMultipleIgnoringSyncContext",
          "System.Private.CoreLib",
          "WaitHandle",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `WaitHandle.CoreCLR.cs` declares this as
            // `(ReadOnlySpan<IntPtr>, int numHandles, bool waitAll, int
            // millisecondsTimeout)`; the LibraryImport stub pins the span and
            // passes its first element's address, so the QCall signature is
            // `(IntPtr*, int32, int32 BOOL, int32)`.
            let operation = "WaitHandle_WaitMultipleIgnoringSyncContext"
            let arrayPtr = handleArrayPointer operation instruction.Arguments.[0]
            let numHandles = NativeCall.int32Argument operation instruction.Arguments.[1]

            // `WaitHandle.WaitMultiple` rejects an empty array
            // (`ArgumentException`) and one over `MaxWaitHandles`
            // (`NotSupportedException`) before the QCall, so both bounds
            // being violated here means the wrapper was bypassed. The PAL
            // would answer `ERROR_INVALID_PARAMETER`; failing loud is more
            // useful, because no correct guest can produce it.
            if numHandles < 1 then
                failwith
                    $"%s{operation}: numHandles = %d{numHandles} is not strictly positive; WaitHandle.WaitMultiple throws ArgumentException for an empty array before the QCall."

            if numHandles > maximumWaitObjects then
                failwith
                    $"%s{operation}: numHandles = %d{numHandles} exceeds MAXIMUM_WAIT_OBJECTS = %d{maximumWaitObjects}; WaitHandle.WaitMultiple throws NotSupportedException before the QCall."

            let waitAll = boolOfBoolArgument operation "waitAll" instruction.Arguments.[2]
            let timeout = NativeCall.int32Argument operation instruction.Arguments.[3]

            let handles =
                readWaitHandleArray operation ctx.BaseClassTypes state arrayPtr numHandles

            // The PAL forces `fWAll = false` when there is a single handle
            // ("makes no difference when nCount is 1"). We do not need to
            // mirror that: with one handle both modes acquire it and report
            // `WAIT_OBJECT_0 + 0`, which is the same value.
            let state, ret =
                if timeout = System.Threading.Timeout.Infinite then
                    match WaitHandle.waitMultiple ctx.Thread handles waitAll None state with
                    | WaitHandle.MultiWaitOutcome.Acquired (index, abandoned, state) ->
                        state, multiWaitResult waitAll index abandoned
                    // The park-time push is the optimistic `WAIT_OBJECT_0`;
                    // the wake rewrites it once the satisfying handle (and
                    // hence the index) is known.
                    | WaitHandle.MultiWaitOutcome.Blocked state -> state, waitObjectZero
                    | WaitHandle.MultiWaitOutcome.Failed state ->
                        withLastSystemError errorInvalidParameter state, waitFailed
                elif timeout = 0 then
                    match WaitHandle.tryWaitMultiple ctx.Thread handles waitAll state with
                    | WaitHandle.MultiTryWaitOutcome.Acquired (index, abandoned, state) ->
                        state, multiWaitResult waitAll index abandoned
                    | WaitHandle.MultiTryWaitOutcome.TimedOut state -> state, waitTimeout
                    | WaitHandle.MultiTryWaitOutcome.Failed state ->
                        withLastSystemError errorInvalidParameter state, waitFailed
                elif timeout < 0 then
                    failwith
                        $"%s{operation}: negative timeout %d{timeout} ms is not Infinite (-1); WaitHandle.WaitMultiple validates this argument before the QCall, so reaching here means the wrapper was bypassed."
                else
                    let deadlineMs = state.Kernel.VirtualClockMs + int64 timeout

                    match WaitHandle.waitMultiple ctx.Thread handles waitAll (Some deadlineMs) state with
                    | WaitHandle.MultiWaitOutcome.Acquired (index, abandoned, state) ->
                        state, multiWaitResult waitAll index abandoned
                    | WaitHandle.MultiWaitOutcome.Blocked state -> state, waitObjectZero
                    | WaitHandle.MultiWaitOutcome.Failed state ->
                        withLastSystemError errorInvalidParameter state, waitFailed

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 ret) ctx.Thread
            |> NativeHandlerResult.completed
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
            |> NativeHandlerResult.completed
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
                |> NativeHandlerResult.completed
                |> Some
            | Error WaitHandle.ReleaseMutexFailure.NotOwner ->
                state
                |> withLastSystemError errorNotOwner
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

        | "CreateEventExW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns _ ->
            // `EventWaitHandle.Windows.cs` declares the return as
            // `SafeWaitHandle`, a reference type at the IL boundary; the
            // LibraryImport stub marshals it as `IntPtr` over the wire.
            // We don't constrain the return type pattern beyond "has a
            // return" for the same reason as `PAL_CreateMutexW`.
            let operation = "CreateEventExW"
            // CoreLib's `CreateEventCore(initialState, mode)` passes
            // `lpSecurityAttributes = 0`. Non-null is reserved for the
            // named CurrentUserOnly path which is unreachable on Unix
            // (named events PNSE before reaching the QCall).
            requireNullIntPtr operation "lpSecurityAttributes" instruction.Arguments.[0]
            // Named events on Unix throw `PlatformNotSupportedException`
            // at `EventWaitHandle.Windows.cs:55`, so a non-null name
            // reaching this QCall is a guest bug — strict null until the
            // named-handle registry lands alongside named semaphores /
            // mutexes.
            requireNullName operation instruction.Arguments.[1]

            let flags = uint32OfArgument operation "flags" instruction.Arguments.[2]
            let initialState, mode = parseCreateEventFlags operation flags
            // `dwDesiredAccess` is hardcoded to MAXIMUM_ALLOWED |
            // SYNCHRONIZE | EVENT_MODIFY_STATE by the BCL; we don't
            // model access rights, so decode for shape validation only.
            let _desiredAccess =
                uint32OfArgument operation "dwDesiredAccess" instruction.Arguments.[3]

            let id, state = WaitHandle.createEvent initialState mode state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.WaitHandlePtr id)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | "SetEvent",
          "System.Private.CoreLib",
          "Kernel32",
          [ _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // The single argument is declared as `SafeWaitHandle` in
            // `Interop.EventWaitHandle.cs`; the LibraryImport stub
            // marshals it as `IntPtr` at the QCall boundary. Decode
            // through `eventHandleOfArgument`, which kind-checks the
            // handle and rejects null / non-event handles loudly.
            let operation = "SetEvent"
            let id = eventHandleOfArgument operation instruction.Arguments.[0] state
            let state = WaitHandle.setEvent id state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | "ResetEvent",
          "System.Private.CoreLib",
          "Kernel32",
          [ _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "ResetEvent"
            let id = eventHandleOfArgument operation instruction.Arguments.[0] state
            let state = WaitHandle.resetEvent id state

            state
            |> withLastSystemError 0
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        | _ -> None
