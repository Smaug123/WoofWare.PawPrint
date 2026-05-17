namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeSystemNative =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.NativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    let private pushInt32 (value : int) (ctx : NativeCallContext) : NativeHandlerResult =
        ctx.State
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value) ctx.Thread
        |> NativeHandlerResult.completed

    /// Matches the parameter type that `Interop.Sys.GetPlatformSignalNumber`
    /// declares. The BCL's LibraryImport source generator preserves the
    /// `PosixSignal` enum in the P/Invoke stub signature (the enum is
    /// blittable, so no conversion thunk is emitted), so PawPrint sees the
    /// enum-typed parameter — not a plain `Int32`. The C-side stub
    /// `static partial extern int __PInvoke(PosixSignal signal)` is what
    /// PawPrint dispatches on. Other guests may take the same entry point
    /// via a hand-rolled `[DllImport]` declaring `int` directly (e.g. the
    /// unit-test stub in `sourcesPure/SystemNativeGetPlatformSignalNumber.cs`),
    /// so we accept either shape — the underlying `int32Argument` decode
    /// peels enum boxing via `unwrapPrimitiveLikeDeep` and produces the same
    /// raw value either way.
    let private (|PosixSignalParam|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcretePrimitive concreteTypes PrimitiveType.Int32 -> Some ()
        | ConcreteType concreteTypes ("System.Private.CoreLib",
                                      "System.Runtime.InteropServices",
                                      "PosixSignal",
                                      generics) when generics.IsEmpty -> Some ()
        | _ -> None

    /// Decode an `nint`-shaped Unix file-descriptor argument. CoreLib passes
    /// fds across the SystemNative boundary as plain `IntPtr` values (the low
    /// 32 bits of `SafeFileHandle.handle`); PawPrint represents these as
    /// `NativeIntSource.Verbatim`. Refuses non-verbatim sources because their
    /// provenance encodes something other than an fd integer, which the
    /// FileDescriptorRegistry has no way to interpret.
    let private fdArgument (operation : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim value)) ->
            if value < int64 System.Int32.MinValue || value > int64 System.Int32.MaxValue then
                // fds are int32-bounded on every Unix kernel we model. A
                // value outside that range cannot correspond to a live fd in
                // the registry; return a sentinel that will miss the table
                // and produce EBADF, rather than silently truncating to a
                // potentially-live fd.
                System.Int32.MinValue
            else
                int value
        | CliType.Numeric (CliNumericType.NativeInt source) ->
            failwith
                $"%s{operation}: expected verbatim IntPtr file descriptor, got tagged native-int source %O{source} (fd integers should arrive as plain numeric values across the SystemNative boundary)"
        | other -> failwith $"%s{operation}: expected IntPtr file descriptor, got %O{other}"

    /// Decode an `nuint`-shaped allocation size argument to an `int` byte count.
    /// Returns `ValueNone` for values that a real `malloc`/`calloc` would treat
    /// as unsatisfiable (negative-as-nuint, or larger than the interpreter's
    /// Int32 byte-offset model can represent) so the caller can return a null
    /// pointer and let CoreLib raise a catchable `OutOfMemoryException`.
    /// Synthetic cross-storage subtraction values still abort, because they
    /// represent a guest-visible value the interpreter cannot translate to a
    /// concrete `size_t` rather than a documented allocation-failure mode.
    let private allocationSizeArgument (operation : string) (arg : CliType) : int voption =
        let checkedCount (count : int64) : int voption =
            if count < 0L then
                // Negative `int64` decoded from a `nuint` is a very large
                // unsigned value (e.g. `nuint.MaxValue` round-trips as -1).
                // Real `malloc` returns null for such sizes.
                ValueNone
            elif count > int64 System.Int32.MaxValue then
                // The interpreter's byte-offset model is Int32-bounded; values
                // beyond that cannot be allocated even in principle. Treat as
                // allocation failure rather than aborting so CoreLib's OOM
                // path is reachable.
                ValueNone
            else
                ValueSome (int count)

        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) -> checkedCount count
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: allocation size came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim count)) -> checkedCount count
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: allocation size came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int32 count) -> checkedCount (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr allocation size, got %O{other}"

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            trySystemNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | Some ("SystemNative_LChflagsCanSetHiddenFlag" | "SystemNative_CanGetHiddenFlag"),
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // PawPrint does not model Unix file flags. Report that hidden flags
            // are unsupported so CoreLib follows the portable attribute path.
            pushInt32 0 ctx |> Some
        | Some "SystemNative_GetErrNo",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pushInt32 state.Kernel.LastSystemError ctx |> Some
        | Some "SystemNative_SetErrNo",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "SystemNative_SetErrNo" instruction.Arguments.[0]

            state.MapKernel (fun kernel ->
                { kernel with
                    LastSystemError = error
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Malloc",
          [ ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePointer _) ->
            // C malloc returns an uninitialised block; mirror that here so guest
            // code that reads before writing is caught by the use-of-uninit
            // detector rather than silently observing zeros. Sizes the
            // interpreter cannot satisfy round-trip as a null return so
            // CoreLib's `NativeMemory.Alloc` (and `Marshal.AllocHGlobal`)
            // can raise a catchable `OutOfMemoryException`.
            let ptrSrc, state =
                match allocationSizeArgument "SystemNative_Malloc" instruction.Arguments.[0] with
                | ValueNone -> ManagedPointerSource.Null, state
                | ValueSome size ->
                    IlMachineState.allocateNativeMemory MemoryBlockInitialization.Uninitialized size state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptrSrc) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Calloc",
          [ ConcreteUIntPtr state.ConcreteTypes ; ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePointer _) ->
            // C calloc multiplies count * size and zero-fills the block. If
            // either argument is unrepresentable or the product overflows the
            // interpreter's Int32 byte-offset model, return null so CoreLib
            // raises `OutOfMemoryException` (rather than aborting the host).
            let ptrSrc, state =
                match
                    allocationSizeArgument "SystemNative_Calloc (num)" instruction.Arguments.[0],
                    allocationSizeArgument "SystemNative_Calloc (size)" instruction.Arguments.[1]
                with
                | ValueNone, _
                | _, ValueNone -> ManagedPointerSource.Null, state
                | ValueSome count, ValueSome elementSize ->
                    // Multiply in int64 so we can detect overflow before
                    // truncating to the interpreter's Int32 byte-count model.
                    let total = int64 count * int64 elementSize

                    if total > int64 System.Int32.MaxValue then
                        ManagedPointerSource.Null, state
                    else
                        IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized (int total) state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptrSrc) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Dup",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcreteIntPtr state.ConcreteTypes) ->
            // `dup(2)`: allocate the lowest non-negative fd not in use, sharing
            // the OFD of `oldFd`. On EBADF we return -1 and set errno=EBADF so
            // CoreLib's `Interop.CheckIo` raises an IOException, matching the
            // libc behaviour `Interop.Sys.Dup` is written against. `LastSystemError`
            // holds the raw kernel errno; the BCL converts it to the
            // `Interop.Error` PAL enum via `SystemNative_ConvertErrorPlatformToPal`
            // before `CheckIo` switches on it.
            let oldFd = fdArgument "SystemNative_Dup" instruction.Arguments.[0]

            let resultFd, state =
                match FileDescriptorRegistry.dup oldFd state.Kernel.FileDescriptors with
                | Ok (newFd, registry) ->
                    int64 newFd,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            FileDescriptors = registry
                        }
                    )
                | Error FileDescriptorDupError.BadFd ->
                    -1L,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            LastSystemError = Errno.EBADF
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim resultFd)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Close",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `close(2)`: remove the fd from the per-process table and return 0.
            // On EBADF (fd not currently live) return -1 and set errno=EBADF, so
            // CoreLib's `Interop.CheckIo` raises an IOException, matching the
            // libc behaviour `Interop.Sys.Close` is written against. The native
            // shim silently retries EINTR (`pal_io.c` treats EINTR-on-close as
            // success); PawPrint doesn't model signals, so EINTR is unreachable
            // here. Per Unix convention, errno is left untouched on success.
            let fd = fdArgument "SystemNative_Close" instruction.Arguments.[0]

            let resultCode, state =
                match FileDescriptorRegistry.close fd state.Kernel.FileDescriptors with
                | Ok registry ->
                    0,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            FileDescriptors = registry
                        }
                    )
                | Error FileDescriptorCloseError.BadFd ->
                    -1,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            LastSystemError = Errno.EBADF
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 resultCode) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Write",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_Write(intptr_t fd, const void* buffer, int32_t bufferSize)`
            // delegates to `Common_Write` in `pal_io_common.h`. The C path:
            //   * negative `bufferSize`            -> errno = ERANGE, return -1
            //   * otherwise call real `write(2)`   -> may return short, may EINTR (retried)
            // PawPrint models only the writable standard streams (fds 1 and
            // 2) and never returns short, never returns EINTR, and never
            // blocks: there is no kernel that could push back on our
            // simulated process. A guest depending on EAGAIN / partial
            // writes from a non-blocking socket would need new
            // FileDescriptorRole entries; we'll add those when that need
            // arises rather than guessing at the contract now.
            let operation = "SystemNative_Write"

            let fd = fdArgument operation instruction.Arguments.[0]

            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[2]

            let setErrno (state : IlMachineState) (errno : int) : IlMachineState =
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = errno
                    }
                )

            // Decoding the `buffer` pointer is deferred until we are
            // genuinely about to dereference it. `Common_Write` is
            // documented (in `pal_io_common.h`) to perform no dereference
            // for `bufferSize < 0` (ERANGE bail) or `bufferSize = 0`
            // (no-op on every Unix we model), so a guest calling e.g.
            // `SystemNative_Write((IntPtr)1, (byte*)123, 0)` must succeed
            // on PawPrint as it does on the real CLR — eagerly decoding
            // `buffer` would crash here in `managedPointerOfPointerArgument`
            // for any non-managed pointer literal.
            let readBuffer (buffer : ManagedPointerSource) (state : IlMachineState) : ImmutableArray<byte> =
                // Drain `bufferSize` bytes from `buffer`. Called only after
                // the bufferSize-> 0 and buffer-> non-null checks succeed.
                let byteConcreteType =
                    NativeCall.requiredByteConcreteType operation ctx.BaseClassTypes state

                let builder = ImmutableArray.CreateBuilder<byte> bufferSize

                for i = 0 to bufferSize - 1 do
                    let src =
                        ManagedPointerByteView.addByteOffset ctx.BaseClassTypes state byteConcreteType i buffer

                    let cell =
                        IlMachineState.readManagedByrefBytesAs
                            ctx.BaseClassTypes
                            state
                            src
                            (CliType.Numeric (CliNumericType.UInt8 0uy))

                    match cell with
                    | CliType.Numeric (CliNumericType.UInt8 b) -> builder.Add b
                    | other ->
                        failwith
                            $"%s{operation}: byte read at offset %d{i} returned non-UInt8 cell %O{other} (this is an interpreter bug)"

                builder.MoveToImmutable ()

            let result, effect, state =
                if bufferSize < 0 then
                    // Matches `Common_Write`: refuse the call before any
                    // dereference of `buffer`. CoreLib callers (`Interop.Sys.
                    // Write`) never pass negative sizes, so this is a guest
                    // misuse path; surface it through errno rather than
                    // crashing so the guest's own error reporting runs.
                    -1, StepEffect.NoEffect, setErrno state Errno.ERANGE
                else
                    match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
                    | None ->
                        // Unknown fd: report EBADF the same way `write(2)`
                        // would.
                        -1, StepEffect.NoEffect, setErrno state Errno.EBADF
                    | Some entry ->
                        match entry.Role with
                        | FileDescriptorRole.StandardInput ->
                            // `write(2)` on a read-only fd returns -1 + EBADF
                            // on Linux (the fd's access mode is wrong for the
                            // operation). Real stdin is opened O_RDONLY by
                            // the shell, so this matches what guests would
                            // observe on the host.
                            -1, StepEffect.NoEffect, setErrno state Errno.EBADF
                        | (FileDescriptorRole.StandardOutput | FileDescriptorRole.StandardError) as role ->
                            if bufferSize = 0 then
                                // `write(fd, _, 0)` is a no-op on every Unix
                                // we model — no errno, no buffer
                                // dereference, no observable effect. CoreLib
                                // in principle never calls with
                                // `bufferSize = 0` (it bails in
                                // `Stream.Write`), but honour the C contract
                                // so guests that DllImport directly behave
                                // the same as on the host. Crucially, do
                                // NOT touch `buffer` here: the pointer is
                                // permitted to be any bit pattern (incl.
                                // garbage) because it is not dereferenced.
                                0, StepEffect.NoEffect, state
                            else
                                // Try to decode `buffer` as a managed
                                // pointer. Real `write(2)` returns -1 +
                                // EFAULT for any non-dereferenceable
                                // address (including NULL and unmapped
                                // verbatim bit patterns); collapse both
                                // those cases to EFAULT here rather than
                                // crashing PawPrint, so a direct P/Invoke
                                // that the BCL would never produce
                                // (`Stream.Write` short-circuits null
                                // upstream) observes the same syscall
                                // failure it would on the host.
                                let dereferenceableBuffer : ManagedPointerSource option =
                                    // `ManagedPointerSource.Null` is *also*
                                    // non-dereferenceable — it can arrive
                                    // wrapped in `CliRuntimePointer.Managed`
                                    // when the guest passes e.g.
                                    // `IntPtr.Zero` after a managed
                                    // conversion, in addition to the
                                    // verbatim-0 path. Collapse both kinds
                                    // of null to EFAULT before `readBuffer`
                                    // is asked to project from them.
                                    let classifyManaged (ptr : ManagedPointerSource) =
                                        match ptr with
                                        | ManagedPointerSource.Null -> None
                                        | _ -> Some ptr

                                    match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                                    | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> classifyManaged ptr
                                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) ->
                                        classifyManaged ptr
                                    | CliType.RuntimePointer (CliRuntimePointer.Verbatim _) ->
                                        // 0L is null; non-zero is a raw
                                        // unmapped address. Either way the
                                        // kernel cannot read from it.
                                        None
                                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim _)) -> None
                                    | other ->
                                        failwith
                                            $"%s{operation}: expected buffer to be a managed pointer, raw verbatim address, or null literal, got %O{other} (this is an interpreter bug)"

                                match dereferenceableBuffer with
                                | None ->
                                    // EFAULT: bad address. Real kernels
                                    // perform no I/O on this path.
                                    -1, StepEffect.NoEffect, setErrno state Errno.EFAULT
                                | Some buffer ->
                                    let bytes = readBuffer buffer state

                                    let logEntry =
                                        {
                                            OutputLogEntry.Role = role
                                            OutputLogEntry.Bytes = bytes
                                        }

                                    let state =
                                        state.MapKernel (fun kernel ->
                                            { kernel with
                                                OutputLog = kernel.OutputLog.Add logEntry
                                            }
                                        )

                                    bufferSize, StepEffect.WroteToFd (role, bytes), state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 result) ctx.Thread
            |> NativeHandlerResult.completedWith effect
            |> Some
        | Some "SystemNative_GetNonCryptographicallySecureRandomBytes",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // CoreCLR fills this buffer from the host's non-crypto PRNG
            // (`arc4random_buf` on BSD/macOS, BCrypt on Windows,
            // `/dev/urandom` XOR'd with `lrand48()` on Linux — see
            // minipal/random.c). PawPrint refuses host entropy because the
            // whole runtime is built around bit-for-bit reproducibility,
            // so we substitute a seeded splitmix64 step kept in
            // `EmulatedKernel.NonCryptoRandomState`. That is *strictly*
            // more deterministic than the real CLR (where each Random
            // ctor, Guid.NewGuid, Marvin seed, and HashCode seed is
            // unreproducible) and is what enables time-travel
            // debugging across runs that touch any of those paths.
            //
            // Returning a constant (e.g. all zeros) is not viable: the
            // BCL's Random ctor at Random.Xoshiro{128,256}StarStarImpl
            // explicitly retries until the buffer is non-zero, so a
            // constant-zero substitute hangs at `new Random()`.
            let operation = "SystemNative_GetNonCryptographicallySecureRandomBytes"

            let buffer =
                NativeCall.managedPointerOfPointerArgument operation "buffer" instruction.Arguments.[0]

            let length = NativeCall.int32Argument operation instruction.Arguments.[1]

            if length < 0 then
                // CoreCLR's `pal_random.c` does not validate `bufferLength`;
                // a negative value would underflow `(size_t)bufferLength` in
                // the C call. CoreLib callers never pass negative lengths,
                // so seeing one here means a guest bug we want to surface
                // rather than a silently truncated buffer.
                failwith $"%s{operation}: bufferLength %d{length} is negative"

            let state =
                if length = 0 then
                    // Match the C behaviour of `arc4random_buf(buf, 0)` /
                    // `read(fd, buf, 0)`: no-op, do not even dereference
                    // `buffer` (which CoreLib may pass as a null pointer
                    // for an empty span).
                    state
                else
                    match buffer with
                    | ManagedPointerSource.Null ->
                        failwith
                            $"%s{operation}: refused to fill %d{length} bytes through null buffer pointer (CoreLib should not invoke this entry point with a null destination for a non-zero length)"
                    | _ ->
                        let bytes, newPrngState =
                            NonCryptoRandom.drawBytes length state.Kernel.NonCryptoRandomState

                        let byteConcreteType =
                            NativeCall.requiredByteConcreteType operation ctx.BaseClassTypes state

                        let mutable state = state

                        for i = 0 to length - 1 do
                            let dest =
                                ManagedPointerByteView.addByteOffset ctx.BaseClassTypes state byteConcreteType i buffer

                            state <-
                                IlMachineState.writeManagedByrefBytesOrTypedCell
                                    ctx.BaseClassTypes
                                    state
                                    dest
                                    (CliType.Numeric (CliNumericType.UInt8 bytes.[i]))

                        state.MapKernel (fun kernel ->
                            { kernel with
                                NonCryptoRandomState = newPrngState
                            }
                        )

            NativeHandlerResult.completed state |> Some
        | Some "SystemNative_Free", [ ConcretePointer _ ], MethodReturnType.Void ->
            let ptr =
                NativeCall.managedPointerOfPointerArgument "SystemNative_Free" "ptr" instruction.Arguments.[0]

            // C `free(x)` is undefined unless `x` is exactly a pointer returned
            // by `malloc`/`calloc`/`realloc` (or null). Interior pointers like
            // `base + 4` must be rejected — silently freeing the whole block
            // would mask guest memory-corruption bugs.
            let rec projectionByteOffset (acc : int) (ps : ByrefProjection list) : Result<int, ByrefProjection> =
                match ps with
                | [] -> Ok acc
                | ByrefProjection.ReinterpretAs _ :: rest -> projectionByteOffset acc rest
                | ByrefProjection.ByteOffset n :: rest -> projectionByteOffset (acc + n) rest
                | (ByrefProjection.Field _ as field) :: _ -> Error field

            let state =
                match ptr with
                // C `free(NULL)` is documented as a no-op. CoreLib's
                // NativeMemory.Free already filters null before reaching the
                // P/Invoke, but Marshal.FreeHGlobal does not, so honour the
                // C semantics here too.
                | ManagedPointerSource.Null -> state
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
                    match projectionByteOffset rootByteOffset projs with
                    | Ok 0 -> IlMachineState.freeNativeMemory block state
                    | Ok offset ->
                        failwith
                            $"SystemNative_Free: refusing to free interior native-heap pointer at byte offset %d{offset} into %O{block} (only the allocation base address returned by SystemNative_Malloc/Calloc may be freed)"
                    | Error field ->
                        failwith
                            $"SystemNative_Free: refusing to free native-heap pointer with non-byte projection %O{field} into %O{block} (only the allocation base address may be freed)"
                | other ->
                    failwith
                        $"SystemNative_Free: expected null or native-heap pointer, got %O{other} (only pointers from SystemNative_Malloc/Calloc may be freed here)"

            NativeHandlerResult.completed state |> Some
        | Some "SystemNative_InitializeTerminalAndSignalHandling",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // The real native side configures the controlling terminal, sets
            // up a self-pipe, and installs a dedicated signal-dispatch worker
            // thread (via `pthread_create(..., SignalHandlerLoop, ...)`).
            // PawPrint has no terminal but mirrors the dedicated-thread shape:
            // on first init we allocate a fresh `ThreadId` for the
            // signal dispatcher and park it (status `ThreadStatus.Parked`),
            // recording its id in `Signals.Init`. A future slice will wake
            // that thread out of `Parked` to actually invoke handlers.
            // The call is idempotent: a second invocation preserves the
            // already-allocated dispatcher (BCL initializers may run more
            // than once across the surface). Real native code returns 0 on
            // setup failure (e.g. EBADF from tcgetattr on a headless
            // process); PawPrint always reports success because there is no
            // underlying syscall that could fail.
            let state =
                if SignalState.isInitialized state.Kernel.Signals then
                    state
                else
                    let state, dispatcher = IlMachineState.allocateParkedThread state

                    state.MapKernel (fun kernel ->
                        { kernel with
                            Signals = SignalState.markInitialized dispatcher kernel.Signals
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetPlatformSignalNumber",
          [ PosixSignalParam state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Real native code keys off the host's <signal.h>; PawPrint always
            // uses the Linux signo table (see `Signal.toLinuxSigno`) so a
            // simulation trace is byte-for-byte identical across host OSes.
            // Unmodelled cross-platform negatives and out-of-range positives
            // (outside `(0, Signal.linuxSignalMax]`) both map to 0 — which
            // `PosixSignalRegistration.Register` promotes to an
            // `ArgumentOutOfRangeException`, matching the real native semantics
            // where unknown signals fall through to the trailing `return 0;`
            // in `SystemNative_GetPlatformSignalNumber`. Positive signos within
            // range that PawPrint doesn't name still round-trip via
            // `Signal.Other`, so a guest that casts `(PosixSignal)4` for SIGILL
            // gets `4` back, matching the C-side check
            // `if (signal > 0 && signal <= GetSignalMax()) return signal;`.
            let raw =
                NativeCall.int32Argument "SystemNative_GetPlatformSignalNumber" instruction.Arguments.[0]

            let signo =
                match Signal.ofPosixSignalEnum raw with
                | ValueSome signal -> Signal.toLinuxSigno signal
                | ValueNone -> 0

            pushInt32 signo ctx |> Some
        | Some "SystemNative_EnablePosixSignalHandling",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Flips the per-signo "managed code wants this" bit. The handler
            // dictionary itself lives on the simulated managed heap (maintained
            // by `PosixSignalRegistration`'s `s_registrations`); this arm only
            // touches the kernel-side enable set. By contract, the BCL only
            // calls this with signos that `SystemNative_GetPlatformSignalNumber`
            // returned non-zero for, so a signo arriving here must lie within
            // `(0, Signal.linuxSignalMax]` (modelled signals get a named case;
            // unmodelled-but-valid signos round-trip via `Signal.Other` so the
            // kernel still tracks the enable bit) — anything else indicates a
            // guest bypassing the standard registration path with a hand-rolled
            // P/Invoke, and we fail loudly rather than silently dropping the
            // request. Real native code asserts `signalCode > 0 && <= GetSignalMax()`.
            let operation = "SystemNative_EnablePosixSignalHandling"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]

            match Signal.ofPlatformSigno signo with
            | ValueNone ->
                failwith
                    $"%s{operation}: refusing to enable out-of-range signo %d{signo} (signos arriving here must lie within (0, Signal.linuxSignalMax]; this looks like a guest bypassing SystemNative_GetPlatformSignalNumber)"
            | ValueSome signal when Signal.isUncatchable signal ->
                // Real native code calls `sigaction(signo, ...)` which the
                // kernel rejects with `EINVAL` for `SIGKILL` (9) and
                // `SIGSTOP` (19). `InstallSignalHandler` returns false and
                // `SystemNative_EnablePosixSignalHandling` propagates 0 with
                // `errno = EINVAL`, which `PosixSignalRegistration.Create`
                // then reads via `Marshal.GetLastSystemError` to throw. We
                // mirror exactly that: leave the enable bit clear, set
                // errno, push 0. Don't fail loud here — uncatchable signals
                // are a documented BCL-observable failure mode, not a
                // simulator bug.
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = Errno.EINVAL
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | ValueSome signal ->
                state.MapKernel (fun kernel ->
                    { kernel with
                        Signals = SignalState.enable signal kernel.Signals
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
        | Some "SystemNative_SetPosixSignalHandler", [ ConcreteFunctionPointer _ ], MethodReturnType.Void ->
            // The BCL calls this exactly once from
            // `PosixSignalRegistration.Initialize` with `&OnPosixSignal`,
            // a function-pointer-typed `delegate* unmanaged<int, PosixSignal, int>`.
            // Real native code stashes the raw pointer into a global
            // (`g_posixSignalHandler`) and the signal-handling thread later
            // invokes it after a signal is queued. PawPrint just records the
            // managed identity of the target method on `SignalState` — the
            // forthcoming signal-delivery slice reads it back at dispatch
            // time. We refuse anything other than a real
            // `NativeIntSource.FunctionPointer`: any other tag means the
            // value didn't come from `Ldftn` on a managed method, so we
            // have no callable identity to record and silently dropping it
            // would let the BCL register handlers that the simulator can
            // never invoke.
            let operation = "SystemNative_SetPosixSignalHandler"

            let mi =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[0] with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer mi)) -> mi
                | other ->
                    failwith
                        $"%s{operation}: expected FunctionPointer argument (from Ldftn on the managed signal callback), got %O{other}"

            state.MapKernel (fun kernel ->
                { kernel with
                    Signals = SignalState.setHandler (SignalHandler.ofMethodInfo mi) kernel.Signals
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_DisablePosixSignalHandling",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // Mirror image of `SystemNative_EnablePosixSignalHandling`: clear
            // the per-signo enable bit. Real native code also conditionally
            // restores the prior `sigaction` disposition; PawPrint has no
            // installed disposition to restore, so the only kernel-visible
            // effect is the cleared bit. Same round-trip contract as enable:
            // an out-of-range signo arriving here (outside
            // `(0, Signal.linuxSignalMax]`) is a guest bypassing
            // `GetPlatformSignalNumber`, and we surface the divergence.
            let operation = "SystemNative_DisablePosixSignalHandling"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]

            match Signal.ofPlatformSigno signo with
            | ValueNone ->
                failwith
                    $"%s{operation}: refusing to disable out-of-range signo %d{signo} (signos arriving here must lie within (0, Signal.linuxSignalMax]; this looks like a guest bypassing SystemNative_GetPlatformSignalNumber)"
            | ValueSome signal ->
                state.MapKernel (fun kernel ->
                    { kernel with
                        Signals = SignalState.disable signal kernel.Signals
                    }
                )
                |> NativeHandlerResult.completed
                |> Some
        | _ -> None
