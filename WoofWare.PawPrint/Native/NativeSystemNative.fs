namespace WoofWare.PawPrint

open System
open System.Buffers.Binary
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeSystemNative =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.TryNativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    /// The OS thread id of the thread currently executing the native call.
    let private osThreadIdOf (operation : string) (ctx : NativeCallContext) : OsThreadId =
        match Map.tryFind ctx.Thread ctx.State.ThreadState with
        | Some threadState -> threadState.OsThreadId
        | None ->
            failwith
                $"%s{operation}: thread %O{ctx.Thread} is executing but has no ThreadState (every running thread is created through IlMachineState, which assigns an OsThreadId)"

    let private pushInt32 (value : int) (ctx : NativeCallContext) : NativeHandlerResult =
        ctx.State
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim value)) ctx.Thread
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

    /// Matches the type `Interop.Sys.ConvertErrorPlatformToPal` *returns*.
    ///
    /// CoreLib declares it as the PAL `Interop.Error` enum, which lives nested
    /// inside the `Interop` static class in the *global* namespace — so it
    /// concretises with an empty namespace and the bare name "Error" (nesting is
    /// not reflected in `ConcreteType`'s name). A guest hand-rolling the
    /// P/Invoke may instead declare the return as a plain `int`, which is the
    /// same thing at the ABI, so both are accepted.
    ///
    /// Deliberately not assembly-qualified, unlike `PosixSignalParam` above.
    /// `Interop.Error` is `internal` to CoreLib, so a guest cannot name *that*
    /// type; requiring it would leave this arm reachable only by real BCL code
    /// and hence untestable. The entry-point name already identifies the call
    /// uniquely, so the assembly adds no discrimination here — it would only
    /// cost the ability to test the arm.
    let private (|PalErrorReturn|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcretePrimitive concreteTypes PrimitiveType.Int32 -> Some ()
        | ConcreteType concreteTypes (_, "", "Error", generics) when generics.IsEmpty -> Some ()
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

    /// Whether a pointer argument is the null pointer — a total test, unlike
    /// `NativeCall.managedPointerOfPointerArgument`, which insists the non-null
    /// case resolve to storage PawPrint can address.
    ///
    /// Entry points whose C counterpart returns an error *without dereferencing
    /// the buffer* need exactly this: a guest may legally hand such a call an
    /// unresolvable bit pattern (`(byte*)123`), the real shim never touches it,
    /// and so PawPrint must decide the error before it tries to resolve the
    /// pointer to a cell.
    let private isNullPointerArgument (arg : CliType) : bool =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> true
        | _ -> false

    /// Classify a buffer-pointer argument as storage PawPrint can address, or
    /// as an address no kernel could have transferred bytes through.
    ///
    /// `None` covers both the null pointer and a raw unmapped bit pattern such
    /// as `(byte*)123`. Real `write(2)` and `getcwd(3)` alike return `EFAULT`
    /// for either, having performed no I/O, so an entry point that is about to
    /// dereference its buffer collapses both to that errno rather than aborting
    /// the interpreter — see `UnixError.EFAULT`. A guest reaches this only by
    /// hand-rolling a P/Invoke; the BCL's own wrappers null-check upstream.
    ///
    /// Note this is a question about *dereferenceability*, so callers that need
    /// to tell null from unmapped (because their C counterpart treats the two
    /// differently) must ask `isNullPointerArgument` first.
    let private dereferenceablePointerArgument
        (operation : string)
        (argName : string)
        (arg : CliType)
        : ManagedPointerSource option
        =
        // `ManagedPointerSource.Null` is non-dereferenceable too: it can arrive
        // wrapped in `CliRuntimePointer.Managed` when the guest passes e.g.
        // `IntPtr.Zero` after a managed conversion, as well as via the
        // verbatim-0 path.
        let classifyManaged (ptr : ManagedPointerSource) : ManagedPointerSource option =
            match ptr with
            | ManagedPointerSource.Null -> None
            | _ -> Some ptr

        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> classifyManaged ptr
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) -> classifyManaged ptr
        // 0L is null; non-zero is a raw unmapped address. Either way the kernel
        // cannot transfer bytes through it.
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim _) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim _)) -> None
        | other ->
            failwith
                $"%s{operation}: expected %s{argName} to be a managed pointer, raw verbatim address, or null literal, got %O{other} (this is an interpreter bug)"

    /// Write `bytes` through a caller-supplied `byte*`, one cell at a time.
    /// The simulated address space is a graph of typed cells rather than a flat
    /// byte array, so "memcpy into the caller's buffer" is necessarily this
    /// per-byte walk; `ManagedPointerByteView.addByteOffset` is what resolves
    /// each offset back to a cell, whatever storage the pointer actually names
    /// (a `localloc` block, a pinned `byte[]`, native heap).
    ///
    /// `buffer` must not be null and must have room for every byte: both are
    /// the caller's business, because what a too-small or null buffer *means*
    /// differs per entry point (ERANGE here, EFAULT elsewhere).
    let private writeBytesThrough
        (ctx : NativeCallContext)
        (operation : string)
        (buffer : ManagedPointerSource)
        (bytes : ImmutableArray<byte>)
        (state : IlMachineState)
        : IlMachineState
        =
        let byteConcreteType =
            NativeCall.requiredByteConcreteType operation ctx.BaseClassTypes state

        let mutable state = state

        for i = 0 to bytes.Length - 1 do
            let dest = ManagedPointerByteView.addByteOffset state byteConcreteType i buffer

            state <-
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    ctx.BaseClassTypes
                    state
                    dest
                    (CliType.Numeric (CliNumericType.UInt8 bytes.[i]))

        state

    /// Turn the NUL-terminated bytes a guest passed as a pathname into a
    /// `UnixPath`, applying the length rule a kernel applies at *its* boundary.
    ///
    /// Deliberately takes bytes rather than machine state, so the boundary — the
    /// one part of the length rules that the resolver can never see — is
    /// testable without a heap. `readGuestPathBytes` is the half that needs a
    /// machine.
    ///
    /// The **order** of the three stages is load-bearing, not incidental:
    ///
    ///  1. **Length first.** `PATH_MAX` is enforced by `getname()`/`copyinstr`
    ///     when the kernel copies the string in, before anything looks at what
    ///     it says. So an over-long path that also contains an invalid UTF-8
    ///     byte must be ENAMETOOLONG — if the strict decode ran first it would
    ///     abort the interpreter over a path a real kernel rejects cheaply.
    ///  2. **Strict decode.** Not `readNullTerminatedUtf8`, which substitutes
    ///     U+FFFD: a kernel looks up raw bytes, so byte 0xFF names a file no
    ///     valid UTF-8 name can, and decoding leniently would silently resolve a
    ///     *different* inode — a seeded file literally called "&#65533;". PawPrint models
    ///     a filename as a .NET string and cannot represent such a path at all;
    ///     it should say so rather than answer about the wrong file.
    ///  3. **Parse.**
    ///
    /// Note the limit counts the NUL, and `readNullTerminatedBytes` has already
    /// dropped it, so the comparison is against `pathMaxBytes - 1`. Measured:
    /// 1023 bytes resolves on macOS and 1024 does not.
    let internal parseGuestPathBytes
        (operation : string)
        (limits : PathLimits)
        (bytes : byte[])
        : Result<UnixPath, UnixError>
        =
        if bytes.Length > PathLimits.pathMaxBytes limits - 1 then
            Error UnixError.ENAMETOOLONG
        else

        let decoded =
            try
                Some (Text.UTF8Encoding(false, true).GetString bytes)
            with :? Text.DecoderFallbackException ->
                None

        match decoded with
        | None ->
            let rendered = bytes |> Array.map (sprintf "%02X") |> String.concat " "

            failwith
                $"%s{operation}: the guest passed a path that is not valid UTF-8 (bytes: %s{rendered}). A Unix kernel looks up the raw bytes, but PawPrint models a filename as a .NET string, so this path has no representation in the emulated filesystem; decoding it leniently would silently resolve a different file. CoreLib never produces such a path — it encodes from a string — so this can only come from a hand-rolled P/Invoke."
        | Some decoded ->

        match UnixPath.parse decoded with
        | Error error ->
            // Unreachable from a guest today: the only rejections are a null
            // candidate (impossible — we have just decoded a string) and text
            // that cannot survive the `char*` boundary, which a string decoded
            // *from* that boundary cannot contain.
            failwith
                $"%s{operation}: the guest's path did not survive parsing: %s{UnixPath.describe error}. This is an interpreter bug: the value was decoded from a NUL-terminated byte string, so it cannot contain an embedded NUL and cannot be null."
        | Ok path -> Ok path

    /// The inode a path names, or the errno the lookup owes the guest.
    ///
    /// A relative path resolves against the simulated process's current
    /// directory, which — unlike a real kernel's, which is an inode the process
    /// holds open — is configuration, and so might name nothing in the seeded
    /// filesystem. That is a host mistake rather than anything a guest did, and
    /// it has no honest errno: ENOENT would blame the guest's path, and any
    /// other answer would invent a directory. Crash, naming both knobs.
    ///
    /// Note the common case never reaches it: CoreLib `Path.GetFullPath`s
    /// before every `Stat`/`LStat`, so the path arriving here is normally
    /// already absolute and the current directory is not consulted at all.
    ///
    /// **Known limitation: no length re-check when a symlink is spliced.**
    /// Darwin re-checks the total length each time it expands a symbolic link
    /// (XNU `namei`: `linklen + ni_pathlen > MAXPATHLEN`), so a *short* argument
    /// can still be ENAMETOOLONG through a long target — measured, a 206-byte
    /// argument through an 885-byte target. Linux does not: measured, a
    /// 3842-byte target with an 806-byte remainder resolves, 4648 spliced.
    /// PawPrint implements neither, so it answers as Linux does on both.
    ///
    /// Reproducing Darwin's rule needs the byte length of the *unconsumed*
    /// remainder, which this walk no longer has: it holds a `PathComponent
    /// list`. Threading a length through it is its own change, and the trigger
    /// has to be designed against measurement rather than arithmetic — a first
    /// attempt reasoned that collapsed `//` runs would make a rendered length an
    /// unsound under-estimate, and probing showed XNU consumes such runs before
    /// splicing, so the sound-looking argument was simply wrong about the
    /// kernel.
    ///
    /// Only a hand-written seed reaches it: symlinks enter the filesystem only
    /// through seeds, and the differential oracle's validator permits only
    /// single-component targets.
    let private resolveGuestPath
        (operation : string)
        (policy : SymlinkPolicy)
        (kernel : EmulatedKernel)
        (path : UnixPath)
        : Result<InodeNumber, UnixError>
        =
        let vfs = kernel.FileSystem
        let root = VirtualFileSystem.root vfs
        let limits = SimulatedUnixPlatform.pathLimits kernel.UnixPlatform

        let startDirectory =
            if UnixPath.isRooted path then
                root
            else

            let cwd = UnixPath.ofAbsolute kernel.CurrentDirectory

            match VirtualFileSystem.resolveExisting limits root SymlinkPolicy.Follow cwd vfs with
            | Ok inode -> inode
            | Error UnixError.ENAMETOOLONG ->
                // Distinguished because the remedy is different, and the
                // message below would send the reader looking for a missing
                // directory that is in fact present.
                failwith
                    $"%s{operation}: the configured current directory \"%s{AbsoluteUnixPath.toString kernel.CurrentDirectory}\" contains a component longer than %O{SimulatedUnixPlatform.flavour kernel.UnixPlatform}'s NAME_MAX, so no process could have been started in it. Shorten KernelConfig.CurrentDirectory."
            | Error error ->
                failwith
                    $"%s{operation}: the guest passed the relative path \"%s{UnixPath.toString path}\", but the configured current directory \"%s{AbsoluteUnixPath.toString kernel.CurrentDirectory}\" does not resolve in the seeded filesystem (%O{error}). A process cannot be started in a directory that does not exist; make KernelConfig.FileSystem contain KernelConfig.CurrentDirectory."

        VirtualFileSystem.resolveExisting limits startDirectory policy path vfs

    /// `sizeof(FileStatus)`: four 32-bit fields, then twelve 64-bit ones, then
    /// a trailing `uint32_t`, rounded up to the struct's 8-byte alignment.
    let private fileStatusSize : int = 120

    /// How much of `FileStatus` is fields rather than trailing padding. The C
    /// shim writes exactly this much — `ConvertFileStatus` assigns fields and
    /// never touches the four bytes after `UserFlags` — so neither does
    /// PawPrint.
    let private fileStatusDataSize : int = 116

    /// Fill in a guest's `Interop.Sys.FileStatus` from an inode, and hand back
    /// the zero that says the call succeeded.
    ///
    /// Shared by `SystemNative_Stat`/`LStat`, which reach the inode from a
    /// path, and by `SystemNative_FStat`, which reaches it from a file
    /// descriptor. One encoder rather than two: the 120-byte layout *is* the
    /// contract with the guest, and two copies of it could disagree — a
    /// disagreement no differential test could catch, since the real runtime
    /// would agree with itself either way.
    ///
    /// The output struct is written as a **byte image at ABI offsets**, not by
    /// setting fields on the pointee type by name. That is what the C does — it
    /// receives a `FileStatus*` and writes through it, and the guest's own
    /// declaration is merely its view of the same bytes. Deriving the offsets
    /// from the pointee instead would honour whatever names and order *that*
    /// declaration happened to use, so a guest whose layout-identical struct
    /// named its fields differently would abort the interpreter, and one that
    /// reordered them would be handed a struct no real kernel would have
    /// written. The pointee handle is still used, for the one thing it is
    /// authoritative about: how much room the caller actually provided.
    let private writeFileStatus
        (ctx : NativeCallContext)
        (operation : string)
        (fileStatusHandle : ConcreteTypeHandle)
        (inode : InodeNumber)
        (entry : Inode)
        (output : ManagedPointerSource)
        (state : IlMachineState)
        : NativeHandlerResult option
        =
        let permissions =
            match VirtualFileSystem.permissions entry with
            | InodePermissions.Stored bits -> bits
            | InodePermissions.PlatformSymlinkDefault ->
                SimulatedUnixPlatform.symlinkPermissions state.Kernel.UnixPlatform

        let size =
            match entry.Content with
            | InodeContent.RegularFile (contents, _) -> int64 contents.Length
            // `readlink` reports the target's byte length as the link's size,
            // and a guest can see it through `FileInfo.Length`.
            | InodeContent.Symlink target -> int64 (SymlinkTarget.toUtf8 target).Length
            // Invented, and the only field here that is: PawPrint has no block
            // allocator, so a directory has no natural size. 4096 is what ext4
            // reports for a small directory, i.e. the least surprising answer a
            // guest could read.
            | InodeContent.Directory _ -> 4096L

        let reportsBirthTime =
            SimulatedUnixPlatform.reportsBirthTime state.Kernel.UnixPlatform

        // How much room the guest gave us. Derived from the pointee handle
        // rather than assumed, because a too-small buffer would otherwise be
        // written past: `MemoryBlock`'s own bounds check bounds the whole
        // backing block, not this struct's extent, so an overflow could land in
        // adjacent memory rather than failing.
        let buffer, state =
            IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes fileStatusHandle

        if CliType.sizeOf buffer <> fileStatusSize then
            failwith
                $"%s{operation}: the output struct is %d{CliType.sizeOf buffer} bytes, but `FileStatus` is %d{fileStatusSize}. Either the guest hand-rolled this P/Invoke with a struct that is not layout-identical to `Interop.Sys.FileStatus`, or upstream has changed the layout — check `ConvertFileStatus` in pal_io.c against `Interop.Stat.cs`."

        let times = entry.Times

        let birthTime =
            // Zeroed rather than reported when the platform has no
            // `st_birthtime`, exactly as `pal_io.c` does under `#else`. The
            // inode knows its birth either way; this governs only what the
            // guest is told.
            if reportsBirthTime then
                times.Birth
            else
                UnixTimestamp.epoch

        let image : byte array = Array.zeroCreate fileStatusDataSize

        let putInt32 (offset : int) (value : int32) : unit =
            BinaryPrimitives.WriteInt32LittleEndian (Span<byte> (image, offset, 4), value)

        let putUInt32 (offset : int) (value : uint32) : unit =
            BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> (image, offset, 4), value)

        let putInt64 (offset : int) (value : int64) : unit =
            BinaryPrimitives.WriteInt64LittleEndian (Span<byte> (image, offset, 8), value)

        let putTime (offset : int) (timestamp : UnixTimestamp) : unit =
            putInt64 offset (UnixTimestamp.seconds timestamp)
            putInt64 (offset + 8) (int64 (UnixTimestamp.nanoseconds timestamp))

        // `FileStatusFlags.HasBirthTime = 1`; nothing else is defined.
        putInt32 0 (if reportsBirthTime then 1 else 0)

        putInt32
            4
            (VirtualFileSystem.fileTypeBits entry.Content
             ||| PermissionBits.toInt permissions)

        putUInt32 8 state.Kernel.UserId
        putUInt32 12 state.Kernel.GroupId
        putInt64 16 size
        putTime 24 times.Access
        putTime 40 times.Modification
        putTime 56 times.StatusChange
        putTime 72 birthTime
        putInt64 88 EmulatedKernel.simulatedDeviceId
        // Non-zero only for device nodes, which `InodeContent` cannot represent.
        putInt64 96 0L

        putInt64
            104
            (match inode with
             | InodeNumber value -> value)

        // macOS's `UF_HIDDEN`, gated on `HAVE_STAT_FLAGS`. PawPrint models no
        // BSD file flags, and nothing in the emulated filesystem is hidden, so
        // zero is the honest answer on either platform.
        putUInt32 112 0u

        writeBytesThrough ctx operation output (ImmutableArray.CreateRange image) state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
        |> NativeHandlerResult.completed
        |> Some

    /// Shared body of `SystemNative_Stat` and `SystemNative_LStat`, which
    /// differ only in whether a symbolic link in the final position is
    /// followed.
    ///
    /// The output struct is written as a **byte image at ABI offsets**, not by
    /// setting fields on the pointee type by name. That is what the C does — it
    /// receives a `FileStatus*` and writes through it, and the guest's own
    /// declaration is merely its view of the same bytes. Deriving the offsets
    /// from the pointee instead would honour whatever names and order *that*
    /// declaration happened to use, so a guest whose layout-identical struct
    /// named its fields differently would abort the interpreter, and one that
    /// reordered them would be handed a struct no real kernel would have
    /// written. The pointee handle is still used, for the one thing it is
    /// authoritative about: how much room the caller actually provided.
    ///
    /// `ConvertFileStatus` in `pal_io.c` writes the output struct only when the
    /// underlying `stat_`/`lstat_` succeeded, so every failure path here must
    /// leave the guest's buffer untouched — a caller that checked the return
    /// value would otherwise read fields nobody wrote.
    let private statLike
        (ctx : NativeCallContext)
        (operation : string)
        (policy : SymlinkPolicy)
        (fileStatusHandle : ConcreteTypeHandle)
        (state : IlMachineState)
        : NativeHandlerResult option
        =
        let instruction = ctx.Instruction

        let fail (error : UnixError) : NativeHandlerResult option =
            // `toRawErrnoUnder` rather than `toRawErrno`, because a resolution
            // can fail with ELOOP — a symlink cycle needs no more than
            // `l -> l` — and that error has no platform-independent number.
            // The emulated kernel knows which Unix it is impersonating, so it
            // can answer where the bare conversion refuses to.
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            state.MapKernel (fun kernel ->
                { kernel with
                    LastSystemError = UnixError.toRawErrnoUnder numbering error
                }
            )
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        // Both pointers are dereferenced by the C on the success path, and
        // neither is inspected before the lookup — but the path is read first,
        // so an unmapped `path` is EFAULT whatever the output pointer is.
        match dereferenceablePointerArgument operation "path" instruction.Arguments.[0] with
        | None -> fail UnixError.EFAULT
        | Some pathPtr ->

        let limits = SimulatedUnixPlatform.pathLimits state.Kernel.UnixPlatform

        // Bounded by PATH_MAX, because that is where a real kernel stops
        // looking: an unterminated buffer must be ENAMETOOLONG rather than a
        // scan that walks off the end of the guest's allocation. On overrun this
        // hands back exactly `pathMaxBytes` bytes, which `parseGuestPathBytes`
        // then refuses by its ordinary length rule — so "too long" is still
        // decided in exactly one place.
        let bytes =
            NativeCall.readNullTerminatedBytesWithin
                operation
                ctx.BaseClassTypes
                state
                pathPtr
                (PathLimits.pathMaxBytes limits)

        match parseGuestPathBytes operation limits bytes with
        | Error error -> fail error
        | Ok path ->

        match resolveGuestPath operation policy state.Kernel path with
        | Error error -> fail error
        | Ok inode ->

        let vfs = state.Kernel.FileSystem

        let entry =
            match VirtualFileSystem.tryGet inode vfs with
            | Some entry -> entry
            | None ->
                failwith
                    $"%s{operation}: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

        // The output pointer is only decoded here, on the path that actually
        // writes through it.
        match dereferenceablePointerArgument operation "output" instruction.Arguments.[1] with
        | None -> fail UnixError.EFAULT
        | Some output ->

        writeFileStatus ctx operation fileStatusHandle inode entry output state

    /// Shared body of `SystemNative_GetNonCryptographicallySecureRandomBytes`
    /// and `SystemNative_GetCryptographicallySecureRandomBytes`. The two entry
    /// points declare the identical `(byte* buffer, int32 bufferLength)`
    /// argument list and differ only in which host entropy source backs them
    /// (and, here, in which kernel PRNG stream they advance), so the decode,
    /// validation, and buffer-fill are factored into one place.
    ///
    /// CoreCLR fills these buffers from the host: `arc4random_buf` on
    /// BSD/macOS, BCrypt/`BCryptGenRandom` on Windows, `/dev/urandom` (XOR'd
    /// with `lrand48()` for the non-crypto variant) on Linux — see
    /// minipal/random.c. PawPrint refuses host entropy because the whole
    /// runtime is built around bit-for-bit reproducibility, so we substitute a
    /// seeded splitmix64 step. That is *strictly* more deterministic than the
    /// real CLR (where each Random ctor, Guid.NewGuid, Marvin seed, and
    /// HashCode seed is unreproducible) and is what enables time-travel
    /// debugging across runs that touch any of those paths. It also means the
    /// "cryptographically secure" entry point is nothing of the sort under
    /// PawPrint; no deterministic interpreter can honour that contract, and a
    /// guest whose security depends on it must not run here.
    ///
    /// Returning a constant (e.g. all zeros) is not viable: the BCL's Random
    /// ctor at Random.Xoshiro{128,256}StarStarImpl explicitly retries until
    /// the buffer is non-zero, so a constant-zero substitute hangs at
    /// `new Random()`.
    ///
    /// Returns the updated machine state and the advanced PRNG state; the
    /// caller writes the latter back to whichever kernel field it owns.
    let private drawRandomBytesInto
        (ctx : NativeCallContext)
        (operation : string)
        (prngState : uint64)
        : IlMachineState * uint64
        =
        let state = ctx.State

        let buffer =
            NativeCall.managedPointerOfPointerArgument operation "buffer" ctx.Instruction.Arguments.[0]

        let length = NativeCall.int32Argument operation ctx.Instruction.Arguments.[1]

        if length < 0 then
            // CoreCLR's `pal_random.c` does not validate `bufferLength`;
            // a negative value would underflow `(size_t)bufferLength` in
            // the C call. CoreLib callers never pass negative lengths,
            // so seeing one here means a guest bug we want to surface
            // rather than a silently truncated buffer.
            failwith $"%s{operation}: bufferLength %d{length} is negative"
        elif length = 0 then
            // Match the C behaviour of `arc4random_buf(buf, 0)` /
            // `read(fd, buf, 0)`: no-op, do not even dereference
            // `buffer` (which CoreLib may pass as a null pointer
            // for an empty span), and do not advance the PRNG.
            state, prngState
        else
            match buffer with
            | ManagedPointerSource.Null ->
                failwith
                    $"%s{operation}: refused to fill %d{length} bytes through null buffer pointer (CoreLib should not invoke this entry point with a null destination for a non-zero length)"
            | _ ->
                let bytes, newPrngState = NonCryptoRandom.drawBytes length prngState

                writeBytesThrough ctx operation buffer (ImmutableArray.CreateRange bytes) state, newPrngState

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
        | Some "SystemNative_ConvertErrorPlatformToPal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_ConvertErrorPlatformToPal(int32_t platformErrno)`
            // (pal_errno.c:6) is a one-line wrapper around
            // `ConvertErrorPlatformToPal` in `pal_error_common.h:146`, a pure
            // switch from the host's raw `<errno.h>` number to the
            // platform-independent `Interop.Error` value CoreLib switches on.
            //
            // Every BCL failure path goes through here: `Interop.Sys.
            // GetLastErrorInfo()` builds `ErrorInfo(Marshal.GetLastPInvokeError())`,
            // whose constructor calls this. So this is the single point at which
            // PawPrint's raw errno vocabulary becomes something the BCL can
            // branch on.
            //
            // `UnixError.palOfRawErrno` refuses errnos whose meaning is
            // platform-dependent rather than answering `ENONSTANDARD` as the C
            // does; see its doc comment for why that divergence is the honest
            // one. In practice the only raw values reaching this are ones
            // PawPrint itself stored via `UnixError.toRawErrno` (which admits
            // only portable errnos) or ones a guest planted with
            // `Marshal.SetLastSystemError`.
            let raw =
                NativeCall.int32Argument "SystemNative_ConvertErrorPlatformToPal" instruction.Arguments.[0]

            // Under the numbering the kernel actually reports, not the portable
            // subset: the errnos this handler is handed are the ones PawPrint's
            // own syscalls stored, so a `stat` that failed with ELOOP puts raw
            // 40 here on Linux and CoreLib must get `Interop.Error.ELOOP` back.
            // Converting it without the platform is exactly the round trip the
            // bare `palOfRawErrno` refuses to complete.
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

            pushInt32 (UnixError.palOfRawErrnoUnder numbering raw) ctx |> Some
        | Some "SystemNative_GetCpuUtilization",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Double) ->
            // `double SystemNative_GetCpuUtilization(ProcessCpuInformation* previous)`
            // (declared in `src/native/libs/System.Native/pal_time.h`, defined
            // in `pal_time.c` -- note *not* `pal_process.c`, where you might
            // reasonably look first. That tree is outside our sparse
            // dotnet/runtime checkout, so it was checked by fetching the file
            // at the pinned commit rather than from `$DOTNET_RUNTIME_SRC`.)
            // It is a stateful sampler: it reads the process's cumulative user/kernel CPU
            // time, diffs it against whatever `*previous` held from the
            // caller's last call, overwrites `*previous` with the fresh
            // sample, and returns the percentage of wall-clock time since
            // that last call spent running this process. CoreLib reaches it
            // from `PortableThreadPool.Unix`'s `CpuUtilizationReader`
            // (divides by `Environment.ProcessorCount` to feed the thread
            // pool's hill-climbing/starvation heuristics),
            // `RuntimeEventSourceHelper.Unix` (same, for an `EventCounter`
            // trace event), and -- via the struct's *cumulative* fields
            // rather than the return value -- the public `Environment.CpuUsage`
            // and `AppDomain.MonitoringTotalProcessorTime` properties.
            //
            // The generated P/Invoke stub takes the struct by raw pointer
            // (matched loosely here as `ConcretePointer _`, confirmed via
            // IlDump against real CoreLib metadata to be `ptr[...]`, not a
            // managed byref: `Interop.Sys.GetCpuUtilization` is
            // `[LibraryImport]`-generated and pins the caller's `ref` before
            // calling through). `ProcessCpuInformation` itself -- also
            // confirmed via metadata, since managed code never reads it
            // directly -- is `[StructLayout(LayoutKind.Sequential)]` with
            // three `ulong` fields (lastRecordedCurrentTime,
            // lastRecordedKernelTime, lastRecordedUserTime), of which only
            // the latter two are ever read back by managed code.
            //
            // PawPrint has no model of per-process CPU consumption: nothing
            // in the interpreter tracks "how much host CPU time was spent
            // executing this guest's instructions" as a quantity, and there
            // is no simulated contention for a virtual CPU that would make
            // "utilization" mean anything for a guest's own workload either.
            // We just return a constant 0; this function is only used to tune the thread pool's performance, for
            // AppDomain.MonitoringTotalProcessorTime, for certain tracing, and System.Environment.ProcessCpuUsage.
            let ptr =
                NativeCall.managedPointerOfPointerArgument
                    "SystemNative_GetCpuUtilization"
                    "previous"
                    instruction.Arguments.[0]

            match ptr with
            | ManagedPointerSource.Null ->
                failwith
                    "SystemNative_GetCpuUtilization: refused to write through null `previous` pointer (CoreLib should not invoke this entry point with a null destination -- ProcessCpuInformation is a fixed `ref` local, never null in valid IL)"
            | _ -> ()

            let byteConcreteType =
                NativeCall.requiredByteConcreteType "SystemNative_GetCpuUtilization" ctx.BaseClassTypes state

            let mutable state = state

            // sizeof(ProcessCpuInformation) = 3 fields * sizeof(ulong) = 24 bytes, verified
            // against both the managed declaration (via IlDump) and the native struct in
            // `pal_time.h` (no padding, three `uint64_t`).
            //
            // Known tech debt: this width is a literal with no structural link to either
            // declaration, and nothing would catch it drifting. The managed-BCL drift test
            // does not cover native `pal_*` headers. It is a literal because the boundary here
            // is genuinely untyped (`void*`), so deriving it would mean resolving
            // `ProcessCpuInformation`'s own ConcreteTypeHandle purely to describe three
            // all-zero `ulong`s. Note the bounds check in `MemoryBlock.writeBytes` is only a
            // partial safety net: it bounds against the whole backing memory block, not
            // against this struct's own extent, so a too-large width could in principle write
            // into adjacent memory within the same block rather than failing loudly.
            for i = 0 to 23 do
                let dest = ManagedPointerByteView.addByteOffset state byteConcreteType i ptr

                state <-
                    IlMachineState.writeManagedByrefBytesOrTypedCell
                        ctx.BaseClassTypes
                        state
                        dest
                        (CliType.Numeric (CliNumericType.UInt8 0uy))

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Float 0.0) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetLowResolutionTimestamp",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64) ->
            // PAL entry behind `Environment.TickCount64` on Unix. Real
            // CoreCLR returns `clock_gettime(CLOCK_MONOTONIC_COARSE)`
            // converted to milliseconds; PawPrint substitutes the
            // deterministic virtual clock the scheduler maintains so the
            // result is bit-for-bit reproducible. The clock counts 100 ns
            // ticks, so the conversion to the milliseconds this entry point
            // returns is an explicit divide — truncating, which is faithful:
            // upstream's coarse clock truncates too. Read-only: the
            // scheduler is the sole writer of `VirtualClockTicks`.
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int64 (Int64Source.Verbatim (EmulatedKernel.lowResolutionTimestampMs state.Kernel)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_SchedGetCpu",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_SchedGetCpu(void)` lives in
            // pal_threading.c, not pal_process.c. It is `sched_getcpu()` under
            // `#if HAVE_SCHED_GETCPU` and a hard `-1` otherwise -- Linux has
            // it, macOS does not. It surfaces
            // as the internal `Thread.GetCurrentProcessorNumber()`
            // (Thread.Unix.cs), which feeds the public
            // `Thread.GetCurrentProcessorId()` via `ProcessorIdCache`, and
            // thence `SharedArrayPool`'s partition selection,
            // `TimerQueue.Instances`, and
            // `PoolingAsyncValueTaskMethodBuilder`'s per-core cache. Every
            // one of those uses it purely as a shard index modulo a count
            // sized off `Environment.ProcessorCount`.
            //
            // PawPrint answers from simulated-process state rather than the
            // host, like every other value the guest could branch on: a host
            // read here would make the guest's shard choice — and so its
            // allocation pattern and its timer bucketing — depend on which
            // core the *interpreter* happened to be running on.
            //
            // We report a real per-thread placement rather than a constant.
            // The value is fixed at thread creation by
            // `EmulatedKernel.cpuForRotation` and stored in
            // `ThreadState.Cpu`; see there for why round-robin, and why
            // "pinned to" and "currently running on" coincide under a
            // scheduler that never migrates threads. Returning the stored
            // value verbatim (rather than re-deriving it here) is deliberate:
            // `effectiveProcessorCount` reads the kernel's env table live, so
            // two derivations at different moments could in principle
            // disagree. Nothing can make them disagree today — PawPrint
            // implements no `setenv`, and `KernelConfig` is applied before the
            // entry thread exists — but deriving once and storing means a
            // future PR that adds environment mutation cannot silently turn a
            // guest's shard index into an out-of-range one.
            //
            // Returning `-1` — claiming the platform lacks `sched_getcpu`, as
            // it genuinely does on macOS — was the alternative. It is a
            // legitimate answer that CoreLib handles (it falls back to
            // `Environment.CurrentManagedThreadId` as a shard proxy), and it
            // has the side effect of short-circuiting `ProcessorNumberSpeedCheck`.
            // We do not take it: PawPrint reports a Linux platform identity
            // through `SystemNative_GetUnixRelease`, and on Linux the call
            // works.
            let cpu =
                match Map.tryFind ctx.Thread state.ThreadState with
                | Some threadState -> threadState.Cpu
                | None ->
                    failwith
                        $"SystemNative_SchedGetCpu: thread %O{ctx.Thread} is executing but has no ThreadState (every running thread is created through IlMachineState, which assigns a CpuId)"

            let (CpuId.CpuId cpu) = cpu

            pushInt32 cpu ctx |> Some
        | Some "SystemNative_TryGetUInt32OSThreadId",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            // `uint32_t SystemNative_TryGetUInt32OSThreadId(void)` (pal_threading.c):
            //
            //     uint32_t result = (uint32_t)minipal_get_current_thread_id();
            //     return result == 0 ? (uint32_t)-1 : result;
            //
            // `minipal_get_current_thread_id()` (src/native/minipal/thread.h) is
            // `syscall(SYS_gettid)` on Linux and `pthread_threadid_np(pthread_self(), ..)`
            // on macOS, cached in a `_Thread_local`. Note the direction: the
            // 64-bit entry point below returns that value *verbatim*, and it is
            // this 32-bit one that truncates. The two agree exactly when the
            // high word is zero — always so on Linux, where a tid is a `pid_t`.
            //
            // The guest reaches this through `Interop.Sys.TryGetUInt32OSThreadId`
            // from `Lock.ThreadId.InitializeForCurrentThread`
            // (Lock.NonNativeAot.cs), which is `#if`-split per target: a Linux
            // CoreLib compiles this branch and a macOS one compiles the
            // `GetUInt64OSThreadId` branch instead. Both are implemented here
            // because they are two width-projections of one value, and because
            // which one PawPrint sees depends on the CoreLib flavour it
            // resolves, not on anything about the guest.
            //
            // We return a real id rather than the `(uint32)-1` "this platform
            // cannot determine a thread id" sentinel. Returning the sentinel
            // would work — CoreLib substitutes `Environment.CurrentManagedThreadId`,
            // which PawPrint also models deterministically — but it is the same
            // answer `SystemNative_SchedGetCpu` above declines to give, and for
            // the same reason: PawPrint presents a Linux platform identity
            // (`SimulatedUnixPlatform`), and on Linux this call works. It also
            // would not answer `GetUInt64OSThreadId`, which has no sentinel, so
            // a real id has to exist regardless.
            //
            // The value is minted at thread creation and stored in
            // `ThreadState.OsThreadId`; see `OsThreadId` for why it must be
            // unique across *all* threads including PawPrint-internal ones, and
            // why it is not a function of `ThreadId`.
            let (OsThreadId.OsThreadId osThreadId) =
                osThreadIdOf "SystemNative_TryGetUInt32OSThreadId" ctx

            state
            |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 osThreadId) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetUInt64OSThreadId",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt64) ->
            // `uint64_t SystemNative_GetUInt64OSThreadId(void)` (pal_threading.c)
            // is `return (uint64_t)minipal_get_current_thread_id();` — the full
            // native-width id, with no truncation and no sentinel. The macOS
            // CoreLib's `Lock.ThreadId.InitializeForCurrentThread` calls this
            // where a Linux one calls `TryGetUInt32OSThreadId` above.
            //
            // PawPrint's ids are 32-bit-canonical (`OsThreadId` explains why:
            // both sentinels it must dodge are 32-bit facts), so this reports
            // the zero-extension.
            let (OsThreadId.OsThreadId osThreadId) =
                osThreadIdOf "SystemNative_GetUInt64OSThreadId" ctx

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int64 (Int64Source.Verbatim (int64 (uint64 osThreadId))))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetTimestamp",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64) ->
            // `int64_t SystemNative_GetTimestamp(void)` (pal_time.c) is a
            // one-line forward to `minipal_hires_ticks()`
            // (src/native/minipal/time.c): `clock_gettime_nsec_np(CLOCK_UPTIME_RAW)`
            // on macOS, `clock_gettime(CLOCK_MONOTONIC)` scaled to nanoseconds
            // on Linux.
            //
            // This is the PAL entry behind `Stopwatch.GetTimestamp()` on Unix,
            // and hence behind every `Stopwatch` instance, `Stopwatch.Elapsed`,
            // and `Stopwatch.GetElapsedTime`. It matters beyond explicit guest
            // timing: the thread pool's hill-climbing step times its own
            // sampling window with it (`PortableThreadPool.AdjustMaxWorkersActive`,
            // PortableThreadPool.cs:379), and `ProcessorIdCache`'s static
            // initialiser uses it to decide whether caching `sched_getcpu` is
            // worthwhile. The paired `minipal_hires_tick_frequency()` is the
            // constant 1e9, which is what `Stopwatch.GetFrequency()` hard-codes
            // on Unix (Stopwatch.Unix.cs) — so the units here are pinned by
            // CoreLib rather than chosen.
            //
            // The reading derives from the same `VirtualClockTicks` that backs
            // `SystemNative_GetLowResolutionTimestamp` above, because upstream
            // *that* entry point is `minipal_lowres_ticks()`, which reads the
            // very same monotonic clock in milliseconds. One field for both
            // reproduces a relationship the guest can observe:
            // `Environment.TickCount64` and `Stopwatch` cannot disagree about
            // elapsed time. The scaling and its overflow guard live in
            // `EmulatedKernel.monotonicTimestampNanos`.
            //
            // Read-only, like every other clock observer: the scheduler is the
            // sole writer of `VirtualClockTicks`.
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int64 (Int64Source.Verbatim (EmulatedKernel.monotonicTimestampNanos state.Kernel)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetSystemTimeAsTicks",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64) ->
            // PAL entry behind `DateTime.UtcNow` on Unix: 100ns ticks since the
            // Unix epoch, which CoreLib offsets by `UnixEpochTicks` and stamps
            // `DateTimeKind.Utc` (DateTime.Unix.cs). Real CoreCLR reads
            // `clock_gettime(CLOCK_REALTIME)`; PawPrint derives the wall clock
            // from the same deterministic virtual clock that backs
            // `Environment.TickCount64`, offset by the kernel's boot-time
            // wall-clock reading. Read-only, like every other clock observer:
            // the scheduler is the sole writer of `VirtualClockTicks`, and
            // `WallClockEpochMs` never changes after configuration.
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int64 (Int64Source.Verbatim (EmulatedKernel.systemTimeAsTicks state.Kernel)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetUnixRelease", [], MethodReturnType.Returns (ConcretePointer _) ->
            // `char* SystemNative_GetUnixRelease(void)` (pal_runtimeinformation.c)
            // is `uname(&u)` followed by `strdup(u.release)`, i.e. it hands the
            // caller an owned C string. CoreLib reaches it from
            // `Environment.OSVersion` via a `StringMarshalling.Utf8`
            // `LibraryImport`, whose generated stub is `() -> byte*` and whose
            // wrapper calls `Utf8StringMarshaller.ConvertToManaged` and then
            // `Utf8StringMarshaller.Free` -> `NativeMemory.Free` ->
            // `SystemNative_Free`. So the pointer we return has to be a
            // native-heap block *base* (a byref into a managed `byte[]` is
            // refused by our `SystemNative_Free`), and it has to be a fresh
            // allocation per call — the guest owns and frees each one.
            //
            // The pointee type is matched loosely (`ConcretePointer _`): the
            // entry-point name plus zero parameters already pins the call
            // unambiguously, and a guest that hand-rolls the `[DllImport]` as
            // `void*`-returning rather than `byte*`-returning means exactly the
            // same thing here.
            //
            // PawPrint answers from `Kernel.UnixPlatform` rather than the host's
            // `uname(2)`: guests branch on `Environment.OSVersion`, so a host
            // read here would change guest control flow between runs. There is
            // correspondingly no failure path — real native code returns NULL if
            // `uname` fails or `strdup` cannot allocate, neither of which has an
            // analogue in the simulator — so we never return null, and (like the
            // C code on success) we leave errno untouched.
            let release = SimulatedUnixPlatform.unixRelease state.Kernel.UnixPlatform

            let ptr, state =
                NativeCall.allocateNativeHeapNullTerminatedUtf8 "SystemNative_GetUnixRelease" release state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetCwd",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePointer _) ->
            // `char* SystemNative_GetCwd(char* buffer, int32_t bufferSize)`
            // (pal_process.c:1302) guards a negative size and otherwise
            // delegates straight to `getcwd(3)`:
            //
            //   bufferSize < 0                      -> errno EINVAL, NULL
            //   bufferSize == 0 (non-NULL buffer)   -> errno EINVAL, NULL
            //   bufferSize < strlen(cwd) + 1        -> errno ERANGE, NULL
            //   otherwise      -> write the NUL-terminated path, return buffer
            //
            // The ERANGE case is used in actual domain logic:
            // CoreLib's `Interop.Sys.GetCwd()` tries a 256-byte `localloc`
            // first, and `GetCwdHelper` reads the errno back (as the PAL
            // `Interop.Error.ERANGE`) to decide whether to retry with
            // ArrayPool buffers at doubling sizes rather than throw. Getting
            // this wrong turns a long cwd into an IOException.
            //
            // The return value is the caller's own `buffer` on success, which
            // is what `getcwd` promises; note CoreLib only tests it against
            // NULL and then decodes `arg0`, so faithfulness here is for guests
            // that hand-roll the P/Invoke.
            //
            // PawPrint answers from `Kernel.CurrentDirectory` rather than the
            // host's `getcwd(3)`; see `EmulatedKernel.CurrentDirectory` for
            // why a host read would make a replay depend on where it was
            // recorded.
            let operation = "SystemNative_GetCwd"

            let bufferArgument = instruction.Arguments.[0]
            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[1]

            // Without the terminator: `getcwd` needs room for the path *and*
            // its NUL, which is exactly why a buffer of `path.Length` is one
            // byte short rather than an exact fit.
            let path = AbsoluteUnixPath.toUtf8 state.Kernel.CurrentDirectory

            /// Set errno and hand the guest a NULL `char*`, as the C does on
            /// every failure path.
            let fail (error : UnixError) : NativeHandlerResult option =
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrno error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Every failure below is decided *without* resolving `buffer` to
            // storage, because the C decides them without dereferencing it: the
            // negative-size guard runs before `getcwd` is even called, and
            // `getcwd` itself validates the size and compares it against the
            // path length before it writes a byte. A guest that hand-rolls this
            // P/Invoke may therefore legally pass a bit pattern PawPrint cannot
            // resolve — `GetCwd((byte*)123, 0)` returns EINVAL on the real
            // runtime — so the pointer is only decoded on the success path,
            // which is the one place it is actually dereferenced.
            if bufferSize < 0 then
                // The shim's own guard. Note it *also* `assert`s this, so a
                // checked native build would abort instead; EINVAL is what a
                // guest running against a retail runtime can observe, and it
                // is the only one of the two behaviours we can reproduce.
                fail UnixError.EINVAL
            elif isNullPointerArgument bufferArgument then
                // `getcwd(NULL, size)` is a glibc/BSD extension that mallocs
                // the result, and PawPrint does not model it: CoreLib's
                // `Interop.Sys.GetCwd` always supplies a `localloc` block or a
                // pinned `byte[]`, so a null here means a guest hand-rolled the
                // P/Invoke and is relying on the allocating form. Tested before
                // the zero-size case below, which would otherwise report EINVAL
                // for `getcwd(NULL, 0)` — a call the real runtime *succeeds*.
                failwith
                    $"%s{operation}: refusing to honour the allocating `getcwd(NULL, %d{bufferSize})` extension (PawPrint models only the caller-supplied-buffer form, which is the only one CoreLib uses)"
            elif bufferSize = 0 then
                // POSIX: size 0 with a non-NULL buffer is EINVAL, *not*
                // ERANGE — so a guest must not treat it as "grow and retry".
                fail UnixError.EINVAL
            elif bufferSize < path.Length + 1 then
                fail UnixError.ERANGE
            else

            // The buffer is genuinely dereferenced from here on, so this is
            // where it must resolve to storage. A pointer that does not is an
            // unmapped address (null was already handled above), which real
            // `getcwd` reports as EFAULT after writing nothing — note that the
            // size checks above come first, so `getcwd((byte*)123, 1)` is
            // ERANGE rather than EFAULT, as on the real kernel.
            match dereferenceablePointerArgument operation "buffer" bufferArgument with
            | None -> fail UnixError.EFAULT
            | Some buffer ->

            // Success. errno is left untouched, per Unix convention (and
            // CoreLib has already zeroed it via `Marshal.SetLastSystemError 0`
            // immediately before the call).
            let terminated = path.Add 0uy

            writeBytesThrough ctx operation buffer terminated state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer buffer) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetEUid",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            // `uint32_t SystemNative_GetEUid(void)` (pal_uid.c:91) is
            // `return geteuid();` — infallible, as `geteuid(2)` is.
            //
            // The same `UserId` `Stat`/`LStat` below report as every inode's
            // `st_uid`, because the emulated process has one identity: no
            // reachable syscall can give an inode an owner of its own
            // (`SystemNative_ChOwn` is not in the interop surface at all), so
            // there is nothing for a second source of truth to disagree with.
            //
            // That equality is why its `GetEGid` and `GetGroups` neighbours are
            // *not* implemented here. Within CoreLib the only route to them is
            // `Interop.Sys.IsMemberOfGroup` — managed code, not an entry point —
            // whose sole caller is `FileStatus.IsModeReadOnlyCore` behind
            // `if (_fileCache.Uid == Interop.Sys.GetEUid())`
            // (FileStatus.Unix.cs:106). With one identity that guard always
            // holds, so the group path is dead by construction and a
            // supplementary-group list would be state no syscall could vary.
            // Implementing `GetEGid` alone would be worse than either: it
            // short-circuits `IsMemberOfGroup` on `gid == GetEGid()`
            // (Interop.IsMemberOfGroup.cs:13), which under one identity is also
            // always true — so the branch would start *succeeding*, on the
            // strength of the very invariant that must have broken for it to be
            // reachable. Leaving them unimplemented means a guest that gets
            // there stops loudly instead, naming the entry point.
            // `sourcesImpure/EffectiveUserIdConfigured.cs` pins the premise.
            state
            |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 state.Kernel.UserId) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        // `int32_t SystemNative_Stat(const char* path, FileStatus* output)` and
        // its `LStat` twin, from `pal_io.c`. CoreLib declares each of them
        // twice — `Interop.Stat.cs` takes a `string`, `Interop.Stat.Span.cs` a
        // `ref byte` filled by `ValueUtf8Converter` — but both generate the same
        // `(byte*, FileStatus*)` stub, so one arm serves both. `File.Exists` and
        // `Directory.Exists` reach the span form.
        //
        // The output parameter is matched loosely, as `PalErrorReturn` is and
        // for the same reason: `Interop.Sys.FileStatus` is internal to CoreLib,
        // so a `sourcesPure` guest exercising this handler must declare its own
        // layout-identical struct, and a tight match would put the arm out of
        // its reach. The pointee handle is still bound, because it is what the
        // field layout is derived from.
        | Some "SystemNative_Stat",
          [ ConcretePointer _ ; ConcretePointer fileStatusHandle ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            statLike ctx "SystemNative_Stat" SymlinkPolicy.Follow fileStatusHandle state
        | Some "SystemNative_LStat",
          [ ConcretePointer _ ; ConcretePointer fileStatusHandle ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            statLike ctx "SystemNative_LStat" SymlinkPolicy.NoFollowFinal fileStatusHandle state
        // `intptr_t SystemNative_Open(const char* path, int32_t flags, int32_t mode)`
        // (pal_io.c:319). The flags parameter is matched loosely because
        // CoreLib declares it as the `Interop.Sys.OpenFlags` enum while a guest
        // hand-rolling the P/Invoke writes `int`; `int32Argument` peels enum
        // boxing, so both reach the same decode.
        | Some "SystemNative_Open",
          [ ConcretePointer _ ; _ ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcreteIntPtr state.ConcreteTypes) ->
            let operation = "SystemNative_Open"
            let flags = NativeCall.int32Argument operation instruction.Arguments.[1]

            // `Interop.Sys.OpenFlags`, which is a **PAL** enum: `ConvertOpenFlags`
            // (pal_io.c:275) translates these to the platform's own `<fcntl.h>`
            // bits, so PawPrint consumes portable values and has no platform
            // question to answer at this boundary.
            let palAccessMask = 0x0003
            let palRdOnly = 0x0000
            let palWrOnly = 0x0001
            let palRdWr = 0x0002
            let palCloExec = 0x0010
            let palCreat = 0x0020
            let palExcl = 0x0040
            let palTrunc = 0x0080
            let palSync = 0x0100
            let palNoFollow = 0x0200

            let fail (error : UnixError) : NativeHandlerResult option =
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrnoUnder numbering error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim -1L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let known =
                palAccessMask
                ||| palCloExec
                ||| palCreat
                ||| palExcl
                ||| palTrunc
                ||| palSync
                ||| palNoFollow

            // The shim's own rejection, in the order the C makes it: an
            // unrecognised *bit* is EINVAL (it `assert`s first, so a checked
            // build aborts instead — the same retail-behaviour-only reasoning
            // `SystemNative_GetCwd` records), and so is an access mode that is
            // none of the three.
            if flags &&& ~~~known <> 0 then
                fail UnixError.EINVAL
            else

            let accessMode = flags &&& palAccessMask

            if accessMode <> palRdOnly && accessMode <> palWrOnly && accessMode <> palRdWr then
                fail UnixError.EINVAL
            else

            // Everything that would *write* is refused loudly rather than
            // answered. PawPrint has no write path: there is no syscall that
            // can change a byte of the emulated filesystem, so an `O_TRUNC`
            // honoured as a no-op, or an `O_WRONLY` handle that silently reads,
            // would hand a guest a descriptor whose contract PawPrint cannot
            // keep. The crash names the flag, which is more use than the
            // generic "unimplemented native" this replaces.
            //
            // Known over-refusal, recorded because a green suite cannot show
            // it: `FileMode.OpenOrCreate` with `FileAccess.Read` on a file that
            // *exists* sets `O_CREAT`, and is a case this handler could answer
            // correctly.
            let refuse (flag : string) : NativeHandlerResult option =
                failwith
                    $"%s{operation}: the guest asked for %s{flag}, but PawPrint has no write path — nothing can yet modify the emulated filesystem, so a descriptor opened for writing could not honour its contract. Implement the write path (issue #956) before opening one."

            if accessMode = palWrOnly then
                refuse "O_WRONLY"
            elif accessMode = palRdWr then
                refuse "O_RDWR"
            elif flags &&& palCreat <> 0 then
                refuse "O_CREAT"
            elif flags &&& palExcl <> 0 then
                refuse "O_EXCL"
            elif flags &&& palTrunc <> 0 then
                refuse "O_TRUNC"
            else

            // `O_CLOEXEC` is accepted and ignored: it sets `FD_CLOEXEC`, which
            // matters only across `exec`, and PawPrint models neither `fork`
            // nor `exec` (see `FileDescriptorRegistry`). `O_SYNC` likewise —
            // it governs when *writes* reach storage, and there are none.
            //
            // The `mode` argument is ignored rather than validated, and must
            // be: `SafeFileHandle.OpenReadOnly` passes `DefaultCreateMode`
            // (0666) even for a read-only open of an existing file
            // (SafeFileHandle.Unix.cs:168), so a handler that refused a nonzero
            // mode without `O_CREAT` would refuse the BCL's own read path.
            let policy =
                if flags &&& palNoFollow <> 0 then
                    SymlinkPolicy.NoFollowFinal
                else
                    SymlinkPolicy.Follow

            match dereferenceablePointerArgument operation "path" instruction.Arguments.[0] with
            | None -> fail UnixError.EFAULT
            | Some pathPtr ->

            let limits = SimulatedUnixPlatform.pathLimits state.Kernel.UnixPlatform

            let bytes =
                NativeCall.readNullTerminatedBytesWithin
                    operation
                    ctx.BaseClassTypes
                    state
                    pathPtr
                    (PathLimits.pathMaxBytes limits)

            match parseGuestPathBytes operation limits bytes with
            | Error error -> fail error
            | Ok path ->

            match resolveGuestPath operation policy state.Kernel path with
            | Error error -> fail error
            | Ok inode ->

            match VirtualFileSystem.tryGetContent inode state.Kernel.FileSystem with
            | None ->
                failwith
                    $"%s{operation}: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
            | Some (InodeContent.Symlink _) ->
                // Only reachable under `O_NOFOLLOW`, which is what
                // `NoFollowFinal` above selects: without it the resolver would
                // have followed the link (or failed ENOENT on a dangling one).
                // ELOOP rather than anything more specific is what both Unixes
                // answer, and is what `SafeFileHandle.OpenNoFollowSymlink`
                // reads back to decide a path was a symlink without racing.
                fail UnixError.ELOOP
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Directory _) ->

            // A directory opens perfectly well for reading, and CoreLib
            // *depends* on that: `SafeFileHandle.Init` opens, then `FStat`s,
            // and raises `UnauthorizedAccessException` on seeing `S_IFDIR`, so
            // refusing here would give `File.ReadAllBytes("d")` the wrong
            // exception. The type check belongs in what `FStat` reports.
            //
            // **No permission check, deliberately.** A real `open(O_RDONLY)`
            // owes EACCES for a file whose owner-read bit is clear, and the
            // resolution owes it for a directory whose owner-search bit is —
            // and neither state is reachable. `SeedEntry.File` carries contents
            // and nothing else, so every seeded file is
            // `PermissionBits.defaultForRegularFile` (0644) and every directory
            // `defaultForDirectory` (0755); no `SystemNative_ChMod` exists to
            // change them, and the emulated process has the single identity
            // that owns them all. So the owner bits are set by construction,
            // and an EACCES arm here would be a branch no guest could take and
            // no seed could provoke — dead code of exactly the kind #1008
            // declined to write for the supplementary-group path.
            //
            // When permissions do become expressible, the *search* half belongs
            // in the resolver rather than here: every component of every path
            // needs it, so `Stat`, `LStat` and `ReadLink` would owe the same
            // answer, and only the final-file read check would be this arm's.
            let fd, registry =
                FileDescriptorRegistry.openFile inode state.Kernel.FileDescriptors

            state.MapKernel (fun kernel ->
                { kernel with
                    FileDescriptors = registry
                }
            )
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.Verbatim (int64 fd)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_FStat",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer fileStatusHandle ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_FStat(intptr_t fd, FileStatus* output)`
            // (pal_io.c). The same struct `Stat`/`LStat` fill in, from a
            // descriptor rather than a path — so it shares their encoder, and
            // the output parameter is matched loosely for the same reason
            // theirs is.
            let operation = "SystemNative_FStat"
            let fd = fdArgument operation instruction.Arguments.[0]

            let fail (error : UnixError) : NativeHandlerResult option =
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrno error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match FileDescriptorRegistry.tryFindObject fd state.Kernel.FileDescriptors with
            | None -> fail UnixError.EBADF
            | Some (OpenFileObject.StandardStream role) ->
                // PawPrint models the standard streams as pipes, and a pipe's
                // `fstat` is a real answer a real kernel gives — `S_IFIFO`, a
                // zero size, a device number. Every one of those would be
                // invented here: the emulated kernel holds no inode for a
                // stream, so there is nothing to report and no way for a test
                // to say the invention was wrong. Refuse loudly instead; the
                // BCL reaches `FStat` only through a `SafeFileHandle` it opened
                // itself, so this is a hand-rolled P/Invoke or a genuinely new
                // code path, and either wants a decision rather than a guess.
                failwith
                    $"%s{operation}: fd %d{fd} is the standard stream %O{role}, and PawPrint holds no inode for one. Every field `fstat` owes a pipe would be invented here; decide what a stream's `struct stat` is (issue #956) rather than guessing."
            | Some (OpenFileObject.File inode) ->

            let entry =
                match VirtualFileSystem.tryGet inode state.Kernel.FileSystem with
                | Some entry -> entry
                | None ->
                    // Not reachable today — nothing unlinks — but stated so
                    // that the write path finds a decision rather than a crash:
                    // a descriptor keeps its inode alive after the last link is
                    // gone, so `unlink` must not remove the inode from the
                    // graph while a descriptor still names it.
                    failwith
                        $"%s{operation}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."

            match dereferenceablePointerArgument operation "output" instruction.Arguments.[1] with
            | None -> fail UnixError.EFAULT
            | Some output ->

            writeFileStatus ctx operation fileStatusHandle inode entry output state
        // `int32_t SystemNative_FLock(intptr_t fd, int32_t operation)`
        // (pal_io.c:744). The operation parameter is matched loosely for the
        // same reason `SystemNative_Open`'s flags are: CoreLib declares it as
        // the `Interop.Sys.LockOperations` enum while a guest hand-rolling the
        // P/Invoke writes `int`.
        | Some "SystemNative_FLock",
          [ ConcreteIntPtr state.ConcreteTypes ; _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_FLock"
            let fd = fdArgument operation instruction.Arguments.[0]
            let request = NativeCall.int32Argument operation instruction.Arguments.[1]

            // Unlike `Interop.Error` and `Interop.Sys.OpenFlags`, these are
            // *not* PAL values that the C translates: `SystemNative_FLock`
            // passes `operation` straight to `flock(2)`. `Interop.FLock.cs`
            // hardcodes 1/2/4/8 anyway, which is correct only because Linux and
            // Darwin happen to agree on all four — measured on both rather than
            // assumed, since nothing in the interop layer would catch it if they
            // did not.
            let lockShared = 1
            let lockExclusive = 2
            let lockNonBlocking = 4
            let lockUnlock = 8

            // Takes the state explicitly rather than closing over the outer one:
            // a failing `flock` still advances the descriptor table, so the
            // error paths below must report from the state that *includes* that
            // advance, not from the one before it.
            let failFrom (state : IlMachineState) (error : UnixError) : NativeHandlerResult option =
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrnoUnder numbering error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // `flock` is one of the places where the two Unixes PawPrint models
            // genuinely disagree, and not only about errno numbering. PawPrint
            // models Linux's rules and refuses under Darwin rather than
            // guessing, because what has been *measured* about Darwin is its
            // return codes and not the lock state they leave behind: Darwin
            // accepts `LOCK_SH|LOCK_EX` and `LOCK_UN|LOCK_SH`, and nothing here
            // knows which lock the description then holds, which is precisely
            // what `FlockMode` would have to commit to. Each refusal below
            // carries what was measured so that the run that hits it starts from
            // data rather than from scratch.
            //
            // Deliberately *not* a `SimulatedUnixPlatform` accessor in the style
            // of `pathLimits`: those exist because their facts are complete, and
            // this one is not yet.
            let refuseDarwin (divergence : string) : 'a =
                failwith
                    $"%s{operation}: %s{divergence} PawPrint models Linux's `flock` and has not modelled Darwin's, but this kernel's SimulatedUnixPlatform is Darwin — so answering would be inventing behaviour rather than reporting it. What is measured about Darwin here is the return code only, not the lock state it leaves; deciding that needs its own measurements (issue #956). Configure a Linux platform, or model Darwin's flock."

            // Linux validates strictly: exactly one of SH/EX/UN, optionally
            // with NB, and nothing else — `0`, `SH|EX`, `UN|SH`, a bare `NB` and
            // any unknown bit are all EINVAL. Darwin is laxer *and* uses a
            // different errno: it answers EBADF for `0`, a bare `NB` and `16`,
            // and succeeds outright for `SH|EX`, `UN|SH` and `SH|16`. Both
            // measured.
            let nonBlocking = request &&& lockNonBlocking <> 0
            let mode = request &&& ~~~lockNonBlocking

            let flockRequest : FlockRequest option =
                if mode = lockUnlock then
                    Some FlockRequest.Release
                elif mode = lockShared then
                    Some (FlockRequest.Acquire FlockMode.Shared)
                elif mode = lockExclusive then
                    Some (FlockRequest.Acquire FlockMode.Exclusive)
                else
                    None

            let flavour = SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform

            match flockRequest with
            | None ->
                match flavour with
                | SimulatedUnixFlavour.Linux -> failFrom state UnixError.EINVAL
                | SimulatedUnixFlavour.Darwin ->
                    refuseDarwin
                        $"operation %d{request} is malformed (not exactly one of LOCK_SH/LOCK_EX/LOCK_UN, optionally with LOCK_NB), which Linux rejects with EINVAL and Darwin does not treat uniformly — measured, Darwin answers EBADF for 0, a bare LOCK_NB and unknown bits alone, but *succeeds* for LOCK_SH|LOCK_EX, LOCK_UN|LOCK_SH and LOCK_SH with an unknown bit."
            | Some flockRequest ->

            // The two remaining divergences, both about a descriptor PawPrint has
            // already resolved, so they are checked here rather than in the
            // registry: that module models one coherent set of rules. Throws or
            // falls through; an unknown fd is EBADF on both platforms, so there
            // is nothing to refuse for one.
            match flavour, FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
            | SimulatedUnixFlavour.Linux, _
            | _, None -> ()
            | SimulatedUnixFlavour.Darwin, Some description ->
                match description.Object with
                | OpenFileObject.StandardStream role ->
                    refuseDarwin
                        $"fd %d{fd} is the standard stream %O{role}, which PawPrint models as a pipe. Linux permits `flock` on a pipe and returns 0; Darwin refuses it with ENOTSUP (raw 45, and note Darwin numbers ENOTSUP and EOPNOTSUPP differently, 45 against 102, while Linux gives both 95)."
                | OpenFileObject.File _ ->

                match flockRequest, description.Flock with
                // Only a *conversion* — an acquire by a description that already
                // holds something — can expose the keep-versus-drop divergence,
                // and only when it fails. Refused on the request rather than on
                // the outcome, so that the refusal is a property of what was
                // asked rather than of who else happened to hold a lock.
                | FlockRequest.Acquire _, Some _ ->
                    refuseDarwin
                        $"fd %d{fd} is converting a lock it already holds. Should that conversion fail, Linux leaves the description holding *nothing* (`flock` removes the old lock before establishing the new one, and the two steps are not atomic) while Darwin leaves the old lock in place — measured on both, and indistinguishable from the return code, which is EWOULDBLOCK either way."
                | _, _ -> ()

            // The table advances even when the call fails: a conversion that
            // could not be granted has already dropped the caller's old lock.
            // So the new table is committed *before* the outcome is inspected,
            // and every branch below reports from `state'`.
            let registry, error =
                FileDescriptorRegistry.flock fd flockRequest state.Kernel.FileDescriptors

            let state' =
                state.MapKernel (fun kernel ->
                    { kernel with
                        FileDescriptors = registry
                    }
                )

            match error with
            | Some FlockError.BadFd -> failFrom state' UnixError.EBADF
            | Some FlockError.WouldBlock ->
                if nonBlocking then
                    failFrom state' UnixError.EAGAIN
                else
                    // A blocking acquisition that *can* be satisfied is served
                    // above, so only genuine contention reaches here. Waiting
                    // for it is a scheduler feature rather than a filesystem
                    // one: the caller must park and be woken when the holder
                    // releases.
                    //
                    // The holder is some other open file description of this
                    // same process, PawPrint simulating exactly one — but not
                    // necessarily another *thread*. A single-threaded guest that
                    // opens one file twice and blocks on its own lock is
                    // deadlocked, and a real kernel duly hangs it forever. So
                    // refusing here is not merely the conservative option: for
                    // the single-threaded case it is strictly more useful than
                    // what Linux does, and it never converts the request into a
                    // non-blocking one, which would hand the guest an
                    // `EWOULDBLOCK` no kernel would have produced.
                    //
                    // CoreLib never reaches this: `SafeFileHandle.Init` always
                    // sets `LOCK_NB`.
                    let requested = if mode = lockShared then "shared" else "exclusive"

                    failwith
                        $"%s{operation}: fd %d{fd} requested a blocking %s{requested} lock, and another open file description holds a conflicting one. PawPrint cannot block a thread on a lock: that needs the scheduler to park it and wake it when the holder releases (issue #956). If the holder is this same thread, a real kernel would deadlock here rather than return. Pass LOCK_NB to get EWOULDBLOCK instead."
            | None ->
                state'
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
        // `int32_t SystemNative_PRead(intptr_t fd, void* buffer, int32_t
        // bufferSize, int64_t fileOffset)` (pal_io.c:1847): `pread(2)` verbatim,
        // with an EINTR retry. Note it does *not* go through `Common_Read` in
        // `pal_io_common.h`, so unlike `SystemNative_Read` it has no
        // negative-size guard — its `assert(bufferSize >= 0)` is debug-only, and
        // a release build casts a negative size to a ~4 GB unsigned count.
        | Some "SystemNative_PRead",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_PRead"
            let fd = fdArgument operation instruction.Arguments.[0]
            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[2]
            let fileOffset = NativeCall.int64Argument operation instruction.Arguments.[3]

            let fail (error : UnixError) : NativeHandlerResult option =
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrnoUnder numbering error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // A negative size is the one input whose real behaviour PawPrint
            // cannot reproduce, so it is refused before anything else is
            // considered. The C casts it to `uint32_t`, asking the kernel for
            // ~4 GB: measured, macOS answers EINVAL (the count exceeds what it
            // will accept) while Linux answers EFAULT (the buffer's mapping does
            // not extend that far) — and Linux's answer therefore depends on the
            // *guest's address space*, which PawPrint does not model to that
            // fidelity. Either choice would be a documented divergence on one
            // platform, and a silent one at that.
            //
            // Refusing first means a negative size beats an otherwise-diagnosable
            // bad fd, which real kernels would report as EBADF. That is a
            // deliberate over-refusal on a two-fault input, recorded here because
            // a green suite cannot show it; the alternative is to answer a
            // question whose premise PawPrint has already refused.
            //
            // CoreLib never sends one: every caller is `RandomAccess`, whose
            // sizes come from span lengths.
            if bufferSize < 0 then
                failwith
                    $"%s{operation}: fd %d{fd} was given bufferSize %d{bufferSize}, which is negative. The C shim casts that to an unsigned ~4 GB count rather than rejecting it (unlike SystemNative_Read, which goes through Common_Read and answers ERANGE), and what a kernel then does is not a fact PawPrint can state: measured, macOS answers EINVAL and Linux answers EFAULT, Linux's answer depending on how far the guest's buffer happens to be mapped. Pass a non-negative size."
            else

            let offsetInvalid = fileOffset < 0L

            // The order of the checks below is measured, not assumed, and it
            // differs between the two platforms. On a *single-fault* input they
            // agree on every row; they part company only when two things are
            // wrong at once, which is why an ordering has to be pinned at all:
            //
            //   input                     Linux    Darwin
            //   negative offset + bad fd  EINVAL   EBADF
            //   negative offset + pipe    EINVAL   ESPIPE
            //   negative offset + dir     EINVAL   EINVAL
            //
            // Linux validates the offset before it even looks the descriptor up
            // (`do_pread` checks `pos < 0` ahead of `fdget`); Darwin resolves
            // the descriptor and its seekability first, and only then the
            // offset. Both orders are followed here rather than one being
            // imposed on the other, because both are fully measured — unlike
            // `SystemNative_FLock`, whose Darwin *return codes* are known but
            // whose resulting lock state is not, and which therefore refuses.
            // Where the answer is known, PawPrint gives it rather than crashing.
            //
            // Note `EISDIR` follows the offset check on *both* platforms, so
            // only the descriptor and seekability steps actually move; that is
            // why one flag suffices rather than two separate orderings.
            let offsetCheckedBeforeDescriptor =
                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> true
                | SimulatedUnixFlavour.Darwin -> false

            if offsetCheckedBeforeDescriptor && offsetInvalid then
                fail UnixError.EINVAL
            else

            match FileDescriptorRegistry.tryFindObject fd state.Kernel.FileDescriptors with
            | None -> fail UnixError.EBADF
            | Some (OpenFileObject.StandardStream role) ->
                // `pread` needs a seekable object, and PawPrint models the
                // standard streams as pipes — stdin the read end, stdout and
                // stderr write ends (which is why `SystemNative_Write` to fd 0
                // is EBADF). Such a descriptor fails two different tests at
                // once for stdout and stderr: it is neither seekable nor open
                // for reading. Measured, the platforms break that tie
                // differently:
                //
                //   descriptor                        Linux    Darwin
                //   pipe read end (unseekable)        ESPIPE   ESPIPE
                //   pipe write end (also unreadable)  ESPIPE   EBADF
                //   regular file O_WRONLY (seekable)  EBADF    EBADF
                //
                // So Linux lets unseekability win for a pipe while Darwin lets
                // unreadability win; the third row is the control showing this
                // is about the tie rather than about readability generally.
                //
                // Reachable from the BCL, and handled by it:
                // `RandomAccess.ReadAtOffset` catches ESPIPE (and ENXIO), clears
                // `SupportsRandomAccess`, and retries through
                // `SystemNative_Read`. So a `FileStream` over a pipe gets one
                // step further than it used to and then stops at that
                // unimplemented handler, which is the honest outcome — the
                // sequential read path is not this slice. Note the Darwin answer
                // for stdout/stderr does *not* get that retry, EBADF not being
                // one of the errnos that clears the flag.
                let unreadable =
                    match role with
                    | FileDescriptorRole.StandardInput -> false
                    | FileDescriptorRole.StandardOutput
                    | FileDescriptorRole.StandardError -> true

                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                | SimulatedUnixFlavour.Darwin when unreadable -> fail UnixError.EBADF
                | SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Linux -> fail UnixError.ESPIPE
            | Some (OpenFileObject.File inode) ->

            // Darwin's turn to validate the offset: it has now resolved the
            // descriptor and rejected an unseekable one, which is exactly the
            // window in which it differs from Linux. On Linux this cannot fire,
            // because the check above already did.
            if not offsetCheckedBeforeDescriptor && offsetInvalid then
                fail UnixError.EINVAL
            else

            let entry =
                match VirtualFileSystem.tryGet inode state.Kernel.FileSystem with
                | Some entry -> entry
                | None ->
                    failwith
                        $"%s{operation}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."

            match entry.Content with
            | InodeContent.Directory _ ->
                // EISDIR on both. Reachable: `SystemNative_Open` opens a
                // directory quite happily, as `open(2)` does.
                fail UnixError.EISDIR
            | InodeContent.Symlink _ ->
                // Not reachable: `open` resolves a symlink, so no descriptor
                // ever names one. Stated rather than merged into the file case
                // so that a future `O_PATH`/`O_NOFOLLOW`-returning-a-link finds
                // a decision here instead of silently reading a target as if it
                // were file content.
                failwith
                    $"%s{operation}: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what reading a link through a descriptor means (issue #956)."
            | InodeContent.RegularFile (contents, _) ->

            let transfer =
                VirtualFileSystem.readTransferCount fileOffset bufferSize contents.Length

            // The buffer is decoded only on the path that actually writes
            // through it. That is not an optimisation: a real kernel faults on
            // `copy_to_user`, so a call that transfers nothing never touches the
            // buffer at all, and `pread(fd, NULL, 5, offsetAtEof)` returns 0
            // rather than EFAULT — measured on both platforms, and easy to get
            // wrong by validating arguments up front.
            if transfer = 0 then
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            else

            // Only the *base* of the buffer is validated here, which is all
            // `dereferenceablePointerArgument` can say. A guest that asks to
            // read more bytes than its buffer holds — `pread(fd, stackalloc
            // byte[1], 5, 0)` — therefore gets as far as the write and then
            // fails inside `MemoryBlock.writeBytes`, naming the block rather
            // than the syscall.
            //
            // Left as it is, deliberately. That is a property of the shared
            // `writeBytesThrough` seam rather than of this handler: measured,
            // `SystemNative_ReadLink` fails identically for a target longer than
            // the buffer it was given, and `Stat`/`LStat`/`FStat` write through
            // the same helper. Fixing it means giving that seam a "is this whole
            // range writable" query, which has to understand every
            // `ManagedPointerSource` shape — its own change, and one that should
            // improve every caller at once rather than this one quietly.
            //
            // Nor is the behaviour wrong, exactly: the guest has overflowed its
            // own buffer, which a real kernel services by corrupting whatever
            // follows it. Detecting that is more useful than reproducing it.
            // What is missing is a message that names the syscall.
            match dereferenceablePointerArgument operation "buffer" instruction.Arguments.[1] with
            | None -> fail UnixError.EFAULT
            | Some buffer ->

            // Indexed rather than `Seq.skip`, which would enumerate the whole
            // prefix on every read and make reading a file quadratic in its
            // length.
            let bytes =
                ImmutableArray.CreateRange (seq { for i in 0 .. transfer - 1 -> contents.[int fileOffset + i] })

            writeBytesThrough ctx operation buffer bytes state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim transfer)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_ReadLink",
          [ ConcretePointer _ ; ConcretePointer _ ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_ReadLink(const char* path, char* buffer,
            // int32_t bufferSize)` (pal_io.c:1183): a `bufferSize <= 0` guard,
            // then `readlink(2)` verbatim.
            //
            // Both pointers are matched loosely, as `Stat`'s are: CoreLib
            // declares this `(ref byte, ref byte, int)` and a guest
            // hand-rolling the P/Invoke writes `(byte*, byte*, int)`, which
            // generate the same stub.
            //
            // **Truncation is not an error path.** `Interop.Sys.ReadLink`
            // starts with a 256-byte `stackalloc` and doubles through
            // `ArrayPool` while `result == buffer.Length`, so a short buffer is
            // how the BCL *sizes* its allocation; a handler that refused to
            // truncate would break `FileInfo.LinkTarget` for every target of
            // 256 bytes or more. `SymlinkTarget.toUtf8` is already the bytes
            // this hands back, and already documented as such.
            //
            // The order below is the C's, and every step of it is observable
            // — `sourcesPure/SystemNativeReadLink.cs` passes inputs that two
            // adjacent checks would reject differently, so the errno names
            // which one ran first.
            let operation = "SystemNative_ReadLink"
            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[2]

            /// Set errno and return -1, as the C does on every failure path.
            let fail (error : UnixError) : NativeHandlerResult option =
                // `toRawErrnoUnder` rather than `toRawErrno`, for the reason
                // `statLike`'s twin gives: a resolution can fail with ELOOP,
                // which has no platform-independent number.
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrnoUnder numbering error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            if bufferSize <= 0 then
                // The shim's own guard, before `readlink` is called at all —
                // so this is EINVAL whatever the path is, including a path
                // that addresses nothing. It is also the *only* reason this
                // entry point is cross-platform here: measured, the raw
                // syscall answers 0 on macOS and EINVAL on Linux for
                // `bufsiz == 0`, and the guard means neither answer escapes.
                //
                // Note the C `assert`s `bufferSize >= 0` first, so a checked
                // build would abort on a negative size rather than reach this;
                // EINVAL is what a guest running against a retail runtime can
                // observe, exactly as for `SystemNative_GetCwd`.
                fail UnixError.EINVAL
            else

            // Read before anything else looks at it, because a real kernel
            // copies the pathname in before it resolves anything: a path that
            // addresses nothing is EFAULT whatever the buffer is.
            match dereferenceablePointerArgument operation "path" instruction.Arguments.[0] with
            | None -> fail UnixError.EFAULT
            | Some pathPtr ->

            let limits = SimulatedUnixPlatform.pathLimits state.Kernel.UnixPlatform

            let bytes =
                NativeCall.readNullTerminatedBytesWithin
                    operation
                    ctx.BaseClassTypes
                    state
                    pathPtr
                    (PathLimits.pathMaxBytes limits)

            match parseGuestPathBytes operation limits bytes with
            | Error error -> fail error
            | Ok path ->

            // `NoFollowFinal` plus "and then it had better be a symlink" is
            // the same composition `TestVirtualFileSystemAgainstHost`'s
            // `modelOutcome` already checks against a real kernel over
            // generated symlink trees, which is why it is composed here rather
            // than extracted into `VirtualFileSystem`: the rule is verified
            // where it lives, and this arm's own job is the wire format.
            //
            // `NoFollowFinal`, which is what makes this `readlink` rather than
            // an expensive way of asking about the target: a final symlink is
            // the thing being read, not something to step through. A trailing
            // separator still overrides that — "lf/" demands that `lf` be a
            // directory — and the resolver owns that rule, answering ENOTDIR.
            match resolveGuestPath operation SymlinkPolicy.NoFollowFinal state.Kernel path with
            | Error error -> fail error
            | Ok inode ->

            match VirtualFileSystem.tryGetContent inode state.Kernel.FileSystem with
            | None ->
                failwith
                    $"%s{operation}: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
            | Some (InodeContent.Directory _)
            | Some (InodeContent.RegularFile _) ->
                // Not a link. EINVAL rather than any other errno is
                // load-bearing rather than cosmetic: `FileSystem.ResolveLinkTarget`
                // (FileSystem.Unix.cs:679) answers *null* for EINVAL and
                // rethrows every other errno as an exception, so this single
                // choice is the difference between `File.ResolveLinkTarget`
                // reporting "not a link" and it throwing.
                //
                // Decided here, before the output pointer is looked at, which
                // is what a real kernel does — `vfs_readlink` refuses on the
                // inode's operations before it copies anything out. Measured
                // on the host: `readlink("f", (char*)8, 16)` is EINVAL, not
                // EFAULT.
                fail UnixError.EINVAL
            | Some (InodeContent.Symlink target) ->

            // The output pointer is only decoded here, on the path that
            // actually writes through it.
            match dereferenceablePointerArgument operation "buffer" instruction.Arguments.[1] with
            | None -> fail UnixError.EFAULT
            | Some buffer ->

            let all = SymlinkTarget.toUtf8 target
            let count = min all.Length bufferSize

            // Truncated in *bytes*, not in characters: a symlink target is a
            // byte string, and truncating a .NET string by `String.Length`
            // would write two bytes where the caller allowed one for any
            // non-ASCII target. `sourcesImpure/ReadLinkRawSeeded.cs` is the
            // only test that can tell the two apart, because the differential
            // oracle's seed validator permits only ASCII targets.
            let written =
                if count = all.Length then
                    all
                else
                    ImmutableArray.CreateRange (Seq.truncate count all)

            // **Known omission: the link's `atime` does not move**, though
            // POSIX says a successful `readlink` marks it for update. This is
            // deferred rather than overlooked, and it is not dead state: the
            // virtual clock advances as the driver loop runs, so a guest that
            // `LStat`s a link before and after reading it really could see the
            // difference.
            //
            // It is deferred because it cannot be settled *here*. Whether the
            // access time moves is a property of the mount, not of this
            // syscall, and the two platforms modelled disagree: measured on
            // macOS — lstat, sleep, readlink, lstat — `st_atime` does not
            // move, while Linux's default `relatime` updates whenever `mtime`
            // or `ctime` is at or after the old `atime`, or it is a day stale
            // (`relatime_need_update`, fs/inode.c) — and a freshly seeded
            // inode has all three equal, so the first read *would* move it.
            // Deciding that inside one entry point would set mount semantics
            // for every future read by accident, and would make `readlink` the
            // only syscall obeying them.
            //
            // It would also be the first mutation of the emulated filesystem
            // in the interpreter: the graph is built once from the seed
            // (`EmulatedKernel.fs`) and no handler writes back
            // `Kernel.FileSystem` today, so there is no write-back seam to
            // reuse. Note the divergence is also not something the differential
            // oracle can arbitrate, since the answer depends on which host ran
            // it.
            //
            // No terminator, and errno left alone: `readlink` writes exactly
            // the bytes it reports and reports success by a non-negative
            // count, so a NUL here would corrupt the byte after a target that
            // exactly fits.
            writeBytesThrough ctx operation buffer written state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim count)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
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
                            LastSystemError = UnixError.toRawErrno UnixError.EBADF
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
                            LastSystemError = UnixError.toRawErrno UnixError.EBADF
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim resultCode)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_IsATty",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_IsATty(intptr_t fd)` (pal_console.c:43)
            // delegates to libc `isatty(3)`. PawPrint models a headless
            // simulated process: no fd ever refers to a terminal, so we
            // always return 0. CoreLib's only consumer is
            // `ConsolePal.Unix.cs:IsHandleRedirected`, which therefore sees
            // every standard stream as redirected — matching how this
            // interpreter is run in practice (piped/captured output).
            //
            // errno mirrors libc: `ENOTTY` for live fds, `EBADF` for
            // unknown fds. The BCL's `[LibraryImport]` wrapper for IsATty
            // does not currently read this back, but a guest that calls
            // the entry point directly may observe `LastSystemError` via
            // `Marshal.GetLastSystemError`, so we set it honestly.
            let fd = fdArgument "SystemNative_IsATty" instruction.Arguments.[0]

            let error =
                match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
                | Some _ -> UnixError.ENOTTY
                | None -> UnixError.EBADF

            let state =
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrno error
                    }
                )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
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

            let setErrno (state : IlMachineState) (error : UnixError) : IlMachineState =
                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrno error
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
                    let src = ManagedPointerByteView.addByteOffset state byteConcreteType i buffer

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
                    -1, StepEffect.NoEffect, setErrno state UnixError.ERANGE
                else
                    match FileDescriptorRegistry.tryFindObject fd state.Kernel.FileDescriptors with
                    | None ->
                        // Unknown fd: report EBADF the same way `write(2)`
                        // would.
                        -1, StepEffect.NoEffect, setErrno state UnixError.EBADF
                    | Some (OpenFileObject.File inode) ->
                        // A descriptor on a real file. EBADF is what a real
                        // kernel answers for a write to an `O_RDONLY`
                        // descriptor, and every descriptor PawPrint hands out
                        // today is one — `SystemNative_Open` refuses every
                        // write flag loudly, so no other kind can exist. That
                        // makes this arm honest rather than a stand-in: it is
                        // not "writing is unimplemented", it is "this
                        // descriptor is read-only", which is a true statement
                        // about every descriptor that can reach it.
                        //
                        // When the write path lands, the open file description
                        // gains an access mode and this arm must consult it
                        // instead of assuming.
                        ignore<InodeNumber> inode
                        -1, StepEffect.NoEffect, setErrno state UnixError.EBADF
                    | Some (OpenFileObject.StandardStream role) ->
                        match role with
                        | FileDescriptorRole.StandardInput ->
                            // `write(2)` on a read-only fd returns -1 + EBADF
                            // on Linux (the fd's access mode is wrong for the
                            // operation). Real stdin is opened O_RDONLY by
                            // the shell, so this matches what guests would
                            // observe on the host.
                            -1, StepEffect.NoEffect, setErrno state UnixError.EBADF
                        | FileDescriptorRole.StandardOutput
                        | FileDescriptorRole.StandardError ->
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
                                    dereferenceablePointerArgument operation "buffer" instruction.Arguments.[1]

                                match dereferenceableBuffer with
                                | None ->
                                    // EFAULT: bad address. Real kernels
                                    // perform no I/O on this path.
                                    -1, StepEffect.NoEffect, setErrno state UnixError.EFAULT
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
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) ctx.Thread
            |> NativeHandlerResult.completedWith effect
            |> Some
        | Some "SystemNative_GetNonCryptographicallySecureRandomBytes",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let state, newPrngState =
                drawRandomBytesInto
                    ctx
                    "SystemNative_GetNonCryptographicallySecureRandomBytes"
                    state.Kernel.NonCryptoRandomState

            state.MapKernel (fun kernel ->
                { kernel with
                    NonCryptoRandomState = newPrngState
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetCryptographicallySecureRandomBytes",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Same substitute PRNG as the non-crypto entry point (see
            // `drawRandomBytesInto`), drawn from its own kernel stream so
            // that a guest's `Random`/`HashCode` draws don't perturb the
            // sequence `Guid.NewGuid` observes.
            //
            // Unlike its non-crypto sibling this entry point reports status:
            // `Interop.GetCryptographicallySecureRandomBytes` branches on the
            // result with `brfalse` and throws `CryptographicException` for
            // anything non-zero. PawPrint's substitute has no failure mode —
            // there is no host entropy source to be exhausted or unreadable —
            // so it always reports success. Malformed arguments abort loudly
            // inside `drawRandomBytesInto` rather than being reported as
            // entropy failure, because a negative length or a null
            // destination is a guest/interpreter bug, not the condition
            // `CryptographicException` is meant to describe.
            let state, newPrngState =
                drawRandomBytesInto
                    ctx
                    "SystemNative_GetCryptographicallySecureRandomBytes"
                    state.Kernel.CryptoRandomState

            let state =
                state.MapKernel (fun kernel ->
                    { kernel with
                        CryptoRandomState = newPrngState
                    }
                )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_Free", [ ConcretePointer _ ], MethodReturnType.Void ->
            let ptr =
                NativeCall.managedPointerOfPointerArgument "SystemNative_Free" "ptr" instruction.Arguments.[0]

            // C `free(x)` is undefined unless `x` is exactly a pointer returned
            // by `malloc`/`calloc`/`realloc` (or null). Interior pointers like
            // `base + 4` must be rejected — silently freeing the whole block
            // would mask guest memory-corruption bugs.
            //
            // Accumulated in `int64`: this file is not `Checked`, so an `int` fold could wrap a
            // genuinely interior pointer back onto zero and free a block from the middle of it,
            // which is precisely the guest memory-corruption bug the check exists to expose
            // (issue #993).
            let rec projectionByteOffset (acc : int64) (ps : ByrefProjection list) : Result<int64, ByrefProjection> =
                match ps with
                | [] -> Ok acc
                | ByrefProjection.ReinterpretAs _ :: rest -> projectionByteOffset acc rest
                | ByrefProjection.ByteOffset n :: rest -> projectionByteOffset (acc + int64<int> n) rest
                | (ByrefProjection.Field _ as field) :: _ -> Error field

            let state =
                match ptr with
                // C `free(NULL)` is documented as a no-op. CoreLib's
                // NativeMemory.Free already filters null before reaching the
                // P/Invoke, but Marshal.FreeHGlobal does not, so honour the
                // C semantics here too.
                | ManagedPointerSource.Null -> state
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
                    match projectionByteOffset (int64<int> rootByteOffset) projs with
                    | Ok 0L -> IlMachineState.freeNativeMemory block state
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
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) ctx.Thread
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
                        LastSystemError = UnixError.toRawErrno UnixError.EINVAL
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | ValueSome signal ->
                state.MapKernel (fun kernel ->
                    { kernel with
                        Signals = SignalState.enable signal kernel.Signals
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) ctx.Thread
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
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) ->
                    FunctionPointerTarget.requireManaged operation target
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
        | Some "SystemNative_HandleNonCanceledPosixSignal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            // The BCL's managed `OnPosixSignal` calls this from a
            // thread-pool worker after all registered handlers have run
            // and none called `PosixSignalContext.Cancel = true`. Real
            // native code runs the signal's kernel-default disposition:
            // no-op for ignore/stop/continue defaults (SIGCHLD, SIGURG,
            // SIGWINCH, SIGTSTP, SIGTTIN, SIGTTOU, SIGCONT), and for
            // terminate-by-default signals it restores the original
            // `sigaction` and re-raises so the process exits with the
            // signal-default behaviour. PawPrint matches the no-op
            // branches exactly (there is nothing to do) and refuses the
            // terminate branch with a clear marker: signal-driven
            // process termination is a follow-up slice that needs a
            // `RunOutcome` variant or equivalent, not silently squashed
            // here.
            //
            // By contract the BCL only calls this with signos that
            // `SystemNative_GetPlatformSignalNumber` previously returned
            // non-zero for, so a signo arriving here must lie within
            // `(0, Signal.linuxSignalMax]` (modelled signals get a named
            // case; unmodelled-but-valid signos round-trip via
            // `Signal.Other`). Anything else indicates a guest bypassing
            // the standard registration path and we fail loudly rather
            // than silently dropping the request.
            let operation = "SystemNative_HandleNonCanceledPosixSignal"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]

            match Signal.ofPlatformSigno signo with
            | ValueNone ->
                failwith
                    $"%s{operation}: refusing to handle out-of-range signo %d{signo} (signos arriving here must lie within (0, Signal.linuxSignalMax]; this looks like a guest bypassing SystemNative_GetPlatformSignalNumber)"
            | ValueSome signal ->
                match Signal.defaultDisposition signal with
                | DefaultDisposition.Ignore
                | DefaultDisposition.Stop
                | DefaultDisposition.Continue ->
                    // Nothing to do: the runtime cannot stop or continue
                    // itself, and Ignore is literally a no-op. Matches
                    // the per-signal no-op branches in `pal_signal.c`'s
                    // `SystemNative_HandleNonCanceledPosixSignal` switch
                    // (and the implicit terminal-reinit call on SIGCONT
                    // is not relevant to PawPrint, which has no terminal).
                    NativeHandlerResult.completed state |> Some
                | DefaultDisposition.Terminate ->
                    // Mirrors `pal_signal.c`'s Terminate branch, which
                    // restores the original `sigaction` and calls
                    // `kill(g_pid, signalCode)` to let the kernel
                    // terminate the process with the signal-default
                    // exit status. PawPrint surfaces this as a
                    // dedicated `SignalTerminated` outcome so the App
                    // layer can compute the POSIX-conventional exit
                    // code (`128 + Signal.toLinuxSigno signal`) and
                    // distinguish signal-driven termination from a
                    // managed `Environment.Exit` call carrying the
                    // same exit code.
                    ExecutionResult.SignalTerminated (state, signal)
                    |> NativeHandlerResult.ofExecutionResult
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
