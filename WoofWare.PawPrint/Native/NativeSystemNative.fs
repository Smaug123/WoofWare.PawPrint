namespace WoofWare.PawPrint

open System
open System.Buffers.Binary
open System.Collections.Immutable

/// Why a file descriptor cannot be seeked, as a *fault* rather than as the errno
/// it becomes.
///
/// Not a `UnixError`, because `SystemNative_LSeek` orders the two faults
/// differently per platform: measured, Linux validates `whence` between them
/// while Darwin does not, so an ordering written over errnos would let a future
/// third fault inherit whichever position its errno's arm happened to occupy.
[<RequireQualifiedAccess>]
type internal DescriptorFault =
    /// No such descriptor in the process's table; `EBADF`. Precedes everything
    /// else on both platforms.
    | NotOpen
    /// The descriptor names something with no file offset — a pipe, which is
    /// what PawPrint models the standard streams as; `ESPIPE`.
    | NotSeekable

/// A buffer-pointer argument to a `SystemNative_*` entry point, classified as
/// far as a kernel's own address check can see it.
///
/// This is the same split `ManagedPointerSource.tryBitPatternBits` makes: a
/// byref names storage, while `Null` and `NativeIntPlaceholder` are bit
/// patterns naming none. A guest produces the latter only by hand-rolling a
/// P/Invoke, since the BCL's own wrappers pass real spans.
[<RequireQualifiedAccess>]
type internal BufferPointer =
    /// Real allocated guest storage, and so an address inside the user address
    /// space of any platform we model. Carried decomposed rather than as a whole
    /// `ManagedPointerSource` so that a bit pattern cannot be spelled here: the
    /// address check below treats every `Storage` as in range without asking.
    | Storage of root : ByrefRoot * projections : ByrefProjection list
    /// A raw address naming no storage. `RawAddress 0UL` is the null pointer,
    /// which some entry points must tell apart from an unmapped one because
    /// their C counterpart does.
    | RawAddress of address : uint64
    /// The address of a runtime data structure — a method table, a type handle,
    /// a GC handle — which PawPrint models symbolically and so has no number
    /// for.
    ///
    /// A user address, because on a real runtime it is ordinary allocated
    /// memory: `write(fd, (byte*)typeof(int).TypeHandle.Value, 0)` succeeds on
    /// the host. Not addressable byte by byte, though, so a caller that reaches
    /// the transfer itself must refuse rather than invent the bytes.
    | Symbolic of operation : string * argName : string * argument : CliType
    /// A value with no address behind it at all: the difference of two pointers
    /// into separate storages, which PawPrint keeps synthetic precisely because
    /// it has no number for it.
    ///
    /// Distinct from `Symbolic`, which names real memory whose address merely
    /// goes unmodelled. Nothing can be transferred through this, and a kernel
    /// that screens addresses up front cannot be asked about it either — but
    /// neither refusal belongs at classification time, because an entry point
    /// that never inspects its buffer still has to answer.
    | Unstatable of operation : string * argName : string * argument : CliType

/// What a `read` will operate on, once the descriptor's access mode has been
/// checked and before its buffer is screened.
///
/// Narrower than `OpenFileTarget`: it excludes the descriptors a read refuses
/// outright, so a handler that screens the buffer between those two steps — as
/// `vfs_read` does — has no unreachable arm left to write.
[<RequireQualifiedAccess>]
type internal ReadTarget =
    /// The read end of the pipe PawPrint models stdin as.
    | Stdin
    /// A file, at the offset its open file description currently holds.
    | File of inode : InodeNumber * offset : int64

[<RequireQualifiedAccess>]
module internal BufferPointer =
    /// The pointer this classification names, for a caller about to transfer
    /// bytes through it.
    ///
    /// `None` for every raw address, null included: real `write(2)` and
    /// `getcwd(3)` alike answer EFAULT for both, having performed no I/O, so an
    /// entry point that is about to dereference its buffer collapses them
    /// rather than aborting the interpreter. Callers whose C counterpart
    /// distinguishes them match on `RawAddress 0UL` instead.
    ///
    /// A symbolic address refuses instead of answering. EFAULT would be a wrong
    /// answer rather than an approximate one: a real runtime's method table is
    /// mapped and readable, so the host transfers those bytes, and PawPrint has
    /// no bytes to transfer.
    let dereferenceable (pointer : BufferPointer) : ManagedPointerSource option =
        match pointer with
        | BufferPointer.Storage (root, projections) -> Some (ManagedPointerSource.Byref (root, projections))
        | BufferPointer.RawAddress _ -> None
        | BufferPointer.Symbolic (operation, argName, argument) ->
            failwith
                $"%s{operation}: %s{argName} is %O{argument}, the address of a runtime data structure PawPrint models symbolically rather than as bytes. A real kernel would transfer the bytes at that address; PawPrint has none to transfer, so it cannot answer. Pass a buffer that names guest storage."
        | BufferPointer.Unstatable (operation, argName, argument) ->
            failwith
                $"%s{operation}: %s{argName} is %O{argument}, the difference of two pointers into separate storages, which names no address. Subtracting pointers that do not point into one object does not produce a buffer."

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
    /// Not assembly-qualified, unlike `PosixSignalParam` above: `Interop.Error`
    /// is `internal` to CoreLib, so requiring its assembly would leave this arm
    /// reachable only by real BCL code and hence untestable. The entry-point
    /// name already identifies the call uniquely.
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

    /// Classify a buffer-pointer argument, keeping the numeric address of the
    /// ones that have one.
    ///
    /// Pure inspection of the argument's shape: it resolves nothing to a cell,
    /// so an entry point whose C counterpart returns an error *without
    /// dereferencing the buffer* may classify first and still answer that error
    /// — a guest may legally hand such a call an unresolvable bit pattern like
    /// `(byte*)123`, and the real shim never touches it.
    ///
    /// The two encodings of one guest-level value agree here. A raw address can
    /// arrive as a `Verbatim` bit pattern or, after a managed conversion, as a
    /// `NativeIntPlaceholder`; both are `RawAddress`, so neither reaches storage
    /// resolution.
    let internal bufferPointerArgument (operation : string) (argName : string) (arg : CliType) : BufferPointer =
        let classify (ptr : ManagedPointerSource) : BufferPointer =
            match ptr with
            | ManagedPointerSource.Byref (root, projections) -> BufferPointer.Storage (root, projections)
            | ManagedPointerSource.Null -> BufferPointer.RawAddress 0UL
            // The placeholder's own contract is that it must never be
            // dereferenced; it exists to carry a bit pattern through a managed
            // reference.
            | ManagedPointerSource.NativeIntPlaceholder bits -> BufferPointer.RawAddress (uint64 bits)

        // The conversions below are reinterpretations, not range checks: a
        // guest's `(byte*)-1` is the top of the address space, which is exactly
        // the value an address check must see.
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer pointer ->
            // Matched without a catch-all so that a new `CliRuntimePointer` case
            // has to be classified here rather than falling into the
            // interpreter-bug arm below, where it would abort a call the host
            // answers.
            match pointer with
            | CliRuntimePointer.Managed ptr -> classify ptr
            | CliRuntimePointer.Verbatim bits -> BufferPointer.RawAddress (uint64 bits)
            | CliRuntimePointer.TypeHandlePtr _
            | CliRuntimePointer.TypeDescPtr _
            | CliRuntimePointer.FieldRegistryHandle _
            | CliRuntimePointer.MethodRegistryHandle _
            | CliRuntimePointer.MethodTablePtr _
            | CliRuntimePointer.MethodTableAuxiliaryDataPtr _
            | CliRuntimePointer.PerInstInfoPtr _
            | CliRuntimePointer.PerInstDictPtr _
            | CliRuntimePointer.GcHandlePtr _ -> BufferPointer.Symbolic (operation, argName, arg)
        | CliType.Numeric (CliNumericType.NativeInt source) ->
            // Exhaustive for the same reason as the pointer match above, and it
            // is reached: `(byte*)(delegate*<void>)&M` stays a
            // `NativeIntSource.FunctionPointer` rather than becoming a
            // `RuntimePointer`, so a buffer parameter does see these.
            match source with
            | NativeIntSource.ManagedPointer ptr -> classify ptr
            | NativeIntSource.Verbatim bits -> BufferPointer.RawAddress (uint64 bits)
            // An opaque hash is an exact bit pattern by its own contract, so it
            // is as good an address as any other number a guest invents.
            | NativeIntSource.OpaqueHashBits bits -> BufferPointer.RawAddress (uint64 bits)
            | NativeIntSource.FunctionPointer _
            | NativeIntSource.TypeHandlePtr _
            | NativeIntSource.TypeDescPtr _
            | NativeIntSource.MethodTablePtr _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _
            | NativeIntSource.PerInstInfoPtr _
            | NativeIntSource.PerInstDictPtr _
            | NativeIntSource.MethodHandlePtr _
            | NativeIntSource.FieldHandlePtr _
            | NativeIntSource.AssemblyHandle _
            | NativeIntSource.ModuleHandle _
            | NativeIntSource.MetadataImportHandle _
            | NativeIntSource.GcHandlePtr _
            | NativeIntSource.EventPipeProviderPtr _
            | NativeIntSource.EventPipeEventPtr _
            | NativeIntSource.LowLevelMonitorPtr _
            | NativeIntSource.WaitHandlePtr _ -> BufferPointer.Symbolic (operation, argName, arg)
            // Not an address at all, but classification must stay total: an
            // entry point that never inspects its buffer has to keep answering,
            // so the refusal waits until something actually needs the address.
            | NativeIntSource.SyntheticCrossArrayOffset _ -> BufferPointer.Unstatable (operation, argName, arg)
        | other ->
            failwith
                $"%s{operation}: expected %s{argName} to be a managed pointer, raw verbatim address, or null literal, got %O{other} (this is an interpreter bug)"

    /// Whether this platform's kernel refuses `bufferSize` bytes at this pointer
    /// before performing the operation at all.
    ///
    /// Only a raw address is ever refused: `BufferPointer.Storage` names real
    /// allocated guest memory, which is a user address by construction.
    let internal faultsBeforeOperation (kernel : EmulatedKernel) (buffer : BufferPointer) (bufferSize : int) : bool =
        System.Diagnostics.Debug.Assert (
            bufferSize >= 0,
            "faultsBeforeOperation: a negative size is the shim's own error and is refused before the kernel sees it"
        )

        match buffer with
        | BufferPointer.Storage _
        | BufferPointer.Symbolic _ -> false
        | BufferPointer.Unstatable (operation, argName, argument) ->
            // A kernel that screens up front compares this address against its
            // limit, and there is no address to compare — so the answer is not
            // "in range", it is unknown. A kernel that screens nothing asks
            // nothing, and the call proceeds to whatever short-circuit or
            // dereference comes next.
            if SimulatedUnixPlatform.screensUserBufferUpFront kernel.UnixPlatform then
                failwith
                    $"%s{operation}: %s{argName} is %O{argument}, the difference of two pointers into separate storages, and this platform screens a buffer's address before performing the operation. There is no address to screen, so PawPrint cannot say whether the kernel would accept it."
            else
                false
        | BufferPointer.RawAddress address ->
            UserBufferCheck.faultsBeforeOperation (EmulatedKernel.userBufferCheck kernel) address (uint64 bufferSize)

    /// Which way bytes move through a caller-supplied buffer.
    [<RequireQualifiedAccess>]
    type private BufferTransfer =
        | Into
        | OutOf

    /// Refuse a transfer whose bytes would not all land inside the storage the
    /// buffer names.
    ///
    /// Nothing a kernel does: `access_ok` bounds a range against the address
    /// space, never against the guest's own allocation, so a real kernel serves
    /// an over-long transfer by touching whatever follows the buffer. PawPrint's
    /// address space is a graph of typed cells with nothing following anything,
    /// so it has no bytes to touch — and reporting the guest's mistake is worth
    /// more here than reproducing the corruption would be.
    ///
    /// Only what has been established is refused: a buffer whose coordinate or
    /// whose storage's size cannot be derived is let past, to fail (or not) at
    /// the access itself.
    let private requireBufferRoom
        (ctx : NativeCallContext)
        (operation : string)
        (direction : BufferTransfer)
        (buffer : ManagedPointerSource)
        (byteCount : int)
        (state : IlMachineState)
        : unit
        =
        match StorageLocation.byteRangeFit ctx.BaseClassTypes state buffer byteCount with
        | StorageLocation.ByteRangeFit.Fits
        | StorageLocation.ByteRangeFit.Undecided -> ()
        | StorageLocation.ByteRangeFit.Escapes (storage, offset, extent) ->
            let verb, wouldTouch =
                match direction with
                | BufferTransfer.Into -> "write", "overwrite whatever follows that buffer"
                | BufferTransfer.OutOf -> "read", "send whatever follows that buffer"

            let byteCountOf (count : int64) : string =
                if count = 1L then "1 byte" else $"%d{count} bytes"

            failwith
                $"%s{operation}: the call would %s{verb} %s{byteCountOf (int64 byteCount)} starting at byte %d{offset} of %O{storage}, which spans %s{byteCountOf extent}, so the range leaves the storage the buffer names. A real kernel would %s{wouldTouch}; PawPrint's address space is a graph of typed cells, so there is nothing following it and no answer to give. Pass a buffer with room for the bytes requested."

    /// Write `bytes` through a caller-supplied `byte*`, whatever storage the
    /// pointer actually names (a `localloc` block, a pinned `byte[]`, native
    /// heap).
    ///
    /// `buffer` must not be null; that is the caller's business, because what a
    /// null buffer *means* differs per entry point (ERANGE here, EFAULT
    /// elsewhere). Room for the bytes is checked here rather than by the caller,
    /// because what must fit is what actually moves — a short read at
    /// end-of-file transfers nothing and so needs no room at all.
    let private writeBytesThrough
        (ctx : NativeCallContext)
        (operation : string)
        (buffer : ManagedPointerSource)
        (bytes : ImmutableArray<byte>)
        (state : IlMachineState)
        : IlMachineState
        =
        requireBufferRoom ctx operation BufferTransfer.Into buffer bytes.Length state

        // One cell at a time: the simulated address space is a graph of typed
        // cells rather than a flat byte array, so "memcpy into the caller's
        // buffer" is necessarily this per-byte walk;
        // `ManagedPointerByteView.addByteOffset` is what resolves each offset
        // back to a cell, whatever storage the pointer actually names.
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
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[i])))

        state

    /// Drain `byteCount` bytes from a caller-supplied `byte*`: the mirror of
    /// `writeBytesThrough`, with the same room requirement and the same per-byte
    /// walk.
    let private readBytesThrough
        (ctx : NativeCallContext)
        (operation : string)
        (buffer : ManagedPointerSource)
        (byteCount : int)
        (state : IlMachineState)
        : ImmutableArray<byte>
        =
        requireBufferRoom ctx operation BufferTransfer.OutOf buffer byteCount state

        let byteConcreteType =
            NativeCall.requiredByteConcreteType operation ctx.BaseClassTypes state

        let builder = ImmutableArray.CreateBuilder<byte> byteCount

        for i = 0 to byteCount - 1 do
            let src = ManagedPointerByteView.addByteOffset state byteConcreteType i buffer

            let cell =
                IlMachineState.readManagedByrefBytesAs
                    ctx.BaseClassTypes
                    state
                    src
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))

            match cell with
            | CliType.Numeric (CliNumericType.UInt8 b) ->
                builder.Add (UInt8Source.value $"%s{operation}: byte read at offset %d{i}" b)
            | other ->
                failwith
                    $"%s{operation}: byte read at offset %d{i} returned non-UInt8 cell %O{other} (this is an interpreter bug)"

        builder.MoveToImmutable ()

    /// Commit a write of `bytes` at `offset` to the regular file `inode`,
    /// together with the `mtime` and `ctime` it moves.
    ///
    /// `bytes` must not be empty: a zero-length write moves no timestamp at all,
    /// so each caller short-circuits it rather than passing it on.
    ///
    /// Does not touch the description's file offset. `write(2)` advances it and
    /// `pwrite(2)` does not, which is the whole difference between them, so it is
    /// each caller's business.
    let private commitFileWrite
        (operation : string)
        (fd : int)
        (inode : InodeNumber)
        (offset : int64)
        (bytes : ImmutableArray<byte>)
        (state : IlMachineState)
        : IlMachineState
        =
        let now = EmulatedKernel.fileTimestamp state.Kernel

        // A content-changing write strips a file's set-user-ID and set-group-ID
        // bits unless the writer is root; measured on both platforms.
        let privilege =
            if EmulatedKernel.isPrivileged state.Kernel then
                WritePrivilege.Privileged
            else
                WritePrivilege.Unprivileged

        match VirtualFileSystem.writeFile inode offset bytes privilege now state.Kernel.FileSystem with
        | Ok filesystem ->
            state.MapKernel (fun kernel ->
                { kernel with
                    FileSystem = filesystem
                }
            )
        | Error (FileWriteRefusal.WouldExceedMaxLength (offset, count)) ->
            failwith
                $"%s{operation}: fd %d{fd} asked to write %d{count} bytes at offset %d{offset} of inode %O{inode}, which would leave the file longer than the %d{VirtualFileSystem.maxFileLength} bytes PawPrint can represent. A real filesystem answers this without difficulty — measured on ext4 and APFS alike, a one-byte write at offset 2^40 succeeds and leaves a sparse 1 TB file — so this is a limit of the model, and refusing is better than reporting an errno no kernel would have produced."

    /// Turn the NUL-terminated bytes a guest passed as a pathname into a
    /// `UnixPath`, applying the length rule a kernel applies at *its* boundary.
    ///
    /// Takes bytes rather than machine state, so the boundary — the
    /// one part of the length rules that the resolver can never see — is
    /// testable without a heap. `readGuestPathBytes` is the half that needs a
    /// machine.
    ///
    /// The three stages must run in this order:
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
    /// The limit counts the NUL, and `readNullTerminatedBytes` has already
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
    /// The common case never reaches it: CoreLib `Path.GetFullPath`s
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
    /// remainder, which this walk does not have: it holds a `PathComponent
    /// list`. Threading a length through it is its own change, and the trigger
    /// has to be designed against measurement rather than arithmetic: probing
    /// showed XNU consumes `//` runs before splicing, contradicting a
    /// plausible-looking argument about how the kernel counts.
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
    /// declaration is merely its view of the same bytes; deriving the offsets
    /// from the pointee would honour that declaration's names and order rather
    /// than the ABI. The pointee handle is still used, for the one thing it is
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
    /// The output struct is written as a byte image at ABI offsets; see
    /// `writeFileStatus`.
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
        match
            bufferPointerArgument operation "path" instruction.Arguments.[0]
            |> BufferPointer.dereferenceable
        with
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
        match
            bufferPointerArgument operation "output" instruction.Arguments.[1]
            |> BufferPointer.dereferenceable
        with
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
        | Some "SystemNative_GetMaximumAddressSize",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_GetMaximumAddressSize(void)` (pal_networking.c)
            // is `return sizeof(struct sockaddr_storage);` — a compile-time
            // constant of the shim, with no socket, no errno and no state
            // involved. `SimulatedUnixPlatform.maximumSocketAddressSize` records
            // the number and why it takes no flavour.
            //
            // The sole managed caller is `System.Net.Sockets.SocketPal`'s class
            // initialiser, which latches it into `SocketPal.MaximumAddressSize`
            // and sizes every address buffer by it. Note the address *sizes* the
            // managed `SocketAddress` type uses come from a different entry point,
            // `SystemNative_GetSocketAddressSizes`, so serving this one says
            // nothing about those.
            pushInt32 SimulatedUnixPlatform.maximumSocketAddressSize ctx |> Some
        | Some "SystemNative_PlatformSupportsDualModeIPv4PacketInfo",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_PlatformSupportsDualModeIPv4PacketInfo(void)`
            // (pal_networking.c) is nothing but `return 1;` or `return 0;` under
            // an `#if` on how the shim was built — no socket, no errno and no
            // state, like `SystemNative_GetMaximumAddressSize` above. Unlike that
            // one it takes the flavour;
            // `SimulatedUnixPlatform.supportsDualModeIPv4PacketInfo` records the
            // cmake condition it comes from and why we answer as the platform we
            // impersonate rather than conservatively.
            //
            // Reported to CoreLib as an `int` that
            // `SocketPal.GetPlatformSupportsDualModeIPv4PacketInfo` compares
            // against zero, so any non-zero value would do; we answer 1 because
            // that is the literal upstream returns.
            //
            // Together with `SystemNative_GetMaximumAddressSize` this supplies
            // both of the native calls in `System.Net.Sockets.SocketPal`'s class
            // initialiser; its one remaining statement derives
            // `SelectOverPollIsBroken` from `OperatingSystem.IsMacOS` and
            // friends, which reach no native code.
            //
            // The latched result has exactly two readers, both branching on
            // `SocketPal.SupportsDualModeIPv4PacketInfo`. When it is false,
            // `CheckDualModePacketInfoSupport` throws
            // `PlatformNotSupportedException` out of `Socket.ReceiveMessageFrom`
            // and `ReceiveMessageFromAsync` on a dual-mode socket (the SR key is
            // named `..._dualmode_receivefrom_notsupported`, but plain
            // `ReceiveFrom` never consults it), and
            // `SetReceivingDualModeIPv4PacketInformation` becomes a no-op. When
            // it is true, that second one sets `SocketOptionLevel.IP` /
            // `SocketOptionName.PacketInformation`.
            let supported =
                SimulatedUnixPlatform.supportsDualModeIPv4PacketInfo state.Kernel.UnixPlatform

            pushInt32 (if supported then 1 else 0) ctx |> Some
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
            // in `pal_time.c` -- *not* `pal_process.c`, where you might
            // reasonably look first. That tree is outside our sparse
            // dotnet/runtime checkout, so fetch the file at the pinned commit
            // rather than looking in `$DOTNET_RUNTIME_SRC`.)
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
            // We return a constant 0.
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

            // sizeof(ProcessCpuInformation) = 3 fields * sizeof(ulong) = 24 bytes, verified
            // against both the managed declaration (via IlDump) and the native struct in
            // `pal_time.h` (no padding, three `uint64_t`).
            //
            // Known tech debt: this width is a literal with no structural link to either
            // declaration, and nothing would catch it drifting. The managed-BCL drift test
            // does not cover native `pal_*` headers. It is a literal because the boundary here
            // is untyped (`void*`), so deriving it would mean resolving
            // `ProcessCpuInformation`'s own ConcreteTypeHandle purely to describe three
            // all-zero `ulong`s. `writeBytesThrough` bounds the write against the storage the
            // pointer names, which catches a width larger than a caller's whole buffer but
            // not one that merely overruns this struct inside a larger block.
            writeBytesThrough
                ctx
                "SystemNative_GetCpuUtilization"
                ptr
                (ImmutableArray.CreateRange (Array.zeroCreate<byte> 24))
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
            // We report a real per-thread placement, not the `-1` "platform
            // lacks sched_getcpu" sentinel (legitimate on macOS, and handled by
            // CoreLib via a `Environment.CurrentManagedThreadId` fallback):
            // PawPrint reports a Linux platform identity through
            // `SystemNative_GetUnixRelease`, and on Linux the call works.
            //
            // The value is fixed at thread creation by
            // `EmulatedKernel.cpuForRotation` and stored in
            // `ThreadState.Cpu`; see there for why round-robin, and why
            // "pinned to" and "currently running on" coincide under a
            // scheduler that never migrates threads. It is returned verbatim
            // rather than re-derived here: `effectiveProcessorCount` reads the
            // kernel's env table live, so if environment mutation is ever
            // added, a re-derivation could silently turn a guest's shard index
            // into an out-of-range one.
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
            // cannot determine a thread id" sentinel (which CoreLib handles by
            // substituting `Environment.CurrentManagedThreadId`): PawPrint
            // presents a Linux platform identity (`SimulatedUnixPlatform`), and
            // on Linux this call works. `GetUInt64OSThreadId` has no sentinel,
            // so a real id has to exist regardless.
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

            // Classifying the pointer inspects the argument's shape and
            // resolves nothing to storage, so it is safe this early. Every
            // failure below is decided *without* that resolution, because the C
            // decides them without dereferencing the buffer: the negative-size
            // guard runs before `getcwd` is even called, and `getcwd` itself
            // validates the size and compares it against the path length before
            // it writes a byte. A guest that hand-rolls this P/Invoke may
            // therefore legally pass a bit pattern PawPrint cannot resolve —
            // `GetCwd((byte*)123, 0)` returns EINVAL on the real runtime.
            let bufferPointer = bufferPointerArgument operation "buffer" bufferArgument

            let bufferIsNull =
                match bufferPointer with
                | BufferPointer.RawAddress address -> address = 0UL
                | BufferPointer.Storage _
                | BufferPointer.Symbolic _
                | BufferPointer.Unstatable _ -> false

            if bufferSize < 0 then
                // The shim's own guard. It *also* `assert`s this, so a
                // checked native build would abort instead; EINVAL is what a
                // guest running against a retail runtime can observe, and it
                // is the only one of the two behaviours we can reproduce.
                fail UnixError.EINVAL
            elif bufferIsNull then
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

            // The buffer is dereferenced from here on, so this is
            // where it must resolve to storage. A pointer that does not is an
            // unmapped address (null was already handled above), which real
            // `getcwd` reports as EFAULT after writing nothing — the
            // size checks above come first, so `getcwd((byte*)123, 1)` is
            // ERANGE rather than EFAULT, as on the real kernel.
            match BufferPointer.dereferenceable bufferPointer with
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

            // The flags that would *create or destroy* content are refused loudly
            // rather than answered: nothing in the emulated filesystem can yet
            // bind a new name or shorten a file, so an `O_TRUNC` honoured as a
            // no-op, or an `O_CREAT` that quietly reported ENOENT, would hand a
            // guest a descriptor whose contract PawPrint cannot keep. The crash
            // names the flag, which is more use than a generic "unimplemented
            // native".
            //
            // Known over-refusal, recorded because a green suite cannot show it:
            // `FileMode.OpenOrCreate` sets `O_CREAT` even for a file that
            // *exists*, which is a case this handler could answer correctly.
            let refuse (flag : string) : NativeHandlerResult option =
                failwith
                    $"%s{operation}: the guest asked for %s{flag}, but PawPrint cannot yet create or truncate a file — nothing binds a new name in the emulated filesystem or shortens an existing one, so a descriptor opened this way could not honour its contract. Implement creation and truncation (issue #956) before opening one."

            if flags &&& palCreat <> 0 then
                refuse "O_CREAT"
            elif flags &&& palExcl <> 0 then
                refuse "O_EXCL"
            elif flags &&& palTrunc <> 0 then
                refuse "O_TRUNC"
            else

            let requestedAccess =
                if accessMode = palWrOnly then FileAccessMode.WriteOnly
                elif accessMode = palRdWr then FileAccessMode.ReadWrite
                else FileAccessMode.ReadOnly

            // `O_CLOEXEC` is accepted and ignored: it sets `FD_CLOEXEC`, which
            // matters only across `exec`, and PawPrint models neither `fork`
            // nor `exec` (see `FileDescriptorRegistry`). `O_SYNC` likewise — it
            // governs when a write reaches storage rather than whether it is
            // visible, and this filesystem holds its bytes in memory, so every
            // write is already as durable as the model gets.
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

            match
                bufferPointerArgument operation "path" instruction.Arguments.[0]
                |> BufferPointer.dereferenceable
            with
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

            let entry =
                match VirtualFileSystem.tryGet inode state.Kernel.FileSystem with
                | Some entry -> entry
                | None ->
                    failwith
                        $"%s{operation}: resolution returned inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

            match entry.Content with
            | InodeContent.Symlink _ ->
                // Only reachable under `O_NOFOLLOW`, which is what
                // `NoFollowFinal` above selects: without it the resolver would
                // have followed the link (or failed ENOENT on a dangling one).
                // ELOOP rather than anything more specific is what both Unixes
                // answer, and is what `SafeFileHandle.OpenNoFollowSymlink`
                // reads back to decide a path was a symlink without racing.
                fail UnixError.ELOOP
            | InodeContent.Directory _ when FileAccessMode.permitsWrite requestedAccess ->
                // Measured on both platforms, for `O_WRONLY` and `O_RDWR` alike,
                // and at uid 0 as well as uid 1000: a directory cannot be opened
                // for writing, and this beats the EACCES check below (a
                // mode-0000 directory opened `O_WRONLY` is EISDIR, not EACCES).
                // CoreLib *depends* on it rather than merely tolerating it —
                // `SafeFileHandle.Init` skips its own directory check entirely
                // when write access was asked for, on the strength of "open will
                // have failed with EISDIR" (SafeFileHandle.Unix.cs:319).
                //
                // This is also what makes every writable descriptor name a
                // regular file, which `VirtualFileSystem.writeFile` relies on.
                fail UnixError.EISDIR
            | InodeContent.RegularFile _
            | InodeContent.Directory _ ->

            // A directory opens perfectly well for *reading*, and CoreLib
            // *depends* on that: `SafeFileHandle.Init` opens, then `FStat`s,
            // and raises `UnauthorizedAccessException` on seeing `S_IFDIR`, so
            // refusing here would give `File.ReadAllBytes("d")` the wrong
            // exception. The type check belongs in what `FStat` reports.
            let permissionBits =
                match VirtualFileSystem.permissions entry with
                | InodePermissions.Stored bits -> PermissionBits.toInt bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"%s{operation}: inode %O{inode} reports platform-default symlink permissions, but the symlink arm above answered ELOOP for every link (this is an interpreter bug)."

            // What `open(2)` itself checks: whether this process may open *this
            // object* for the access it asked for. Measured identically on macOS
            // and Linux, at uid 1000:
            //
            //   mode   O_RDONLY  O_WRONLY  O_RDWR
            //   0644   ok        ok        ok
            //   0444   ok        EACCES    EACCES
            //   0200   EACCES    ok        EACCES
            //   0000   EACCES    EACCES    EACCES
            //
            // Only the owner triple is ever consulted, and that is exact rather
            // than a simplification: `stat` reports `Kernel.UserId` as *every*
            // inode's `st_uid`, so the emulated process owns everything it can
            // see and the group and other triples can never be the applicable
            // ones.
            let neededBits =
                (if FileAccessMode.permitsRead requestedAccess then
                     0o400
                 else
                     0)
                ||| (if FileAccessMode.permitsWrite requestedAccess then
                         0o200
                     else
                         0)

            // Root gets read and write whatever the mode says — measured on Linux
            // as uid 0, where a mode-0000 file opens for writing. (Only *execute*
            // still needs a bit set for root, and `open` never asks for it.)
            let privileged = EmulatedKernel.isPrivileged state.Kernel

            if not privileged && permissionBits &&& neededBits <> neededBits then
                fail UnixError.EACCES
            else

            // The *search* half of the permission rule is still missing, and
            // belongs in the resolver rather than here: every component of every
            // path needs it, so `Stat`, `LStat` and `ReadLink` all owe the same
            // answer. Until it lands, a seed can describe a directory whose
            // owner-search bit is clear and PawPrint will walk through it anyway.
            let fd, registry =
                FileDescriptorRegistry.openFile inode requestedAccess state.Kernel.FileDescriptors

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
                // itself, so this is a hand-rolled P/Invoke or a new
                // code path, and either wants a decision rather than a guess.
                failwith
                    $"%s{operation}: fd %d{fd} is the standard stream %O{role}, and PawPrint holds no inode for one. Every field `fstat` owes a pipe would be invented here; decide what a stream's `struct stat` is (issue #956) rather than guessing."
            | Some OpenFileObject.AnonymousInode ->
                // Refused for the same reason as the standard streams above: an
                // epoll instance is an anonymous kernel object, so PawPrint
                // holds no inode to report and every field would be invented.
                //
                // Measured, the two platforms share not one field, and Linux's
                // identity fields are facts about the machine that produced
                // them rather than portable ones — which is precisely what a
                // deterministic replay must not depend on. Linux gives
                // `st_mode` 0600 (permission bits, *no* file-type bits),
                // `st_nlink` 1, `st_blksize` 4096, and a real anon-inode
                // `st_dev`/`st_ino`; Darwin gives `st_mode` S_IFIFO (no
                // permission bits), `st_nlink` 0, `st_blksize` 32, and zero for
                // both identity fields.
                failwith
                    $"%s{operation}: fd %d{fd} is a socket event port, an anonymous kernel object for which PawPrint holds no inode. Every field `fstat` owes one would be invented here, and the platforms agree on none of them; decide what an inode-free descriptor's `struct stat` is — for streams, ports and sockets together (issue #956) — rather than guessing."
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

            match
                bufferPointerArgument operation "output" instruction.Arguments.[1]
                |> BufferPointer.dereferenceable
            with
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
            // disagree, and not only about errno numbering. PawPrint
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
                match OpenFileDescription.object description with
                | OpenFileObject.StandardStream role ->
                    refuseDarwin
                        $"fd %d{fd} is the standard stream %O{role}, which PawPrint models as a pipe. Linux permits `flock` on a pipe and returns 0; Darwin refuses it with ENOTSUP (raw 45, and note Darwin numbers ENOTSUP and EOPNOTSUPP differently, 45 against 102, while Linux gives both 95)."
                | OpenFileObject.AnonymousInode ->
                    // Same divergence as the pipe above, and refused for the
                    // same reason rather than reported: measured, `flock` on a
                    // kqueue is ENOTSUP for LOCK_SH, LOCK_EX and LOCK_UN alike,
                    // where Linux's epoll descriptor takes the lock and returns
                    // 0. Reporting the errno would model one row of Darwin's
                    // `flock` while the rest of it stays unmodelled.
                    refuseDarwin
                        $"fd %d{fd} is a socket event port. Linux permits `flock` on an epoll descriptor and returns 0; Darwin refuses it on a kqueue with ENOTSUP (raw 45), for every operation including LOCK_UN."
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
                    // deadlocked, and a real kernel duly hangs it forever. The
                    // refusal must never convert the request into a
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
            // bad fd, which real kernels would report as EBADF — a known
            // over-refusal on a two-fault input, recorded here because a green
            // suite cannot show it.
            //
            // CoreLib never sends one: every caller is `RandomAccess`, whose
            // sizes come from span lengths.
            if bufferSize < 0 then
                failwith
                    $"%s{operation}: fd %d{fd} was given bufferSize %d{bufferSize}, which is negative. The C shim casts that to an unsigned ~4 GB count rather than rejecting it (unlike SystemNative_Read, which goes through Common_Read and answers EINVAL), and what a kernel then does is not a fact PawPrint can state: measured, macOS answers EINVAL and Linux answers EFAULT, Linux's answer depending on how far the guest's buffer happens to be mapped. Pass a non-negative size."
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
            // `EISDIR` follows the offset check on *both* platforms, so
            // only the descriptor and seekability steps actually move; that is
            // why one flag suffices rather than two separate orderings.
            let offsetCheckedBeforeDescriptor =
                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                | SimulatedUnixFlavour.Linux -> true
                | SimulatedUnixFlavour.Darwin -> false

            if offsetCheckedBeforeDescriptor && offsetInvalid then
                fail UnixError.EINVAL
            else

            match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
            | None -> fail UnixError.EBADF
            | Some description ->

            // Whether this description was opened for reading at all. Both arms
            // below need it and neither may guess: for a standard stream it
            // breaks the ESPIPE/EBADF tie, and for a regular file it is the whole
            // answer.
            let readable = FileAccessMode.permitsRead description.AccessMode

            match description.Target with
            | OpenFileTarget.StandardStream _ ->
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
                // `SystemNative_Read`. The Darwin answer for stdout/stderr does
                // *not* get that retry, EBADF not being one of the errnos that
                // clears the flag.
                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                | SimulatedUnixFlavour.Darwin when not readable -> fail UnixError.EBADF
                | SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Linux -> fail UnixError.ESPIPE
            | OpenFileTarget.SocketEventPort ->
                // Unseekable on both platforms, with no tie to break: a port is
                // open for reading (`ReadWrite`), so Darwin's unreadability arm
                // above cannot apply. Measured, `pread(port, buf, 8, 0)` and
                // `pread(port, buf, 0, 0)` are both ESPIPE on both platforms,
                // and so is `pread(port, (void*)-1, 8, 0)` — unseekability
                // precedes the buffer screen.
                fail UnixError.ESPIPE
            | OpenFileTarget.File (inode, _) ->

            // A descriptor not open for reading: EBADF on both platforms, which
            // is `vfs_read`'s answer for a file whose `FMODE_READ` is clear.
            //
            // Ahead of Darwin's offset check rather than after it, and measured:
            // `pread(wronlyFd, buf, 4, -1)` is EBADF on Darwin but EINVAL on
            // Linux, so on Darwin the descriptor's access mode is settled before
            // the offset is looked at — exactly as its seekability is above.
            // On Linux this ordering cannot be observed, the offset check having
            // already run.
            if not readable then
                fail UnixError.EBADF
            else if

                // Darwin's turn to validate the offset: it has now resolved the
                // descriptor, its seekability and its access mode, which is exactly
                // the window in which it differs from Linux. On Linux this cannot
                // fire, because the check above already did.
                not offsetCheckedBeforeDescriptor && offsetInvalid
            then
                fail UnixError.EINVAL
            else

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            // `ksys_pread64` reaches `vfs_read` only after the descriptor and
            // its seekability, and `vfs_read` screens the buffer before the file
            // operation — so on Linux this beats EISDIR and fires even when the
            // window below would have transferred nothing. Darwin screens
            // nothing here and discovers a bad address at the copy.
            //
            // `vfs_read`'s own EBADF for a descriptor not open for reading is the
            // check just above, which precedes this one: measured,
            // `pread(wronlyFd, (void*)-1, 4, 0)` is EBADF rather than EFAULT.
            if faultsBeforeOperation state.Kernel buffer bufferSize then
                fail UnixError.EFAULT
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

            // A buffer that survived the check above is resolved to storage only
            // on the path that actually writes through it. That is not an
            // optimisation: a kernel faults in `copy_to_user`, so a call that
            // transfers nothing never touches the buffer, and
            // `pread(fd, NULL, 5, offsetAtEof)` returns 0 rather than EFAULT —
            // measured on both platforms, and easy to get wrong by validating
            // arguments up front. `NULL` is an ordinary user address; what the
            // screen above rejects is a range leaving the user address space.
            if transfer = 0 then
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            else

            // The screen above bounds the buffer's *address*, which is all a
            // kernel checks: `access_ok` compares a range against the address
            // space, never against the guest's own allocation. Whether the bytes
            // fit in the storage the buffer names is a separate question, asked
            // by `writeBytesThrough` of what actually moves rather than here of
            // what was requested — a read at end-of-file transfers nothing and
            // so needs no room at all.
            match BufferPointer.dereferenceable buffer with
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
        // `int32_t SystemNative_PWrite(intptr_t fd, void* buffer, int32_t
        // bufferSize, int64_t fileOffset)` (pal_io.c:1859): `pwrite(2)` verbatim
        // with an EINTR retry, and — like `SystemNative_PRead`, and unlike
        // `SystemNative_Write` — no `Common_Write` wrapper, so no negative-size
        // guard of its own beyond a debug-only `assert`.
        //
        // This is the entry point the BCL's whole write path goes through:
        // `RandomAccess.WriteAtOffset` prefers it for any handle that supports
        // random access, and falls back to `SystemNative_Write` only on ENXIO or
        // ESPIPE (RandomAccess.Unix.cs:113).
        | Some "SystemNative_PWrite",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_PWrite"
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

            let succeed (count : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim count)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Refused before anything else, for the reason `SystemNative_PRead`
            // gives at length: the C casts a negative size to an unsigned ~4 GB
            // count, and what a kernel then does depends on how far the guest's
            // buffer happens to be mapped — which PawPrint does not model to that
            // fidelity. CoreLib never sends one, every caller's size coming from a
            // span length.
            if bufferSize < 0 then
                failwith
                    $"%s{operation}: fd %d{fd} was given bufferSize %d{bufferSize}, which is negative. The C shim casts that to an unsigned ~4 GB count rather than rejecting it (unlike SystemNative_Write, which goes through Common_Write and answers ERANGE), and what a kernel then does is not a fact PawPrint can state. Pass a non-negative size."
            else if

                // **Ahead of the descriptor, on both platforms** — which is where
                // `pwrite` differs from `pread`, and it is measured rather than
                // assumed. Every two-fault row is EINVAL on Linux *and* Darwin:
                //
                //   input                          Linux    Darwin
                //   negative offset + bad fd       EINVAL   EINVAL
                //   negative offset + pipe         EINVAL   EINVAL
                //   negative offset + O_RDONLY fd  EINVAL   EINVAL
                //
                // For `pread`, Darwin resolves the descriptor first and answers EBADF
                // or ESPIPE for the same shapes, so `SystemNative_PRead` needs a
                // platform flag here and this does not. Do not copy that flag over.
                fileOffset < 0L
            then
                fail UnixError.EINVAL
            else

            match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
            | None -> fail UnixError.EBADF
            | Some description ->

            let writable = FileAccessMode.permitsWrite description.AccessMode

            match description.Target with
            | OpenFileTarget.StandardStream _ ->
                // The mirror of `SystemNative_PRead`'s tie: `pwrite` needs a
                // seekable object, and PawPrint models the standard streams as
                // pipes, so stdin fails *two* tests at once — it is neither
                // seekable nor open for writing. Measured:
                //
                //   descriptor                        Linux    Darwin
                //   pipe write end (unseekable)       ESPIPE   ESPIPE
                //   pipe read end (also unwritable)   ESPIPE   EBADF
                //   regular file O_RDONLY (seekable)  EBADF    EBADF
                //
                // Linux lets unseekability win while Darwin lets unwritability
                // win, exactly as they do for `pread`; the third row is the
                // control showing this is about the tie rather than about
                // writability generally.
                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                | SimulatedUnixFlavour.Darwin when not writable -> fail UnixError.EBADF
                | SimulatedUnixFlavour.Darwin
                | SimulatedUnixFlavour.Linux -> fail UnixError.ESPIPE
            | OpenFileTarget.SocketEventPort ->
                // A port is unseekable on both platforms, and — unlike a
                // standard stream — there is no tie to break, because it is open
                // for writing (`ReadWrite`, see
                // `FileDescriptorRegistry.createSocketEventPort`). Measured,
                // `pwrite(port, buf, 8, 0)` is ESPIPE on both, as is
                // `pwrite(port, buf, 0, 0)` — so the zero-length shortcut does
                // not apply either.
                fail UnixError.ESPIPE
            | OpenFileTarget.File (inode, _) ->

            // `vfs_write`'s EBADF for a descriptor not open for writing, which
            // precedes both the buffer screen and the zero-size no-op: measured,
            // `pwrite(rdonlyFd, (void*)-1, 4, 0)` is EBADF rather than EFAULT and
            // `pwrite(rdonlyFd, buf, 0, 0)` is EBADF rather than 0.
            //
            // This is also what makes a directory descriptor unreachable below:
            // one can only be opened `O_RDONLY`, `SystemNative_Open` answering
            // EISDIR for every write access mode.
            if not writable then
                fail UnixError.EBADF
            else

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            // Linux screens the buffer's address before performing the operation,
            // so this fires even for a zero-length write: measured,
            // `pwrite(f, (void*)-1, 0, 0)` is EFAULT there and 0 on macOS. Darwin
            // screens nothing and discovers a bad address at the copy.
            if faultsBeforeOperation state.Kernel buffer bufferSize then
                fail UnixError.EFAULT
            else if

                // A no-op on both platforms, and specifically one that leaves the
                // inode alone: measured, a zero-length write moves neither `mtime` nor
                // `ctime` and does not extend the file, even at an offset far past its
                // end. The buffer is not resolved to storage, because nothing is read
                // through it — `NULL` is an ordinary user address, so it reaches here
                // rather than being screened above.
                bufferSize = 0
            then
                succeed 0 state
            else

            match BufferPointer.dereferenceable buffer with
            | None -> fail UnixError.EFAULT
            | Some buffer ->

            let bytes = readBytesThrough ctx operation buffer bufferSize state

            // Never short: PawPrint's filesystem cannot run out of space, and
            // there is no signal that could interrupt the copy part-way.
            commitFileWrite operation fd inode fileOffset bytes state |> succeed bufferSize
        // `int32_t SystemNative_Read(intptr_t fd, void* buffer, int32_t
        // bufferSize)` (pal_io.c:1178) forwards to `Common_Read`
        // (pal_io_common.h:36), which rejects a negative size itself and then
        // calls `read(2)` with an EINTR retry.
        //
        // The counts, the short read at EOF, EBADF, EISDIR, EFAULT and the
        // untouched buffer when nothing transfers were measured identically on
        // Linux and macOS. The one place the platforms part company is *when*
        // the buffer is screened, and it is visible on single-fault inputs
        // rather than only on pathological ones (Linux / macOS):
        //
        //   read(f, (void*)-1, 5) at EOF          EFAULT / 0
        //   read(f, (void*)-1, 0)                 EFAULT / 0
        //   read(dir, (void*)-1, 5)               EFAULT / EISDIR
        //   read(f, 0xffffffffffff, 5) at EOF     EFAULT / 0
        //   read(f, 0x7fff00000000, 5) at EOF     0      / 0
        //   read(f, NULL, 5) at EOF               0      / 0
        //   read(badfd, (void*)-1, 5)             EBADF  / EBADF
        //
        // Linux's `vfs_read` runs `access_ok(buf, count)` between the
        // descriptor's access-mode check and the file operation, so its order is
        // fd -> readability -> address range -> kind -> window -> copy.
        // `SimulatedUnixPlatform.userBufferCheck` carries the rule, including
        // which ranges each platform accepts: the fourth and fifth rows above
        // straddle x86-64's `TASK_SIZE_MAX`, and an arm64 kernel accepts both.
        //
        // This handler follows that factoring rather than deciding readability
        // and the file operation together, so that the screen appears exactly
        // once.
        | Some "SystemNative_Read",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_Read"
            let fd = fdArgument operation instruction.Arguments.[0]
            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[2]

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

            let succeed (count : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim count)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // `Common_Read`'s own guard, and hence *ahead of the descriptor*:
            // the C returns before `ToFileDescriptor` is ever evaluated, so
            // `Read(badfd, buf, -1)` is EINVAL rather than EBADF. That ordering
            // is a fact about the shim rather than about any kernel, which is
            // why it can be stated without a platform.
            //
            // EINVAL, not ERANGE: `Common_Write` answers ERANGE for the
            // same mistake, and the asymmetry is upstream's rather than a typo
            // here (pal_io_common.h:41-45 against :59-63).
            if bufferSize < 0 then
                fail UnixError.EINVAL
            else

            // The descriptor's access mode, which `vfs_read` decides before it
            // screens the buffer: measured on both platforms,
            // `read(wronlyFd, (void*)-1, 4)` is EBADF rather than EFAULT, and
            // even `read(wronlyFd, buf, 0)` is EBADF rather than a no-op.
            let target : Result<ReadTarget, UnixError> =
                match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
                | None -> Error UnixError.EBADF
                | Some description ->

                if not (FileAccessMode.permitsRead description.AccessMode) then
                    // A regular file opened `O_WRONLY` and a pipe's write end
                    // alike: EBADF on both platforms. `read` has no seekability
                    // requirement, so unlike `pread` there is no tie for the
                    // platforms to break differently.
                    Error UnixError.EBADF
                else

                match description.Target with
                | OpenFileTarget.StandardStream FileDescriptorRole.StandardInput -> Ok ReadTarget.Stdin
                | OpenFileTarget.StandardStream role ->
                    failwith
                        $"%s{operation}: fd %d{fd} names standard stream %O{role}, whose access mode permits reading. PawPrint models the output streams as the write ends of pipes, so only stdin is readable (this is an interpreter bug)."
                | OpenFileTarget.SocketEventPort ->
                    // An epoll instance has no read operation, so the read is
                    // refused for the *kind* of object rather than for the
                    // access mode — which is why the port is `ReadWrite` and
                    // still gets here rather than being EBADF above. The two
                    // platforms name that refusal differently: measured, Linux
                    // answers EINVAL (`vfs_read`'s `FMODE_CAN_READ` test) and
                    // Darwin answers ENXIO.
                    //
                    // Placed in this classification rather than after the buffer
                    // screen because it precedes it on both: measured,
                    // `read(port, (void*)-1, 8)` is EINVAL on Linux and ENXIO on
                    // Darwin, not EFAULT. Length is irrelevant too —
                    // `read(port, buf, 0)` gives the same answer as a non-zero
                    // length, unlike stdin's zero-return shortcut below.
                    match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                    | SimulatedUnixFlavour.Linux -> Error UnixError.EINVAL
                    | SimulatedUnixFlavour.Darwin -> Error UnixError.ENXIO
                | OpenFileTarget.File (inode, offset) -> Ok (ReadTarget.File (inode, offset))

            match target with
            | Error error -> fail error
            | Ok target ->

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            // Everything below this point is the file operation, which on Linux
            // the buffer screen precedes: hence EFAULT ahead of both EISDIR and
            // stdin's end-of-file, and a fault even for a zero-length request.
            // Darwin screens nothing here, so its answers come from the
            // operation itself.
            if faultsBeforeOperation state.Kernel buffer bufferSize then
                fail UnixError.EFAULT
            else

            match target with
            | ReadTarget.Stdin ->
                // **Immediate EOF, and this is a claim about the launch rather
                // than a fallback.** PawPrint models stdin as the read end of a
                // pipe whose write end was closed by whoever started the
                // process, so there is nothing to read and never will be.
                //
                // That is exactly the shape the differential oracle launches
                // guests in: `RealRuntime` redirects all three streams and then
                // closes the child's stdin immediately, so a guest that reads fd
                // 0 gets 0 under real .NET too. The alternative — an open write
                // end nobody writes to — would *block*, which PawPrint has no
                // way to represent and which would make the oracle hang rather
                // than answer.
                //
                // The buffer is not resolved to storage: measured on both
                // platforms, a read that returns end-of-file never touches it,
                // so `read(0, NULL, 5)` is 0 rather than EFAULT. Same rule as
                // the transfer-window shortcut below.
                //
                // Seeding stdin content is a separate feature; when it lands it
                // changes this one sentence — "the write end is closed at
                // launch" — rather than this arm's structure.
                succeed 0 state
            | ReadTarget.File (inode, offset) ->

            let entry =
                match VirtualFileSystem.tryGet inode state.Kernel.FileSystem with
                | Some entry -> entry
                | None ->
                    failwith
                        $"%s{operation}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."

            match entry.Content with
            | InodeContent.Directory _ ->
                // EISDIR on both, and ahead of the buffer: measured,
                // `read(dir, NULL, 5)` is EISDIR rather than EFAULT.
                fail UnixError.EISDIR
            | InodeContent.Symlink _ ->
                failwith
                    $"%s{operation}: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what reading a link through a descriptor means (issue #956)."
            | InodeContent.RegularFile (contents, _) ->

            // The same window `pread` computes, from the description's offset
            // rather than from an argument — which is the entire difference
            // between the two syscalls.
            let transfer = VirtualFileSystem.readTransferCount offset bufferSize contents.Length

            if transfer = 0 then
                // Nothing moves, so neither the buffer nor the offset is
                // touched: measured, `read(f, NULL, 5)` at EOF is 0 on both
                // platforms, and the offset stays where it was rather than being
                // clamped to the file's length. `NULL` is an ordinary user
                // address, so it reaches here rather than being screened above.
                succeed 0 state
            else

            match BufferPointer.dereferenceable buffer with
            | None ->
                // Measured: an EFAULT leaves the offset alone. A kernel faults
                // in `copy_to_user`, after deciding what it would have
                // transferred but before consuming anything.
                fail UnixError.EFAULT
            | Some buffer ->

            let bytes =
                ImmutableArray.CreateRange (seq { for i in 0 .. transfer - 1 -> contents.[int offset + i] })

            // Advanced by what actually moved, not by what was asked for: a
            // short read at the end of a file leaves the offset at the end
            // rather than past it, which is what makes a subsequent read return
            // 0 instead of a second short read.
            let state =
                writeBytesThrough ctx operation buffer bytes state
                |> fun state ->
                    state.MapKernel (fun kernel ->
                        { kernel with
                            FileDescriptors =
                                FileDescriptorRegistry.setOffset fd (offset + int64 transfer) kernel.FileDescriptors
                        }
                    )

            succeed transfer state
        // `int64_t SystemNative_LSeek(intptr_t fd, int64_t offset, int32_t
        // whence)` (pal_io.c:767): `lseek(2)`/`lseek64(2)` verbatim, with an
        // EINTR retry and no argument validation of its own.
        //
        // The `whence` parameter is matched with a wildcard, as
        // `SystemNative_FLock`'s operation and `SystemNative_Open`'s flags are:
        // CoreLib declares it as the `Interop.Sys.SeekWhence` *enum*
        // (Interop.LSeek.cs), so requiring `PrimitiveType.Int32` here would
        // match a hand-rolled P/Invoke passing an `int` and silently miss every
        // call the BCL itself makes. `NativeCall.int32Argument` unwraps the
        // enum to its underlying value.
        | Some "SystemNative_LSeek",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64 ; _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64) ->
            let operation = "SystemNative_LSeek"
            let fd = fdArgument operation instruction.Arguments.[0]
            let offset = NativeCall.int64Argument operation instruction.Arguments.[1]
            let whence = NativeCall.int32Argument operation instruction.Arguments.[2]

            let fail (error : UnixError) : NativeHandlerResult option =
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (fun kernel ->
                    { kernel with
                        LastSystemError = UnixError.toRawErrnoUnder numbering error
                    }
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 (Int64Source.Verbatim -1L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let flavour = SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform

            // `Interop.Sys.SeekWhence` (Interop.LSeek.cs), which is also POSIX's
            // numbering and both platforms' `<unistd.h>` — for these three. It
            // stops here; 3 and 4 are handled below and are *not* portable.
            let seekSet = 0
            let seekCur = 1
            let seekEnd = 2
            let seekMax = 4

            // The two orderings below are measured, and this is the syscall
            // where they differ most. On a single-fault input the platforms
            // agree on every row; they part company on two:
            //
            //   input                       Linux    Darwin
            //   pipe + whence 99            EINVAL   ESPIPE
            //   pipe + whence 99 + overflow EINVAL   ESPIPE
            //
            // So Linux validates `whence` before it asks whether the object is
            // seekable, and Darwin the other way round. The descriptor itself
            // precedes both on either platform — `lseek(badfd, ..)` is EBADF for
            // every whence and offset measured, including 99, 3, 4 and
            // INT64_MAX — and the offset arithmetic follows both, pinned by
            // `lseek(pipe, -1, SEEK_SET)` = ESPIPE on both (seekability first)
            // and `lseek(f, 1, 99)` from INT64_MAX = EINVAL on both (whence
            // first).
            let whenceValid = whence >= seekSet && whence <= seekMax

            let target = FileDescriptorRegistry.tryFindTarget fd state.Kernel.FileDescriptors

            let descriptorFault : DescriptorFault option =
                match target with
                | None -> Some DescriptorFault.NotOpen
                | Some (OpenFileTarget.StandardStream _) ->
                    // Not seekable: PawPrint models the standard streams as
                    // pipes, and `lseek` on a pipe is ESPIPE on both platforms
                    // whichever end it is. This is the answer `SafeFileHandle`
                    // reads back to decide `CanSeek`, so it is on the BCL's own
                    // path rather than a corner.
                    Some DescriptorFault.NotSeekable
                | Some OpenFileTarget.SocketEventPort ->
                    // The one target whose *seekability* depends on the
                    // platform, rather than merely the errno or the ordering.
                    // Measured: Darwin refuses `lseek` on a kqueue with ESPIPE,
                    // while Linux gives an epoll descriptor `noop_llseek`, which
                    // succeeds and reports 0 without consulting the offset or
                    // moving anything. So Darwin has a descriptor fault here and
                    // Linux has none; the Linux success is served below, after
                    // the whence check the syscall still applies.
                    match flavour with
                    | SimulatedUnixFlavour.Darwin -> Some DescriptorFault.NotSeekable
                    | SimulatedUnixFlavour.Linux -> None
                | Some (OpenFileTarget.File _) -> None

            // The two descriptor faults are ordered *differently* against the
            // whence check, so they are kept apart as faults rather than as
            // errnos: a future third fault must then decide where it sits
            // instead of silently inheriting whichever position its errno's
            // arm happened to occupy.
            let ordered : UnixError option =
                match descriptorFault with
                | Some DescriptorFault.NotOpen ->
                    // Ahead of everything on both platforms.
                    Some UnixError.EBADF
                | notOpenRejected ->

                let unseekable =
                    match notOpenRejected with
                    | Some DescriptorFault.NotSeekable -> true
                    | Some DescriptorFault.NotOpen
                    | None -> false

                match flavour with
                | SimulatedUnixFlavour.Linux ->
                    if not whenceValid then Some UnixError.EINVAL
                    elif unseekable then Some UnixError.ESPIPE
                    else None
                | SimulatedUnixFlavour.Darwin ->
                    if unseekable then Some UnixError.ESPIPE
                    elif not whenceValid then Some UnixError.EINVAL
                    else None

            match ordered with
            | Some error -> fail error
            | None ->

            // Linux's `noop_llseek`, reached only under the Linux flavour (Darwin
            // answered ESPIPE above). It returns the file position unchanged, and
            // an epoll descriptor's is always 0, so the answer is 0 for every
            // input that gets here — measured for `SEEK_SET` with offset -1 and
            // with INT64_MAX alike, and for whence 3 and 4.
            //
            // Ahead of the SEEK_DATA/SEEK_HOLE refusal below, which is why that
            // refusal is not simply hoisted to the whence check: it is a
            // statement about a *file's* sparseness, and a port has none. The
            // syscall's own `whence <= SEEK_MAX` guard still applies and has
            // already run, so whence 5 and above were rejected as EINVAL.
            match target with
            | Some OpenFileTarget.SocketEventPort ->
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 (Int64Source.Verbatim 0L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | _ ->

            // Whence *validity* is settled; whence *semantics* is not, and the
            // two sit at different points in Linux's order — which is why
            // refusing 3 and 4 up front would be wrong. Measured,
            // `lseek(badfd, 0, 3)` is EBADF and `lseek(pipe, 0, 3)` is ESPIPE on
            // both platforms, so a guest reaching here with whence 3 or 4 really
            // is asking about a seekable file's sparseness.
            //
            // Which PawPrint refuses to answer. `SEEK_DATA` and `SEEK_HOLE` are
            // numbered 3 and 4 on Linux and 4 and 3 on Darwin, so the raw value
            // does not even name the same operation on the two kernels; and they
            // ask about holes, which the emulated filesystem does not represent
            // — every file is a byte array, so a hole-free answer would be a
            // claim about the *filesystem* rather than about the kernel.
            // Measured, real filesystems do not agree on it either: on a 5-byte
            // file `lseek(f, 0, 3)` is 0 on Linux/ext4, 5 on macOS, and on a
            // directory it is 0 on ext4, EINVAL on tmpfs and ENXIO on macOS.
            //
            // CoreLib cannot reach this: `Interop.Sys.SeekWhence` declares only
            // 0, 1 and 2.
            if whence > seekEnd then
                let named =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> if whence = 3 then "SEEK_DATA" else "SEEK_HOLE"
                    | SimulatedUnixFlavour.Darwin -> if whence = 3 then "SEEK_HOLE" else "SEEK_DATA"

                failwith
                    $"%s{operation}: fd %d{fd} asked for whence %d{whence}, which is %s{named} on the simulated platform. PawPrint models file contents as a byte array with no notion of sparseness, so it cannot say where the data and holes are; and the two platforms transpose the numbers (3 is SEEK_DATA on Linux and SEEK_HOLE on Darwin), so the raw value does not name one operation. CoreLib never sends these — Interop.Sys.SeekWhence is 0, 1, 2 — so this is a hand-rolled P/Invoke."
            else

            let whence =
                if whence = seekSet then
                    SeekWhence.Set
                elif whence = seekCur then
                    SeekWhence.Current
                elif whence = seekEnd then
                    SeekWhence.End
                else
                    failwith
                        $"%s{operation}: whence %d{whence} passed the validity and semantics checks but is not one of SEEK_SET, SEEK_CUR or SEEK_END (this is an interpreter bug)"

            let inode, current =
                match target with
                | Some (OpenFileTarget.File (inode, current)) -> inode, current
                | _ ->
                    failwith
                        $"%s{operation}: fd %d{fd} is not a seekable file, but the descriptor checks above did not reject it (this is an interpreter bug)"

            let entry =
                match VirtualFileSystem.tryGet inode state.Kernel.FileSystem with
                | Some entry -> entry
                | None ->
                    failwith
                        $"%s{operation}: fd %d{fd} names inode %O{inode}, which the filesystem does not contain. A descriptor outliving its inode means an unlink removed a still-open file; the open file description must keep it alive."

            // Deferred, because only `SEEK_END` consults it and a directory has
            // no size PawPrint will state. `seekTarget` forces this exactly on
            // the `End` path, so `SEEK_SET` and `SEEK_CUR` on a directory — both
            // portable — keep working rather than tripping over a size they
            // never read.
            let size =
                lazy
                    match entry.Content with
                    | InodeContent.RegularFile (contents, _) -> int64 contents.Length
                    | InodeContent.Symlink _ ->
                        // Not reachable: `open` resolves symlinks, so no descriptor
                        // names one. Stated rather than folded in so that an
                        // `O_PATH`-style descriptor finds a decision here.
                        failwith
                            $"%s{operation}: fd %d{fd} names inode %O{inode}, which is a symbolic link. `open` resolves symlinks, so no descriptor should name one; if this is reachable, decide what seeking a link through a descriptor means (issue #956)."
                    | InodeContent.Directory _ ->
                        // There is no portable answer. Measured, `lseek(dir, 0,
                        // SEEK_END)` is EINVAL on Linux/tmpfs, 4096 on Linux/ext4
                        // and 64 on macOS/APFS: a directory's "size" is a property
                        // of how the filesystem stores its entries, and PawPrint
                        // stores them as a map. `FStat` reports 4096 for a directory,
                        // but that number is *forced* — `stat` must fill the field in
                        // — whereas nothing forces this one, and inventing a second
                        // number from the same non-fact would make a guest's
                        // `SEEK_END` agree with `FStat` by coincidence rather than by
                        // construction.
                        //
                        // No BCL caller reaches it: `SafeFileHandle.Init` raises
                        // `UnauthorizedAccessException` on opening a directory for
                        // reading (SafeFileHandle.Unix.cs:320-327), and directory
                        // enumeration goes through `opendir`/`readdir`.
                        failwith
                            $"%s{operation}: fd %d{fd} names inode %O{inode}, a directory, and was asked to seek relative to its end. A directory's size is a filesystem artefact rather than a fact about its contents, and there is no portable answer: measured, lseek(dir, 0, SEEK_END) is EINVAL on Linux/tmpfs, 4096 on Linux/ext4 and 64 on macOS/APFS. SEEK_SET and SEEK_CUR on a directory are portable and are supported."

            match VirtualFileSystem.seekTarget whence current size offset with
            | Error SeekFault.Negative ->
                // EINVAL on both, and the offset is left where it was —
                // measured, a failed `lseek` does not move the description.
                fail UnixError.EINVAL
            | Error SeekFault.Overflow ->
                // The one place the *errno* differs rather than the ordering.
                // Measured on a tmpfs-backed file, so that the filesystem is
                // held constant: `lseek(f, INT64_MAX-4, SEEK_END)` on a 5-byte
                // file is EINVAL on Linux and EOVERFLOW on Darwin.
                match flavour with
                | SimulatedUnixFlavour.Linux -> fail UnixError.EINVAL
                | SimulatedUnixFlavour.Darwin -> fail UnixError.EOVERFLOW
            | Ok position ->

            state.MapKernel (fun kernel ->
                { kernel with
                    FileDescriptors = FileDescriptorRegistry.setOffset fd position kernel.FileDescriptors
                }
            )
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 (Int64Source.Verbatim position)) ctx.Thread
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
                // The C `assert`s `bufferSize >= 0` first, so a checked
                // build would abort on a negative size rather than reach this;
                // EINVAL is what a guest running against a retail runtime can
                // observe, exactly as for `SystemNative_GetCwd`.
                fail UnixError.EINVAL
            else

            // Read before anything else looks at it, because a real kernel
            // copies the pathname in before it resolves anything: a path that
            // addresses nothing is EFAULT whatever the buffer is.
            match
                bufferPointerArgument operation "path" instruction.Arguments.[0]
                |> BufferPointer.dereferenceable
            with
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
                // Not a link. It must be EINVAL and no other errno:
                // `FileSystem.ResolveLinkTarget`
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

            // The output pointer is only resolved to storage here, on the path
            // that actually writes through it. `readlink(2)` runs no up-front
            // address check on either platform: the target is built in the
            // kernel and handed over with a single `copy_to_user`, so an
            // unusable buffer is discovered at the copy and every earlier
            // refusal wins.
            match
                bufferPointerArgument operation "buffer" instruction.Arguments.[1]
                |> BufferPointer.dereferenceable
            with
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
            // POSIX says a successful `readlink` marks it for update. The
            // virtual clock advances as the driver loop runs, so a guest that
            // `LStat`s a link before and after reading it really could see the
            // difference.
            //
            // Deferred because it cannot be settled *here*. Whether the
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
            // `Kernel.FileSystem` today, so there is no write-back path to
            // reuse. The divergence is also not something the differential
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
        | Some "SystemNative_CreateSocketEventPort",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_CreateSocketEventPort(intptr_t* port)`
            // (pal_networking.c:3429). `epoll_create1(EPOLL_CLOEXEC)` on Linux
            // and `kqueue()` on Darwin, both of which hand back an ordinary
            // descriptor onto an anonymous kernel object.
            //
            // Returns a PAL `Interop.Error` rather than -1-and-errno, so
            // `LastSystemError` is not touched: the sole managed caller,
            // `SocketAsyncEngine`, switches on the returned value.
            //
            // PawPrint's allocation cannot fail — no descriptor limit is
            // modelled, `RLIMIT_NOFILE` not being in the interop surface — so
            // the only failure here is the wrapper's own null screen.
            let operation = "SystemNative_CreateSocketEventPort"

            let portArgument = bufferPointerArgument operation "port" instruction.Arguments.[0]

            match portArgument with
            | BufferPointer.RawAddress 0UL ->
                // The wrapper's *only* screen is `port == NULL`, and it answers
                // before creating anything, so no descriptor leaks here.
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixError.toPal UnixError.EFAULT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | _ ->

            match BufferPointer.dereferenceable portArgument with
            | None ->
                // Deliberately *not* EFAULT. A non-null address naming no
                // storage passes the wrapper's null check, so the real code
                // creates the descriptor and then faults on its unconditional
                // `*port = fd` store — a SIGSEGV that kills the process, not an
                // error code the guest can catch. Answering EFAULT here would
                // turn that crash into a plausible wrong answer and let the
                // guest continue with a descriptor table the real run would
                // never have reached.
                failwith
                    $"%s{operation}: `port` is %O{portArgument}, which is not null but names no storage. The C wrapper screens only `port == NULL`, so a real run would create the descriptor and then fault storing through this address; PawPrint does not model that fault. Pass a real out-parameter."
            | Some port ->

            let fd, registry =
                FileDescriptorRegistry.createSocketEventPort state.Kernel.FileDescriptors

            let state =
                state.MapKernel (fun kernel ->
                    { kernel with
                        FileDescriptors = registry
                    }
                )

            // `*port = fd`, as an `intptr_t`: eight bytes on every platform
            // PawPrint models, little-endian on both x64 and arm64. The C
            // performs this store unconditionally, on the error path too, where
            // it writes the inner function's -1; PawPrint's creation is
            // infallible, so only the success value is ever stored.
            let bytes = Array.zeroCreate<byte> 8
            BinaryPrimitives.WriteInt64LittleEndian (Span<byte> bytes, int64 fd)

            writeBytesThrough ctx operation port (ImmutableArray.CreateRange bytes) state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixError.palSuccess))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_CloseSocketEventPort",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_CloseSocketEventPort(intptr_t port)`
            // (pal_networking.c:3442), which is `close(2)` on the descriptor and
            // nothing else. It does *not* check that the descriptor names a
            // port, so closing an ordinary file through it succeeds — modelled
            // here by deferring to the same registry operation
            // `SystemNative_Close` uses.
            //
            // The C maps both success and EINTR to `Error_SUCCESS`; PawPrint
            // delivers no signals during a close, so EINTR is unreachable and
            // EBADF is the only failure.
            //
            // The PAL code is the return value, but `close(2)` itself still sets
            // `errno` on the way past — so a guest that declared this entry point
            // `SetLastError = true`, or that reads `Marshal.GetLastSystemError`
            // after a raw P/Invoke, must see EBADF rather than whatever was there
            // before. Same failure path as `SystemNative_Close`, and errno is
            // left untouched on success by the same Unix convention.
            let fd = fdArgument "SystemNative_CloseSocketEventPort" instruction.Arguments.[0]

            let error, state =
                match FileDescriptorRegistry.close fd state.Kernel.FileDescriptors with
                | Ok registry ->
                    UnixError.palSuccess,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            FileDescriptors = registry
                        }
                    )
                | Error FileDescriptorCloseError.BadFd ->
                    UnixError.toPal UnixError.EBADF,
                    state.MapKernel (fun kernel ->
                        { kernel with
                            LastSystemError = UnixError.toRawErrno UnixError.EBADF
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim error)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_WaitForSocketEvents",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_WaitForSocketEvents(intptr_t port,
            // SocketEvent* buffer, int32_t* count)` (pal_networking.c:3492):
            //
            //     if (buffer == NULL || count == NULL || *count < 0) return Error_EFAULT;
            //     return WaitForSocketEventsInner(ToFileDescriptor(port), buffer, count);
            //
            // and the inner function is `epoll_wait(port, events, *count, -1)`
            // under epoll or `kevent(port, NULL, 0, events, *count, NULL)` under
            // kqueue — an *infinite* timeout in both cases, with the EINTR retry
            // in the loop condition, so signal delivery must not wake a thread
            // parked here.
            //
            // The buffer's element type is matched with a wildcard: CoreLib
            // declares it `SocketEvent*` (Interop.SocketEvent.cs), and a guest
            // hand-rolling the P/Invoke will say `byte*` or `void*`. Nothing here
            // addresses an element, only the byte range they span, so the pointee
            // type is not consulted.
            //
            // Five of the eight rows of this entry point's contract differ between
            // the two flavours, so the ladder below is flavour-branching
            // throughout rather than in one place. Each ordering is measured — on
            // Linux 6.18.5 and Darwin 25.6.0 — rather than read off the kernel
            // sources, because the widely-reproduced `do_epoll_wait` listing
            // checks `maxevents` and `access_ok` *before* `fdget` and current
            // kernels do not.
            let operation = "SystemNative_WaitForSocketEvents"

            // `port` is deliberately *not* decoded yet, and neither is `count`
            // classified: the wrapper's screen is `buffer == NULL || count == NULL
            // || *count < 0`, which short-circuits, and only then reaches
            // `ToFileDescriptor(port)`. So a guest may legally pass a `port` that is
            // no number at all — a function pointer, a type handle — alongside a null
            // buffer, and the answer is EFAULT rather than anything about a
            // descriptor. `fdArgument` refuses such a value, which is right, so it
            // must not run until the call is known to consult the argument.
            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            let flavour = SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform

            // The wrapper's own three rows. It answers them in user space, having
            // run no syscall — so `errno` keeps whatever it already held, and
            // `*count` is left exactly as the caller set it.
            let refuseBeforeSyscall () : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixError.toPal UnixError.EFAULT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match buffer with
            | BufferPointer.RawAddress 0UL -> refuseBeforeSyscall ()
            | _ ->

            let countPointer = bufferPointerArgument operation "count" instruction.Arguments.[2]

            match countPointer with
            | BufferPointer.RawAddress 0UL -> refuseBeforeSyscall ()
            | _ ->

            let countCell =
                match BufferPointer.dereferenceable countPointer with
                | Some cell -> cell
                | None ->
                    // Deliberately not EFAULT. `*count` is dereferenced by the
                    // wrapper itself, in user space, so a non-null address naming
                    // no storage is a SIGSEGV that kills the process rather than
                    // an error code the guest can catch — the same reasoning
                    // `SystemNative_CreateSocketEventPort` applies to its own
                    // out-parameter store.
                    failwith
                        $"%s{operation}: `count` is %O{countPointer}, which is not null but names no storage. The C wrapper dereferences it in user space, so a real run would fault; PawPrint does not model that fault. Pass a real in-out parameter."

            let requestedCount =
                let bytes = readBytesThrough ctx operation countCell 4 state
                BinaryPrimitives.ReadInt32LittleEndian (bytes.AsSpan ())

            if requestedCount < 0 then
                // EFAULT, which is the wrapper's own choice and neither kernel's:
                // `epoll_wait` answers EINVAL for a non-positive `maxevents`, and
                // never sees this value.
                refuseBeforeSyscall ()
            else

            // Past the wrapper, so the call really does consult `port` now.
            let fd = fdArgument operation instruction.Arguments.[0]

            let openFile = FileDescriptorRegistry.tryFindWithId fd state.Kernel.FileDescriptors

            // A row the syscall reached and failed. Two consequences beyond the
            // returned PAL code: the inner function writes its flavour's sentinel
            // through `count`, and the syscall set `errno` on the way past — which
            // a guest declaring this entry point `SetLastError = true` can read
            // back, exactly as for `SystemNative_CloseSocketEventPort`.
            let failFromSyscall (error : UnixError) : NativeHandlerResult option =
                let sentinel =
                    // `*count = 0` under epoll and `*count = -1` under kqueue,
                    // both unconditional in their own error branch.
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> 0
                    | SimulatedUnixFlavour.Darwin -> -1

                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform
                let bytes = Array.zeroCreate<byte> 4
                BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, sentinel)

                writeBytesThrough ctx operation countCell (ImmutableArray.CreateRange bytes) state
                |> fun state ->
                    state.MapKernel (fun kernel ->
                        { kernel with
                            LastSystemError = UnixError.toRawErrnoUnder numbering error
                        }
                    )
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixError.toPal error)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Park re-entrantly: leave the native frame on the stack and the
            // caller's program counter naming the call, so that a wake re-enters
            // this handler and writes the event batch through the caller's own
            // `buffer` — rather than the wake having to reach into a frame it does
            // not own from some other thread's step.
            //
            // Nothing wakes it today, and that is faithful rather than a stub: no
            // descriptor can be registered with a port yet
            // (`SystemNative_TryChangeSocketEventRegistration` is deliberately
            // still unimplemented), and both PAL implementations carry the comment
            // that with an infinite timeout the wait blocks until a descriptor is
            // added *and* an event occurs on it. `SocketAsyncEngine.EventLoop`'s
            // thread does exactly this in a process that never opens a socket.
            let park (port : OpenFileDescriptionId) : NativeHandlerResult option =
                Scheduler.blockOnSocketEvents ctx.Thread port state
                |> NativeHandlerResult.blockedRetainingFrame
                |> Some

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                // Measured on 6.18.5, each adjacent pair separated by an input that
                // provokes exactly one of the two: descriptor, then `maxevents`,
                // then the buffer, then is-it-an-epoll-instance.
                match openFile with
                | None -> failFromSyscall UnixError.EBADF
                | Some (port, description) ->

                // The kernel's predicate is `maxevents <= 0 || maxevents > EP_MAX_EVENTS`;
                // the wrapper has already turned every negative value into EFAULT,
                // so zero is the only non-positive one that gets here.
                if requestedCount = 0 || requestedCount > LinuxEpollLimits.MaxEvents then
                    failFromSyscall UnixError.EINVAL
                else

                // The byte range `access_ok(events, maxevents * sizeof(struct
                // epoll_event))` screens. This multiplication is safe only *below*
                // the cap just applied, which is what `EP_MAX_EVENTS` exists for:
                // it is `INT_MAX / EventSize`, so every count that reaches here has
                // a product inside `int32`.
                let bufferExtent = requestedCount * LinuxEpollLimits.EventSize

                // Not a mappedness check. On 64-bit Linux `access_ok` only rejects
                // ranges reaching into the kernel half, so a merely-unmapped
                // userspace address passes and the wait then blocks, faulting at
                // delivery — which is why this must not eagerly validate that the
                // buffer is real before parking. `faultsBeforeOperation` is exactly
                // that range test against `UserAddressLimit`.
                if faultsBeforeOperation state.Kernel buffer bufferExtent then
                    failFromSyscall UnixError.EFAULT
                else

                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _ ->
                    // A live descriptor onto the wrong kind of object. EINVAL is
                    // epoll's own answer for it, and it is the last of the four
                    // screens — behind the buffer, which is why an unmappable
                    // buffer on a non-port descriptor is EFAULT rather than this.
                    failFromSyscall UnixError.EINVAL
                | OpenFileTarget.SocketEventPort -> park port
            | SimulatedUnixFlavour.Darwin ->
                // Measured on 25.6.0, and flatter: `kevent` resolves the descriptor
                // before its `nevents == 0` early return, has no "wrong kind of
                // object" answer to give, and screens no buffer at all — so the
                // whole ladder is one question about the descriptor followed by one
                // about the count.
                match openFile with
                | None -> failFromSyscall UnixError.EBADF
                | Some (port, description) ->

                match description.Target with
                | OpenFileTarget.StandardStream _
                | OpenFileTarget.File _ ->
                    // EBADF, where epoll says EINVAL: kqueue folds "not a kqueue"
                    // into "bad descriptor".
                    failFromSyscall UnixError.EBADF
                | OpenFileTarget.SocketEventPort ->

                if requestedCount = 0 then
                    // The one input on which the flavours disagree about whether
                    // the call blocks at all. Measured:
                    // `kevent(kq, NULL, 0, evs, 0, NULL)` returns 0 immediately,
                    // where `epoll_wait` with `maxevents == 0` is EINVAL. The
                    // "we should never see 0 events" assertion that follows is
                    // compiled out of the shipped release build, so the wrapper
                    // falls through, writes `*count = 0` and reports success.
                    //
                    // `errno` is untouched, the syscall having not failed.
                    let bytes = Array.zeroCreate<byte> 4
                    BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, 0)

                    writeBytesThrough ctx operation countCell (ImmutableArray.CreateRange bytes) state
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.Int32 (Int32Source.Verbatim UnixError.palSuccess))
                        ctx.Thread
                    |> NativeHandlerResult.completed
                    |> Some
                else

                // No buffer screen, so an unmappable buffer parks here rather than
                // faulting: `UserBufferCheck.AtCopyTime` is Darwin's answer, and a
                // wait that never delivers an event never copies anything.
                park port
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
            // PawPrint writes to the standard output streams (fds 1 and 2) and to
            // a regular file opened for writing, and never returns short, never
            // returns EINTR, and never blocks: there is no kernel that could push
            // back on our simulated process, and its filesystem cannot run out of
            // space. A guest depending on EAGAIN / partial writes from a
            // non-blocking socket would need new FileDescriptorRole entries;
            // we'll add those when that need arises rather than guessing at the
            // contract now.
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
            // about to dereference it. `Common_Write` is
            // documented (in `pal_io_common.h`) to perform no dereference
            // for `bufferSize < 0` (ERANGE bail) or `bufferSize = 0`
            // (no-op on every Unix we model), so a guest calling e.g.
            // `SystemNative_Write((IntPtr)1, (byte*)123, 0)` must succeed
            // on PawPrint as it does on the real CLR — eagerly decoding
            // `buffer` would crash here in `managedPointerOfPointerArgument`
            // for any non-managed pointer literal.
            // Drain `bufferSize` bytes from `buffer`. Called only after the
            // bufferSize->0 and buffer->non-null checks succeed.
            let readBuffer (buffer : ManagedPointerSource) (state : IlMachineState) : ImmutableArray<byte> =
                readBytesThrough ctx operation buffer bufferSize state

            let result, effect, state =
                if bufferSize < 0 then
                    // Matches `Common_Write`: refuse the call before any
                    // dereference of `buffer`. CoreLib callers (`Interop.Sys.
                    // Write`) never pass negative sizes, so this is a guest
                    // misuse path; surface it through errno rather than
                    // crashing so the guest's own error reporting runs.
                    -1, StepEffect.NoEffect, setErrno state UnixError.ERANGE
                else
                    match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
                    | None ->
                        // Unknown fd: report EBADF the same way `write(2)`
                        // would.
                        -1, StepEffect.NoEffect, setErrno state UnixError.EBADF
                    | Some description when not (FileAccessMode.permitsWrite description.AccessMode) ->
                        // `write(2)` on a descriptor not open for writing is
                        // EBADF on both platforms, and this precedes both the
                        // buffer screen and the zero-size no-op: measured,
                        // `write(rdonlyFd, buf, 0)` is EBADF rather than 0.
                        //
                        // Covers stdin — which a redirected launch opens
                        // `O_RDONLY`, the shape `FileDescriptorRegistry.initial`
                        // commits to — and a regular file opened `O_RDONLY`
                        // alike, including a directory, which can only ever be
                        // opened for reading.
                        -1, StepEffect.NoEffect, setErrno state UnixError.EBADF
                    | Some description ->
                        match description.Target with
                        | OpenFileTarget.SocketEventPort ->
                            // An epoll instance has no write operation, so the
                            // refusal is for the *kind* of object rather than
                            // for the access mode — the port permits writing
                            // and so passes the EBADF arm above. Measured, Linux
                            // answers EINVAL and Darwin ENXIO.
                            //
                            // Ahead of the buffer screen and of the zero-size
                            // no-op, on both platforms: measured,
                            // `write(port, (void*)-1, 8)` is EINVAL/ENXIO rather
                            // than EFAULT, and no length is a no-op.
                            let error =
                                match SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform with
                                | SimulatedUnixFlavour.Linux -> UnixError.EINVAL
                                | SimulatedUnixFlavour.Darwin -> UnixError.ENXIO

                            -1, StepEffect.NoEffect, setErrno state error
                        | OpenFileTarget.File (inode, offset) ->
                            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

                            // `vfs_write` screens the buffer between the access
                            // mode above and the file operation, so on Linux this
                            // beats the zero-size no-op below: measured,
                            // `pwrite(f, (void*)-1, 0, 0)` is EFAULT there and 0
                            // on macOS.
                            if faultsBeforeOperation state.Kernel buffer bufferSize then
                                -1, StepEffect.NoEffect, setErrno state UnixError.EFAULT
                            elif bufferSize = 0 then
                                // A no-op on both platforms, and specifically one
                                // that moves no timestamp: measured, a
                                // zero-length write leaves `mtime` and `ctime`
                                // where they were and does not extend the file,
                                // even at an offset past its end.
                                0, StepEffect.NoEffect, state
                            else
                                match BufferPointer.dereferenceable buffer with
                                | None -> -1, StepEffect.NoEffect, setErrno state UnixError.EFAULT
                                | Some buffer ->

                                let bytes = readBuffer buffer state

                                // At the description's own offset, and advancing
                                // it by what moved — the entire difference from
                                // `pwrite`, which takes the offset as an argument
                                // and leaves the description alone. Both measured.
                                //
                                // The commit comes first, so the advance cannot
                                // overflow: a write that would carry the offset
                                // past what the model can represent has already
                                // been refused there.
                                let state = commitFileWrite operation fd inode offset bytes state

                                let state =
                                    state.MapKernel (fun kernel ->
                                        { kernel with
                                            FileDescriptors =
                                                FileDescriptorRegistry.setOffset
                                                    fd
                                                    (offset + int64 bytes.Length)
                                                    kernel.FileDescriptors
                                        }
                                    )

                                bufferSize, StepEffect.NoEffect, state
                        | OpenFileTarget.StandardStream role ->
                            let bufferPointer =
                                bufferPointerArgument operation "buffer" instruction.Arguments.[1]

                            // `vfs_write` screens the buffer between the
                            // descriptor's access mode and the file operation,
                            // so on Linux this beats the zero-size no-op below:
                            // measured, `write(1, (void*)-1, 0)` is EFAULT there
                            // and 0 on macOS. The EBADF arms above still win,
                            // being the access-mode check.
                            if faultsBeforeOperation state.Kernel bufferPointer bufferSize then
                                -1, StepEffect.NoEffect, setErrno state UnixError.EFAULT
                            elif bufferSize = 0 then
                                // `write(fd, _, 0)` is a no-op on every Unix we
                                // model — no errno, no buffer dereference, no
                                // observable effect. CoreLib in principle never
                                // calls with `bufferSize = 0` (it bails in
                                // `Stream.Write`), but honour the C contract so
                                // guests that DllImport directly behave the same
                                // as on the host. Do NOT resolve `buffer` to
                                // storage here: any address that got past the
                                // screen above is permitted, because it is not
                                // dereferenced.
                                0, StepEffect.NoEffect, state
                            else
                                // Real `write(2)` returns -1 + EFAULT for any
                                // non-dereferenceable address (including NULL
                                // and unmapped bit patterns); collapse both
                                // cases to EFAULT here rather than crashing
                                // PawPrint, so a direct P/Invoke that the BCL
                                // would never produce (`Stream.Write`
                                // short-circuits null upstream) observes the
                                // same syscall failure it would on the host.
                                match BufferPointer.dereferenceable bufferPointer with
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

            // `free(NULL)` is a documented no-op. CoreLib's NativeMemory.Free
            // already filters null before reaching the P/Invoke, but
            // Marshal.FreeHGlobal does not, so honour the C semantics here too.
            let state =
                match NativeCall.tryResolveNativeHeapFreeTarget ptr with
                | Ok None -> state
                | Ok (Some block) -> IlMachineState.freeNativeMemory block state
                | Error reason -> failwith $"SystemNative_Free: %s{reason}"

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
