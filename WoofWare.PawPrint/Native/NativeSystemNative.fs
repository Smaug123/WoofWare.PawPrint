namespace WoofWare.PawPrint

open System
open System.Buffers.Binary
open System.Collections.Immutable
open WoofWare.PosixKernel

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

[<RequireQualifiedAccess>]
module internal BufferPointer =
    /// How this classification looks to a kernel, which is less than PawPrint
    /// knows.
    ///
    /// `Symbolic` and `Unstatable` carry diagnostic payloads only so that a
    /// refusal can name the value the guest passed; a kernel has no concept of
    /// either, and what it does have a concept of — mapped, unmapped, and the
    /// two shapes it cannot answer about — is what crosses.
    let toUserBuffer (pointer : BufferPointer) : UserBuffer =
        match pointer with
        | BufferPointer.Storage _ -> UserBuffer.Mapped
        | BufferPointer.RawAddress address -> UserBuffer.Unmapped address
        | BufferPointer.Symbolic _ -> UserBuffer.Opaque
        | BufferPointer.Unstatable _ -> UserBuffer.Addressless

    /// PawPrint's half of a refused buffer: which entry point asked, which
    /// argument it was, what the guest actually passed, and what PawPrint would
    /// have to represent to answer. The library's half says why no kernel answer
    /// exists.
    ///
    /// Total rather than partial, and loudly so for the two answerable cases:
    /// storage and a raw address are both things a kernel answers about, so a
    /// refusal naming one is an interpreter bug rather than a message to render.
    let refusalMessage (pointer : BufferPointer) (refusal : BufferRefusal) : string =
        match pointer with
        | BufferPointer.Symbolic (operation, argName, argument) ->
            $"%s{operation}: %s{argName} is %O{argument}, the address of a runtime data structure PawPrint models symbolically rather than as bytes. %s{BufferRefusal.describe refusal} Pass a buffer that names guest storage."
        | BufferPointer.Unstatable (operation, argName, argument) ->
            $"%s{operation}: %s{argName} is %O{argument}, the difference of two pointers into separate storages. %s{BufferRefusal.describe refusal} Subtracting pointers that do not point into one object does not produce a buffer."
        | BufferPointer.Storage _
        | BufferPointer.RawAddress _ ->
            failwith
                $"BufferPointer.refusalMessage: %O{pointer} names a buffer a kernel can answer about, so there is no refusal to describe (this is an interpreter bug)."

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
        | BufferPointer.Symbolic _ -> failwith (refusalMessage pointer BufferRefusal.OpaqueAtTransfer)
        | BufferPointer.Unstatable _ -> failwith (refusalMessage pointer BufferRefusal.AddresslessAtTransfer)

[<RequireQualifiedAccess>]
module NativeSystemNative =
    let private trySystemNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.TryNativeImport with
        | Some import when import.ModuleName = "libSystem.Native" -> Some import.EntryPointName
        | _ -> None

    /// The OS thread id of the thread currently executing the native call.
    let private osThreadIdOf (_operation : string) (ctx : NativeCallContext) : OsThreadId =
        UnixTaskTable.osThreadIdOf ctx.Thread ctx.State.Kernel.Tasks

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
        | CorelibType concreteTypes ("System.Runtime.InteropServices", "PosixSignal", generics) when generics.IsEmpty ->
            Some ()
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
        | NamedType concreteTypes ("", "Error", generics) when generics.IsEmpty -> Some ()
        | _ -> None

    /// Store the errno a failed syscall earned, in the raw numbering this
    /// kernel's flavour uses, and hand back the state to push a sentinel from.
    ///
    /// The library speaks `UnixError`; which integer that is on this platform is
    /// a fact `SimulatedUnixPlatform` owns, and applying it is the last thing
    /// PawPrint does before the guest sees a number.
    ///
    /// For a syscall whose signature returns no system, so that there is nothing
    /// to write back. Handing the unchanged projection to `withErrno` instead
    /// would work, but it would say the syscall wrote.
    let private withErrnoOnly (ctx : NativeCallContext) (error : UnixError) (state : IlMachineState) : IlMachineState =
        let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

        state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error))

    /// Write back the system a syscall failed from, and record the errno that
    /// failure produced. A failure still changes the system in general: `flock`
    /// advances the descriptor table before it discovers the conflict.
    let private withErrno
        (ctx : NativeCallContext)
        (error : UnixError)
        (system : UnixSystem<ThreadId, SignalHandler>)
        (state : IlMachineState)
        : IlMachineState
        =
        state.MapKernel (EmulatedKernel.withUnix system) |> withErrnoOnly ctx error

    /// The signal that a signo handed to `SystemNative_EnablePosixSignalHandling`,
    /// `SystemNative_DisablePosixSignalHandling` or
    /// `SystemNative_HandleNonCanceledPosixSignal` names under the configured
    /// platform's numbering.
    ///
    /// By contract the BCL only calls those entry points with a signo that
    /// `SystemNative_GetPlatformSignalNumber` returned non-zero for, and the
    /// real shim asserts `signalCode > 0 && signalCode <= GetSignalMax()`. A
    /// signo outside that range can only be a guest bypassing the registration
    /// path with a hand-rolled P/Invoke, and this fails loudly rather than
    /// mirror the shim indexing its tables out of bounds. Within the range,
    /// `ValueNone` is the one number the shim admits that the kernel has no
    /// signal for: Darwin's 32, which is its `NSIG`.
    let private signalWithinShimRange
        (operation : string)
        (numbering : SignalNumbering)
        (signo : int)
        : Signal voption
        =
        let signalMax = PosixSignalPal.signalMax numbering

        if signo <= 0 || signo > signalMax then
            failwith
                $"%s{operation}: refusing out-of-range signo %d{signo} (signos arriving here must lie within (0, %d{signalMax}] under the %O{numbering} numbering; this looks like a guest bypassing SystemNative_GetPlatformSignalNumber)"

        Signal.ofRawSignoUnder numbering signo

    /// Write back the system a syscall answered from, having neither failed nor
    /// been refused. Errno is left alone, as a successful syscall leaves it.
    let private withAnswered (system : UnixSystem<ThreadId, SignalHandler>) (state : IlMachineState) : IlMachineState =
        state.MapKernel (EmulatedKernel.withUnix system)

    /// The client's half of a refused `close`: which entry point asked, which
    /// descriptor it named, and what PawPrint would have to build to lift the
    /// refusal. The library's half says only what it measured.
    ///
    /// Shared by the three entry points that close a descriptor, so that the
    /// same refusal reads the same way whichever of them the guest went through.
    let private closeRefusalMessage (operation : string) (fd : int) (refusal : CloseRefusal<ThreadId>) : string =
        let remedy =
            match refusal with
            | CloseRefusal.LinuxLastPortDescriptorWithWaiter _ ->
                "Implement port retention for in-flight waits before closing one out from under a waiter."
            | CloseRefusal.DarwinPortDescriptorWithWaiter _ ->
                "Measure what the woken wait reports before closing a kqueue out from under a waiter, or configure a Linux platform."
            | CloseRefusal.LastFlockedDescriptorWithWaiter _ ->
                "Model a blocked flock's reference to the file it waits on before closing the description out from under a waiter."
            | CloseRefusal.ListenerWouldResetUnacceptedClient _ ->
                "Accept the connection or close the client before closing the listener."

        $"%s{operation}: fd %d{fd}: %s{CloseRefusal.describe refusal} %s{remedy}"

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
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
            // `IntPtr.Zero` can arrive spelled as the null managed pointer
            // rather than as `Verbatim 0L`; the guest-level value is the
            // integer zero either way, and zero is a legitimate descriptor
            // number — fd 0 is stdin.
            0
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

    /// The storage a buffer pointer names, for a pointer the C dereferences with
    /// no null check at all — `SystemNative_GetPort`'s `port` out-parameter, and
    /// its siblings.
    ///
    /// Always a refusal when there is nothing to write through, and null is not
    /// special: upstream would fault on a null exactly as it would on any other
    /// address naming nothing.
    let private requireUnscreenedStorage
        (operation : string)
        (argName : string)
        (pointer : BufferPointer)
        : ManagedPointerSource
        =
        match BufferPointer.dereferenceable pointer with
        | Some storage -> storage
        | None ->
            failwith
                $"%s{operation}: `%s{argName}` is %O{pointer}, which names no storage. The C never screens this parameter, so a real run would store through it and fault; PawPrint does not model that fault. Pass a real out-parameter."

    /// The storage a buffer pointer names, for a pointer whose caller has already
    /// screened it for null.
    ///
    /// Refuses rather than answering EFAULT, for the reason
    /// `SystemNative_CreateSocketEventPort` gives at length: a non-null address
    /// naming no storage passes a C null check, so the real code runs on and
    /// faults dereferencing it — a SIGSEGV that kills the process, not an error
    /// code the guest can catch. Answering EFAULT would turn that crash into a
    /// plausible wrong answer.
    let private requireStorage
        (operation : string)
        (argName : string)
        (pointer : BufferPointer)
        : ManagedPointerSource
        =
        match BufferPointer.dereferenceable pointer with
        | Some storage -> storage
        | None ->
            failwith
                $"%s{operation}: `%s{argName}` is %O{pointer}, which is not null but names no storage. The C screens only for null, so a real run would dereference this address and fault; PawPrint does not model that fault. Pass a real buffer."

    /// The storage `offset` bytes into a caller's buffer.
    ///
    /// `readBytesThrough` and `writeBytesThrough` both start at the pointer they
    /// are given, so reaching one field of a struct — or one element of an array
    /// of them — means advancing the pointer first. Reaching fields individually
    /// rather than transferring the whole struct is deliberate: it keeps the
    /// bytes PawPrint touches to the bytes the C touches, which matters because
    /// the two differ. A `SocketAddress` for an IPv4 endpoint is 16 bytes and a
    /// guest can read all of them back, while PawPrint's typed address space
    /// aborts on a read that runs past the storage rather than inventing what
    /// follows it; and `SystemNative_Poll` writes back only each entry's
    /// `TriggeredEvents`, leaving the `FileDescriptor` and `Events` the caller
    /// set exactly as they were.
    let private bufferFieldAt
        (ctx : NativeCallContext)
        (operation : string)
        (buffer : ManagedPointerSource)
        (offset : int)
        (state : IlMachineState)
        : ManagedPointerSource
        =
        if offset = 0 then
            buffer
        else

        let byteConcreteType =
            NativeCall.requiredByteConcreteType operation ctx.BaseClassTypes state

        ManagedPointerByteView.addByteOffset state byteConcreteType offset buffer

    /// `IsInBounds(sockAddr, socketAddressLen, &sockAddr->sa_family,
    /// sizeof_member(sockaddr, sa_family))` (pal_networking.c:692), which every
    /// entry point below applies before it reads or writes a blob's family.
    ///
    /// Upstream's `IsInBounds` compares addresses; since the family field is at a
    /// fixed offset from the base, that reduces to whether the caller's declared
    /// length covers the field. It is the *declared* length that this answers
    /// for, not the storage the pointer actually names — a guest may declare less
    /// than it allocated, and the shim believes it.
    ///
    /// A negative `socketAddressLen` fails this, which is what upstream does too
    /// and is not obvious from reading it: the cast to `size_t` makes the bound
    /// `SIZE_MAX`, so `baseAddr + len` wraps to *below* the base and the
    /// comparison fails. Measured rather than reasoned — `SystemNative_GetPort`
    /// with a length of -1 answers EFAULT on both platforms, including for a
    /// family whose own switch arm would have answered EAFNOSUPPORT.
    let private sockaddrFamilyIsInBounds (platform : SimulatedUnixPlatform) (socketAddressLen : int) : bool =
        SockaddrFamilyField.reachedBy (SimulatedUnixPlatform.sockaddrFamilyField platform) socketAddressLen

    /// `sockAddr->sa_family`, in the platform's own `AF_*` numbering.
    ///
    /// Little-endian for the two-byte flavour because `sa_family_t` is a plain
    /// host-order `unsigned short` — unlike `sin_port`, which is network order —
    /// and both architectures PawPrint models are little-endian.
    let private readSockaddrFamily
        (ctx : NativeCallContext)
        (operation : string)
        (platform : SimulatedUnixPlatform)
        (buffer : ManagedPointerSource)
        (state : IlMachineState)
        : int
        =
        let field = SimulatedUnixPlatform.sockaddrFamilyField platform
        let offset = SockaddrFamilyField.offset field

        let bytes =
            readBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation buffer offset state)
                (SockaddrFamilyField.width field)
                state

        match SockaddrFamilyField.width field with
        | 1 -> int bytes.[0]
        | _ -> int (BinaryPrimitives.ReadUInt16LittleEndian (bytes.AsSpan ()))

    /// `sockAddr->sa_family = (sa_family_t) value`, truncated to this platform's
    /// width exactly as the C's assignment through a `sa_family_t*` is. The
    /// truncation is not hypothetical: upstream's conversion failure path stores
    /// the unconverted PAL number through that same pointer.
    let private writeSockaddrFamily
        (ctx : NativeCallContext)
        (operation : string)
        (platform : SimulatedUnixPlatform)
        (buffer : ManagedPointerSource)
        (platformFamily : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let field = SimulatedUnixPlatform.sockaddrFamilyField platform
        let offset = SockaddrFamilyField.offset field

        let bytes =
            match SockaddrFamilyField.width field with
            | 1 -> [| byte platformFamily |]
            | _ ->
                let buf = Array.zeroCreate<byte> 2
                BinaryPrimitives.WriteUInt16LittleEndian (Span<byte> buf, uint16 platformFamily)
                buf

        writeBytesThrough
            ctx
            operation
            (bufferFieldAt ctx operation buffer offset state)
            (ImmutableArray.CreateRange bytes)
            state

    /// Turn the NUL-terminated bytes a guest passed as a pathname into a
    /// `UnixPath`, applying the length rule a kernel applies at *its* boundary.
    ///
    /// Takes bytes rather than machine state, so the boundary — the one part of
    /// the length rules that the resolver can never see — is testable without a
    /// heap. `readGuestPathBytes` is the half that needs a machine.
    ///
    /// The rules themselves, and the order they run in, are
    /// `PathArgument.parse`'s. What is PawPrint's is the reachability: CoreLib
    /// never produces a path that is not valid UTF-8, because it encodes from a
    /// string, so only a hand-rolled P/Invoke can reach that refusal.
    let internal parseGuestPathBytes
        (operation : string)
        (limits : PathLimits)
        (bytes : byte[])
        : Result<UnixPath, UnixError>
        =
        match PathArgument.parse limits (ImmutableArray.CreateRange bytes) with
        | Ok (PathArgument.Parsed path) -> Ok path
        | Ok (PathArgument.Failed error) -> Error error
        | Error PathArgumentRefusal.NotUtf8 ->
            let rendered = bytes |> Array.map (sprintf "%02X") |> String.concat " "

            failwith
                $"%s{operation}: the guest passed a path that is not valid UTF-8 (bytes: %s{rendered}). This kernel models a filename as a string of characters, so this path has no representation in the emulated filesystem, and decoding it leniently would silently resolve a different file. CoreLib never produces such a path — it encodes from a string — so this can only come from a hand-rolled P/Invoke."

    /// The resolution of a guest path, or the errno the lookup owes the guest.
    ///
    /// A relative path resolves against `EmulatedKernel.CurrentDirectoryInode`,
    /// the directory the simulated process holds open — so this function cannot
    /// fail for a reason that is the *host's* fault. Whether the configured
    /// current directory names anything is settled once, when the kernel is
    /// built, which is where the crash for a host that misconfigured it lives.
    ///
    /// `trailingSeparatorPolicy` is the caller's, not the path's: a *creating*
    /// open refuses a trailing separator on Linux where every lookup merely
    /// records the demand. See `TrailingSeparatorPolicy`.
    let private resolveGuestPathFull
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (kernel : EmulatedKernel)
        (path : UnixPath)
        : Result<Resolution, UnixError>
        =
        UnixPathResolution.resolvePathFull policy trailingSeparatorPolicy path (EmulatedKernel.unix kernel)

    /// The inode a path names, or the errno the lookup owes the guest — what
    /// every non-creating caller wants.
    let private resolveGuestPath
        (policy : SymlinkPolicy)
        (kernel : EmulatedKernel)
        (path : UnixPath)
        : Result<InodeNumber, UnixError>
        =
        UnixPathResolution.resolvePath policy path (EmulatedKernel.unix kernel)

    /// How big the `d_name` buffer inside one directory stream is.
    ///
    /// Fixed for the stream's life, because its address *is* the `DIR*` the
    /// guest holds. 1024 is what Darwin's `struct dirent` declares
    /// (`__DARWIN_MAXPATHLEN`), and it bounds every name either modelled kernel
    /// can store: Linux permits 255 bytes, and Darwin 255 UTF-16 code units,
    /// which is at most 765 bytes.
    let private directoryNameBufferBytes : int = 1024

    /// `sizeof(DirectoryEntry)`: a pointer, then two 32-bit fields.
    let private directoryEntrySize : int = 16

    // `Interop.Sys.NodeType`, which is the platform's own `DT_*`; measured
    // identical on both kernels for every inode kind PawPrint can represent.
    let private directoryEntryTypeDirectory : int = 4
    let private directoryEntryTypeRegular : int = 8
    let private directoryEntryTypeSymlink : int = 10

    /// The native block a guest's `DIR*` names.
    ///
    /// Loudly partial: a `DIR*` is opaque, and the only legal values are the
    /// ones `SystemNative_OpenDir` handed out. Passing anything else to
    /// `readdir`/`closedir` is undefined behaviour on a real libc rather than an
    /// error it reports, so there is no errno to give back.
    let private directoryStreamBlock (operation : string) (ptr : ManagedPointerSource) : NativeMemoryBlockId =
        match NativeCall.tryResolveNativeHeapFreeTarget ptr with
        | Ok (Some block) -> block
        | Ok None ->
            failwith
                $"%s{operation}: the guest passed a null DIR*. `opendir` returns NULL only on failure, which CoreLib checks before ever calling this, so a null here is a guest that ignored that failure — undefined behaviour on a real libc."
        | Error reason ->
            failwith $"%s{operation}: the DIR* argument is not a directory stream this kernel handed out: %s{reason}"

    /// Fill in a guest's `Interop.Sys.DirectoryEntry`.
    ///
    /// Unlike `writeFileStatus` this cannot write a byte image at ABI offsets,
    /// because the first field is a *pointer* and a pointer has no byte image
    /// here — PawPrint's address space is a graph of typed cells, not a flat
    /// array. So the struct is written as a whole value, exactly as the guest's
    /// own `stobj` would write it, with each field found **by its offset and
    /// width** rather than by name: the ABI is the contract, and a guest that
    /// hand-rolls this P/Invoke may call its fields whatever it likes.
    let private writeDirectoryEntry
        (ctx : NativeCallContext)
        (operation : string)
        (directoryEntryHandle : ConcreteTypeHandle)
        (output : ManagedPointerSource)
        (name : ManagedPointerSource)
        (nameLength : int)
        (inodeType : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let zeroed, state =
            IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes directoryEntryHandle

        if CliType.sizeOf zeroed <> directoryEntrySize then
            failwith
                $"%s{operation}: the output struct is %d{CliType.sizeOf zeroed} bytes, but `DirectoryEntry` is %d{directoryEntrySize}. Either the guest hand-rolled this P/Invoke with a struct that is not layout-identical to `Interop.Sys.DirectoryEntry`, or upstream has changed the layout — check `ConvertDirent` in pal_io.c against `Interop.ReadDir.cs`."

        let structure =
            match zeroed with
            | CliType.ValueType structure -> structure
            | other ->
                failwith
                    $"%s{operation}: `DirectoryEntry` came back as %O{other} rather than a value type; this is an interpreter bug."

        /// The one field of `structure` that covers exactly `size` bytes at
        /// `offset`. Ambiguity would mean an overlapping layout, which this
        /// struct does not have and which the caller could not resolve anyway.
        let fieldAt (offset : int) (size : int) (structure : CliValueType) : CliConcreteField =
            match CliValueType.FieldsAt offset structure |> List.filter (fun f -> f.Size = size) with
            | [ field ] -> field
            | found ->
                failwith
                    $"%s{operation}: `DirectoryEntry` has %d{List.length found} fields of %d{size} bytes at offset %d{offset}, expected exactly one. The guest's struct is not layout-identical to `Interop.Sys.DirectoryEntry`."

        let setInt32At (offset : int) (value : int32) (structure : CliValueType) : CliValueType =
            let field = fieldAt offset 4 structure
            let bytes = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, value)
            // Shaped from the field's own zero value rather than assembled as a
            // bare `Int32`: `InodeType` is a CLR enum in CoreLib's declaration
            // and a plain `int` in a hand-rolled one, and this writes whichever
            // the guest declared.
            CliValueType.WithFieldSetById field.Id (CliType.OfBytesLike field.Contents bytes) structure

        let namePointer =
            match name with
            | ManagedPointerSource.Null -> NativeIntSource.Verbatim 0L
            | pointer -> NativeIntSource.ManagedPointer pointer

        let structure =
            structure
            |> (fun structure ->
                CliValueType.WithFieldSetById
                    (fieldAt 0 8 structure).Id
                    (CliType.Numeric (CliNumericType.NativeInt namePointer))
                    structure
            )
            |> setInt32At 8 nameLength
            |> setInt32At 12 inodeType

        IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state output (CliType.ValueType structure)

    /// `sizeof(FileStatus)`: four 32-bit fields, then twelve 64-bit ones, then
    /// a trailing `uint32_t`, rounded up to the struct's 8-byte alignment.
    let private fileStatusSize : int = 120

    /// How much of `FileStatus` is fields rather than trailing padding. The C
    /// shim writes exactly this much — `ConvertFileStatus` assigns fields and
    /// never touches the four bytes after `UserFlags` — so neither does
    /// PawPrint.
    let private fileStatusDataSize : int = 116

    /// Fill in a guest's `Interop.Sys.FileStatus` from a status the kernel
    /// reported, and hand back the zero that says the call succeeded.
    ///
    /// Shared by `SystemNative_Stat`/`LStat`, which reach the inode from a
    /// path, and by `SystemNative_FStat`, which reaches it from a file
    /// descriptor. One encoder rather than two: the 120-byte layout *is* the
    /// contract with the guest, and two copies of it could disagree — a
    /// disagreement no differential test could catch, since the real runtime
    /// would agree with itself either way.
    ///
    /// This is the whole of PawPrint's half of `stat`. `UnixPathResolution.fstat`
    /// answers what a kernel knows; the layout it goes into is .NET's platform
    /// abstraction layer, which is PawPrint's business and not a POSIX
    /// simulator's — so the offsets, the `FileStatusFlags` word and the fields
    /// this kernel does not model are all decided here.
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
        (status : FileStatus)
        (output : ManagedPointerSource)
        (state : IlMachineState)
        : NativeHandlerResult option
        =
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

        // `FileStatusFlags.HasBirthTime = 1`; nothing else is defined. The
        // kernel says whether the platform it simulates would report a birth
        // time at all, and `pal_io.c` zeroes the field under `#else` when it
        // would not — so a withheld birth time is a zero *and* a clear flag,
        // which is the pair the BCL reads.
        putInt32 0 (if status.BirthTime.IsSome then 1 else 0)

        putInt32 4 status.Mode
        putUInt32 8 status.UserId
        putUInt32 12 status.GroupId
        putInt64 16 status.Size
        putTime 24 status.AccessTime
        putTime 40 status.ModificationTime
        putTime 56 status.StatusChangeTime

        putTime
            72
            (match status.BirthTime with
             | Some birth -> birth
             | None -> UnixTimestamp.epoch)

        putInt64 88 status.DeviceId
        // `st_rdev`, non-zero only for device nodes, which the emulated
        // filesystem cannot represent — so this kernel reports no such field and
        // PawPrint writes what a real runtime would see for a file that is not
        // one.
        putInt64 96 0L

        putInt64
            104
            (match status.Inode with
             | InodeNumber value -> value)

        // macOS's `UF_HIDDEN`, gated on `HAVE_STAT_FLAGS`. The emulated kernel
        // models no BSD file flags and nothing in its filesystem is hidden, so
        // zero is the honest answer on either platform.
        putUInt32 112 0u

        writeBytesThrough ctx operation output (ImmutableArray.CreateRange image) state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
        |> NativeHandlerResult.completed
        |> Some

    /// PawPrint's half of a refused `fstat`: which entry point asked, which
    /// descriptor it named, and what PawPrint would have to decide to lift the
    /// refusal. The library's half says what it measured and what its model has
    /// not got.
    let private fstatRefusalMessage (operation : string) (fd : int) (refusal : FStatRefusal) : string =
        let reachability =
            match refusal with
            | FStatRefusal.StandardStream _ ->
                "The BCL reaches FStat only through a SafeFileHandle it opened itself, so this is a hand-rolled P/Invoke or a new code path -- and either wants a decision rather than a guess."
            | FStatRefusal.SocketEventPort
            | FStatRefusal.Socket _ ->
                "Decide what an inode-free descriptor's struct stat is -- for streams, ports and sockets together (issue #956) -- rather than guessing."

        $"%s{operation}: fd %d{fd}: %s{FStatRefusal.describe refusal} %s{reachability}"

    /// The one shape `SystemNative_MkDir`, `SystemNative_Unlink` and
    /// `SystemNative_RmDir` share: decode a NUL-terminated path out of guest
    /// memory, hand it to the kernel, and turn the answer into the zero or the
    /// -1-with-errno the C returns.
    ///
    /// The guest-memory half is PawPrint's — the pointer, the `PATH_MAX`-bounded
    /// scan, the byte-to-`UnixPath` parse — and the syscall itself is the
    /// kernel's; `call` is the whole of what distinguishes the three.
    let private pathSyscall
        (ctx : NativeCallContext)
        (operation : string)
        (call : UnixPath -> UnixSystem<ThreadId, SignalHandler> -> SyscallAnswer * UnixSystem<ThreadId, SignalHandler>)
        (state : IlMachineState)
        : NativeHandlerResult option
        =
        let fail (error : UnixError) : NativeHandlerResult option =
            withErrnoOnly ctx error state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

        match
            bufferPointerArgument operation "path" ctx.Instruction.Arguments.[0]
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

        match call path (EmulatedKernel.unix state.Kernel) with
        | SyscallAnswer.Failed error, system ->
            withErrno ctx error system state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | SyscallAnswer.Completed _, system ->
            withAnswered system state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some

    /// One pathname argument's *bytes*: the pointer and the `PATH_MAX`-bounded
    /// scan, and no more than that.
    ///
    /// Deliberately not decoded here. A syscall taking two pathnames copies them
    /// in at points the kernel chooses and may never reach the second, so the
    /// decode — which can refuse a pathname outright — belongs where the kernel
    /// performs it. Reading the bytes early costs nothing, being a pure read of
    /// guest memory; refusing early would answer about a pathname the syscall
    /// never looked at.
    let private pathArgumentBytes
        (ctx : NativeCallContext)
        (operation : string)
        (parameter : string)
        (argument : CliType)
        (state : IlMachineState)
        : PathArgumentBytes
        =
        match
            bufferPointerArgument operation parameter argument
            |> BufferPointer.dereferenceable
        with
        | None -> PathArgumentBytes.Unreadable
        | Some pointer ->

        let limits = SimulatedUnixPlatform.pathLimits state.Kernel.UnixPlatform

        NativeCall.readNullTerminatedBytesWithin
            operation
            ctx.BaseClassTypes
            state
            pointer
            (PathLimits.pathMaxBytes limits)
        |> ImmutableArray.CreateRange
        |> PathArgumentBytes.Bytes

    /// `SystemNative_Rename`: the only syscall here that takes two pathnames,
    /// and so the only one where *when* each is read out of guest memory is
    /// observable.
    ///
    /// The second pathname is not read until the kernel says it has reached the
    /// point of copying it in. That is not tidiness: reading one can refuse
    /// outright — a symbolic or unstatable pointer is a refusal at transfer, and
    /// bytes that are not valid UTF-8 name a file this kernel cannot represent —
    /// and both flavours have calls that finish without ever reading the
    /// destination. Reading it early turns those into a crash where the guest is
    /// owed the source's errno.
    let private renameSyscall (ctx : NativeCallContext) (state : IlMachineState) : NativeHandlerResult option =
        let operation = "SystemNative_Rename"

        let answer (outcome : Result<SyscallAnswer * UnixSystem<ThreadId, SignalHandler>, PathArgumentRefusal>) =
            match outcome with
            | Error PathArgumentRefusal.NotUtf8 ->
                // Reached only for a pathname the syscall actually copied in,
                // which is the whole reason the decode is the kernel's rather
                // than this boundary's.
                failwith
                    $"%s{operation}: the guest passed a path that is not valid UTF-8. This kernel models a filename as a string of characters, so this path has no representation in the emulated filesystem, and decoding it leniently would silently resolve a different file. CoreLib never produces such a path -- it encodes from a string -- so this can only come from a hand-rolled P/Invoke."
            | Ok (SyscallAnswer.Failed error, system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (SyscallAnswer.Completed _, system) ->
                withAnswered system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

        let source =
            pathArgumentBytes ctx operation "oldPath" ctx.Instruction.Arguments.[0] state

        match UnixNamespace.renameSourcePhase source (EmulatedKernel.unix state.Kernel) with
        | Error refusal -> answer (Error refusal)
        | Ok (RenameProgress.Answered (syscallAnswer, system)) -> answer (Ok (syscallAnswer, system))
        | Ok (RenameProgress.NeedsDestination paused) ->
            pathArgumentBytes ctx operation "newPath" ctx.Instruction.Arguments.[1] state
            |> fun destination -> UnixNamespace.renameWithDestination destination paused
            |> answer

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

            state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error))
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

        match UnixPathResolution.stat policy path (EmulatedKernel.unix state.Kernel) with
        | FileStatusAnswer.Failed error -> fail error
        | FileStatusAnswer.Reported status ->

        // The output pointer is only decoded here, on the path that actually
        // writes through it.
        match
            bufferPointerArgument operation "output" instruction.Arguments.[1]
            |> BufferPointer.dereferenceable
        with
        | None -> fail UnixError.EFAULT
        | Some output ->

        writeFileStatus ctx operation fileStatusHandle status output state

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
        | Some "SystemNative_GetSocketAddressSizes",
          [ ConcretePointer _ ; ConcretePointer _ ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_GetSocketAddressSizes(int32_t*, int32_t*,
            // int32_t*, int32_t*)` (pal_networking.c:700): four `sizeof`s of the
            // shim's own compile, with no socket, no errno and no state involved.
            //
            // The sole managed caller is `System.Net.Primitives`'
            // `SocketAddressPal` class initialiser, which latches all four and
            // sizes every `SocketAddress` by them — and which *discards the
            // return value* (`pop` at IL_0019), so the screen below is
            // unobservable through that caller and exists for a hand-rolled one.
            //
            // Not to be confused with `SystemNative_GetMaximumAddressSize` below,
            // despite the similar name and despite the fourth of these being the
            // same `sizeof(struct sockaddr_storage)`: different entry point,
            // different caller, and `SocketPal` latches that one separately.
            let operation = "SystemNative_GetSocketAddressSizes"

            let sizeOut (name : string) (index : int) : BufferPointer =
                bufferPointerArgument operation name instruction.Arguments.[index]

            let outputs =
                [
                    "ipv4SocketAddressSize", sizeOut "ipv4SocketAddressSize" 0
                    "ipv6SocketAddressSize", sizeOut "ipv6SocketAddressSize" 1
                    "udsSocketAddressSize", sizeOut "udsSocketAddressSize" 2
                    "maxSocketAddressSize", sizeOut "maxSocketAddressSize" 3
                ]

            if outputs |> List.exists (fun (_, p) -> p = BufferPointer.RawAddress 0UL) then
                // All four are screened together, before any of them is written,
                // so a call with one null out-parameter leaves the other three
                // untouched.
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EFAULT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            else

            let cells =
                outputs
                |> List.map (fun (name, pointer) -> requireStorage operation name pointer)

            let sizes = SimulatedUnixPlatform.socketAddressSizes state.Kernel.UnixPlatform

            let values =
                [ sizes.InterNetwork ; sizes.InterNetworkV6 ; sizes.UnixDomain ; sizes.Storage ]

            let state =
                List.zip cells values
                |> List.fold
                    (fun state (cell, value) ->
                        let bytes = Array.zeroCreate<byte> 4
                        BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, value)
                        writeBytesThrough ctx operation cell (ImmutableArray.CreateRange bytes) state
                    )
                    state

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetAddressFamily",
          [ ConcretePointer _ ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_GetAddressFamily(const uint8_t* socketAddress,
            // int32_t socketAddressLen, int32_t* addressFamily)`
            // (pal_networking.c:714). Reads the family out of a blob the guest
            // owns and reports it in PAL numbering. No socket and no kernel state:
            // this and the seven below are pure `struct sockaddr` accessors, and
            // the only thing about them that is not arithmetic is which platform's
            // layout and `AF_*` numbering they use.
            let operation = "SystemNative_GetAddressFamily"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]

            let familyOut =
                bufferPointerArgument operation "addressFamily" instruction.Arguments.[2]

            let fail (error : UnixError) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal error)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            if
                blob = BufferPointer.RawAddress 0UL
                || familyOut = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
            then
                fail UnixError.EFAULT
            elif not (sockaddrFamilyIsInBounds platform socketAddressLen) then
                // The declared length does not reach the family field. Screened
                // before the read, so a blob shorter than its family is EFAULT
                // rather than whatever the bytes past it happen to be.
                fail UnixError.EFAULT
            else

            let blobStorage = requireStorage operation "socketAddress" blob
            let familyStorage = requireStorage operation "addressFamily" familyOut

            let platformFamily = readSockaddrFamily ctx operation platform blobStorage state

            // A family the shim's switch has no case for is reported as
            // `AddressFamily_AF_UNKNOWN`, and the call still succeeds — upstream's
            // conversion writes the raw platform number through the out-parameter
            // on the way to returning false, and this entry point then overwrites
            // it, so that value never reaches a guest.
            let palFamily =
                match SocketArgumentsPal.addressFamilyPlatformToPal platform platformFamily with
                | Some pal -> pal
                | None -> -1

            let bytes = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, palFamily)

            writeBytesThrough ctx operation familyStorage (ImmutableArray.CreateRange bytes) state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_SetAddressFamily",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_SetAddressFamily(uint8_t* socketAddress,
            // int32_t socketAddressLen, int32_t addressFamily)`
            // (pal_networking.c:735).
            let operation = "SystemNative_SetAddressFamily"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let palFamily = NativeCall.int32Argument operation instruction.Arguments.[2]

            let fail (error : UnixError) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal error)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            if
                blob = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
                || not (sockaddrFamilyIsInBounds platform socketAddressLen)
            then
                fail UnixError.EFAULT
            else

            let blobStorage = requireStorage operation "socketAddress" blob

            // The conversion writes through the blob whether or not it succeeds:
            // upstream's failing branch stores `(sa_family_t) palAddressFamily`,
            // truncated to the field's width, and *then* returns EAFNOSUPPORT. So
            // an unconvertible family leaves the low byte or two of the value
            // behind in the blob rather than leaving it as it was.
            let converted = SocketArgumentsPal.addressFamilyPalToPlatform platform palFamily

            let written =
                match converted with
                | Some platformFamily -> platformFamily
                | None -> palFamily

            let state = writeSockaddrFamily ctx operation platform blobStorage written state

            match converted with
            | Some _ ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | None ->
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EAFNOSUPPORT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
        | Some "SystemNative_GetPort",
          [ ConcretePointer _ ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_GetPort(const uint8_t* socketAddress,
            // int32_t socketAddressLen, uint16_t* port)` (pal_networking.c:752).
            //
            // Note the screen order, which is what makes a short blob and an
            // unsupported family distinguishable: the family bounds check comes
            // first and answers EFAULT, then the family switch answers
            // EAFNOSUPPORT for anything but the two internet families, and only
            // inside those arms is the blob's length compared against the whole
            // struct. So a two-byte AF_UNIX blob is EAFNOSUPPORT while a two-byte
            // AF_INET blob is EFAULT.
            let operation = "SystemNative_GetPort"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let portOut = bufferPointerArgument operation "port" instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            if blob = BufferPointer.RawAddress 0UL then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            elif not (sockaddrFamilyIsInBounds platform socketAddressLen) then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob
            let platformFamily = readSockaddrFamily ctx operation platform blobStorage state
            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            // `switch (sockAddr->sa_family)` over `AF_INET` and `AF_INET6`, on the
            // raw platform number in the blob rather than on a converted one.
            let required =
                if platformFamily = SimulatedUnixPlatform.internetAddressFamily then
                    Some sizes.InterNetwork
                elif platformFamily = SimulatedUnixPlatform.internetV6AddressFamily platform then
                    Some sizes.InterNetworkV6
                else
                    None

            match required with
            | None ->
                // The switch's default. `port` is never touched, so a caller that
                // passed nothing to write through is not refused here.
                complete (UnixErrorPal.toPal UnixError.EAFNOSUPPORT) state
            | Some minimumLength ->

            if socketAddressLen < minimumLength then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let portStorage = requireUnscreenedStorage operation "port" portOut

            let bytes =
                readBytesThrough
                    ctx
                    operation
                    (bufferFieldAt ctx operation blobStorage InternetSockaddr.port.Offset state)
                    2
                    state

            // `ntohs`: the port sits in the blob in network order and is reported
            // to the caller in the machine's own.
            let port = BinaryPrimitives.ReadUInt16BigEndian (bytes.AsSpan ())

            let output = Array.zeroCreate<byte> 2
            BinaryPrimitives.WriteUInt16LittleEndian (Span<byte> output, port)

            writeBytesThrough ctx operation portStorage (ImmutableArray.CreateRange output) state
            |> complete UnixErrorPal.palSuccess
        | Some "SystemNative_SetPort",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_SetPort(uint8_t* socketAddress, int32_t
            // socketAddressLen, uint16_t port)` (pal_networking.c:794): the mirror
            // of `SystemNative_GetPort` above, screen for screen.
            let operation = "SystemNative_SetPort"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let port = NativeCall.uint16Argument operation instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            if blob = BufferPointer.RawAddress 0UL then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            elif not (sockaddrFamilyIsInBounds platform socketAddressLen) then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob
            let platformFamily = readSockaddrFamily ctx operation platform blobStorage state
            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            // `switch (sockAddr->sa_family)` over `AF_INET` and `AF_INET6`, on the
            // raw platform number in the blob rather than on a converted one.
            let required =
                if platformFamily = SimulatedUnixPlatform.internetAddressFamily then
                    Some sizes.InterNetwork
                elif platformFamily = SimulatedUnixPlatform.internetV6AddressFamily platform then
                    Some sizes.InterNetworkV6
                else
                    None

            match required with
            | None -> complete (UnixErrorPal.toPal UnixError.EAFNOSUPPORT) state
            | Some minimumLength ->

            if socketAddressLen < minimumLength then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            // `htons`.
            let bytes = Array.zeroCreate<byte> 2
            BinaryPrimitives.WriteUInt16BigEndian (Span<byte> bytes, port)

            writeBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation blobStorage InternetSockaddr.port.Offset state)
                (ImmutableArray.CreateRange bytes)
                state
            |> complete UnixErrorPal.palSuccess
        | Some "SystemNative_GetIPv4Address",
          [ ConcretePointer _ ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_GetIPv4Address(const uint8_t* socketAddress,
            // int32_t socketAddressLen, uint32_t* address)`
            // (pal_networking.c:836).
            //
            // Unlike the port accessors this screens the whole struct's length up
            // front rather than inside a family arm, and answers EINVAL rather
            // than EAFNOSUPPORT for the wrong family — the two entry points are
            // not written to the same shape, and the difference is guest-visible.
            let operation = "SystemNative_GetIPv4Address"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let addressOut = bufferPointerArgument operation "address" instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            if
                blob = BufferPointer.RawAddress 0UL
                || addressOut = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
                || socketAddressLen < sizes.InterNetwork
                || not (sockaddrFamilyIsInBounds platform socketAddressLen)
            then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob

            if
                readSockaddrFamily ctx operation platform blobStorage state
                <> SimulatedUnixPlatform.internetAddressFamily
            then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            else

            // `*address = sin_addr.s_addr`, a whole-word copy with no `ntohl`:
            // both sides of this call hold the address in network order.
            let bytes =
                readBytesThrough
                    ctx
                    operation
                    (bufferFieldAt ctx operation blobStorage InternetSockaddr.address.Offset state)
                    4
                    state

            writeBytesThrough ctx operation (requireStorage operation "address" addressOut) bytes state
            |> complete UnixErrorPal.palSuccess
        | Some "SystemNative_SetIPv4Address",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_SetIPv4Address(uint8_t* socketAddress,
            // int32_t socketAddressLen, uint32_t address)` (pal_networking.c:861).
            let operation = "SystemNative_SetIPv4Address"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let address = NativeCall.uint32Argument operation instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            if
                blob = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
                || socketAddressLen < sizes.InterNetwork
                || not (sockaddrFamilyIsInBounds platform socketAddressLen)
            then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob

            if
                readSockaddrFamily ctx operation platform blobStorage state
                <> SimulatedUnixPlatform.internetAddressFamily
            then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            else

            // Upstream also assigns `sin_family = AF_INET` here. The guard above
            // has already established that the field holds exactly that, and the
            // assignment is the same width, so it moves no byte; only the address
            // is written.
            let bytes = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> bytes, address)

            writeBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation blobStorage InternetSockaddr.address.Offset state)
                (ImmutableArray.CreateRange bytes)
                state
            |> complete UnixErrorPal.palSuccess
        | Some "SystemNative_GetIPv6Address",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_GetIPv6Address(const uint8_t* socketAddress,
            // int32_t socketAddressLen, uint8_t* address, int32_t addressLen,
            // uint32_t* scopeId)` (pal_networking.c:882).
            let operation = "SystemNative_GetIPv6Address"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let addressOut = bufferPointerArgument operation "address" instruction.Arguments.[2]
            let addressLen = NativeCall.int32Argument operation instruction.Arguments.[3]
            let scopeIdOut = bufferPointerArgument operation "scopeId" instruction.Arguments.[4]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            if
                blob = BufferPointer.RawAddress 0UL
                || addressOut = BufferPointer.RawAddress 0UL
                || scopeIdOut = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
                || socketAddressLen < sizes.InterNetworkV6
                || addressLen < InternetV6Sockaddr.address.Width
                || not (sockaddrFamilyIsInBounds platform socketAddressLen)
            then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob

            if
                readSockaddrFamily ctx operation platform blobStorage state
                <> SimulatedUnixPlatform.internetV6AddressFamily platform
            then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            else

            // `memcpy_s` of exactly `NUM_BYTES_IN_IPV6_ADDRESS`, whatever the
            // caller declared `addressLen` to be beyond that.
            let addressBytes =
                readBytesThrough
                    ctx
                    operation
                    (bufferFieldAt ctx operation blobStorage InternetV6Sockaddr.address.Offset state)
                    InternetV6Sockaddr.address.Width
                    state

            let state =
                writeBytesThrough ctx operation (requireStorage operation "address" addressOut) addressBytes state

            // `*scopeId = sin6_scope_id`, host order on both sides, so this is a
            // straight four-byte copy like the IPv4 address above rather than the
            // byte-swap the port needs.
            //
            // Read *after* the address has been written, which is the order of the
            // two statements upstream and is observable: `address` may legally
            // point at byte 24 of this very blob, where `memcpy_s`'s own overlap
            // assertion still passes, and then the copy lands on `sin6_scope_id`
            // before it is read. Measured — a `fe80::` address aliased there
            // reports a scope of 33022 rather than the one that was set, on both
            // platforms alike.
            let scopeIdBytes =
                readBytesThrough
                    ctx
                    operation
                    (bufferFieldAt ctx operation blobStorage InternetV6Sockaddr.scopeId.Offset state)
                    4
                    state

            state
            |> writeBytesThrough ctx operation (requireStorage operation "scopeId" scopeIdOut) scopeIdBytes
            |> complete UnixErrorPal.palSuccess
        | Some "SystemNative_SetIPv6Address",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_SetIPv6Address(uint8_t* socketAddress,
            // int32_t socketAddressLen, uint8_t* address, int32_t addressLen,
            // uint32_t scopeId)` (pal_networking.c:912).
            let operation = "SystemNative_SetIPv6Address"
            let platform = state.Kernel.UnixPlatform

            let blob = bufferPointerArgument operation "socketAddress" instruction.Arguments.[0]
            let socketAddressLen = NativeCall.int32Argument operation instruction.Arguments.[1]
            let addressIn = bufferPointerArgument operation "address" instruction.Arguments.[2]
            let addressLen = NativeCall.int32Argument operation instruction.Arguments.[3]
            let scopeId = NativeCall.uint32Argument operation instruction.Arguments.[4]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let sizes = SimulatedUnixPlatform.socketAddressSizes platform

            if
                blob = BufferPointer.RawAddress 0UL
                || addressIn = BufferPointer.RawAddress 0UL
                || socketAddressLen < 0
                || socketAddressLen < sizes.InterNetworkV6
                || addressLen < InternetV6Sockaddr.address.Width
                || not (sockaddrFamilyIsInBounds platform socketAddressLen)
            then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let blobStorage = requireStorage operation "socketAddress" blob

            if
                readSockaddrFamily ctx operation platform blobStorage state
                <> SimulatedUnixPlatform.internetV6AddressFamily platform
            then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            else

            // `memcpy_s(&sin6_addr, 16, address, addressLen)`, whose failure mode
            // is not to fail: when `addressLen` exceeds the sixteen bytes of the
            // destination, the PAL's `memcpy_s` zeroes the destination, returns
            // ERANGE, and `ConvertByteArrayToIn6Addr` discards that — so the call
            // still reports success while having stored the all-zeroes address.
            // (The `assert(sizeInBytes >= count)` above it is compiled out of the
            // shipped Release build.) It reads nothing from the caller's buffer on
            // that path either, so neither does this.
            //
            // The getter is not symmetric: there `addressLen` is the *destination*
            // size, so a larger one is simply room to spare.
            let oversizedAddress = addressLen > InternetV6Sockaddr.address.Width

            let addressBytes =
                if oversizedAddress then
                    ImmutableArray.CreateRange (Array.zeroCreate<byte> InternetV6Sockaddr.address.Width)
                else
                    readBytesThrough
                        ctx
                        operation
                        (requireStorage operation "address" addressIn)
                        InternetV6Sockaddr.address.Width
                        state

            let flowInfo = Array.zeroCreate<byte> 4

            let scopeIdBytes = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> scopeIdBytes, scopeId)

            // Upstream's `sin6_family = AF_INET6` is a no-op for the same reason
            // `SystemNative_SetIPv4Address`'s is. `sin6_flowinfo = 0` is not: it
            // clears whatever the caller's buffer held there, so it is written.
            state
            |> writeBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation blobStorage InternetV6Sockaddr.address.Offset state)
                addressBytes
            |> writeBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation blobStorage InternetV6Sockaddr.flowInfo.Offset state)
                (ImmutableArray.CreateRange flowInfo)
            |> writeBytesThrough
                ctx
                operation
                (bufferFieldAt ctx operation blobStorage InternetV6Sockaddr.scopeId.Offset state)
                (ImmutableArray.CreateRange scopeIdBytes)
            |> complete UnixErrorPal.palSuccess
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
            pushInt32 (EmulatedKernel.lastSystemErrorFor ctx.Thread state.Kernel) ctx
            |> Some
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
            // `UnixErrorPal.ofRawErrno` refuses errnos whose meaning is
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

            pushInt32 (UnixErrorPal.ofRawErrnoUnder numbering raw) ctx |> Some
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
                (EvalStackValue.Int64 (
                    Int64Source.Verbatim (UnixMachineState.lowResolutionTimestampMs state.Kernel.Machine)
                ))
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
            let cpu = UnixTaskTable.cpuOf ctx.Thread state.Kernel.Tasks

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
            // `UnixMachineState.monotonicTimestampNanos`.
            //
            // Read-only, like every other clock observer: the scheduler is the
            // sole writer of `VirtualClockTicks`.
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int64 (
                    Int64Source.Verbatim (UnixMachineState.monotonicTimestampNanos state.Kernel.Machine)
                ))
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
                (EvalStackValue.Int64 (Int64Source.Verbatim (UnixMachineState.systemTimeAsTicks state.Kernel.Machine)))
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
        | Some "SystemNative_GetProcessPath", [], MethodReturnType.Returns (ConcretePointer _) ->
            // `char* SystemNative_GetProcessPath(void)` (pal_process.c:898-901) has
            // no body of its own: it is `return minipal_getexepath();`, so
            // `src/native/minipal/getexepath.h` is the whole specification.
            // There, both arms PawPrint models end in `realpath(..., NULL)` —
            // macOS on the buffer `_NSGetExecutablePath` filled, Linux on
            // `/proc/self/exe` and then on `AT_EXECFN` — which means the
            // result is `malloc`'d, canonical, and only produced at all if the
            // path resolved. (Arms PawPrint does not model differ: FreeBSD and
            // wasm `strdup` instead, so they promise ownership but not
            // resolution.) Ownership makes this a native-heap block base, as
            // for `GetUnixRelease` above: CoreLib reaches it through a
            // `StringMarshalling.Utf8` `LibraryImport` whose wrapper ends in
            // `Utf8StringMarshaller.Free` -> `SystemNative_Free`, which refuses
            // a byref into a managed array.
            //
            // The pointee type is matched loosely (`ConcretePointer _`) for the
            // same reason `GetUnixRelease` does so: name plus zero parameters
            // already pins the call, and a guest hand-rolling the import as
            // `void*`-returning means the same thing.
            match state.Kernel.ProcessPath with
            | None ->
                // No executable path. Answered the way both flavours answer a
                // process whose executable no longer resolves — NULL, errno
                // ENOENT — since that is the state PawPrint is genuinely in: it
                // models no `exec(2)`, so no file started this process. Measured
                // on macOS arm64 and Linux arm64 by having a guest unlink its own
                // executable before its first read; both give errno 2 and a null
                // `Environment.ProcessPath`.
                //
                // Not macOS's `errno = EINVAL` branch, which fires only when
                // `_NSGetExecutablePath` itself fails: that is "the dyld query
                // broke", a state with no analogue here, and one Linux never
                // reports. Not a `failwith` either — the flavours agree on an
                // answer, `Interop.Sys.GetProcessPath` is declared `string?`, and
                // `Environment.ProcessPath` handles null by design.
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno UnixError.ENOENT))
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Some path ->
                // A fresh allocation per call, because the guest owns and frees
                // each one. errno is left untouched, as on the C's success path.
                // (Measured, real .NET: macOS leaves a pre-set errno alone here
                // while Linux clobbers it with EINVAL, so no cross-flavour claim
                // is available and nothing may test it. CoreLib cannot see the
                // difference: its `SetLastError = true` stub zeroes errno before
                // the call and overwrites it after.)
                //
                // `Kernel.ProcessPath` is reported verbatim, and specifically is
                // *not* resolved against `Kernel.FileSystem` — see
                // `EmulatedKernel.ProcessPath` and docs/divergences.md, and note
                // `SystemNative_GetCwd` already answers `Kernel.CurrentDirectory`
                // the same way.
                let ptr, state =
                    NativeCall.allocateNativeHeapNullTerminatedUtf8
                        "SystemNative_GetProcessPath"
                        (AbsoluteUnixPath.toString path)
                        state

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

            /// Set errno and hand the guest a NULL `char*`, as the C does on
            /// every failure path.
            let fail (error : UnixError) (state : IlMachineState) : NativeHandlerResult option =
                withErrnoOnly ctx error state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Classifying the pointer inspects the argument's shape and
            // resolves nothing to storage, so it is safe this early. Both
            // guards below are decided *without* that resolution, because the C
            // decides them without dereferencing the buffer: the negative-size
            // guard runs before `getcwd` is even called. A guest that
            // hand-rolls this P/Invoke may therefore legally pass a bit pattern
            // PawPrint cannot resolve — `GetCwd((byte*)123, 0)` returns EINVAL
            // on the real runtime.
            let bufferPointer = bufferPointerArgument operation "buffer" bufferArgument

            let bufferIsNull =
                match bufferPointer with
                | BufferPointer.RawAddress address -> address = 0UL
                | BufferPointer.Storage _
                | BufferPointer.Symbolic _
                | BufferPointer.Unstatable _ -> false

            if bufferSize < 0 then
                // The shim's own guard, and the reason `UnixPathResolution.getcwd`
                // refuses a negative capacity rather than answering one: no
                // `getcwd(3)` sees a negative size, its argument being a
                // `size_t`. It *also* `assert`s this, so a checked native build
                // would abort instead; EINVAL is what a guest running against a
                // retail runtime can observe, and it is the only one of the two
                // behaviours we can reproduce.
                fail UnixError.EINVAL state
            elif bufferIsNull then
                // `getcwd(NULL, size)` is a glibc/BSD extension that mallocs
                // the result, and PawPrint does not model it: CoreLib's
                // `Interop.Sys.GetCwd` always supplies a `localloc` block or a
                // pinned `byte[]`, so a null here means a guest hand-rolled the
                // P/Invoke and is relying on the allocating form. Tested before
                // the size is handed to the kernel, which would otherwise report
                // EINVAL for `getcwd(NULL, 0)` — a call the real runtime
                // *succeeds*, and which the two flavours do not even agree on
                // (measured: `getcwd(NULL, 1)` mallocs the full path on Darwin
                // and is ERANGE under glibc).
                failwith
                    $"%s{operation}: refusing to honour the allocating `getcwd(NULL, %d{bufferSize})` extension (PawPrint models only the caller-supplied-buffer form, which is the only one CoreLib uses)"
            else

            match
                UnixPathResolution.getcwd
                    (BufferPointer.toUserBuffer bufferPointer)
                    bufferSize
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error (GetCwdRefusal.Buffer refusal) -> failwith (BufferPointer.refusalMessage bufferPointer refusal)
            | Error (GetCwdRefusal.FatalToTheProcess as refusal) ->
                // The library says what it measured; PawPrint says which
                // argument carried it and what a caller could do instead.
                failwith
                    $"%s{operation}: `buffer` is %O{bufferPointer}, and %s{GetCwdRefusal.describe refusal} Pass a buffer that names guest storage."
            | Ok answer ->

            match answer with
            | GetCwdAnswer.Failed error ->
                // Nothing is written here, and the buffer pointer is
                // deliberately not resolved: a call that writes nothing cannot
                // fault, and resolving it would turn an answer into a crash.
                // Darwin's own failure paths do scribble on the destination;
                // docs/divergences.md records what they leave and why this does
                // not reproduce it.
                fail error state
            | GetCwdAnswer.Reported terminated ->

            // Success returns the caller's own buffer, which is what `getcwd`
            // promises; note CoreLib only tests it against NULL and then decodes
            // `arg0`, so faithfulness here is for guests that hand-roll the
            // P/Invoke. errno is left untouched, per Unix convention (and
            // CoreLib has already zeroed it via `Marshal.SetLastSystemError 0`
            // immediately before the call).
            let destination =
                match BufferPointer.dereferenceable bufferPointer with
                | Some destination -> destination
                | None ->
                    failwith
                        $"%s{operation}: the kernel reported a path for a buffer that names no storage. Every such buffer is answered or refused before the transfer (this is an interpreter bug)."

            writeBytesThrough ctx operation destination terminated state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer destination) ctx.Thread
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
            let uid = UnixDescriptor.effectiveUserId (EmulatedKernel.unix state.Kernel)

            state
            |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 uid) ctx.Thread
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

                state.MapKernel (
                    EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error)
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

            // The shim's own rejections, in the order the C makes them, and both
            // stay here rather than crossing: neither is a kernel's decision, and
            // neither is expressible once the flags are a record. An
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

            // Each bit becomes the fact it stands for. `O_EXCL` is passed
            // through exactly as the guest set it rather than combined with
            // `O_CREAT` here: that it does nothing on its own is the kernel's
            // rule, and `UnixNamespace.openPath` owns it.
            let openFlags : OpenFlags =
                {
                    Access =
                        if accessMode = palWrOnly then FileAccessMode.WriteOnly
                        elif accessMode = palRdWr then FileAccessMode.ReadWrite
                        else FileAccessMode.ReadOnly
                    Create = flags &&& palCreat <> 0
                    Exclusive = flags &&& palExcl <> 0
                    Truncate = flags &&& palTrunc <> 0
                    NoFollow = flags &&& palNoFollow <> 0
                    CloseOnExec = flags &&& palCloExec <> 0
                    Synchronous = flags &&& palSync <> 0
                }

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

            // The `mode` argument crosses raw and unvalidated; see
            // `UnixNamespace.openPath` for why refusing a nonzero one without
            // `O_CREAT` would refuse the BCL's own read path.
            let mode = NativeCall.int32Argument operation instruction.Arguments.[2]

            match UnixNamespace.openPath openFlags path mode (EmulatedKernel.unix state.Kernel) with
            | SyscallAnswer.Failed error, system ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim -1L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | SyscallAnswer.Completed fd, system ->

            withAnswered system state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim fd)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        // `int32_t SystemNative_MkDir(const char* path, int32_t mode)`
        // (pal_io.c:696), an EINTR-retrying `mkdir(2)` and nothing else. The mode
        // parameter is matched loosely for the same reason `SystemNative_Open`'s
        // flags are: CoreLib declares it as `(int)UnixFileMode` while a guest
        // hand-rolling the P/Invoke writes `int`. Unlike `open`'s flags it is a
        // *raw* mode rather than a PAL value -- the C passes it straight to
        // `mkdir`.
        | Some "SystemNative_MkDir",
          [ ConcretePointer _ ; _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_MkDir"
            // Read before the path, and harmlessly so: it is an immediate rather
            // than a pointer, so decoding it dereferences nothing and cannot
            // pre-empt the EFAULT a bad path earns.
            let mode = NativeCall.int32Argument operation instruction.Arguments.[1]

            pathSyscall ctx operation (fun path system -> UnixNamespace.mkdir path mode system) state
        // `int32_t SystemNative_Unlink(const char* path)` (pal_io.c:368), an
        // EINTR-retrying `unlink(2)` and nothing else. CoreLib declares it as
        // `int Unlink(string)` under UTF-8 marshalling, so the argument that
        // arrives here is the same NUL-terminated byte pointer
        // `SystemNative_MkDir` takes.
        | Some "SystemNative_Unlink",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pathSyscall ctx "SystemNative_Unlink" UnixNamespace.unlink state
        // `int32_t SystemNative_ChDir(const char* path)` (pal_io.c): `chdir(2)`
        // and nothing else. CoreLib declares it as `int ChDir(string)` under
        // UTF-8 marshalling, so what arrives is the same NUL-terminated byte
        // pointer `SystemNative_MkDir` takes.
        | Some "SystemNative_ChDir",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pathSyscall ctx "SystemNative_ChDir" UnixPathResolution.chdir state
        // `int32_t SystemNative_RmDir(const char* path)` (pal_io.c): an
        // EINTR-retrying `rmdir(2)` and nothing else, taking a UTF-8 path
        // exactly as `SystemNative_Unlink` does.
        | Some "SystemNative_RmDir",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pathSyscall ctx "SystemNative_RmDir" UnixNamespace.rmdir state
        // `int32_t SystemNative_Rename(const char* oldPath, const char* newPath)`
        // (pal_io.c): `rename(2)` and nothing else -- not even an EINTR retry,
        // which `rename` cannot return. CoreLib declares both a UTF-8 `string`
        // overload and a `ref byte` one; the `ReadOnlySpan<char>` wrapper every
        // BCL caller reaches goes through the latter, so what arrives here is a
        // pair of NUL-terminated byte pointers.
        | Some "SystemNative_Rename",
          [ ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            renameSyscall ctx state
        // `DIR* SystemNative_OpenDir(const char* path)` (pal_io.c:532), an
        // EINTR-retrying `opendir(3)` and nothing else. NULL with errno set on
        // failure; the handle is opaque to the guest, which only passes it back
        // to `ReadDir` and `CloseDir`.
        | Some "SystemNative_OpenDir",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (ConcreteIntPtr state.ConcreteTypes) ->
            let operation = "SystemNative_OpenDir"

            let fail (error : UnixError) : NativeHandlerResult option =
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (
                    EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error)
                )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

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

            // `Follow`, and a trailing separator that merely records its demand:
            // measured on both kernels, `opendir` follows a final symlink and a
            // trailing separator changes no row at all — "ld" and "ld/" both
            // succeed, "f" and "f/" are both ENOTDIR. Nothing reads
            // `TrailingSeparatorDemanded`, because a directory is demanded
            // outright whether the separator was there or not.
            match UnixNamespace.opendir path (EmulatedKernel.unix state.Kernel) with
            | OpenDirAnswer.Failed error, system ->
                let numbering = SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform

                state.MapKernel (EmulatedKernel.withUnix system)
                |> fun state ->
                    state.MapKernel (
                        EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error)
                    )
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | OpenDirAnswer.Opened id, system ->

            // The block whose address the guest holds as its `DIR*`, and whose
            // bytes are the `d_name` buffer each `ReadDir` refills. One
            // allocation per stream rather than one per entry, because
            // `DirectoryEntry.Name` points into the stream's own storage and
            // stays valid only until the next `readdir`.
            //
            // This address is PawPrint's half of the stream: the library minted
            // the identity and has no addresses of its own, so binding the two
            // is the client's step. `checkInvariants` refuses a state in which
            // only one of them happened.
            let handle, state =
                NativeCall.allocateNativeHeapBlob
                    operation
                    (Array.zeroCreate directoryNameBufferBytes)
                    (withAnswered system state)

            let block =
                match handle with
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, 0), []) -> block
                | other ->
                    failwith
                        $"%s{operation}: the name buffer allocation returned an unexpected pointer shape (%O{other}); this is an interpreter bug."

            state.MapKernel (EmulatedKernel.withDirectoryStreamBlock block id)
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer handle))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        // `int32_t SystemNative_ReadDir(DIR* dir, DirectoryEntry* outputEntry)`
        // (pal_io.c:506): 0 when an entry was retrieved, **-1 at end of stream**,
        // and a **raw** errno otherwise — this entry point does not use
        // `SetLastError`, and `FileSystemEnumerator.FindNextEntry` feeds the
        // return value straight to `new Interop.ErrorInfo(result)`, which
        // converts it with `ConvertErrorPlatformToPal`. No failure arm exists
        // here: the cursor walk is total, and a `DIR*` this kernel never issued
        // is undefined behaviour on a real libc rather than an errno, so
        // `EmulatedKernel.directoryStreamId` refuses instead of inventing EBADF.
        //
        // The output parameter is matched loosely, as `SystemNative_Stat`'s is
        // and for the same reason: `Interop.Sys.DirectoryEntry` is internal to
        // CoreLib, so a guest exercising this handler declares its own
        // layout-identical struct. The pointee handle is bound because the
        // field offsets are derived from it.
        | Some "SystemNative_ReadDir",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer directoryEntryHandle ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_ReadDir"

            let block =
                directoryStreamBlock
                    operation
                    (NativeCall.managedPointerOfPointerArgument operation "dir" instruction.Arguments.[0])

            let output =
                NativeCall.managedPointerOfPointerArgument operation "outputEntry" instruction.Arguments.[1]

            let id = EmulatedKernel.directoryStreamId block state.Kernel

            // `errno = 0` before the `readdir`, which the C does itself
            // (pal_io.c:511) so that it can tell "end of stream" from "failed"
            // by reading errno back afterwards.
            //
            // Guest-observable, and *not* the same thing as the `SetLastError`
            // stub PawPrint does not model: this import declares no
            // `SetLastError`, so on real .NET nothing saves or restores errno
            // around the call and `Marshal.GetLastSystemError` reads what the C
            // left — zero. Without this, a guest that failed a syscall and then
            // enumerated a directory would still see the old errno.
            // Note what this does *not* touch: the offset on the descriptor
            // `opendir` opened. A real `readdir` moves it, but to a cookie
            // PawPrint cannot produce — measured, it jumps once when libc's
            // `getdents` buffer fills and then stays put as entries are consumed
            // out of it, and its value is the filesystem's own (a block boundary
            // on ext4, `2147483647` on APFS for a three-entry directory). An
            // entry index would be wrong in shape as well as in value. See
            // `docs/divergences.md`.
            let state = state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread 0)

            match UnixNamespace.readdir id (EmulatedKernel.unix state.Kernel) with
            | ReadDirAnswer.EndOfStream, system ->
                // "0 returned with null result -> end-of-stream". The C
                // `memset`s the output struct first, with the comment "managed
                // out param must be initialized", so the guest sees a null
                // `Name` rather than the previous entry's.
                writeDirectoryEntry
                    ctx
                    operation
                    directoryEntryHandle
                    output
                    ManagedPointerSource.Null
                    0
                    0
                    (withAnswered system state)
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | ReadDirAnswer.Entry (name, kind), system ->

            let nameBytes = name.AsSpan().ToArray ()

            if nameBytes.Length + 1 > directoryNameBufferBytes then
                // No name either kernel can store reaches this: Linux bounds a
                // component at 255 bytes and Darwin at 255 UTF-16 code units,
                // which is at most 765 bytes. A longer one can only have come
                // from a seed, which bypasses `NAME_MAX` — and such a name is
                // unreachable by every other syscall, because the walk checks
                // the limit before each lookup.
                //
                // The buffer's size is the ABI's rather than the kernel's, which
                // is why this check is here and not in `UnixNamespace.readdir`.
                failwith
                    $"%s{operation}: an entry of %d{nameBytes.Length} bytes does not fit the %d{directoryNameBufferBytes}-byte `d_name` buffer. No name either modelled kernel can store is this long, so this filesystem was seeded with one that could not exist."

            let nameLength =
                match SimulatedUnixPlatform.directoryEntryNameLength state.Kernel.UnixPlatform with
                | DirectoryEntryNameLength.Reported -> nameBytes.Length
                | DirectoryEntryNameLength.WalkToTerminator -> -1

            // The PAL's own `DT_*` numbering, which is not the `S_IFMT` one
            // `stat` reports — so the kind crosses as a kind and is encoded
            // here.
            let inodeType =
                match kind with
                | DirectoryEntryKind.RegularFile -> directoryEntryTypeRegular
                | DirectoryEntryKind.Directory -> directoryEntryTypeDirectory
                | DirectoryEntryKind.Symlink -> directoryEntryTypeSymlink

            // The name, then its terminator. The block was zero-filled at
            // allocation, so the terminator is already there for a first entry —
            // written explicitly all the same, because a *shorter* name after a
            // longer one would otherwise read back with the tail of its
            // predecessor.
            let terminated = Array.zeroCreate<byte> (nameBytes.Length + 1)
            Array.blit nameBytes 0 terminated 0 nameBytes.Length

            let state =
                (withAnswered system state)
                    .MapKernel (fun kernel ->
                        { kernel with
                            NativeMemoryPool = NativeMemoryPool.writeBytes block 0 terminated kernel.NativeMemoryPool
                        }
                    )

            writeDirectoryEntry
                ctx
                operation
                directoryEntryHandle
                output
                (ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, 0), []))
                nameLength
                inodeType
                state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        // `int32_t SystemNative_CloseDir(DIR* dir)` (pal_io.c:542), which is
        // `closedir(3)` with EINTR folded into success.
        | Some "SystemNative_CloseDir",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "SystemNative_CloseDir"

            let block =
                directoryStreamBlock
                    operation
                    (NativeCall.managedPointerOfPointerArgument operation "dir" instruction.Arguments.[0])

            let stream = EmulatedKernel.directoryStream block state.Kernel

            // Forget the stream *before* closing the descriptor under it: the
            // close is what reaps a directory whose last name went away while
            // this stream held it, and `heldInodes` counts this entry among the
            // things holding it.
            let state =
                state.MapKernel (EmulatedKernel.withoutDirectoryStream block)
                |> IlMachineState.freeNativeMemory block

            let state, result =
                match UnixDescriptor.close stream.Fd (EmulatedKernel.unix state.Kernel) with
                | Error refusal -> failwith (closeRefusalMessage operation stream.Fd refusal)
                | Ok (SyscallAnswer.Completed _, system) -> withAnswered system state, 0
                | Ok (SyscallAnswer.Failed error, system) ->
                    // EBADF, reachable only if the guest closed the stream's own
                    // descriptor behind its back, which it can do because fd
                    // numbers are guessable. `closedir` really does call
                    // `close` on that fd, so EBADF is what a real one reports.
                    withErrno ctx error system state, -1

            // Reaped here rather than left to `UnixDescriptor.close`, which does it
            // only for the descriptor it actually closed. Two paths reach this
            // with the directory still in the graph and nothing holding it: the
            // guest closed the stream's own descriptor beforehand (the EBADF arm
            // above), or that descriptor number has since been reused, in which
            // case `close` reaped the *replacement's* inode instead. Both are
            // undefined behaviour on a real libc, but neither may leave this
            // kernel with an inode no path reaches — `checkInvariants` would
            // report it, and it would be PawPrint's bookkeeping at fault rather
            // than the guest's. Idempotent when the descriptor did the job.
            state.MapKernel (EmulatedKernel.mapUnix (UnixDescriptor.forgetIfUnheld stream.Inode))
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetFileSystemType",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            // `uint32_t SystemNative_GetFileSystemType(intptr_t fd)`
            // (pal_io.c:1700): `fstatfs(2)` behind an EINTR retry. **Every
            // failure is reported as 0**, not -1, which is why the answer table
            // below is a success-or-failure rather than a bare number.
            //
            // Reached from `SafeFileHandle.CanLockTheFile`, and only there:
            // a `LOCK_SH` taken under write access is refused on NFS, CIFS and
            // SMB, where `flock` is unsafe. That is the combination
            // `File.WriteAllBytes` asks for (`FileMode.Create`,
            // `FileAccess.Write`, `FileShare.Read`), so this native is what
            // stands between a guest and the BCL's commonest write API.
            // `File.Create` never arrives here: it is `FileShare.None`, and
            // `CanLockTheFile` answers `LOCK_EX` without consulting anything.
            let operation = "SystemNative_GetFileSystemType"
            let fd = fdArgument operation instruction.Arguments.[0]

            // One call into the table, which is shared with the unit tests and
            // with the host-comparison oracle. Deliberately no per-descriptor
            // arms here: a mutation swapping two rows would have somewhere to
            // hide if the classification were re-done in the handler.
            let answer =
                FileDescriptorRegistry.tryFindObject fd state.Kernel.FileDescriptors
                |> EmulatedFileSystemType.reportedFor
                    (SimulatedUnixPlatform.flavour state.Kernel.UnixPlatform)
                    state.Kernel.FileSystemType

            match answer with
            | FileSystemTypeAnswer.Reported magic ->
                // errno untouched on success, as `fstatfs` leaves it.
                state
                |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 magic) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | FileSystemTypeAnswer.Failed error ->
                // CoreLib never reads this errno — its `LibraryImport` declares
                // no `SetLastError`, so `TryGetFileSystemType` sees only the 0.
                // A hand-rolled guest that does declare it would see the errno
                // on a real host, though, so recording it is what keeps the two
                // agreeing. `toRawErrno` rather than `toRawErrnoUnder`: both
                // errnos here are portable, and the stricter form would crash
                // loudly if a platform-dependent one were ever routed through.
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno error))
                |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 0u) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
        | Some "SystemNative_FTruncate",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_FTruncate(intptr_t fd, int64_t length)`
            // (pal_io.c:1094): `ftruncate(2)` verbatim behind an EINTR retry, with
            // no validation of its own, so the order the library applies is the
            // kernel's.
            //
            // This is how the BCL's commonest creation APIs truncate: with file
            // locking enabled `FileMode.Create` and `FileMode.Truncate` emit no
            // `O_TRUNC` at all and call this from `SafeFileHandle.Init`
            // (SafeFileHandle.Unix.cs:416) *after* the open has succeeded.
            // `FileStream.SetLength` reaches it through
            // `RandomAccess.SetFileLength`.
            let operation = "SystemNative_FTruncate"
            let fd = fdArgument operation instruction.Arguments.[0]
            let length = NativeCall.int64Argument operation instruction.Arguments.[1]

            match UnixDescriptor.ftruncate fd length (EmulatedKernel.unix state.Kernel) with
            | Error refusal -> failwith $"%s{operation}: %s{TruncationRefusal.describe refusal}"
            | Ok (SyscallAnswer.Failed error, system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (SyscallAnswer.Completed _, system) ->
                withAnswered system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
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

            match UnixPathResolution.fstat fd (EmulatedKernel.unix state.Kernel) with
            | Error refusal -> failwith (fstatRefusalMessage operation fd refusal)
            | Ok (FileStatusAnswer.Failed error) ->
                withErrnoOnly ctx error state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (FileStatusAnswer.Reported status) ->

            // The output pointer is decoded only here, on the path that actually
            // writes through it — which is also the order the C has: `fstat_`
            // runs before `ConvertFileStatus` touches the caller's struct, so a
            // bad descriptor beats a bad address.
            match
                bufferPointerArgument operation "output" instruction.Arguments.[1]
                |> BufferPointer.dereferenceable
            with
            | None ->
                withErrnoOnly ctx UnixError.EFAULT state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Some output ->

            writeFileStatus ctx operation fileStatusHandle status output state
        // `int32_t SystemNative_FLock(intptr_t fd, int32_t operation)`
        // (pal_io.c:744). The operation parameter is matched loosely for the
        // same reason `SystemNative_Open`'s flags are: CoreLib declares it as
        // the `Interop.Sys.LockOperations` enum while a guest hand-rolling the
        // P/Invoke writes `int`.
        | Some "SystemNative_FLock",
          [ ConcreteIntPtr state.ConcreteTypes ; _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_FLock(intptr_t fd, int32_t operation)`
            // (pal_io.c). The operation bits are passed straight through to
            // `flock(2)` — they are not PAL values that the C translates — so
            // they reach the library raw, which is where deciding that a
            // combination is malformed belongs.
            let operation = "SystemNative_FLock"
            let fd = fdArgument operation instruction.Arguments.[0]
            let request = NativeCall.int32Argument operation instruction.Arguments.[1]

            // Park re-entrantly: leave the native frame on the stack and the
            // caller's program counter naming the call, so a wake —
            // `Program.fireSyscallWakes` flipping this thread back to
            // Runnable once the lock could be granted — re-enters this handler
            // and finishes the acquisition from the caller's own frame.
            let park (condition : WakeCondition) (system : UnixSystem<ThreadId, SignalHandler>) =
                // The record and the status are written together. The record is
                // derived from the condition rather than built beside it, so a
                // task cannot be parked on one lock while the sweep polls for
                // another; and `close` needs it, to refuse destroying the
                // description this thread is waiting on.
                withAnswered (UnixDescriptor.parkFlock ctx.Thread condition system) state
                |> Scheduler.parkInSyscall ctx.Thread
                |> NativeHandlerResult.blockedRetainingFrame
                |> Some

            let granted (system : UnixSystem<ThreadId, SignalHandler>) =
                withAnswered system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            let refused (refusal : FLockRefusal) : NativeHandlerResult option =
                // The library says which measured divergence it will not answer
                // across; PawPrint says which managed caller could have asked,
                // which is a fact about CoreLib rather than about any kernel.
                failwith
                    $"%s{operation}: fd %d{fd}: %s{FLockRefusal.describe refusal} Configure a Linux platform, or model Darwin's flock (issue #956)."

            // A re-entry is told apart from a first entry by the record, not by
            // anything about the frame: the wake leaves the call site exactly as
            // the park found it. The record is also what says *what* to finish —
            // the descriptor the guest passed cannot be trusted for that, since
            // numbers are reused as soon as they are freed and another thread
            // may have closed and reopened this one while this call slept.
            match UnixTaskTable.parkedFor ctx.Thread state.Kernel.Tasks with
            | Some (ParkedSyscall.SocketWait _) ->
                // Unreachable: a task parked in a socket wait is not running IL,
                // and a woken one re-enters its own handler before it can reach
                // this one. Refused rather than treated as a first entry, which
                // would park over the stale record and destroy the evidence.
                failwith
                    $"%s{operation}: thread %O{ctx.Thread} entered an flock while its task is parked in a socket wait. A task blocks in one syscall at a time, so the wait's completion failed to clear its record (this is an interpreter bug)."
            | Some (ParkedSyscall.Flock parked) ->
                match UnixDescriptor.flockAcquire parked.Requester parked.Mode (EmulatedKernel.unix state.Kernel) with
                | Error refusal -> refused refusal
                | Ok (SyscallOutcome.WouldBlock condition, system) ->
                    // Woken and beaten: a release wakes every waiter and they
                    // race, so all but one of them find the lock gone. Park
                    // again on the same condition, which is the ordinary case
                    // rather than an edge one.
                    park condition system
                | Ok (SyscallOutcome.Answered answer, system) ->

                let system =
                    { system with
                        Tasks = UnixTaskTable.withParked ctx.Thread None system.Tasks
                    }

                match answer with
                | SyscallAnswer.Completed _ -> granted system
                | SyscallAnswer.Failed error ->
                    failwith
                        $"%s{operation}: finishing a parked acquisition on %O{parked.Requester} answered %O{error}. A resume acquires on a description the close path is obliged to keep alive, so it can only be granted or still blocked (this is an interpreter bug)."
            | None ->

            match UnixDescriptor.flock fd request (EmulatedKernel.unix state.Kernel) with
            | Error refusal -> refused refusal
            | Ok (SyscallOutcome.WouldBlock condition, system) ->
                // The system this parks with is not the one the call arrived
                // with: a conversion has already dropped the caller's old lock,
                // which is what a real kernel does before it sleeps.
                //
                // CoreLib never reaches this: SafeFileHandle.Init always sets
                // LOCK_NB. A guest hand-rolling the P/Invoke can.
                park condition system
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Failed error), system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed _), system) -> granted system
        // `int32_t SystemNative_PRead(intptr_t fd, void* buffer, int32_t
        // bufferSize, int64_t fileOffset)` (pal_io.c:1832): `pread(2)` verbatim,
        // with an EINTR retry and — unlike `SystemNative_Read`, which goes
        // through `Common_Read` — no argument validation of its own.
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

            // A negative size is the one input whose real behaviour PawPrint
            // cannot reproduce, so it is refused before anything else is
            // considered — and here rather than in the library, because it is
            // this shim's cast that produces it. The C casts the size to
            // `uint32_t`, asking the kernel for ~4 GB: measured, macOS answers
            // EINVAL (the count exceeds what it will accept) while Linux answers
            // EFAULT (the buffer's mapping does not extend that far), so Linux's
            // answer depends on the *guest's address space*, which PawPrint does
            // not model to that fidelity. Either choice would be a documented
            // divergence on one platform, and a silent one at that.
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

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            // `RandomAccess.ReadAtOffset` is the BCL's only caller and it reads
            // these answers: it catches ESPIPE (and ENXIO), clears
            // `SupportsRandomAccess`, and retries through `SystemNative_Read`.
            // The Darwin answer for stdout and stderr does *not* get that retry,
            // EBADF not being one of the errnos that clears the flag.
            match
                UnixReadWrite.pread
                    fd
                    (BufferPointer.toUserBuffer buffer)
                    bufferSize
                    fileOffset
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> failwith (BufferPointer.refusalMessage buffer refusal)
            | Ok (ReadAnswer.Failed error) ->
                withErrnoOnly ctx error state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (ReadAnswer.Completed bytes) ->

            // Empty means the read moved nothing *and did not touch the buffer*,
            // so the pointer must not be resolved: `pread(f, NULL, 5, atEof)` is
            // 0 rather than EFAULT, and resolving it here would turn that answer
            // into a crash for a symbolic address.
            let state =
                if bytes.IsEmpty then
                    state
                else

                let destination =
                    match BufferPointer.dereferenceable buffer with
                    | Some destination -> destination
                    | None ->
                        failwith
                            $"%s{operation}: fd %d{fd}: the kernel produced %d{bytes.Length} bytes for a buffer that names no storage. Every such buffer is answered or refused before the transfer (this is an interpreter bug)."

                writeBytesThrough ctx operation destination bytes state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim bytes.Length)) ctx.Thread
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

            // Refused before anything else, for the reason `SystemNative_PRead`
            // gives at length, and here rather than in the library because it is
            // this shim's cast that produces it: the C casts a negative size to
            // an unsigned ~4 GB count, and what a kernel then does depends on how
            // far the guest's buffer happens to be mapped — which PawPrint does
            // not model to that fidelity. CoreLib never sends one, every caller's
            // size coming from a span length.
            if bufferSize < 0 then
                failwith
                    $"%s{operation}: fd %d{fd} was given bufferSize %d{bufferSize}, which is negative. The C shim casts that to an unsigned ~4 GB count rather than rejecting it (unlike SystemNative_Write, which goes through Common_Write and answers ERANGE), and what a kernel then does is not a fact PawPrint can state. Pass a non-negative size."
            else

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            let refused (refusal : PWriteRefusal) : NativeHandlerResult option =
                match refusal with
                | PWriteRefusal.Buffer refusal -> failwith (BufferPointer.refusalMessage buffer refusal)
                | PWriteRefusal.ExceedsRepresentableLength _ ->
                    // The library says which limit of the model was reached;
                    // PawPrint says which managed caller could have reached it.
                    failwith
                        $"%s{operation}: fd %d{fd}: %s{PWriteRefusal.describe refusal} Reachable from the BCL: `RandomAccess.WriteAtOffset` passes the guest's own offset through, so a guest writing far past the end of a file gets here. Represent file contents sparsely (issue #956) before answering it."

            match
                UnixReadWrite.admitPWrite
                    fd
                    (BufferPointer.toUserBuffer buffer)
                    bufferSize
                    fileOffset
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> refused refusal
            | Ok (WriteAdmission.Answered (WriteAnswer.Failed error)) ->
                withErrnoOnly ctx error state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (WriteAdmission.Answered (WriteAnswer.Completed written)) ->
                // The zero-length no-op, which changes nothing at all — so there
                // is no system to write back, and the buffer was never resolved.
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim written)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (WriteAdmission.Transfer count) ->

            // Only now are the guest's bytes extracted: the admission answered
            // every question a `pwrite` settles without reading the buffer, so
            // resolving the pointer here is what a real kernel's `copy_from_user`
            // would do.
            let source =
                match BufferPointer.dereferenceable buffer with
                | Some source -> source
                | None ->
                    failwith
                        $"%s{operation}: fd %d{fd}: the kernel asked for %d{count} bytes from a buffer that names no storage. Every such buffer is answered or refused by the admission (this is an interpreter bug)."

            let bytes = readBytesThrough ctx operation source count state

            match UnixReadWrite.pwrite fd bytes fileOffset (EmulatedKernel.unix state.Kernel) with
            | Error refusal -> refused refusal
            | Ok (WriteAnswer.Failed error, system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (WriteAnswer.Completed written, system) ->
                withAnswered system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim written)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
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

            // `Common_Read`'s own guard, and hence *ahead of the descriptor*:
            // the C returns before `ToFileDescriptor` is ever evaluated, so
            // `Read(badfd, buf, -1)` is EINVAL rather than EBADF. That ordering
            // is a fact about the shim rather than about any kernel, which is
            // why it is answered here rather than passed on — `UnixReadWrite.read`
            // refuses a negative count outright.
            //
            // EINVAL, not ERANGE: `Common_Write` answers ERANGE for the same
            // mistake, and the asymmetry is upstream's rather than a typo here
            // (pal_io_common.h:41-45 against :59-63).
            if bufferSize < 0 then
                withErrno ctx UnixError.EINVAL (EmulatedKernel.unix state.Kernel) state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            else

            let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            match
                UnixReadWrite.read fd (BufferPointer.toUserBuffer buffer) bufferSize (EmulatedKernel.unix state.Kernel)
            with
            | Error (ReadRefusal.Buffer refusal) -> failwith (BufferPointer.refusalMessage buffer refusal)
            | Error (ReadRefusal.SocketConnectionState _ as refusal) ->
                // The library says what it measured; PawPrint says which managed
                // caller could have reached it, which is a fact about CoreLib.
                failwith
                    $"%s{operation}: fd %d{fd}: %s{ReadRefusal.describe refusal} Nothing in the BCL waits on this — CoreLib reaches a socket through `SystemNative_Receive`, `SafeSocketHandle` not being a `SafeFileHandle` — so this is a hand-rolled P/Invoke. Model the connection state (issue #956) before answering it."
            | Ok (ReadAnswer.Failed error, system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim -1)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (ReadAnswer.Completed bytes, system) ->

            // Empty means the read moved nothing *and did not touch the buffer*,
            // so the pointer must not be resolved: `read(f, NULL, 5)` at
            // end-of-file is 0 rather than EFAULT, and resolving it here would
            // turn that answer into a crash for a symbolic address.
            let state =
                if bytes.IsEmpty then
                    withAnswered system state
                else

                let destination =
                    match BufferPointer.dereferenceable buffer with
                    | Some destination -> destination
                    | None ->
                        failwith
                            $"%s{operation}: fd %d{fd}: the kernel produced %d{bytes.Length} bytes for a buffer that names no storage. Every such buffer is answered or refused before the transfer (this is an interpreter bug)."

                withAnswered system state |> writeBytesThrough ctx operation destination bytes

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim bytes.Length)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
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

            match UnixDescriptor.lseek fd offset whence (EmulatedKernel.unix state.Kernel) with
            | Error refusal ->
                // The kernel's half of the message is the library's, because it
                // is what that library measured; which managed caller could have
                // sent this is PawPrint's, because CoreLib is a fact about the
                // client rather than about any kernel.
                let reachability =
                    match refusal with
                    | LSeekRefusal.Sparseness _ ->
                        "CoreLib never sends these -- Interop.Sys.SeekWhence is 0, 1, 2 -- so this is a hand-rolled P/Invoke."
                    | LSeekRefusal.DirectoryEnd _ ->
                        "No BCL caller reaches it: SafeFileHandle.Init raises UnauthorizedAccessException on opening a directory for reading (SafeFileHandle.Unix.cs:320-327), and directory enumeration goes through opendir/readdir."

                failwith $"%s{operation}: fd %d{fd}: %s{LSeekRefusal.describe refusal} %s{reachability}"
            | Ok (SyscallAnswer.Failed error, system) ->
                withErrno ctx error system state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 (Int64Source.Verbatim -1L)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (SyscallAnswer.Completed position, system) ->
                withAnswered system state
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

                state.MapKernel (
                    EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error)
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

            // The whole ordering below `bufferSize` and the pathname is the
            // library's, including the composition `NoFollowFinal` plus "and
            // then it had better be a symlink" that
            // `TestVirtualFileSystemAgainstHost`'s `modelOutcome` checks against
            // a real kernel over generated symlink trees. What is left here is
            // the wire format.
            let destination = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            match
                UnixNamespace.readlink
                    path
                    (BufferPointer.toUserBuffer destination)
                    bufferSize
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> failwith (BufferPointer.refusalMessage destination refusal)
            | Ok (ReadLinkAnswer.Failed error) -> fail error
            | Ok (ReadLinkAnswer.Reported written) ->

            let storage =
                match BufferPointer.dereferenceable destination with
                | Some storage -> storage
                | None ->
                    failwith
                        $"%s{operation}: the kernel produced %d{written.Length} bytes for a buffer that names no storage. Every such buffer is answered or refused before the transfer (this is an interpreter bug)."

            // No terminator, and errno left alone: `readlink` writes exactly the
            // bytes it reports and reports success by a non-negative count, so a
            // NUL here would corrupt the byte after a target that exactly fits.
            writeBytesThrough ctx operation storage written state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim written.Length)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_SetErrNo",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "SystemNative_SetErrNo" instruction.Arguments.[0]

            state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (error))
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
                match UnixDescriptor.dup oldFd (EmulatedKernel.unix state.Kernel) with
                | SyscallAnswer.Completed newFd, system -> newFd, withAnswered system state
                | SyscallAnswer.Failed error, system -> -1L, withErrno ctx error system state

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
            let operation = "SystemNative_Close"
            let fd = fdArgument operation instruction.Arguments.[0]

            let resultCode, state =
                match UnixDescriptor.close fd (EmulatedKernel.unix state.Kernel) with
                | Error refusal -> failwith (closeRefusalMessage operation fd refusal)
                | Ok (SyscallAnswer.Completed _, system) -> 0, withAnswered system state
                | Ok (SyscallAnswer.Failed error, system) -> -1, withErrno ctx error system state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim resultCode)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_FcntlSetIsNonBlocking",
          [ ConcreteIntPtr state.ConcreteTypes ; _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_FcntlSetIsNonBlocking(intptr_t fd,
            // int32_t isNonBlocking)` (pal_io.c:655): `fcntl(F_GETFL)`, toggle
            // `O_NONBLOCK`, `fcntl(F_SETFL)`. Returns 0, or -1-and-errno; any
            // nonzero second argument sets. The modelled targets draw two
            // errnos: EBADF from `F_GETFL` on a dead descriptor, and Darwin's
            // ENOTTY from `F_SETFL` on a kqueue.
            //
            // The flag lands on the open file description
            // (`OpenFileDescription.NonBlocking`), where POSIX keeps the status
            // flags — but only for the targets whose every modelled operation
            // honours it: a socket (`SystemNative_Accept` and
            // `SystemNative_Connect` consult it, and each transfer syscall
            // that lands must too), a regular file (both kernels give `O_NONBLOCK` no effect
            // there, so handlers that never look are right not to), and a
            // socket event port (whose waits block per their own timeout
            // argument, never per this flag). The one target whose modelled
            // transfers would *ignore* a stored flag — a standard stream — is
            // refused below rather than silently diverging.
            //
            // The second parameter is matched loosely for the reason
            // `SystemNative_Socket`'s enums are: CoreLib declares it `int`
            // while a guest hand-rolling the P/Invoke may write `bool`, whose
            // default marshalling is the same four-byte cell.
            let operation = "SystemNative_FcntlSetIsNonBlocking"
            let fd = fdArgument operation instruction.Arguments.[0]

            let isNonBlocking =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Bool b -> b <> 0uy
                | _ -> NativeCall.int32Argument operation instruction.Arguments.[1] <> 0

            let complete (code : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim code)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match UnixSocket.setNonBlocking fd isNonBlocking (EmulatedKernel.unix state.Kernel) with
            | Error refusal -> failwith $"%s{operation}: fd %d{fd}: %s{SetNonBlockingRefusal.describe refusal}"
            | Ok (answer, unix) ->

            // The system comes back on the failing arm too: on one flavour the
            // event port's bit toggles and the call reports a failure anyway.
            let state = state.MapKernel (EmulatedKernel.withUnix unix)

            match answer with
            | SetNonBlockingAnswer.Set -> complete 0 state
            | SetNonBlockingAnswer.Failed error ->
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno error))
                |> complete (-1)
        | Some "SystemNative_FcntlGetIsNonBlocking",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // `int32_t SystemNative_FcntlGetIsNonBlocking(intptr_t fd,
            // int32_t* isNonBlocking)` (pal_io.c:677). A NULL out-pointer is
            // answered with `Error_EFAULT` — the PAL *enum* value, from a
            // function whose other answers are 0 or -1-and-errno; faithful to
            // the C, odd as it is. On failure the C stores 0 through the
            // pointer before returning -1, and the only failure the modelled
            // targets can produce is EBADF.
            //
            // Reads for every target kind, where the setter refuses some:
            // `false` is the truth for a target the setter will not flag.
            let operation = "SystemNative_FcntlGetIsNonBlocking"

            let outArgument =
                bufferPointerArgument operation "isNonBlocking" instruction.Arguments.[1]

            let complete (code : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim code)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // The NULL screen precedes everything, including any look at `fd`:
            // the C tests the pointer before its first `fcntl`, so a null
            // pointer with a nonsensical descriptor is EFAULT, not EBADF.
            match outArgument with
            | BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _ ->

            let fd = fdArgument operation instruction.Arguments.[0]
            let outCell = requireStorage operation "isNonBlocking" outArgument

            let store (value : int) (state : IlMachineState) : IlMachineState =
                let bytes = Array.zeroCreate<byte> 4
                BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> bytes, value)
                writeBytesThrough ctx operation outCell (ImmutableArray.CreateRange bytes) state

            match UnixSocket.isNonBlocking fd (EmulatedKernel.unix state.Kernel) with
            | None ->
                // The C stores 0 through the pointer before returning -1, and the
                // only failure the modelled targets can produce is EBADF.
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno UnixError.EBADF))
                |> store 0
                |> complete (-1)
            | Some isNonBlocking -> state |> store (if isNonBlocking then 1 else 0) |> complete 0
        // `int32_t SystemNative_Socket(int32_t addressFamily, int32_t socketType,
        // int32_t protocolType, intptr_t* createdSocket)` (pal_networking.c:2812).
        //
        // The three enum parameters are matched loosely for the reason
        // `SystemNative_Open`'s flags are: CoreLib declares them `int` while a
        // guest hand-rolling the P/Invoke may write `AddressFamily` and friends,
        // and `int32Argument` peels the enum boxing so both reach one decode.
        //
        // Returns a PAL `Interop.Error` rather than -1-and-errno, and — unlike
        // `SystemNative_Open` or `SystemNative_FLock` — leaves `LastSystemError`
        // strictly alone. Every path PawPrint models here is one of the C's own
        // pre-syscall screens, which set no errno; the only paths that would are
        // the `socket(2)` failures, and those are refused rather than reported.
        | Some "SystemNative_Socket",
          [ _ ; _ ; _ ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_Socket"

            let palAddressFamily = NativeCall.int32Argument operation instruction.Arguments.[0]
            let palSocketType = NativeCall.int32Argument operation instruction.Arguments.[1]
            let palProtocolType = NativeCall.int32Argument operation instruction.Arguments.[2]

            let createdSocketArgument =
                bufferPointerArgument operation "createdSocket" instruction.Arguments.[3]

            match createdSocketArgument with
            | BufferPointer.RawAddress 0UL ->
                // The wrapper's first screen, ahead of every conversion, and it
                // stores nothing — the `*createdSocket = -1` assignments below
                // belong to the three conversion failures, not to this.
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EFAULT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | _ ->

            match BufferPointer.dereferenceable createdSocketArgument with
            | None ->
                // Deliberately *not* EFAULT, for the reason
                // `SystemNative_CreateSocketEventPort` gives: a non-null address
                // naming no storage passes the wrapper's null check, so the real
                // code runs on and faults storing through it — a SIGSEGV that
                // kills the process, not an error code the guest can catch.
                failwith
                    $"%s{operation}: `createdSocket` is %O{createdSocketArgument}, which is not null but names no storage. The C screens only `createdSocket == NULL`, so a real run would store through this address and fault; PawPrint does not model that fault. Pass a real out-parameter."
            | Some createdSocket ->

            // `*createdSocket = <fd>`, as an `intptr_t`: eight bytes, little-endian
            // on both architectures PawPrint models.
            let storeCreatedSocket (value : int64) (state : IlMachineState) : IlMachineState =
                let bytes = Array.zeroCreate<byte> 8
                BinaryPrimitives.WriteInt64LittleEndian (Span<byte> bytes, value)
                writeBytesThrough ctx operation createdSocket (ImmutableArray.CreateRange bytes) state

            let completeWith (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match
                SocketArgumentsPal.socketCreation
                    state.Kernel.UnixPlatform
                    palAddressFamily
                    palSocketType
                    palProtocolType
            with
            | Error refusal ->
                let error =
                    match refusal with
                    | SocketCreationRefusal.AddressFamily -> UnixError.EAFNOSUPPORT
                    | SocketCreationRefusal.SocketType -> UnixError.EPROTOTYPE
                    | SocketCreationRefusal.Protocol -> UnixError.EPROTONOSUPPORT
                    | SocketCreationRefusal.Unmodelled ->
                        // Past every screen the shim applies, so a real run
                        // would call `socket(2)` here, and PawPrint has not
                        // decided what this socket is. Two quite different
                        // decisions are owed depending on why, so ask which:
                        // `shapeOf` failing means the shape has no name in the
                        // library's vocabulary at all, where `shapeOf`
                        // succeeding means it has one and the kernel's table
                        // simply does not list it.
                        match SocketArgumentsPal.shapeOf palAddressFamily palSocketType palProtocolType with
                        | None ->
                            failwith
                                $"%s{operation}: PAL address family %d{palAddressFamily}, type %d{palSocketType} and protocol %d{palProtocolType} pass every screen the native shim applies, but name no socket WoofWare.PosixKernel can represent — SocketDomain, SocketKind and SocketProtocol each cover only the values a modelled socket can take. Deciding what this socket *is* comes before deciding whether the kernel creates it: widen the library's vocabulary first, and note that AF_PACKET and AF_CAN sockets also need send/receive paths nothing offers yet."
                        | Some _ ->
                            // The remaining answers are configuration PawPrint
                            // does not model (`CAP_NET_RAW` for any raw socket,
                            // `net.ipv4.ping_group_range` for Linux's ICMP
                            // datagram sockets) or a deterministic kernel
                            // refusal nobody has measured.
                            failwith
                                $"%s{operation}: PawPrint names the socket with PAL address family %d{palAddressFamily}, type %d{palSocketType} and protocol %d{palProtocolType}, but SimulatedUnixPlatform.creatableSockets does not list it under %O{state.Kernel.UnixPlatform}. Every screen the native shim applies passed, so a real run would reach socket(2); what that answers is privilege-dependent for a raw socket, sysctl-dependent for a Linux ICMP datagram socket, and otherwise a deterministic kernel refusal nobody has modelled. Add a measured row to that table rather than guessing an errno."

                // Each of the three conversion failures stores -1 before
                // returning, so a caller that ignores the return code sees an
                // invalid handle rather than whatever was in the variable.
                state |> storeCreatedSocket -1L |> completeWith (UnixErrorPal.toPal error)
            | Ok (domain, kind, protocol) ->

            let fd, unix =
                UnixSocket.createSocket domain kind protocol (EmulatedKernel.unix state.Kernel)

            state.MapKernel (EmulatedKernel.withUnix unix)
            |> storeCreatedSocket (int64 fd)
            |> completeWith UnixErrorPal.palSuccess
        // `int32_t SystemNative_Bind(intptr_t socket, int32_t protocolType,
        // uint8_t* socketAddress, int32_t socketAddressLen)`
        // (pal_networking.c:1760).
        //
        // The C screens a null blob and a negative length, sets SO_REUSEADDR when
        // `protocolType` is PT_TCP, and calls `bind(2)`. Both screens precede
        // `ToFileDescriptor`, so they beat EBADF.
        | Some "SystemNative_Bind",
          [ ConcreteIntPtr state.ConcreteTypes
            _
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_Bind"
            let fd = fdArgument operation instruction.Arguments.[0]
            let palProtocolType = NativeCall.int32Argument operation instruction.Arguments.[1]
            let declaredLength = NativeCall.int32Argument operation instruction.Arguments.[3]

            let addressArgument =
                bufferPointerArgument operation "socketAddress" instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match addressArgument with
            | BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _ ->

            if declaredLength < 0 then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let platform = state.Kernel.UnixPlatform

            // `toRawErrnoUnder` rather than `toRawErrno`: several of these errnos
            // are numbered differently on the two flavours — EADDRNOTAVAIL is 99
            // on Linux and 49 on Darwin — and the emulated kernel's own platform
            // is what decides which a guest sees.
            let failFromSyscall (error : UnixError) (state : IlMachineState) : NativeHandlerResult option =
                let raw =
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error

                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread raw)
                |> complete (UnixErrorPal.toPal error)

            let refuse (refusal : BindRefusal) : NativeHandlerResult option =
                match refusal with
                | BindRefusal.Copy (SockaddrCopyRefusal.Buffer refusal) ->
                    failwith (BufferPointer.refusalMessage addressArgument refusal)
                | BindRefusal.Copy (SockaddrCopyRefusal.UnmodelledDomain (_, domain)) ->
                    // The library says why no kernel answer exists; PawPrint says
                    // how a guest could be holding such a socket, which is a fact
                    // about CoreLib rather than about any kernel.
                    let reachedBy =
                        match domain with
                        | SocketDomain.InterNetworkV6 ->
                            "No *managed* guest can hold one -- `SocketPal.CreateSocket` sets IPV6_V6ONLY on every non-raw AF_INET6 socket and `SystemNative_SetSockOpt` is unimplemented -- so this is a hand-rolled P/Invoke. Implement SetSockOpt first: the cross-family bind-conflict rules measured so far are facts about IPV6_V6ONLY=0, and Linux inverts several of them at 1."
                        | SocketDomain.Unix -> "That belongs with the filesystem work (issue #956), not here."
                        | SocketDomain.InterNetwork ->
                            failwith
                                $"%s{operation}: the library refused an IPv4 socket's domain, which it models. This is an interpreter bug."

                    failwith $"%s{operation}: fd %d{fd}: %s{BindRefusal.describe refusal} %s{reachedBy}"
                | BindRefusal.UnmodelledMulticast _
                | BindRefusal.EphemeralPortsExhausted _ ->
                    failwith $"%s{operation}: fd %d{fd}: %s{BindRefusal.describe refusal}"

            // `bind(2)`'s buffer, not the wrapper's: the C never dereferences it
            // itself, so an address naming no storage faults in the *kernel* and
            // comes back as EFAULT rather than killing the process, which is what
            // the admission answers. The opposite of
            // `SystemNative_CreateSocketEventPort`'s out-parameter, which the
            // wrapper itself dereferences and `requireStorage` refuses for.
            match
                UnixSocket.admitSockaddrCopy
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> refuse (BindRefusal.Copy refusal)
            | Ok admission ->

            let family, endpoint =
                match admission with
                | SockaddrCopyAdmission.Answered _ ->
                    // The call still goes through `UnixSocket.bind`, which
                    // re-derives this answer — because the `SO_REUSEADDR` write
                    // survives every one of these failures and only `bind`
                    // applies it. No field is read: the kernel never touches the
                    // buffer on this path.
                    None, None
                | SockaddrCopyAdmission.Transfer (length, fields) ->

                let blob =
                    if length = 0 then
                        None
                    else
                        match BufferPointer.dereferenceable addressArgument with
                        | Some blob -> Some blob
                        | None ->
                            failwith
                                $"%s{operation}: the kernel copies %d{length} bytes from `socketAddress`, which names no storage, yet the library admitted the copy rather than answering EFAULT. This is an interpreter bug."

                // The copy takes the caller's whole declared length, so a blob
                // shorter than that is one a real kernel reads past. Whether
                // that faults depends on which pages happen to be mapped beyond
                // the object, which PawPrint does not model: measured, a
                // 128-byte declared length over a 64-byte stack buffer succeeds
                // on Linux, because the stack below it is mapped. Refusing is
                // the honest answer to a question whose real one is not a
                // property of the program.
                match blob with
                | Some blob -> requireBufferRoom ctx operation BufferTransfer.OutOf blob length state
                | None -> ()

                match fields, blob with
                | SockaddrCopyFields.Nothing, _ -> None, None
                | SockaddrCopyFields.Family, Some blob ->
                    Some (readSockaddrFamily ctx operation platform blob state), None
                | SockaddrCopyFields.FamilyAndEndpoint, Some blob ->
                    let portBytes =
                        readBytesThrough
                            ctx
                            operation
                            (bufferFieldAt ctx operation blob InternetSockaddr.port.Offset state)
                            InternetSockaddr.port.Width
                            state

                    let addressBytes =
                        readBytesThrough
                            ctx
                            operation
                            (bufferFieldAt ctx operation blob InternetSockaddr.address.Offset state)
                            InternetSockaddr.address.Width
                            state

                    Some (readSockaddrFamily ctx operation platform blob state),
                    Some (
                        InternetEndpoint.ofParts
                            (BinaryPrimitives.ReadUInt32BigEndian (addressBytes.AsSpan ()))
                            (BinaryPrimitives.ReadUInt16BigEndian (portBytes.AsSpan ()))
                    )
                | (SockaddrCopyFields.Family | SockaddrCopyFields.FamilyAndEndpoint), None ->
                    failwith
                        $"%s{operation}: the library asked for %O{fields} out of a copy of %d{length} bytes, which cannot be zero. This is an interpreter bug."

            match
                UnixSocket.bind
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    // `setsockopt(SO_REUSEADDR)` runs inside the shim before
                    // `bind(2)`, and no failure of the bind undoes it.
                    (SocketArgumentsPal.isTcpProtocolType palProtocolType)
                    family
                    endpoint
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> refuse refusal
            | Ok (answer, unix) ->

            let state = state.MapKernel (EmulatedKernel.withUnix unix)

            match answer with
            | BindAnswer.Failed error -> failFromSyscall error state
            | BindAnswer.Bound _ -> complete UnixErrorPal.palSuccess state
        // `int32_t SystemNative_Listen(intptr_t socket, int32_t backlog)`
        // (pal_networking.c:1892). No screens of its own: it is `listen(2)`,
        // which is why this handler is nothing but the errno write.
        | Some "SystemNative_Listen",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_Listen"
            let fd = fdArgument operation instruction.Arguments.[0]
            let backlog = NativeCall.int32Argument operation instruction.Arguments.[1]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match UnixSocket.listen fd backlog (EmulatedKernel.unix state.Kernel) with
            | Error refusal ->
                // The library says why no kernel answer exists; PawPrint says how
                // a guest could be holding such a socket, which is a fact about
                // CoreLib rather than about any kernel.
                let reachedBy =
                    match refusal with
                    | ListenRefusal.UnmodelledDomain (_, SocketDomain.InterNetworkV6) ->
                        " No *managed* guest can hold one -- `SocketPal.CreateSocket` sets IPV6_V6ONLY on every non-raw AF_INET6 socket and `SystemNative_SetSockOpt` is unimplemented -- so this is a hand-rolled P/Invoke."
                    | ListenRefusal.UnmodelledDomain (_, SocketDomain.Unix) ->
                        " That belongs with the filesystem work (issue #956), not here."
                    | ListenRefusal.UnmodelledDomain (_, SocketDomain.InterNetwork) ->
                        failwith
                            $"%s{operation}: the library refused an IPv4 socket's domain, which it models. This is an interpreter bug."
                    | ListenRefusal.UnmeasuredKind _
                    | ListenRefusal.UnmeasuredPhase _
                    | ListenRefusal.EphemeralPortsExhausted _ -> ""

                failwith $"%s{operation}: fd %d{fd}: %s{ListenRefusal.describe refusal}%s{reachedBy}"
            | Ok (answer, unix) ->

            let state = state.MapKernel (EmulatedKernel.withUnix unix)

            match answer with
            | ListenAnswer.Listening _ -> complete UnixErrorPal.palSuccess state
            | ListenAnswer.Failed error ->
                // `listen(2)`'s own answers, so each leaves the platform errno
                // for a `SetLastError=true` caller: measured 9 for a closed
                // descriptor on both, and 95 on Linux against 102 on Darwin for
                // a datagram socket.
                let raw =
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform) error

                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread raw)
                |> complete (UnixErrorPal.toPal error)
        // `int32_t SystemNative_Accept(intptr_t socket, uint8_t* socketAddress,
        // int32_t* socketAddressLen, intptr_t* acceptedSocket)`
        // (pal_networking.c:1705).
        | Some "SystemNative_Accept",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer _ ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_Accept"

            let addressArgument =
                bufferPointerArgument operation "socketAddress" instruction.Arguments.[1]

            let lengthArgument =
                bufferPointerArgument operation "socketAddressLen" instruction.Arguments.[2]

            let acceptedArgument =
                bufferPointerArgument operation "acceptedSocket" instruction.Arguments.[3]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // The wrapper's own screens, which precede the descriptor lookup —
            // even decoding the fd, whose argument may be a pointer no fd
            // integer could equal — and store nothing: any of the three
            // pointers NULL, or a negative `*socketAddressLen`, is
            // `Error_EFAULT` directly.
            match addressArgument, lengthArgument, acceptedArgument with
            | BufferPointer.RawAddress 0UL, _, _
            | _, BufferPointer.RawAddress 0UL, _
            | _, _, BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _, _, _ ->

            let lengthCell = requireStorage operation "socketAddressLen" lengthArgument

            let declaredLength =
                BinaryPrimitives.ReadInt32LittleEndian ((readBytesThrough ctx operation lengthCell 4 state).AsSpan ())

            // The shim's own screen, before the cast to `socklen_t` that would
            // otherwise make the bound SIZE_MAX. No kernel is ever asked, which
            // is why the library refuses one instead.
            if declaredLength < 0 then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let fd = fdArgument operation instruction.Arguments.[0]

            // Resolved ahead of every remaining answer: each of them stores
            // through `acceptedSocket` — the C writes -1 there on every syscall
            // failure, before returning the PAL error — so a stray pointer is
            // the SIGSEGV `requireStorage` explains, whichever answer it would
            // have accompanied. The address buffer is different: the kernel
            // writes it only on *success*, so it is resolved below and only
            // when there is something to write.
            let acceptedCell = requireStorage operation "acceptedSocket" acceptedArgument

            // `toRawErrnoUnder` rather than `toRawErrno`: EOPNOTSUPP is 95 on
            // Linux against 102 on Darwin, and ENOTSOCK 88 against 38.
            let failFromSyscall (error : UnixError) (state : IlMachineState) : NativeHandlerResult option =
                let raw =
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform) error

                let bytes = Array.zeroCreate<byte> 8
                BinaryPrimitives.WriteInt64LittleEndian (Span<byte> bytes, -1L)

                writeBytesThrough ctx operation acceptedCell (ImmutableArray.CreateRange bytes) state
                |> fun state -> state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread raw)
                |> complete (UnixErrorPal.toPal error)

            match
                UnixConnection.accept
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error (AcceptRefusal.UnmodelledDomain (_, domain) as refusal) ->
                // The library says why no kernel answer exists; PawPrint says how
                // a guest could be holding such a socket, which is a fact about
                // CoreLib rather than about any kernel.
                let reachedBy =
                    match domain with
                    | SocketDomain.InterNetworkV6 ->
                        "No *managed* guest can hold one -- `SocketPal.CreateSocket` sets IPV6_V6ONLY on every non-raw AF_INET6 socket and `SystemNative_SetSockOpt` is unimplemented -- so this is a hand-rolled P/Invoke. Implement SetSockOpt first: the cross-family bind-conflict rules measured so far are facts about IPV6_V6ONLY=0, and Linux inverts several of them at 1."
                    | SocketDomain.Unix -> "That belongs with the filesystem work (issue #956), not here."
                    | SocketDomain.InterNetwork ->
                        failwith
                            $"%s{operation}: the library refused an IPv4 socket's domain, which it models. This is an interpreter bug."

                failwith $"%s{operation}: fd %d{fd}: %s{AcceptRefusal.describe refusal} %s{reachedBy}"
            | Error (AcceptRefusal.Buffer refusal) -> failwith (BufferPointer.refusalMessage addressArgument refusal)
            | Error (AcceptRefusal.UnmeasuredCopyOutFault _ as refusal) ->
                // The library never saw the pointer, only how PawPrint
                // classified it, so naming the argument is PawPrint's half.
                failwith
                    $"%s{operation}: fd %d{fd}: %s{AcceptRefusal.describe refusal} `socketAddress` is %O{addressArgument}; pass a real buffer."
            | Error refusal -> failwith $"%s{operation}: fd %d{fd}: %s{AcceptRefusal.describe refusal}"
            | Ok (AcceptAnswer.Failed error, _) ->
                // No system is carried back: the library documents that a failing
                // accept changes nothing, so writing one would be a no-op that
                // hid a future change to that contract.
                failFromSyscall error state
            | Ok (AcceptAnswer.Accepted (acceptedFd, peer, reportedLength), unix) ->

            // `#if !defined(__linux__)`: "On macOS and FreeBSD new socket
            // inherits flags from accepting fd. Our socket code expects new
            // socket to be in blocking mode by default"
            // (pal_networking.c:1733). Applied on every flavour rather than
            // under a platform test, because on Linux the kernel never set the
            // flag and clearing it is a no-op. The shim closes the accepted
            // socket if the `fcntl` fails; nothing here can fail.
            let unix =
                { unix with
                    Process =
                        { unix.Process with
                            FileDescriptors =
                                FileDescriptorRegistry.setNonBlocking acceptedFd false unix.Process.FileDescriptors
                        }
                }

            let state = state.MapKernel (EmulatedKernel.withUnix unix)

            let blob =
                SimulatedUnixPlatform.encodeInternetSockaddr state.Kernel.UnixPlatform peer

            // The caller's declared length bounds what is *written* and not
            // what is *reported*, exactly as for `getsockname(2)`: both come
            // out of the kernel's one sockaddr copy-out helper.
            let written = min declaredLength reportedLength

            let state =
                if written = 0 then
                    // A call that writes nothing never resolves the destination,
                    // which is why a declared length of zero succeeds through a
                    // pointer naming no storage.
                    state
                else
                    let storage =
                        match BufferPointer.dereferenceable addressArgument with
                        | Some storage -> storage
                        | None ->
                            failwith
                                $"%s{operation}: `socketAddress` is %O{addressArgument}, which names no storage, yet the library accepted a connection rather than refusing the copy-out. This is an interpreter bug."

                    writeBytesThrough
                        ctx
                        operation
                        storage
                        (ImmutableArray.CreateRange (Array.sub blob 0 written))
                        state

            let reported = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> reported, reportedLength)

            let acceptedBytes = Array.zeroCreate<byte> 8
            BinaryPrimitives.WriteInt64LittleEndian (System.Span<byte> acceptedBytes, int64 acceptedFd)

            state
            |> writeBytesThrough ctx operation lengthCell (ImmutableArray.CreateRange reported)
            |> writeBytesThrough ctx operation acceptedCell (ImmutableArray.CreateRange acceptedBytes)
            |> complete UnixErrorPal.palSuccess
        // `int32_t SystemNative_Connect(intptr_t socket, uint8_t* socketAddress,
        // int32_t socketAddressLen)` (pal_networking.c:1785):
        //
        //     if (socketAddress == NULL || socketAddressLen < 0) return Error_EFAULT;
        //     while ((err = connect(fd, ..., (socklen_t)socketAddressLen)) < 0 && errno == EINTR);
        //
        // so both screens precede even the fd decode, and everything else is
        // `connect(2)`'s own ladder — `UnixConnection.connect`, which holds the
        // measured per-flavour table.
        | Some "SystemNative_Connect",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_Connect"

            let addressArgument =
                bufferPointerArgument operation "socketAddress" instruction.Arguments.[1]

            let declaredLength = NativeCall.int32Argument operation instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match addressArgument with
            | BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _ ->

            if declaredLength < 0 then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            let fd = fdArgument operation instruction.Arguments.[0]
            let platform = state.Kernel.UnixPlatform

            // `toRawErrnoUnder` rather than `toRawErrno`: most of connect's
            // errnos are numbered differently on the two flavours (EISCONN is
            // 106 on Linux and 56 on Darwin, EINPROGRESS 115 against 36).
            let failFromSyscall (error : UnixError) (state : IlMachineState) : NativeHandlerResult option =
                let raw =
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error

                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread raw)
                |> complete (UnixErrorPal.toPal error)

            let refuse (refusal : SockaddrCopyRefusal) : NativeHandlerResult option =
                match refusal with
                | SockaddrCopyRefusal.Buffer refusal -> failwith (BufferPointer.refusalMessage addressArgument refusal)
                | SockaddrCopyRefusal.UnmodelledDomain (_, domain) ->
                    // The library says why no kernel answer exists; PawPrint says
                    // how a guest could be holding such a socket, which is a fact
                    // about CoreLib rather than about any kernel.
                    let reachedBy =
                        match domain with
                        | SocketDomain.InterNetworkV6 ->
                            "No *managed* guest can hold one -- `SocketPal.CreateSocket` sets IPV6_V6ONLY on every non-raw AF_INET6 socket and `SystemNative_SetSockOpt` is unimplemented -- so this is a hand-rolled P/Invoke. Implement SetSockOpt first: the cross-family bind-conflict rules measured so far are facts about IPV6_V6ONLY=0, and Linux inverts several of them at 1."
                        | SocketDomain.Unix -> "That belongs with the filesystem work (issue #956), not here."
                        | SocketDomain.InterNetwork ->
                            failwith
                                $"%s{operation}: the library refused an IPv4 socket's domain, which it models. This is an interpreter bug."

                    failwith $"%s{operation}: fd %d{fd}: %s{SockaddrCopyRefusal.describe refusal} %s{reachedBy}"

            let answer (outcome : ConnectOutcome) (state : IlMachineState) : NativeHandlerResult option =
                match outcome with
                // A successful connect leaves errno alone.
                | ConnectOutcome.Completed -> complete UnixErrorPal.palSuccess state
                | ConnectOutcome.Failed error -> failFromSyscall error state

            // Which fields of the caller's sockaddr the kernel's copy will
            // reach, asked before the pointer is resolved: a call whose copy
            // takes no bytes never touches it, so a pointer PawPrint cannot
            // dereference is only a problem when bytes actually move.
            match
                UnixSocket.admitSockaddrCopy
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> refuse refusal
            | Ok (SockaddrCopyAdmission.Answered error) ->
                // Every admission answer precedes the ladder, so none of them
                // changed anything: the system is the one we passed in. They are
                // all failures, which is why the shared admission carries an
                // errno rather than an outcome.
                failFromSyscall error state
            | Ok (SockaddrCopyAdmission.Transfer (length, fields)) ->

            let blob =
                if length = 0 then
                    None
                else
                    match BufferPointer.dereferenceable addressArgument with
                    | Some blob -> Some blob
                    | None ->
                        failwith
                            $"%s{operation}: the kernel copies %d{length} bytes from `socketAddress`, which names no storage, yet the library admitted the copy rather than answering EFAULT. This is an interpreter bug."

            // The copy takes the caller's whole declared length, so a blob
            // shorter than that is one a real kernel reads past, which
            // `requireBufferRoom` refuses as it does for bind.
            match blob with
            | Some blob -> requireBufferRoom ctx operation BufferTransfer.OutOf blob length state
            | None -> ()

            let readFamily (blob : ManagedPointerSource) : int =
                readSockaddrFamily ctx operation platform blob state

            let readEndpoint (blob : ManagedPointerSource) : InternetEndpoint =
                let portBytes =
                    readBytesThrough
                        ctx
                        operation
                        (bufferFieldAt ctx operation blob InternetSockaddr.port.Offset state)
                        2
                        state

                let addressBytes =
                    readBytesThrough
                        ctx
                        operation
                        (bufferFieldAt ctx operation blob InternetSockaddr.address.Offset state)
                        4
                        state

                InternetEndpoint.ofParts
                    (BinaryPrimitives.ReadUInt32BigEndian (addressBytes.AsSpan ()))
                    (BinaryPrimitives.ReadUInt16BigEndian (portBytes.AsSpan ()))

            let family, destination =
                match fields, blob with
                | SockaddrCopyFields.Nothing, _ -> None, None
                | SockaddrCopyFields.Family, Some blob -> Some (readFamily blob), None
                | SockaddrCopyFields.FamilyAndEndpoint, Some blob -> Some (readFamily blob), Some (readEndpoint blob)
                | (SockaddrCopyFields.Family | SockaddrCopyFields.FamilyAndEndpoint), None ->
                    failwith
                        $"%s{operation}: the library asked for %O{fields} out of a copy of %d{length} bytes, which cannot be zero. This is an interpreter bug."

            match
                UnixConnection.connect
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    family
                    destination
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error refusal -> refuse refusal
            | Ok (outcome, unix) ->
                // The system comes back on the failing arms too: several of
                // connect's failures latch a phase change first.
                answer outcome (state.MapKernel (EmulatedKernel.withUnix unix))

        // `int32_t SystemNative_GetSockName(intptr_t socket, uint8_t* socketAddress,
        // int32_t* socketAddressLen)` (pal_networking.c:1871).
        | Some "SystemNative_GetSockName",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            let operation = "SystemNative_GetSockName"
            let fd = fdArgument operation instruction.Arguments.[0]

            let addressArgument =
                bufferPointerArgument operation "socketAddress" instruction.Arguments.[1]

            let lengthArgument =
                bufferPointerArgument operation "socketAddressLen" instruction.Arguments.[2]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match addressArgument, lengthArgument with
            | BufferPointer.RawAddress 0UL, _
            | _, BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _, _ ->

            // The length pointer is dereferenced by the C itself, so it refuses
            // rather than answering EFAULT. This is the opposite of the address
            // blob beside it, which the shim passes through untouched and whose
            // bad address is therefore the kernel's own EFAULT.
            let lengthCell = requireStorage operation "socketAddressLen" lengthArgument

            let declaredLength =
                BinaryPrimitives.ReadInt32LittleEndian ((readBytesThrough ctx operation lengthCell 4 state).AsSpan ())

            // The shim's own screen (`pal_networking.c:1873`), before the cast to
            // `socklen_t` that would otherwise make the bound SIZE_MAX. No kernel
            // is ever asked, which is why the library refuses one instead.
            if declaredLength < 0 then
                complete (UnixErrorPal.toPal UnixError.EFAULT) state
            else

            // `toRawErrnoUnder` rather than `toRawErrno`: ENOTSOCK's raw number
            // is platform-dependent (88 on Linux, 38 on Darwin).
            let failFromSyscall (error : UnixError) (state : IlMachineState) : NativeHandlerResult option =
                state.MapKernel (
                    EmulatedKernel.withLastSystemError
                        ctx.Thread
                        (UnixError.toRawErrnoUnder
                            (SimulatedUnixPlatform.rawErrnoNumbering state.Kernel.UnixPlatform)
                            error)
                )
                |> complete (UnixErrorPal.toPal error)

            match
                UnixSocket.getsockname
                    fd
                    (BufferPointer.toUserBuffer addressArgument)
                    declaredLength
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error (GetSockNameRefusal.Buffer refusal) ->
                failwith (BufferPointer.refusalMessage addressArgument refusal)
            | Error (GetSockNameRefusal.UnmodelledDomain (_, domain) as refusal) ->
                // The library says why no kernel answer exists; PawPrint says how
                // a guest could be holding such a socket, which is a fact about
                // CoreLib rather than about any kernel.
                let reachedBy =
                    match domain with
                    | SocketDomain.InterNetworkV6 ->
                        "No *managed* guest can hold one -- `SocketPal.CreateSocket` sets IPV6_V6ONLY on every non-raw AF_INET6 socket and `SystemNative_SetSockOpt` is unimplemented -- so this is a hand-rolled P/Invoke. Implement SetSockOpt first: the cross-family bind-conflict rules measured so far are facts about IPV6_V6ONLY=0, and Linux inverts several of them at 1."
                    | SocketDomain.Unix -> "That belongs with the filesystem work (issue #956), not here."
                    | SocketDomain.InterNetwork ->
                        failwith
                            $"%s{operation}: the library refused an IPv4 socket's domain, which it models. This is an interpreter bug."

                failwith $"%s{operation}: fd %d{fd}: %s{GetSockNameRefusal.describe refusal} %s{reachedBy}"
            | Ok (GetSockNameAnswer.Failed (error, _lengthOverwritten)) ->
                // `lengthOverwritten` is dropped, and that is what the shim does
                // rather than an omission: it passes `getsockname(2)` a local
                // `socklen_t` and copies it back to the caller only when the call
                // succeeded, so Linux's store of the untruncated length lands on
                // the shim's stack and dies there. A client speaking raw POSIX
                // would have to honour it.
                failFromSyscall error state
            | Ok (GetSockNameAnswer.Reported (endpoint, reportedLength)) ->

            let blob =
                SimulatedUnixPlatform.encodeInternetSockaddr state.Kernel.UnixPlatform endpoint

            // The caller's declared length bounds what is *written*, and does not
            // bound what is *reported* -- see `UnixSocket.getsockname`, which is
            // where that measurement is recorded.
            let written = min declaredLength reportedLength

            let state =
                if written = 0 then
                    // A call that writes nothing never resolves the destination,
                    // which is why a declared length of zero succeeds through a
                    // pointer naming no storage.
                    state
                else
                    let storage =
                        match BufferPointer.dereferenceable addressArgument with
                        | Some storage -> storage
                        | None ->
                            failwith
                                $"%s{operation}: `socketAddress` is %O{addressArgument}, which names no storage, yet the library answered with %d{written} bytes to write rather than EFAULT. This is an interpreter bug."

                    writeBytesThrough
                        ctx
                        operation
                        storage
                        (ImmutableArray.CreateRange (Array.sub blob 0 written))
                        state

            let reported = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> reported, reportedLength)

            state
            |> writeBytesThrough ctx operation lengthCell (ImmutableArray.CreateRange reported)
            |> complete UnixErrorPal.palSuccess
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
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EFAULT)))
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
                        Process =
                            { kernel.Process with
                                FileDescriptors = registry
                            }
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
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
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
            let operation = "SystemNative_CloseSocketEventPort"
            let fd = fdArgument operation instruction.Arguments.[0]

            let error, state =
                match UnixDescriptor.close fd (EmulatedKernel.unix state.Kernel) with
                | Error refusal -> failwith (closeRefusalMessage operation fd refusal)
                | Ok (SyscallAnswer.Completed _, system) -> UnixErrorPal.palSuccess, withAnswered system state
                | Ok (SyscallAnswer.Failed error, system) -> UnixErrorPal.toPal error, withErrno ctx error system state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim error)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_CreateSocketEventBuffer",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ; ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_CreateSocketEventBuffer(int32_t count,
            // SocketEvent** buffer)` (pal_networking.c:3447):
            //
            //     if (buffer == NULL || count < 0) return Error_EFAULT;
            //     size_t bufferSize;
            //     if (!multiply_s(SocketEventBufferElementSize, (size_t)count, &bufferSize) ||
            //         (*buffer = (SocketEvent*)malloc(bufferSize)) == NULL)
            //         return Error_ENOMEM;
            //     return Error_SUCCESS;
            //
            // The two EFAULT conditions answer identically, so no input can
            // distinguish the order they are tested in and there is nothing here to
            // order. The element type of `buffer` is matched with a wildcard for the
            // same reason `SystemNative_WaitForSocketEvents` does it: CoreLib says
            // `SocketEvent**`, a hand-rolled P/Invoke says `byte**` or `void**`, and
            // what is stored through it is a pointer either way.
            let operation = "SystemNative_CreateSocketEventBuffer"

            let requestedCount = NativeCall.int32Argument operation instruction.Arguments.[0]

            let bufferArgument =
                bufferPointerArgument operation "buffer" instruction.Arguments.[1]

            // EFAULT is the wrapper's only answer, and it answers before the
            // allocation, so neither of the two arms below can leak a block.
            let refuseBeforeAllocating () : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EFAULT)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match bufferArgument with
            | BufferPointer.RawAddress 0UL -> refuseBeforeAllocating ()
            | _ ->

            if requestedCount < 0 then
                refuseBeforeAllocating ()
            else

            match BufferPointer.dereferenceable bufferArgument with
            | None ->
                // Deliberately not EFAULT, for the reason `CreateSocketEventPort`
                // records of its own out-parameter: the wrapper screens only
                // `buffer == NULL`, so a non-null address naming no storage reaches
                // the unconditional store and faults there. That is a SIGSEGV, not an
                // error code the guest can catch.
                failwith
                    $"%s{operation}: `buffer` is %O{bufferArgument}, which is not null but names no storage. The C wrapper screens only `buffer == NULL`, so a real run would allocate and then fault storing through this address; PawPrint does not model that fault. Pass a real out-parameter."
            | Some destination ->

            // `*buffer = ptr`, routed the way the guest's own `stind.i` would route it
            // rather than as eight synthesised bytes: a pointer has no byte image here,
            // and the guest reads the slot back with `ldind.i`, which wants a native-int
            // cell carrying the pointer's provenance. `AppContextSeed.allocatePointerArray`
            // records the same value shape for the same reason.
            //
            // Room for the eight bytes the C stores is required first, so a guest that
            // passed the address of a single byte is told so rather than having a pointer
            // cell dropped over a one-byte location.
            let storePointer (pointer : ManagedPointerSource) (state : IlMachineState) : IlMachineState =
                requireBufferRoom ctx operation BufferTransfer.Into destination 8 state

                IlMachineState.writeIndirectPrimitiveStore
                    ctx.BaseClassTypes
                    state
                    destination
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer pointer)))

            let stride =
                SimulatedUnixPlatform.socketEventBufferElementSize state.Kernel.UnixPlatform

            // In `int64`, because the point is to decide whether the product fits the
            // interpreter's `int32` byte-offset model before anything truncates it.
            let extent = int64 requestedCount * int64 stride

            if extent > int64 System.Int32.MaxValue then
                // A native block is addressed by an `int32` byte offset, so this
                // request has no block to hand back. Reported as the allocation
                // failure it is, which is what `SystemNative_Malloc` and
                // `SystemNative_Calloc` already do with a size the interpreter cannot
                // represent, and ENOMEM is one of this entry point's own two answers.
                //
                // Note this is a divergence rather than a limit the real thing shares:
                // `multiply_s` cannot overflow a 64-bit `size_t` for any `int32` count,
                // and a `malloc` of tens of gigabytes succeeds by overcommit (measured:
                // `malloc(16 * INT_MAX)` returns a block on this host). So every count
                // above `Int32.MaxValue / stride` succeeds on a real runtime and fails
                // here.
                //
                // Which of the C's two ENOMEM routes this is decides two things. Whether
                // `*buffer` is written: the `multiply_s` route short-circuits before the
                // store and leaves the caller's value alone, while the
                // `malloc`-returned-NULL route has already stored. And whether `errno`
                // moves: `multiply_s` is arithmetic and touches nothing, while a failed
                // `malloc` sets `errno` to ENOMEM on both platforms modelled here
                // (measured: `malloc(SIZE_MAX)` leaves errno 12 where it was 7 before).
                //
                // This is the `malloc` route — the product is representable, it is the
                // block that is not — so the out-parameter is nulled and `errno` is set.
                // The PAL code is the return value, as for
                // `SystemNative_CloseSocketEventPort`, but a guest that declared this
                // entry point `SetLastError = true`, or that reads
                // `Marshal.GetLastSystemError` after a raw P/Invoke, still sees what libc
                // would have left behind. ENOMEM's raw number is 12 under both
                // numberings, so no flavour decision arises.
                let state = storePointer ManagedPointerSource.Null state

                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno UnixError.ENOMEM))
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.ENOMEM)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            else

            // Uninitialised, as `malloc` hands it over: a guest that reads an element
            // before `WaitForSocketEvents` has filled it is caught by the
            // use-of-uninitialised detector rather than silently reading zeros.
            //
            // `count == 0` allocates a zero-byte block, which is a distinct non-null
            // pointer that cannot be dereferenced. C permits `malloc(0)` to answer
            // NULL instead, but both libcs PawPrint is ever compared against hand back
            // a unique pointer (measured on Darwin, documented for glibc).
            let allocated, state =
                IlMachineState.allocateNativeMemory MemoryBlockInitialization.Uninitialized (int extent) state

            storePointer allocated state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_FreeSocketEventBuffer",
          [ ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_FreeSocketEventBuffer(SocketEvent* buffer)`
            // (pal_networking.c:3464), which is `free(buffer)` and then
            // `return Error_SUCCESS` — no screen of any kind, so `free(NULL)`'s
            // documented no-op is the only reason a null argument is safe.
            //
            // Implemented alongside the create half rather than after it: the sole
            // managed caller's own failure path (`SocketAsyncEngine.FreeNativeResources`,
            // reached from the constructor's catch) releases the buffer, so a guest
            // that got as far as allocating one can already reach this.
            //
            // A pointer naming no live native block is a `failwith` rather than the
            // C's unconditional SUCCESS, which is the same choice `SystemNative_Free`
            // makes: the real `free` would corrupt its heap and report success, and
            // PawPrint reports the corruption instead.
            let operation = "SystemNative_FreeSocketEventBuffer"

            let ptr =
                NativeCall.managedPointerOfPointerArgument operation "buffer" instruction.Arguments.[0]

            let state =
                match NativeCall.tryResolveNativeHeapFreeTarget ptr with
                | Ok None -> state
                | Ok (Some block) -> IlMachineState.freeNativeMemory block state
                | Error reason -> failwith $"%s{operation}: %s{reason}"

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_TryChangeSocketEventRegistration",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcreteIntPtr state.ConcreteTypes ; _ ; _ ; _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_TryChangeSocketEventRegistration(intptr_t
            // port, intptr_t socket, int32_t currentEvents, int32_t newEvents,
            // uintptr_t data)` (pal_networking.c:3471):
            //
            //     const int32_t SupportedEvents = SA_READ | SA_WRITE | SA_READCLOSE | SA_CLOSE | SA_ERROR;
            //     if ((currentEvents & ~SupportedEvents) != 0 || (newEvents & ~SupportedEvents) != 0)
            //         return Error_EINVAL;
            //     if (currentEvents == newEvents) return Error_SUCCESS;
            //     return TryChangeSocketEventRegistrationInner(portFd, socketFd, ..., data);
            //
            // The event arguments are matched with wildcards: CoreLib declares
            // them as its `SocketEvents` enum, a guest hand-rolling the
            // P/Invoke writes `int`, and both arrive as the same four-byte
            // cell — the reason `SystemNative_Socket`'s enums match loosely.
            // `data` likewise: CoreLib says `IntPtr`, a guest may say `void*`.
            let operation = "SystemNative_TryChangeSocketEventRegistration"

            let currentEvents = NativeCall.int32Argument operation instruction.Arguments.[2]
            let newEvents = NativeCall.int32Argument operation instruction.Arguments.[3]

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // The wrapper's two screens. Both answer in user space having run
            // no syscall — errno keeps whatever it held — and both precede any
            // *use* of the descriptor arguments (`ToFileDescriptor` is a cast,
            // not a lookup), so the fds are decoded only past them: a guest
            // may pass a pointer as `port` beside equal masks, and the real
            // wrapper truncates it unread.
            if
                currentEvents &&& ~~~SocketEventsPal.supported <> 0
                || newEvents &&& ~~~SocketEventsPal.supported <> 0
            then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            elif currentEvents = newEvents then
                complete UnixErrorPal.palSuccess state
            else

            // Past the wrapper: the call consults its descriptors now.
            let portFd = fdArgument operation instruction.Arguments.[0]
            let targetFd = fdArgument operation instruction.Arguments.[1]

            // `uintptr_t data`, held verbatim for delivery in
            // `SocketEvent.Data` when an event fires; CoreLib passes
            // `SocketAsyncContext.GlobalContextIndex`, a small integer, and a
            // hand-rolled `void*` import arrives as a runtime pointer whose
            // verbatim bit pattern is just as storable. Decoded *leniently*:
            // `epoll_ctl` treats `data` as opaque and its failures never read
            // it, so an undecodable value — one with provenance PawPrint
            // cannot materialise to eight bytes — must not abort a call that
            // was going to answer EBADF/EPERM/EEXIST/ENOENT anyway. The
            // `Error` here aborts below, only once the registration is known
            // to commit.
            let data : Result<uint64, string> =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[4] with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim value)) -> Ok (uint64 value)
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
                    Ok 0UL
                | CliType.RuntimePointer (CliRuntimePointer.Verbatim value) -> Ok (uint64 value)
                | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> Ok 0UL
                | other ->
                    Error
                        $"%s{operation}: `data` is %O{other}, which is not a verbatim bit pattern. The stored value is delivered back verbatim in SocketEvent.Data when an event fires, and PawPrint cannot materialise a provenance-tracked value to the eight bytes a real kernel would hold. Pass an integer-valued IntPtr."

            // The op is derived from the caller's *claims*, not from the
            // table: ADD iff the claimed current set is NONE, DEL iff the new
            // set is NONE, MOD otherwise. (Both NONE cannot reach here — the
            // equal-mask screen took it.) An undecodable `data` flows in as a
            // zero placeholder: it can never be stored, because the commit
            // path below aborts on `Error` before the new table escapes.
            let change =
                if newEvents = 0 then
                    SocketEventRegistrationChange.Remove
                else
                    let interest = SocketEventsPal.toInterest operation newEvents

                    let placeholder =
                        match data with
                        | Ok value -> value
                        | Error _ -> 0UL

                    if currentEvents = 0 then
                        SocketEventRegistrationChange.Add (interest, placeholder)
                    else
                        SocketEventRegistrationChange.Modify (interest, placeholder)

            match EmulatedKernel.changeSocketEventRegistration portFd targetFd change state.Kernel with
            | Error refusal ->
                // The library says why no kernel answer exists; PawPrint says
                // which entry point asked. `SystemNative_FLock` refuses the same
                // flavour for the same shape of reason.
                failwith $"%s{operation}: %s{SocketEventRegistrationRefusal.describe refusal}"
            | Ok (SocketEventRegistrationAnswer.Failed reason, _) ->
                // The syscall failed, so it set errno on the way past. All five
                // numbers are portable, but only Linux reaches here anyway.
                let unixError = SocketEventRegistrationError.toErrno reason

                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno unixError))
                |> complete (UnixErrorPal.toPal unixError)
            | Ok (SocketEventRegistrationAnswer.Changed, kernel) ->
                match change, data with
                | SocketEventRegistrationChange.Add _, Error message
                | SocketEventRegistrationChange.Modify _, Error message ->
                    // The registration would commit, so the real kernel would
                    // now store the caller's bits; this is the first point at
                    // which the value matters, and the zero placeholder above
                    // must not survive into the table.
                    failwith message
                | SocketEventRegistrationChange.Remove, Error _
                | _, Ok _ ->

                // A successful `epoll_ctl` leaves errno alone. An ADD or MOD
                // of an already-ready target has made the registration
                // pending inside the kernel change, and if a waiter is
                // parked on the port, `Program`'s readiness sweep wakes it
                // before the next scheduling decision.
                state.MapKernel (fun _ -> kernel) |> complete UnixErrorPal.palSuccess
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
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal UnixError.EFAULT)))
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
                    state.MapKernel (
                        EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrnoUnder numbering error)
                    )
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim (UnixErrorPal.toPal error)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Park re-entrantly: leave the native frame on the stack and the
            // caller's program counter naming the call, so that a wake —
            // `Program`'s readiness sweep flipping this thread back to
            // Runnable once the port has something deliverable — re-enters
            // this handler and writes the event batch through the caller's
            // own `buffer`, rather than the wake having to reach into a
            // frame it does not own from some other thread's step.
            let park
                (port : OpenFileDescriptionId)
                (requestedCount : int)
                (state : IlMachineState)
                : NativeHandlerResult option
                =
                // The capture that survives the park: what the syscall was
                // entered with, consulted by the re-entry in place of the
                // arguments the guest may have scribbled on since. It is also
                // the only place the port is written down — the park status
                // carries nothing — so `Program`'s readiness sweep reads this
                // to decide whether to wake, and cannot ask a different
                // question from the one the delivery below answers.
                state.MapKernel (
                    EmulatedKernel.mapTasks (
                        UnixTaskTable.withParked
                            ctx.Thread
                            (Some (
                                ParkedSyscall.SocketWait
                                    {
                                        ParkedSocketWait.Port = port
                                        MaxEvents = requestedCount
                                    }
                            ))
                    )
                )
                |> Scheduler.parkInSyscall ctx.Thread
                |> NativeHandlerResult.blockedRetainingFrame
                |> Some

            // What one `epoll_wait` returning does: drain up to
            // `requestedCount` events from the port's ready list and convert
            // each to the PAL's `SocketEvent` shape, which is
            // `SocketEventsPal.delivered`.
            let deliver
                (delivered : (uint64 * ReadinessLevel) list)
                (kernel : EmulatedKernel)
                : NativeHandlerResult option
                =
                let bufferPointer =
                    match BufferPointer.dereferenceable buffer with
                    | Some pointer -> pointer
                    | None ->
                        failwith
                            $"%s{operation}: the event buffer is %O{buffer}, which names no storage. A real epoll_wait passes access_ok at wait time and fails only when the copy-out faults (EFAULT with the consumed events lost), behaviour PawPrint does not model. Pass a real buffer."

                let elementSize =
                    SimulatedUnixPlatform.socketEventBufferElementSize state.Kernel.UnixPlatform

                let bytes = Array.zeroCreate<byte> (List.length delivered * elementSize)

                delivered
                |> List.iteri (fun i (data, reported) ->
                    let palEvents = SocketEventsPal.delivered reported

                    BinaryPrimitives.WriteUInt64LittleEndian (Span<byte> (bytes, i * elementSize, 8), data)

                    BinaryPrimitives.WriteInt32LittleEndian (Span<byte> (bytes, i * elementSize + 8, 4), palEvents)
                // The trailing four bytes of each element are the struct's
                // explicit padding, already zero.
                )

                let countBytes = Array.zeroCreate<byte> 4
                BinaryPrimitives.WriteInt32LittleEndian (Span<byte> countBytes, List.length delivered)

                // A successful wait leaves errno alone. The wait is over, so
                // the captured in-flight state (if this was a re-entry) goes
                // with it.
                state.MapKernel (fun _ -> EmulatedKernel.mapTasks (UnixTaskTable.withParked ctx.Thread None) kernel)
                |> writeBytesThrough ctx operation bufferPointer (ImmutableArray.CreateRange bytes)
                |> writeBytesThrough ctx operation countCell (ImmutableArray.CreateRange countBytes)
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // Walk the port as `epoll_wait` would: report the pending
            // entries whose re-poll is nonempty, silently consuming the
            // stale ones — so even a walk that delivers nothing may change
            // the kernel, and that write-back happens before any park.
            let deliverOrPark (port : OpenFileDescriptionId) (requestedCount : int) : NativeHandlerResult option =
                let delivered, system =
                    SocketEventPort.drain port requestedCount (EmulatedKernel.unix state.Kernel)

                let kernel = EmulatedKernel.withUnix system state.Kernel

                match delivered with
                | [] -> park port requestedCount (state.MapKernel (fun _ -> kernel))
                | delivered -> deliver delivered kernel

            // A woken thread re-enters this handler from the top, but the
            // syscall was already *entered*: the port identity and maxevents
            // it captured outlive anything the guest has done to the
            // arguments since — the count cell can be overwritten, and the
            // fd the wait was called through can be closed (a dup keeps the
            // description alive; `UnixDescriptor.close`'s retention refusal keeps the
            // last descriptor from destroying it). So a re-entry consults no
            // screen and no descriptor table: it delivers from the captured
            // description, or parks again.
            match UnixTaskTable.parkedFor ctx.Thread state.Kernel.Tasks with
            | Some (ParkedSyscall.SocketWait inFlight) -> deliverOrPark inFlight.Port inFlight.MaxEvents
            | Some (ParkedSyscall.Flock _) ->
                // Unreachable, and refused rather than treated as a first entry
                // for the reason `SystemNative_FLock`'s mirror of this gives: a
                // task parked in an `flock` is not running IL, so this can only
                // mean the acquisition's completion failed to clear its record,
                // and parking over it would destroy the evidence.
                failwith
                    $"%s{operation}: thread %O{ctx.Thread} entered a socket wait while its task is parked in an flock. A task blocks in one syscall at a time, so the acquisition's completion failed to clear its record (this is an interpreter bug)."
            | None ->

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

            match
                UnixPoll.admitSocketWait
                    fd
                    requestedCount
                    (BufferPointer.toUserBuffer buffer)
                    (EmulatedKernel.unix state.Kernel)
            with
            | Error (SocketWaitRefusal.Buffer refusal) -> failwith (BufferPointer.refusalMessage buffer refusal)
            | Ok (SocketWaitAdmission.Failed error) -> failFromSyscall error
            | Ok SocketWaitAdmission.NoEvents ->
                // Darwin's zero-event row. The wrapper's "we should never see 0
                // events" assertion is compiled out of the shipped release build,
                // so it falls through, writes `*count = 0` and reports success.
                //
                // `errno` is untouched, the syscall having not failed.
                let bytes = Array.zeroCreate<byte> 4
                BinaryPrimitives.WriteInt32LittleEndian (Span<byte> bytes, 0)

                writeBytesThrough ctx operation countCell (ImmutableArray.CreateRange bytes) state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.Int32 (Int32Source.Verbatim UnixErrorPal.palSuccess))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
            | Ok (SocketWaitAdmission.DeliverOrWait (port, maxEvents)) -> deliverOrPark port maxEvents
        | Some "SystemNative_Poll",
          [ ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer _ ],
          MethodReturnType.Returns (PalErrorReturn state.ConcreteTypes) ->
            // `int32_t SystemNative_Poll(PollEvent* pollEvents, uint32_t
            // eventCount, int32_t milliseconds, uint32_t* triggered)`
            // (pal_io.c:1109), whose whole body is `Common_Poll`
            // (pal_io_common.h:143):
            //
            //     if (pollEvents == NULL || triggered == NULL) return Error_EFAULT;
            //     if (milliseconds < -1)                       return Error_EINVAL;
            //     ... convert each entry's Events, poll(2), convert each revents
            //     *triggered = (uint32_t)rv;
            //     return Error_SUCCESS;
            //
            // Both the buffer and the count pointer are matched with a wildcard
            // pointee: CoreLib declares them `PollEvent*` and `uint*`, and a
            // guest hand-rolling the P/Invoke will say `byte*` or `void*`.
            // Element bytes are addressed individually, so the pointee type is
            // never consulted.
            let operation = "SystemNative_Poll"

            let complete (palError : int) (state : IlMachineState) : NativeHandlerResult option =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim palError)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            // The wrapper's two screens, in its order. Both are answered in user
            // space having run no syscall, so `errno` keeps whatever it held and
            // `*triggered` is left exactly as the caller set it — the C returns
            // before assigning it.
            //
            // The null check deliberately precedes any use of `eventCount`, which
            // is the one row where this entry point and libc `poll(2)` disagree:
            // `poll(NULL, 0, 0)` succeeds with `rv = 0` on both kernels, while
            // `SystemNative_Poll(NULL, 0, …)` answers EFAULT.
            let pollEvents =
                bufferPointerArgument operation "pollEvents" instruction.Arguments.[0]

            let triggeredPointer =
                bufferPointerArgument operation "triggered" instruction.Arguments.[3]

            match pollEvents, triggeredPointer with
            | BufferPointer.RawAddress 0UL, _
            | _, BufferPointer.RawAddress 0UL -> complete (UnixErrorPal.toPal UnixError.EFAULT) state
            | _ ->

            let milliseconds = NativeCall.int32Argument operation instruction.Arguments.[2]

            if milliseconds < -1 then
                complete (UnixErrorPal.toPal UnixError.EINVAL) state
            else

            let eventCount = NativeCall.uint32Argument operation instruction.Arguments.[1]

            // Resolved only when at least one entry is actually read. With
            // `eventCount = 0` the C dereferences `pollEvents` nowhere — the
            // copy-in loop is its only reader, and it does not run — so any
            // non-null bit pattern is legal there: the call succeeds and stores
            // zero through `triggered`. `SocketPal.Select` reaches exactly that
            // shape when every list it was given is empty.
            let entriesStorage : ManagedPointerSource option =
                if eventCount = 0u then
                    None
                else
                    Some (requireStorage operation "pollEvents" pollEvents)

            // `struct PollEvent` is `{ int32 FileDescriptor; int16 Events; int16
            // TriggeredEvents; }` (Interop.Poll.Structs.cs) — eight bytes, no
            // padding on either architecture PawPrint models. The layout is the
            // PAL's own transcription rather than a kernel fact, which is why it
            // stays here: `struct pollfd` is `{ int; short; short }` too, but a
            // kernel never sees this array.
            let entryStride = 8
            let eventsOffset = 4
            let triggeredEventsOffset = 6

            let totalBytes = int64 eventCount * int64 entryStride

            if totalBytes > int64 Int32.MaxValue then
                // An interpreter limit rather than a kernel one, and named as
                // such.
                //
                // A real `poll(2)` bounds `nfds` by RLIMIT_NOFILE (measured,
                // pollnfds.c: EINVAL above it), and PawPrint does not reproduce
                // that bound. This is a modelling choice rather than a gap:
                // PawPrint behaves as `RLIMIT_NOFILE = RLIM_INFINITY`, which is
                // a lawful setting, and it is the *only* self-consistent one
                // here, because the descriptor table answers `EMFILE`/`ENFILE`
                // nowhere either. Enforcing the bound in `poll` alone would let
                // a guest open five thousand descriptors and then be told that
                // polling two thousand is EINVAL — a worse model than no limit.
                // Nothing can observe the difference: `getrlimit` is not in the
                // interop surface.
                //
                // If a descriptor limit ever enters `KernelConfig`, this becomes
                // EINVAL above the soft limit, and `open`/`socket`/`dup` gain
                // their `EMFILE` at the same time.
                failwith
                    $"%s{operation}: eventCount %d{eventCount} spans %d{totalBytes} bytes, which overflows the int32 byte offsets PawPrint's address space uses. PawPrint models no descriptor limit (RLIMIT_NOFILE is not in the interop surface), so this is a limit of the interpreter rather than a kernel refusal to reproduce."
            else

            // Decode every entry before answering, exactly as the C fills its
            // whole `struct pollfd` array before calling `poll(2)`.
            let entries : PollEntry list =
                match entriesStorage with
                | None -> []
                | Some entriesStorage ->

                requireBufferRoom ctx operation BufferTransfer.OutOf entriesStorage (int totalBytes) state

                List.init
                    (int eventCount)
                    (fun i ->
                        let fdBytes =
                            readBytesThrough
                                ctx
                                operation
                                (bufferFieldAt ctx operation entriesStorage (i * entryStride) state)
                                4
                                state

                        let eventsBytes =
                            readBytesThrough
                                ctx
                                operation
                                (bufferFieldAt ctx operation entriesStorage (i * entryStride + eventsOffset) state)
                                2
                                state

                        {
                            Fd = BinaryPrimitives.ReadInt32LittleEndian (fdBytes.AsSpan ())
                            Requested =
                                PollEvents.ofBits (BinaryPrimitives.ReadInt16LittleEndian (eventsBytes.AsSpan ()))
                        }
                    )

            match UnixPoll.poll entries milliseconds (EmulatedKernel.unix state.Kernel) with
            | Error refusal ->
                // The library says why no kernel answer exists; PawPrint says
                // which guest call asked, and what a guest could do instead.
                let reachedBy =
                    match refusal with
                    | PollRefusal.UnmodelledFlavour _ ->
                        // Deliberately coarser than it has to be: it precedes the
                        // entries, so it also refuses a zero-entry poll, whose
                        // answer is measured identical on both flavours. That row
                        // would be a branch with no consumer, since no
                        // Darwin-flavoured guest reaches this entry point today.
                        " The measured Darwin rows are in docs/plans/2026-08-23-socket-poll."
                    | PollRefusal.UnmeasuredTarget _ ->
                        " No managed caller reaches it: CoreLib polls only sockets (System.Net.Sockets), a standard stream (ConsolePal.Write) and an inotify descriptor (FileSystemWatcher, a kind PawPrint does not model), so this is a hand-rolled P/Invoke."
                    | PollRefusal.WouldPark _ ->
                        " There is no thread status carrying this call's captured entry set and its deadline, and no wake for it beside the readiness sweep that serves SystemNative_WaitForSocketEvents."

                failwith $"%s{operation}: %s{PollRefusal.describe refusal}%s{reachedBy}"
            | Ok (reported, triggeredCount) ->

            // Write back only `TriggeredEvents`. The C leaves `FileDescriptor`
            // and `Events` alone (it asserts they are unchanged), so PawPrint
            // must not touch those bytes either.
            let state =
                match entriesStorage with
                | None -> state
                | Some entriesStorage ->

                reported
                |> List.indexed
                |> List.fold
                    (fun state (i, reported) ->
                        let bytes = Array.zeroCreate<byte> 2

                        BinaryPrimitives.WriteInt16LittleEndian (Span<byte> bytes, PollEvents.toBits reported)

                        writeBytesThrough
                            ctx
                            operation
                            (bufferFieldAt ctx operation entriesStorage (i * entryStride + triggeredEventsOffset) state)
                            (ImmutableArray.CreateRange bytes)
                            state
                    )
                    state

            let triggeredBytes = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> triggeredBytes, uint32 triggeredCount)

            writeBytesThrough
                ctx
                operation
                (requireStorage operation "triggered" triggeredPointer)
                (ImmutableArray.CreateRange triggeredBytes)
                state
            |> complete UnixErrorPal.palSuccess
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
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno error))

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
            // The emulated kernel never returns short, never returns EINTR and
            // never blocks. A guest depending on EAGAIN or a partial write from
            // a non-blocking socket would need connection state PawPrint does
            // not model, which `UnixReadWrite.write` refuses rather than guesses.
            let operation = "SystemNative_Write"
            let fd = fdArgument operation instruction.Arguments.[0]
            let bufferSize = NativeCall.int32Argument operation instruction.Arguments.[2]

            let refused (refusal : WriteRefusal) : 'a =
                // The library says why no answer exists; PawPrint says which
                // managed caller could have reached it.
                let reachability =
                    match refusal with
                    | WriteRefusal.SocketConnectionState _ ->
                        "Nothing in the BCL waits on this: CoreLib reaches a socket through `SystemNative_Send`, `SafeSocketHandle` not being a `SafeFileHandle`, so this is a hand-rolled P/Invoke. Model the connection state (issue #956) before answering it."
                    | WriteRefusal.ExceedsRepresentableLength _ ->
                        "Write less, or raise the model's file-length limit (issue #956)."
                    | WriteRefusal.Buffer _ -> "Pass a buffer that names guest storage."

                failwith $"%s{operation}: fd %d{fd}: %s{WriteRefusal.describe refusal} %s{reachability}"

            let answered
                (answer : WriteAnswer)
                (system : UnixSystem<ThreadId, SignalHandler>)
                (state : IlMachineState)
                =
                match answer with
                | WriteAnswer.Failed error -> -1, withErrno ctx error system state
                | WriteAnswer.Completed written -> written, withAnswered system state

            let result, effect, state =
                if bufferSize < 0 then
                    // `Common_Write`'s own guard, which refuses before any
                    // dereference of `buffer`. ERANGE, where `Common_Read`
                    // answers EINVAL for the same mistake: the asymmetry is
                    // upstream's rather than a typo here (pal_io_common.h:41-45
                    // against :59-63). CoreLib's own callers never pass a
                    // negative size, so this is a guest-misuse path; surfaced
                    // through errno rather than a crash so the guest's own error
                    // reporting runs.
                    let _, state =
                        answered (WriteAnswer.Failed UnixError.ERANGE) (EmulatedKernel.unix state.Kernel) state

                    -1, StepEffect.NoEffect, state
                else

                // Decoding the buffer pointer is deferred until the kernel says
                // it would be read: `Common_Write` performs no dereference for a
                // zero size, so `SystemNative_Write((IntPtr)1, (byte*)123, 0)`
                // must succeed here as it does on the real CLR. Classification
                // itself is total, so it is the *extraction* below that waits.
                let buffer = bufferPointerArgument operation "buffer" instruction.Arguments.[1]

                match
                    UnixReadWrite.admitWrite
                        fd
                        (BufferPointer.toUserBuffer buffer)
                        bufferSize
                        (EmulatedKernel.unix state.Kernel)
                with
                | Error (WriteRefusal.Buffer refusal) -> failwith (BufferPointer.refusalMessage buffer refusal)
                | Error refusal -> refused refusal
                | Ok (WriteAdmission.Answered answer) ->
                    let result, state = answered answer (EmulatedKernel.unix state.Kernel) state
                    result, StepEffect.NoEffect, state
                | Ok (WriteAdmission.Transfer count) ->

                let source =
                    match BufferPointer.dereferenceable buffer with
                    | Some source -> source
                    | None ->
                        failwith
                            $"%s{operation}: fd %d{fd}: the kernel asked for %d{count} bytes from a buffer that names no storage. Every such buffer is answered or refused before the transfer (this is an interpreter bug)."

                let bytes = readBytesThrough ctx operation source count state

                match UnixReadWrite.write fd bytes (EmulatedKernel.unix state.Kernel) with
                | Error refusal -> refused refusal
                | Ok (answer, system) ->

                let result, state = answered answer system state

                // The host's own view of what the guest printed, which is
                // PawPrint's business rather than the kernel's: the kernel
                // records the bytes in its output log, and this is what makes
                // them appear on a console.
                let effect =
                    match FileDescriptorRegistry.tryFind fd state.Kernel.FileDescriptors with
                    | Some description ->
                        match description.Target with
                        | OpenFileTarget.StandardStream role -> StepEffect.WroteToFd (role, bytes)
                        | OpenFileTarget.File _
                        | OpenFileTarget.Socket _
                        | OpenFileTarget.SocketEventPort _ -> StepEffect.NoEffect
                    | None -> StepEffect.NoEffect

                result, effect, state

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
                    Machine =
                        { kernel.Machine with
                            NonCryptoRandomState = newPrngState
                        }
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
                        Machine =
                            { kernel.Machine with
                                CryptoRandomState = newPrngState
                            }
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
                            Process =
                                { kernel.Process with
                                    Signals = SignalState.markInitialized dispatcher kernel.Signals
                                }
                        }
                    )

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | Some "SystemNative_GetPlatformSignalNumber",
          [ PosixSignalParam state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Real native code keys off the <signal.h> it was compiled
            // against; PawPrint keys off the configured platform's, so the
            // answer is a fact about `KernelConfig.UnixPlatform` rather than
            // about the host running the simulation, and a Darwin guest is
            // handed 20 for `PosixSignal.SIGCHLD` where a Linux one is handed
            // 17. `PosixSignalPal.platformSignalNumber` is the C side's rule
            // exactly: the enum's members map to their signo, a positive
            // number within `GetSignalMax()` is echoed back uninterpreted,
            // and everything else answers 0, which
            // `PosixSignalRegistration.Register` reports as
            // `PlatformNotSupportedException`.
            let raw =
                NativeCall.int32Argument "SystemNative_GetPlatformSignalNumber" instruction.Arguments.[0]

            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

            pushInt32 (PosixSignalPal.platformSignalNumber numbering raw) ctx |> Some
        | Some "SystemNative_EnablePosixSignalHandling",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // Flips the per-signo "managed code wants this" bit. The handler
            // dictionary itself lives on the simulated managed heap (maintained
            // by `PosixSignalRegistration`'s `s_registrations`); this arm only
            // touches the kernel-side enable set.
            let operation = "SystemNative_EnablePosixSignalHandling"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]
            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

            // Real native code calls `sigaction(signo, ...)`, which fails with
            // `EINVAL` for a number the kernel has no signal for (Darwin's 32),
            // for SIGKILL and SIGSTOP, and — in glibc's wrapper rather than
            // the kernel — for the 32 and 33 glibc reserves for itself.
            // `InstallSignalHandler` then returns false and the shim
            // propagates 0 with `errno = EINVAL`, which
            // `PosixSignalRegistration.Register` reads via
            // `Marshal.GetLastSystemError` to throw an `IOException`. We
            // mirror exactly that: leave the enable bit clear, set errno, push
            // 0. Not a loud failure — this is a documented BCL-observable
            // failure mode, not a simulator bug.
            let refused () : NativeHandlerResult option =
                state.MapKernel (EmulatedKernel.withLastSystemError ctx.Thread (UnixError.toRawErrno UnixError.EINVAL))
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread
                |> NativeHandlerResult.completed
                |> Some

            match signalWithinShimRange operation numbering signo with
            | ValueNone -> refused ()
            | ValueSome signal when Signal.isUncatchableUnder numbering signal -> refused ()
            | ValueSome signal ->
                state.MapKernel (fun kernel ->
                    { kernel with
                        Process =
                            { kernel.Process with
                                Signals = SignalState.enable signal kernel.Signals
                            }
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
                    Process =
                        { kernel.Process with
                            Signals = SignalState.setHandler (SignalHandler.ofMethodInfo mi) kernel.Signals
                        }
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
            // native code has an explicit no-op arm for seven signals
            // (SIGCONT, SIGTSTP, SIGTTIN, SIGTTOU, SIGCHLD, SIGURG,
            // SIGWINCH; see `PosixSignalPal.handledWithoutRestoring`) and
            // for everything else restores the original `sigaction` and
            // re-raises the signal with `kill(2)`, so the process gets the
            // kernel's default. PawPrint matches the no-op arms exactly
            // (there is nothing to do) and, for the default arm, runs the
            // kernel default: `SignalTerminated` where that is to
            // terminate, and — where it is to discard the signal, which
            // is Darwin's SIGIO and SIGINFO — clears the enable bit,
            // because the shim's handler is gone and no later occurrence
            // reaches managed code.
            //
            // Which signal a signo names, and so which arm it takes, is
            // read under the configured platform's numbering: 29 is SIGIO
            // on Linux and terminates, and SIGINFO on Darwin and is
            // discarded.
            let operation = "SystemNative_HandleNonCanceledPosixSignal"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]
            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

            match signalWithinShimRange operation numbering signo with
            | ValueNone ->
                // Darwin's 32. The shim's `default:` branch restores a
                // handler that was never installed and calls
                // `kill(g_pid, 32)`, which the kernel refuses with EINVAL
                // and the shim does not check; the process carries on.
                NativeHandlerResult.completed state |> Some
            | ValueSome signal when PosixSignalPal.handledWithoutRestoring numbering signal ->
                // Nothing to do: the runtime cannot stop or continue
                // itself, and the ignored ones are literally no-ops (the
                // terminal re-initialisation on SIGCONT is not relevant to
                // PawPrint, which has no terminal). The shim's handler
                // stays installed, so the enable bit stays set.
                NativeHandlerResult.completed state |> Some
            | ValueSome signal ->
                match Signal.defaultDispositionUnder numbering signal with
                | DefaultDisposition.Ignore ->
                    // The `default:` arm for a signal the kernel discards:
                    // `RestoreSignalHandler` puts back `SIG_DFL`, then
                    // `kill(g_pid, signo)` delivers a signal the kernel
                    // drops. The process carries on, but with no native
                    // handler for this signo any more, so nothing the BCL
                    // still records for it can be reached; `g_hasPosix-
                    // SignalRegistrations` stays set there, but it is only
                    // read on a delivery that can no longer happen, and a
                    // later `EnablePosixSignalHandling` — which the BCL
                    // sends only once every token is unregistered —
                    // reinstalls the handler, exactly as it re-enables
                    // here.
                    state.MapKernel (fun kernel ->
                        { kernel with
                            Process =
                                { kernel.Process with
                                    Signals = SignalState.disable signal kernel.Signals
                                }
                        }
                    )
                    |> NativeHandlerResult.completed
                    |> Some
                | DefaultDisposition.Stop
                | DefaultDisposition.Continue ->
                    // The `default:` arm would restore `SIG_DFL` and
                    // re-raise, and the kernel would then stop or continue
                    // the whole process, which PawPrint does not model. No
                    // signal the BCL can register gets here — the ones
                    // with these defaults either have an explicit arm or
                    // are SIGSTOP, which `EnablePosixSignalHandling`
                    // refuses — so this is a guest hand-rolling the
                    // P/Invoke, and it is refused rather than answered
                    // with an invented continuation.
                    failwith
                        $"%s{operation}: signo %d{signo} (%O{signal} under the %O{numbering} numbering) reaches the shim's default arm, which would restore the kernel's disposition and re-raise it; the kernel would then stop or continue the process, which PawPrint does not model. Only a guest bypassing PosixSignalRegistration can reach this."
                | DefaultDisposition.Terminate ->
                    // Mirrors `pal_signal.c`'s Terminate branch, which
                    // restores the original `sigaction` and calls
                    // `kill(g_pid, signalCode)` to let the kernel
                    // terminate the process with the signal-default
                    // exit status. PawPrint surfaces this as a
                    // dedicated `SignalTerminated` outcome so the App
                    // layer can compute the POSIX-conventional exit
                    // code (`128 + signo`, under the platform's
                    // numbering) and distinguish signal-driven
                    // termination from a managed `Environment.Exit`
                    // call carrying the same exit code.
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
            // effect is the cleared bit.
            let operation = "SystemNative_DisablePosixSignalHandling"
            let signo = NativeCall.int32Argument operation instruction.Arguments.[0]
            let numbering = SimulatedUnixPlatform.signalNumbering state.Kernel.UnixPlatform

            match signalWithinShimRange operation numbering signo with
            | ValueNone ->
                // Darwin's 32: nothing can have enabled it, and the
                // `sigaction` the real shim calls to restore its prior
                // disposition fails unchecked. Nothing to clear.
                NativeHandlerResult.completed state |> Some
            | ValueSome signal ->
                state.MapKernel (fun kernel ->
                    { kernel with
                        Process =
                            { kernel.Process with
                                Signals = SignalState.disable signal kernel.Signals
                            }
                    }
                )
                |> NativeHandlerResult.completed
                |> Some
        | _ -> None
