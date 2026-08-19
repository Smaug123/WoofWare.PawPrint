namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.InteropServices
open Checked

type SizeofResult =
    {
        Alignment : int
        Size : int
    }

/// Why we couldn't compute the unmanaged marshalled size of a type.
/// Distinguishes "CoreCLR would reject this too" from "PawPrint hasn't implemented this yet",
/// so callers at runtime boundaries can react appropriately: `NotMarshalable` is a real
/// guest-visible error (`ArgumentException` for `Marshal.SizeOf`); `NotImplemented` is a
/// missing PawPrint feature and should typically surface as a host `failwith` until covered.
type MarshalSizeError =
    /// CoreCLR rejects this shape from being marshalled as an unmanaged structure.
    /// Examples: `LayoutKind.Auto`, `[MarshalAs(ByValTStr)]` on a non-`System.String` field,
    /// width-mismatched scalar `[MarshalAs]`, mixed explicit/automatic field offsets.
    | NotMarshalable of reason : string
    /// CoreCLR would compute a size here, but PawPrint hasn't implemented the case yet.
    /// Examples: `CharSet.Auto` (deliberately not chosen platform-dependent), bare `System.Boolean`
    /// (CoreCLR marshals as a 4-byte BOOL), `UnmanagedType` variants we don't decode yet.
    | NotImplemented of reason : string

    member this.Reason : string =
        match this with
        | MarshalSizeError.NotMarshalable reason
        | MarshalSizeError.NotImplemented reason -> reason

[<RequireQualifiedAccess>]
module MarshalSizeError =
    /// Prepend a `field X: ` label to the inner reason, preserving the case.
    /// Used to attribute a per-field error to its containing struct field.
    let prefixField (fieldName : string) (err : MarshalSizeError) : MarshalSizeError =
        match err with
        | MarshalSizeError.NotMarshalable reason -> MarshalSizeError.NotMarshalable $"field %s{fieldName}: %s{reason}"
        | MarshalSizeError.NotImplemented reason -> MarshalSizeError.NotImplemented $"field %s{fieldName}: %s{reason}"

    /// Prepend an arbitrary label to the inner reason, preserving the case.
    let prefix (label : string) (err : MarshalSizeError) : MarshalSizeError =
        match err with
        | MarshalSizeError.NotMarshalable reason -> MarshalSizeError.NotMarshalable $"%s{label}%s{reason}"
        | MarshalSizeError.NotImplemented reason -> MarshalSizeError.NotImplemented $"%s{label}%s{reason}"

type CliByteAddressabilityRejection =
    | ObjectReference
    | RuntimePointer
    | NativeIntSourceNotByteAddressable of NativeIntSource
    /// A byte that names a position in a native int rather than holding a number.
    | UInt8SourceNotByteAddressable of UInt8Source
    | Int64SourceNotByteAddressable of Int64Source
    /// The handle is the value type supplied to the classifier, not necessarily
    /// the innermost offending field's declaring type.
    | ValueTypeContainsObjectReferences of ConcreteTypeHandle
    /// The handle is the value type supplied to the classifier, not necessarily
    /// the innermost offending field's declaring type.
    | ValueTypeContainsRuntimePointers of ConcreteTypeHandle
    | ValueTypeContainsNonByteAddressableField of ConcreteTypeHandle * FieldId * CliByteAddressabilityRejection

    member this.Description : string =
        match this with
        | CliByteAddressabilityRejection.ObjectReference -> "object reference"
        | CliByteAddressabilityRejection.RuntimePointer -> "runtime pointer"
        | CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable source ->
            $"native int with non-byte-addressable provenance %O{source}"
        | CliByteAddressabilityRejection.UInt8SourceNotByteAddressable source -> $"byte naming a native int %O{source}"
        | CliByteAddressabilityRejection.Int64SourceNotByteAddressable source ->
            $"int64 with non-byte-addressable provenance %O{source}"
        | CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _ ->
            "value type containing object references"
        | CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers _ -> "value type containing runtime pointers"
        | CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField (_, field, rejection) ->
            $"value type containing non-byte-addressable field %O{field}: %s{rejection.Description}"

type CliByteAddressability =
    | ByteAddressable
    /// Every byte of this value can be *named*, but at least one of them is a byte of a native
    /// int PawPrint models as an identity rather than as an address, so it is not a number. The
    /// payload says which — it is the same obstruction `Rejected` would have reported.
    ///
    /// The distinction is what a caller can do about it. A byte image is still exact here: it is
    /// a `UInt8Source[]` rather than a `byte[]`, and `CliType.SymbolicBytesAt` produces one.
    /// Callers whose currency is `byte[]` — which is all of them but the byref byte-view reader —
    /// must refuse this as they refuse `Rejected`, since there is no number to hand back.
    | SymbolicallyAddressable of CliByteAddressabilityRejection
    | Rejected of CliByteAddressabilityRejection

    member this.Description : string =
        match this with
        | CliByteAddressability.ByteAddressable -> "byte-addressable"
        | CliByteAddressability.SymbolicallyAddressable obstruction ->
            $"addressable only as named bytes: %s{obstruction.Description}"
        | CliByteAddressability.SymbolicallyAddressable rejection
        | CliByteAddressability.Rejected rejection -> $"rejected: %s{rejection.Description}"

[<RequireQualifiedAccess>]
module private ByteAddressabilityClassifier =
    let nativeIntSource (source : NativeIntSource) : CliByteAddressability =
        match source with
        | NativeIntSource.Verbatim _
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> CliByteAddressability.ByteAddressable
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
        | NativeIntSource.WaitHandlePtr _ ->
            // Named, not refused: each of these is a *handle*, an identity PawPrint carries in
            // place of an address, so byte i of it is a position within that identity.
            // `SignatureHelper.InternalAddRuntimeType` copies exactly that, one byte at a time,
            // into a `Reflection.Emit` signature blob for a type it has no module to spell as a
            // token.
            CliByteAddressability.SymbolicallyAddressable (
                CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable source
            )
        // The three below stay refused, and the line is not "how much provenance is there" but
        // "is there an identity for a byte to be a position in".
        //
        // A byref is a storage location, and PawPrint already carries it through a narrowing
        // intact (`Int32Source.NarrowedManagedPointer`) so that the alignment masks managed code
        // applies stay answerable. Giving it a second route into a byte would widen that model
        // with nothing asking for it.
        //
        // The other two are numbers rather than identities: a cross-storage offset is a
        // deterministic sentinel standing in for a distance that does not exist, and
        // `OpaqueHashBits` is synthesised. Naming byte i of either would imply the whole has a
        // value worth taking a byte of.
        | NativeIntSource.ManagedPointer _
        | NativeIntSource.SyntheticCrossArrayOffset _
        | NativeIntSource.OpaqueHashBits _ ->
            CliByteAddressability.Rejected (CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable source)

    let int64Source (source : Int64Source) : CliByteAddressability =
        match source with
        | Int64Source.Verbatim _ -> CliByteAddressability.ByteAddressable
        // `WidenedNativeInt` is itself provenance: `CliNumericType.ToBytes`
        // refuses every widened value, even non-canonical wrappers around a
        // byte-renderable native-int source. `OpaqueHashBits` is a synthesised
        // hash with no meaningful byte interpretation — spilling it to memory
        // would imply it's a real numeric value, which it isn't.
        | Int64Source.SyntheticCrossArrayOffset _
        | Int64Source.WidenedNativeInt _
        | Int64Source.OpaqueHashBits _ ->
            CliByteAddressability.Rejected (CliByteAddressabilityRejection.Int64SourceNotByteAddressable source)

    let numeric (numeric : CliNumericType) : CliByteAddressability =
        match numeric with
        | CliNumericType.Int32 _
        | CliNumericType.NativeFloat _
        | CliNumericType.Int8 _
        | CliNumericType.Int16 _
        | CliNumericType.UInt8 (UInt8Source.Verbatim _)
        | CliNumericType.UInt16 _
        | CliNumericType.Float32 _
        | CliNumericType.Float64 _ -> CliByteAddressability.ByteAddressable
        // A cell already holding a named byte is exactly as unrenderable as the native int it
        // names, and for the same reason; storing one does not turn it into a number.
        | CliNumericType.UInt8 (UInt8Source.NativeIntByte _ as source) ->
            CliByteAddressability.SymbolicallyAddressable (
                CliByteAddressabilityRejection.UInt8SourceNotByteAddressable source
            )
        | CliNumericType.Int64 source -> int64Source source
        | CliNumericType.NativeInt source -> nativeIntSource source

/// How CoreCLR's auto layout treats a single field. `MethodTableBuilder` splits fields into
/// "primitive" ones, bucketed by power-of-two size class, and value-class ones, placed after
/// every bucket (methodtablebuilder.cpp:4445 draws the split; :8433 and :8500 place the two
/// groups). The split follows the field's *normalised* metadata element type, so an enum counts
/// as its underlying integer and `IntPtr`/`UIntPtr` count as `ELEMENT_TYPE_I`; every other value
/// type is a value class, including single-field BCL wrappers such as `RuntimeTypeHandle`.
[<RequireQualifiedAccess>]
type private AutoLayoutFieldClass =
    /// Occupies one slot of the `1 <<< log2Size` size class. Object references share the
    /// pointer-sized class with other pointer-sized primitives but are placed at its front.
    | Primitive of log2Size : int * isObjectReference : bool
    /// Placed after every size-class bucket, in declaration order.
    | ValueClass

/// This is the kind of type that can be stored in arguments, local variables, statics, array elements, fields.
type CliType =
    /// III.1.1.1
    | Numeric of CliNumericType
    /// III.1.1.2
    | Bool of byte
    /// III.1.1.3
    | Char of high : byte * low : byte
    /// III.1.1.4 - this is a completely opaque handle to a managed object; arithmetic is forbidden
    | ObjectRef of ManagedHeapAddress option
    /// III.1.1.5
    | RuntimePointer of CliRuntimePointer
    /// This is *not* a CLI type as such. I don't actually know its status. A value type is represented
    /// as a concatenated list of its fields.
    | ValueType of CliValueType

    static member SizeOf (t : CliType) : SizeofResult =
        match t with
        | CliType.Numeric ty ->
            let size = CliNumericType.SizeOf ty

            {
                Size = size
                Alignment = size
            }
        | CliType.Bool _ ->
            {
                Size = 1
                Alignment = 1
            }
        | CliType.Char _ ->
            {
                Size = 2
                Alignment = 2
            }
        | CliType.ObjectRef _ ->
            {
                Size = 8
                Alignment = 8
            }
        | CliType.RuntimePointer _ ->
            {
                Size = 8
                Alignment = 8
            }
        | CliType.ValueType vt -> CliValueType.SizeOf vt

    static member ContainsObjectReferences (t : CliType) : bool =
        match t with
        | CliType.ObjectRef _ -> true
        | CliType.ValueType vt -> CliValueType.ContainsObjectReferences vt
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _ -> false
        | CliType.RuntimePointer _ ->
            // Runtime/native pointers are not GC-tracked object references in these zero-value layouts.
            false

    static member ContainsRuntimePointers (t : CliType) : bool =
        match t with
        | CliType.RuntimePointer _ -> true
        | CliType.ValueType vt -> CliValueType.ContainsRuntimePointers vt
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.ObjectRef _ -> false

    static member ByteAddressability (t : CliType) : CliByteAddressability =
        match t with
        | CliType.Numeric numeric -> ByteAddressabilityClassifier.numeric numeric
        | CliType.Bool _
        | CliType.Char _ -> CliByteAddressability.ByteAddressable
        | CliType.ObjectRef _ -> CliByteAddressability.Rejected CliByteAddressabilityRejection.ObjectReference
        | CliType.RuntimePointer _ -> CliByteAddressability.Rejected CliByteAddressabilityRejection.RuntimePointer
        | CliType.ValueType vt -> CliValueType.ByteAddressability vt

    static member TryFindMarshalSizeDifference (t : CliType) : string option =
        match t with
        | CliType.Bool _ -> Some "System.Boolean marshals as a 4-byte BOOL by default, not a 1-byte CLI bool"
        | CliType.Char _ -> Some "System.Char marshalling depends on CharSet and does not always match 2-byte CLI char"
        | CliType.ObjectRef _ -> Some "object references require managed-to-unmanaged marshalling"
        | CliType.ValueType vt -> CliValueType.TryFindMarshalSizeDifference vt
        | CliType.Numeric _
        | CliType.RuntimePointer _ -> None

    /// Compute the unmanaged size that `Marshal.SizeOf` would return for `t`. Returns `Error`
    /// classifying the failure as either `NotMarshalable` (CoreCLR rejects the same shape, e.g.
    /// `LayoutKind.Auto`) or `NotImplemented` (CoreCLR would compute a size, but PawPrint hasn't
    /// implemented the case yet). Value types delegate to `CliValueType.TryComputeMarshalSize`,
    /// which consumes per-field `FieldMarshalDescriptor` and the declaring type's `CharSet` to
    /// size `[MarshalAs(ByValTStr)]` and `[MarshalAs(ByValArray)]` fields correctly. Type-system
    /// context is required so descriptors that depend on the field's nominal type can be validated.
    static member TryComputeMarshalSize
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : Result<SizeofResult, MarshalSizeError>
        =
        match t with
        | CliType.Numeric _
        | CliType.RuntimePointer _ -> Result.Ok (CliType.SizeOf t)
        | CliType.Bool _ ->
            MarshalSizeError.NotImplemented "System.Boolean marshals as a 4-byte BOOL by default, not a 1-byte CLI bool"
            |> Result.Error
        | CliType.Char _ ->
            MarshalSizeError.NotImplemented
                "System.Char marshalling depends on CharSet and does not always match 2-byte CLI char"
            |> Result.Error
        | CliType.ObjectRef _ ->
            MarshalSizeError.NotImplemented "object references require managed-to-unmanaged marshalling"
            |> Result.Error
        | CliType.ValueType vt -> CliValueType.TryComputeMarshalSize concreteTypes assemblies corelib vt

    static member ToBytes (t : CliType) : byte[] =
        match t with
        | CliType.Numeric n -> CliNumericType.ToBytes n
        | CliType.Bool b -> [| b |]
        | CliType.Char (high, low) -> [| low ; high |]
        | CliType.ObjectRef None -> Array.zeroCreate NATIVE_INT_SIZE
        | CliType.ObjectRef (Some i) -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.ToBytes cvt

    static member OfBytesAsType (targetType : ConcreteTypeHandle) (bytes : byte[]) : CliType = failwith "TODO"

    /// Reconstruct a `CliType` from a byte image in which some bytes may name a native int rather
    /// than holding a number (see <see cref="UInt8Source" />).
    ///
    /// An image whose bytes all have values is <see cref="OfBytesLike" />, unchanged. Otherwise
    /// the only template this can serve is a single byte, which is what a `ldind.u1` through a
    /// byte cursor asks for and what `SignatureHelper.InternalAddRuntimeType` then stores.
    /// Reassembling several named bytes back into the native int they came from is a different
    /// question — it has to check that they are consecutive, in order, and all from one source —
    /// and belongs with the consumer that knows it is looking at one.
    static member OfSymbolicBytesLike (template : CliType) (bytes : UInt8Source[]) : CliType =
        match UInt8Source.tryValues bytes with
        | ValueSome plain -> CliType.OfBytesLike template plain
        | ValueNone ->

        match template, bytes with
        | CliType.Numeric (CliNumericType.UInt8 _), [| single |] -> CliType.Numeric (CliNumericType.UInt8 single)
        | _ ->
            let described = bytes |> Array.map (sprintf "%O") |> String.concat ", "

            failwith
                $"CliType.OfSymbolicBytesLike: cannot read [%s{described}] as %O{template}; a byte naming a native int can only be read back as a single byte, because PawPrint has no numeric value to widen or combine"

    /// Reconstruct a primitive `CliType` from its byte encoding, using
    /// `template` only for its shape (which primitive flavour to produce).
    /// Inverse of `CliType.ToBytes` for the primitive cases it handles.
    /// Little-endian throughout, matching `CliType.ToBytes`; every platform
    /// the CLR runs on (x64/arm64/x86) is little-endian, so this assumes a
    /// little-endian host. Value types delegate to `CliValueType.OfBytesLike`;
    /// object refs, runtime pointers etc. are out of scope for this helper and
    /// fall through to a specific `failwith`.
    static member OfBytesLike (template : CliType) (bytes : byte[]) : CliType =
        let expected = CliType.SizeOf(template).Size

        if bytes.Length <> expected then
            failwith
                $"CliType.OfBytesLike: byte count mismatch - template %O{template} expects %d{expected} bytes, got %d{bytes.Length}"

        match template with
        | CliType.Bool _ -> CliType.Bool bytes.[0]
        | CliType.Char _ ->
            // CliType.Char is stored as (high, low) but serialised little-endian
            // (low byte first). Invert that on the way back in.
            CliType.Char (bytes.[1], bytes.[0])
        | CliType.Numeric (CliNumericType.Int8 _) ->
            // Direct `sbyte 0xBE` throws under checked conversion; preserve
            // the bit pattern by routing through an in-range int16 cast.
            CliType.Numeric (CliNumericType.Int8 (sbyte (int16 bytes.[0] - (if bytes.[0] >= 128uy then 256s else 0s))))
        | CliType.Numeric (CliNumericType.UInt8 _) ->
            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[0]))
        | CliType.Numeric (CliNumericType.Int16 _) ->
            CliType.Numeric (CliNumericType.Int16 (BitConverter.ToInt16 (bytes, 0)))
        | CliType.Numeric (CliNumericType.UInt16 _) ->
            CliType.Numeric (CliNumericType.UInt16 (BitConverter.ToUInt16 (bytes, 0)))
        | CliType.Numeric (CliNumericType.Int32 _) ->
            CliType.Numeric (CliNumericType.Int32 (BitConverter.ToInt32 (bytes, 0)))
        | CliType.Numeric (CliNumericType.Int64 _) ->
            CliType.Numeric (CliNumericType.Int64 (BitConverter.ToInt64 (bytes, 0) |> Int64Source.Verbatim))
        | CliType.Numeric (CliNumericType.Float32 _) ->
            CliType.Numeric (CliNumericType.Float32 (BitConverter.ToSingle (bytes, 0)))
        | CliType.Numeric (CliNumericType.Float64 _) ->
            CliType.Numeric (CliNumericType.Float64 (BitConverter.ToDouble (bytes, 0)))
        | CliType.Numeric (CliNumericType.NativeFloat _) ->
            CliType.Numeric (CliNumericType.NativeFloat (BitConverter.ToDouble (bytes, 0)))
        | CliType.Numeric (CliNumericType.NativeInt _) ->
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (BitConverter.ToInt64 (bytes, 0))))
        | CliType.ValueType vt -> CliValueType.OfBytesLike vt bytes |> CliType.ValueType
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ ->
            failwith
                $"TODO: CliType.OfBytesLike: non-primitive template %O{template} (bytes reconstruction for non-primitive storage not yet modelled)"

    /// The all-zero value of the same CLI shape as `template`: same primitive flavour, same
    /// declared struct type and field list, same storage form. Total, unlike `OfBytesLike`,
    /// which has no byte rendering for object references or runtime pointers.
    ///
    /// This is derived from the *cell*, not from the cell's declared type via
    /// `CliType.zeroOf`. Both agree whenever the cell is a faithful instance of its declared
    /// type, but only this formulation guarantees shape preservation by construction, and
    /// shape preservation is the invariant that matters at the call site: a bulk zeroing
    /// writes whole cells back through the typed write paths, which overwrite wholesale and
    /// would silently rewrite a cell's CLI shape on a mismatch (see
    /// `CellAwareMemOps.cellsHaveCompatibleShape` for the same concern on the copy path).
    ///
    /// The zero of a reference cell is the null reference and the zero of a pointer cell is
    /// the null pointer, which is what zero *bits* mean in those slots; consumers that must
    /// not encounter such cells are responsible for rejecting them, rather than relying on
    /// this function to be partial.
    static member ZeroLike (template : CliType) : CliType =
        match template with
        | CliType.Bool _ -> CliType.Bool 0uy
        | CliType.Char _ -> CliType.Char (0uy, 0uy)
        | CliType.ObjectRef _ -> CliType.ObjectRef None
        | CliType.RuntimePointer _ -> CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
        | CliType.ValueType vt -> CliValueType.ZeroLike vt |> CliType.ValueType
        | CliType.Numeric numeric ->
            let zeroed =
                match numeric with
                | CliNumericType.Int8 _ -> CliNumericType.Int8 0y
                | CliNumericType.UInt8 _ -> CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)
                | CliNumericType.Int16 _ -> CliNumericType.Int16 0s
                | CliNumericType.UInt16 _ -> CliNumericType.UInt16 0us
                | CliNumericType.Int32 _ -> CliNumericType.Int32 0
                | CliNumericType.Int64 _ -> CliNumericType.Int64 (Int64Source.Verbatim 0L)
                | CliNumericType.Float32 _ -> CliNumericType.Float32 0.0f
                | CliNumericType.Float64 _ -> CliNumericType.Float64 0.0
                | CliNumericType.NativeFloat _ -> CliNumericType.NativeFloat 0.0
                // Provenance is dropped: the result is the numeric zero, and a
                // zeroed slot no longer points at whatever the old source described.
                | CliNumericType.NativeInt _ -> CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)

            CliType.Numeric zeroed

    static member private CheckByteRange
        (operation : string)
        (offset : int)
        (count : int)
        (length : int)
        (description : string)
        : unit
        =
        if offset < 0 then
            failwith $"%s{operation}: byte offset %i{offset} is negative for %s{description}"

        if count < 0 then
            failwith $"%s{operation}: byte count %i{count} is negative for %s{description}"

        if count > length - offset then
            let start = int64 offset
            let endExclusive = start + int64 count

            failwith $"%s{operation}: byte range [%d{start}, %d{endExclusive}) exceeds %i{length}-byte %s{description}"

    static member BytesAt (offset : int) (count : int) (value : CliType) : byte[] =
        match CliType.ByteAddressability value with
        | CliByteAddressability.SymbolicallyAddressable rejection
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"CliType.BytesAt: refusing byte slice over %s{rejection.Description}. Value layout:\n%s{CliType.DescribeByteLayout None value}"
        | CliByteAddressability.ByteAddressable ->
            match value with
            | CliType.ValueType vt -> CliValueType.BytesAt offset count vt
            | _ ->
                let bytes = CliType.ToBytes value
                CliType.CheckByteRange "CliType.BytesAt" offset count bytes.Length $"CLI value %O{value}"

                let result = Array.zeroCreate<byte> count
                Array.blit bytes offset result 0 count
                result

    /// The bytes of `[offset, offset + count)`, where a byte covered by a native int PawPrint
    /// models as an identity rather than as an address is *named* rather than materialised.
    ///
    /// Agrees with <see cref="BytesAt" /> wherever that succeeds — same bytes, spelled
    /// `UInt8Source.Verbatim`. It differs in accepting a value `BytesAt` refuses, and in being
    /// slice-precise: a range that misses every such native int comes back entirely verbatim.
    /// A value with no byte image at all (an object reference, a runtime pointer, an int64
    /// carrying provenance) is still refused.
    static member SymbolicBytesAt (offset : int) (count : int) (value : CliType) : UInt8Source[] =
        match CliType.ByteAddressability value with
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"CliType.SymbolicBytesAt: refusing byte slice over %s{rejection.Description}. Value layout:\n%s{CliType.DescribeByteLayout None value}"
        | CliByteAddressability.ByteAddressable -> CliType.BytesAt offset count value |> Array.map UInt8Source.Verbatim
        | CliByteAddressability.SymbolicallyAddressable _ ->

        match value with
        | CliType.ValueType vt -> CliValueType.SymbolicBytesAt offset count vt
        | CliType.Numeric (CliNumericType.UInt8 source) ->
            CliType.CheckByteRange "CliType.SymbolicBytesAt" offset count 1 $"CLI value %O{value}"
            Array.create count source
        | CliType.Numeric (CliNumericType.NativeInt source) ->
            CliType.CheckByteRange "CliType.SymbolicBytesAt" offset count NATIVE_INT_SIZE $"CLI value %O{value}"
            Array.init count (fun i -> UInt8Source.NativeIntByte (source, offset + i))
        | _ ->
            // `ByteAddressability` says this value is nameable, so every arm it can reach must be
            // handled above; a value that is neither a struct nor a native int has no route to
            // that answer.
            failwith
                $"CliType.SymbolicBytesAt: %O{value} reports nameable bytes but is neither a value type nor a native int (this is an interpreter bug)"

    /// Did zeroing actually change anything? Zeroing a cell holding `-0.0` really does change
    /// memory, and reporting "unchanged" would leave the sign bit set, so `-0.0` versus `0.0`
    /// counts as a change.
    static member internal ZeroingChangedAnything (before : CliType) (after : CliType) : bool =
        // Deliberately not `=`: structural equality on floats follows IEEE, so it calls `-0.0`
        // equal to `0.0` even though they differ in every byte that matters. Where a byte
        // rendering exists it is the ground truth; where it does not (references,
        // provenance-carrying native ints) there is no such subtlety and structural equality is
        // exactly right.
        match CliType.ByteAddressability before, CliType.ByteAddressability after with
        | CliByteAddressability.ByteAddressable, CliByteAddressability.ByteAddressable ->
            CliType.ToBytes before <> CliType.ToBytes after
        | _ ->

        // No byte rendering on at least one side, so descend instead of comparing the whole
        // thing: a struct can be unrenderable because of a *pointer* field while also holding a
        // `-0.0` float, and comparing the aggregates structurally would hit exactly the IEEE
        // trap this function exists to avoid. Each field is judged by the same rule, so
        // byte-renderable subfields still get the byte comparison.

        match before, after with
        | CliType.ValueType b, CliType.ValueType a -> CliValueType.ZeroingChangedAnything b a
        // Structural inequality can over-report here — two values identical in memory may
        // differ in field write-timestamp bookkeeping — but it never under-reports, and the
        // only cost of a false "changed" is one redundant write of an identical value.
        | _ -> before <> after

    /// The extents `(offset, size)`, relative to `value`, of the nested storage cells that contain
    /// byte `byteOffset`. Outermost first, so sizes descend.
    ///
    /// This is a *candidate generator*, not an answer: it navigates the layout and performs none of
    /// `CellPathsExactlyCovering`'s aliasing or padding checks, so an extent it reports may name no
    /// cell at all. It exists for callers that must discover a byte range before they can ask
    /// whether that range names a cell — a bulk copy stepping through a buffer knows where its
    /// cursor is but not how wide the next move should be. Such a caller proposes from here and
    /// disposes with `CellPathsExactlyCovering`, which remains the sole authority on nameability.
    /// Correctness therefore never rests on this function: an extent it invents is thrown out by
    /// the validator, and an extent it fails to report costs the caller only the fallback route it
    /// would have taken anyway.
    ///
    /// It is nonetheless *complete* for the anchored queries `CellAwareMemOps` makes of it — every
    /// width the validator would accept at a given anchor byte is proposed here — and that is what
    /// makes proposing from one endpoint enough. `CellPathsExactlyCovering` reaches a cell only by
    /// descending through the unique unaliased field containing the whole range; any field
    /// containing the range contains every byte of it, so each cell it names lies on the
    /// containment chain of every byte in the range, which is precisely what this walks. Where this
    /// stops early — several fields containing the byte, or none, the latter meaning the byte is
    /// padding — the validator necessarily declines too: a second field containing the byte
    /// overlaps the range and trips the alias check, and a range whose bytes no field contains has
    /// no containing field either. Only the whole value survives those cases, and it is always
    /// proposed. `TestCliTypeCellPaths` pins this against a brute-force enumeration of every width.
    static member CandidateCellExtentsContainingByte (byteOffset : int) (value : CliType) : (int * int) list =
        let size = CliType.SizeOf(value).Size

        if byteOffset < 0 || byteOffset >= size then
            []
        else

        let here = 0, size

        match value with
        | CliType.ValueType vt ->
            match
                CliValueType.TryAllFields vt
                |> List.filter (fun f -> f.Offset <= byteOffset && byteOffset < f.Offset + f.Size)
            with
            | [ f ] ->
                here
                :: (CliType.CandidateCellExtentsContainingByte (byteOffset - f.Offset) f.Contents
                    |> List.map (fun (offset, size) -> f.Offset + offset, size))
            // Either several fields contain the byte (explicit layout can overlap them), in which
            // case there is no single one to descend into, or none does and the byte is padding.
            // Both times the whole value is the only candidate.
            | _ -> [ here ]
        | _ -> [ here ]

    /// Every storage cell of `value` whose extent is exactly the byte range
    /// `[offset, offset + size)`, as a path of `FieldId`s from `value` down to the cell, paired
    /// with the cell's contents. Ordered outermost first. Empty if the range names no cell.
    ///
    /// Naming a cell is what lets the
    /// byref layer serve an access whose storage has no byte rendering at all — a value type
    /// containing object references — since there the bytewise path cannot run and the cell is the
    /// only thing left to read or write. A range that is *part* of a cell, that spans two, or that
    /// one cell covers exactly while another *aliases* it, names nothing: the first two have no
    /// single cell to point at, and the third would leave the alias stale on write.
    ///
    /// More than one answer is normal rather than an ambiguity: a transparent wrapper and the field
    /// it wraps occupy the same bytes, and which one a caller wants depends on the type it is
    /// reinterpreting to. Answers therefore form a nesting chain, and a caller that takes the first
    /// type-compatible one gets the shallowest cell that will do — the one that disturbs least on
    /// write.
    ///
    /// This is *structural*: it does not look at what a cell contains, so callers must
    /// apply their own compatibility rule to the contents. It walks fields rather than bytes, so it
    /// stays defined precisely where the byte path is not. Raw-bytes storage has no fields, so it
    /// never answers.
    static member CellPathsExactlyCovering
        (offset : int)
        (size : int)
        (value : CliType)
        : (FieldId list * CliType) list
        =
        if size <= 0 then
            []
        else

        match value with
        | CliType.ValueType vt ->
            let fields = CliValueType.TryAllFields vt

            // Widened to 64 bits because `offset` is guest-controlled — it accumulates
            // `Unsafe.Add`/`Unsafe.AddByteOffset` arithmetic — so the range's end point need not
            // fit in an `int`. This file compiles under `open Checked`, so computing it narrowly
            // would raise `OverflowException` out of a lookup whose contract is to return `[]`.
            // Field offsets and sizes come from a laid-out type and are small; only the range is
            // suspect.
            let rangeStart = int64 offset
            let rangeEnd = int64 offset + int64 size

            // The range must sit inside a single field for any cell to name it. Under explicit
            // layout several fields can contain it, in which case a write through one would strand
            // the others, so we refuse rather than pick.
            let containing =
                fields
                |> List.filter (fun f -> int64 f.Offset <= rangeStart && rangeEnd <= int64 f.Offset + int64 f.Size)

            match containing with
            | [ f ] ->
                let aliased =
                    fields
                    |> List.exists (fun g ->
                        not (FieldId.exactlyEqual g.Id f.Id)
                        && int64 g.Offset < rangeEnd
                        && rangeStart < int64 g.Offset + int64 g.Size
                    )

                if aliased then
                    []
                else

                // A field whose laid-out extent exceeds its contents' own size has padding in it,
                // so the range covers the field but not the *value*; descend without naming it.
                let namesThisField =
                    f.Offset = offset && f.Size = size && f.Size = CliType.SizeOf(f.Contents).Size

                let deeper =
                    CliType.CellPathsExactlyCovering (offset - f.Offset) size f.Contents
                    |> List.map (fun (path, leaf) -> f.Id :: path, leaf)

                if namesThisField then
                    ([ f.Id ], f.Contents) :: deeper
                else
                    deeper
            | _ -> []
        | _ -> []

    /// Zero the byte range `[offset, offset + count)` of `value`, returning `None` if that would
    /// leave it unchanged.
    ///
    /// Unlike `WithBytesAtIfChanged`, this works on storage that has no byte rendering at all:
    /// it walks structure rather than materialising bytes, so a range covering a whole object
    /// reference nulls it, and a range covering a plain field of a *reference-containing* struct
    /// zeroes just that field.
    ///
    /// A field only *partially* covered by the range is an error unless it can itself absorb a
    /// partial zeroing — a nested value type recurses, and a byte-addressable primitive has its
    /// own sub-range zeroed. Partially zeroing an object reference or a runtime pointer has no
    /// meaning and fails loudly
    static member WithZeroedRangeIfChanged (offset : int) (count : int) (value : CliType) : CliType option =
        // `SpanHelpers.ClearWithReferences` uses this, reinterpreting the
        // element data as `IntPtr` slots and storing zero into each, and for a struct like
        // `Dictionary<K,V>.Entry` the slots do not line up one-to-one with reference fields.
        let size = CliType.SizeOf(value).Size

        // Bounds-check without forming `offset + count`, which wraps for large inputs and would
        // let an out-of-range request through as though it were valid.
        if offset < 0 || count < 0 || offset > size || count > size - offset then
            failwith
                $"CliType.WithZeroedRangeIfChanged: range of %d{count} byte(s) at offset %d{offset} is outside the %d{size}-byte value %O{value}"

        if count = 0 then
            None
        elif offset = 0 && count = size then
            // Whole-value zeroing needs no structural walk and is defined for every shape.
            let zeroed = CliType.ZeroLike value

            if CliType.ZeroingChangedAnything value zeroed then
                Some zeroed
            else
                None
        else

        match value with
        | CliType.ValueType vt ->
            CliValueType.WithZeroedRangeIfChanged offset count vt
            |> Option.map CliType.ValueType
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ ->
            // A reference or pointer occupies its slot indivisibly: there is no such thing as
            // half a reference, so a range that covers only part of one cannot be honoured.
            // Callers reach this only through storage whose layout says the range straddles a
            // reference, which is either a misaligned explicit layout or an interpreter bug —
            // both worth failing loudly for rather than guessing.
            failwith
                $"CliType.WithZeroedRangeIfChanged: refusing to zero the partial range [%d{offset}, %d{offset + count}) of the %d{size}-byte %O{value}; a reference or pointer cannot be partially cleared"
        | CliType.Bool _
        | CliType.Char _
        | CliType.Numeric _ ->
            // A partial zeroing of a primitive is an ordinary byte write. This still refuses
            // storage carrying non-`Verbatim` provenance, which is right: half-clearing a
            // tagged native int would leave a corrupt pointer.
            CliType.WithBytesAtIfChanged offset (Array.zeroCreate count) value

    /// The maximal run of *padding* bytes containing `byteOffset` — bytes that no field covers, at
    /// whatever depth of nesting the byte finally lands — as `(start, length)` in `value`'s own
    /// coordinates.
    ///
    /// Padding needs naming because `CellPathsExactlyCovering` cannot name it and, inside a value
    /// type that holds object references, the byte path cannot render it either: such a value has
    /// no byte image at all, so `BytesAt` refuses the whole of it. A bulk move whose range starts
    /// or ends *inside* such a value therefore has a cursor with nowhere to go — no cell begins
    /// there and no byte can be read — which is exactly the state
    /// `CellAwareMemOps.tryPaddingMoveAt` uses this to get out of. Padding is the one part of such
    /// a value that is pure bytes, and `PreservedBytes` already holds it.
    ///
    /// Descent follows `CandidateCellExtentsContainingByte`'s rule — step through the single field
    /// containing the byte — so the two agree on which value the byte finally belongs to. `None`
    /// means the byte is not padding, which covers four cases: it lies inside some field's
    /// contents; the value is not a field-backed value type (a primitive is all content, and a
    /// raw-bytes value *is* its bytes); explicit layout puts two or more fields over it, leaving no
    /// single field to descend through; or a field's laid-out extent disagrees with the size of
    /// what it holds, which makes the value internally inconsistent (see
    /// `CliValueType.TryDescendableFieldAt`).
    static member TryPaddingRunAt (byteOffset : int) (value : CliType) : (int * int) option =
        match value with
        | CliType.ValueType vt -> CliValueType.TryPaddingRunAt byteOffset vt
        | CliType.Bool _
        | CliType.Char _
        | CliType.Numeric _
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> None

    /// The bytes of `[offset, offset + count)`, which must lie wholly within a single padding run
    /// of `value`. Unlike `BytesAt` this is defined even when `value` has no byte image, which is
    /// the whole point: padding inside a reference-containing struct is pure bytes even though the
    /// struct around it is not.
    static member PaddingBytesAt (offset : int) (count : int) (value : CliType) : byte[] =
        CliType.CheckPaddingRange "CliType.PaddingBytesAt" offset count value

        match value with
        | CliType.ValueType vt -> CliValueType.PaddingBytesAt offset count vt
        | other ->
            failwith
                $"CliType.PaddingBytesAt: %O{other} is not a value type and so has no padding (this is an interpreter bug: CheckPaddingRange should have refused)"

    /// Replace the bytes of `[offset, offset + bytes.Length)`, which must lie wholly within a
    /// single padding run of `value`, returning `None` if that would leave it unchanged.
    ///
    /// No field's contents can be touched by construction, so the value's CLI shape — and every
    /// cell `CellPathsExactlyCovering` names in it — is preserved.
    static member WithPaddingBytesAtIfChanged (offset : int) (bytes : byte[]) (value : CliType) : CliType option =
        CliType.CheckPaddingRange "CliType.WithPaddingBytesAtIfChanged" offset bytes.Length value

        match value with
        | CliType.ValueType vt ->
            CliValueType.WithPaddingBytesAtIfChanged offset bytes vt
            |> Option.map CliType.ValueType
        | other ->
            failwith
                $"CliType.WithPaddingBytesAtIfChanged: %O{other} is not a value type and so has no padding (this is an interpreter bug: CheckPaddingRange should have refused)"

    /// Both padding accessors are partial in the same way, so they check their precondition the
    /// same way: the range must be non-empty and contained in the single run `TryPaddingRunAt`
    /// reports at its start. A caller reaching either without having asked that question is a bug,
    /// not a shape to degrade on.
    static member private CheckPaddingRange (operation : string) (offset : int) (count : int) (value : CliType) : unit =
        if count <= 0 then
            failwith $"%s{operation}: byte count %d{count} is not positive"

        match CliType.TryPaddingRunAt offset value with
        | None ->
            failwith
                $"%s{operation}: byte offset %d{offset} of %O{value} is not padding. Value layout:\n%s{CliType.DescribeByteLayout None value}"
        | Some (start, length) ->
            // `offset` is inside the run by construction, so only the far end can escape it.
            // Phrased to avoid forming `offset + count`, which this file's `open Checked` would
            // overflow on a guest-driven offset.
            if count > start + length - offset then
                failwith
                    $"%s{operation}: byte range of %d{count} byte(s) at offset %d{offset} of %O{value} leaves the padding run [%d{start}, %d{start + length})"

    /// Return a byte-addressable CLI value with the requested byte range replaced, or `None` if
    /// the materialised byte image would be unchanged. Value types delegate to
    /// `CliValueType.WithBytesAtIfChanged`, so represented padding and overlapping-field
    /// provenance stay within the value-layout model.
    static member WithBytesAtIfChanged (offset : int) (bytes : byte[]) (value : CliType) : CliType option =
        match CliType.ByteAddressability value with
        | CliByteAddressability.SymbolicallyAddressable rejection
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"CliType.WithBytesAtIfChanged: refusing byte write over %s{rejection.Description}. Value layout:\n%s{CliType.DescribeByteLayout None value}"
        | CliByteAddressability.ByteAddressable ->
            match value with
            | CliType.ValueType vt ->
                CliValueType.WithBytesAtIfChanged offset bytes vt
                |> Option.map CliType.ValueType
            | _ ->
                let existing = CliType.ToBytes value

                CliType.CheckByteRange
                    "CliType.WithBytesAtIfChanged"
                    offset
                    bytes.Length
                    existing.Length
                    $"CLI value %O{value}"

                let mutable identical = true
                let mutable i = 0

                while identical && i < bytes.Length do
                    identical <- existing.[offset + i] = bytes.[i]
                    i <- i + 1

                if identical then
                    None
                else
                    // `ToBytes` returns a fresh buffer, so mutating this local copy
                    // before reconstructing the CLI value cannot mutate `value`.
                    Array.blit bytes 0 existing offset bytes.Length
                    Some (CliType.OfBytesLike value existing)

    /// Return a byte-addressable CLI value with the requested byte range replaced.
    /// Byte-identical writes return the original value.
    static member WithBytesAt (offset : int) (bytes : byte[]) (value : CliType) : CliType =
        match CliType.WithBytesAtIfChanged offset bytes value with
        | None -> value
        | Some updated -> updated

    static member DescribeByteLayout (concreteTypes : AllConcreteTypes option) (value : CliType) : string =
        match value with
        | CliType.ValueType vt -> CliValueType.DescribeByteLayout concreteTypes vt
        | _ ->
            let size = CliType.SizeOf value
            let byteAddressability = CliType.ByteAddressability value

            let storageKind =
                match value with
                | CliType.Numeric numeric -> $"numeric %O{numeric}"
                | CliType.Bool _ -> "bool"
                | CliType.Char _ -> "char"
                | CliType.ObjectRef _ -> "object reference"
                | CliType.RuntimePointer _ -> "runtime pointer"
                | CliType.ValueType _ -> failwith "unreachable"

            [
                "CLI value byte layout:"
                $"storage: %s{storageKind}"
                $"size: %i{size.Size} bytes (alignment %i{size.Alignment})"
                $"byte-addressability: %s{byteAddressability.Description}"
                $"value: %O{value}"
            ]
            |> String.concat "\n"

and CliField =
    {
        Id : FieldId
        Name : string
        Contents : CliType
        /// "None" for "no explicit offset specified"; we expect most offsets to be None.
        Offset : int option
        Type : ConcreteTypeHandle
        /// Parsed `[MarshalAs(...)]` descriptor for this field, or `None` if absent. Carried
        /// on the field itself so that unmanaged-size computation can consult it without
        /// re-walking the underlying `FieldInfo`.
        MarshallingDescriptor : FieldMarshalDescriptor option
    }

/// One type in a base chain, together with the fields *it* declares and the facts that govern
/// how they are placed.
///
/// CoreCLR lays each type in a chain out separately, starting a derived type's own fields at the
/// parent's instance size (`HandleAutoLayout`, methodtablebuilder.cpp:8283-8296), so an inherited
/// field's offset is a property of the type that declares it and of nothing below. Layout
/// therefore consumes a chain rather than one flattened field list: the flat list cannot say
/// where one type's fields end and the next type's begin, and — more importantly — it has thrown
/// away the per-level `Pack`, declared kind and declared `Size` that placement reads.
and TypeLayoutLevel =
    {
        /// The type declaring `OwnFields`.
        Declared : ConcreteTypeHandle
        /// How *this* type's own fields are placed.
        Facts : DeclaredTypeFacts
        /// Only the fields this type declares; inherited ones belong to the level that declares
        /// them.
        OwnFields : CliField list
        /// True for `System.Object` and `System.ValueType`, and for those two only.
        ///
        /// CoreCLR's `hasNonTrivialParent` (`PlaceInstanceFields`, methodtablebuilder.cpp:8132)
        /// is `pParentMT && !IsObjectClass() && !IsValueTypeClass()`, so a type deriving directly
        /// from either is treated as having no parent at all. This is not an
        /// optimisation: the promotion rule below demotes a `Sequential` type whose parent is not
        /// managed-sequential, and neither `Object` nor `ValueType` is, so without this every
        /// plain `[StructLayout(Sequential)] struct` would be promoted to auto layout.
        ///
        /// `System.Enum` is *not* in this set: it is a reference type deriving
        /// from `ValueType`, it declares no instance fields, and CoreCLR treats it as an ordinary
        /// zero-sized parent — which is why an enum's `value__` still lands at offset 0.
        IsTrivialParent : bool
    }

/// What one level of a base chain needs to know about the level below it. `None` where
/// `TypeLayoutLevel.IsTrivialParent` says there is effectively no parent.
and ParentLayout =
    {
        /// Where this level's own fields start: the parent's `GetNumInstanceFieldBytes()`
        /// (methodtable.inl:151).
        InstanceSize : int
        /// True when the parent ended up on the sequential path — declared `Sequential` *and* not
        /// promoted. CoreCLR's `MethodTable::IsManagedSequential`, which the promotion rule and
        /// `TryGetParentLayoutInfo` (classlayoutinfo.cpp:20-45) both read.
        IsManagedSequential : bool
        /// The parent's alignment requirement, contributed to a sequential or explicit child but
        /// only when the parent is managed-sequential or blittable-explicit (ibid).
        AlignmentRequirement : int
        /// True when the parent was bumped from a computed size of 0 up to 1. Such a parent
        /// contributes 0 to a derived type rather than its padded size, "but ONLY for inheritance
        /// situations" (`TryGetParentLayoutInfo`).
        IsZeroSized : bool
        /// True when the parent, or anything above it in the chain, holds an object reference.
        /// The promotion rule reads the *parent's* GC-ness as well as the type's own
        /// (`hasGCFields`, methodtablebuilder.cpp:8179).
        ContainsReferences : bool
    }

and CliConcreteField =
    private
        {
            Name : string
            Contents : CliType
            Offset : int
            Size : int
            Alignment : int
            ConfiguredOffset : int option
            EditedAtTime : uint64
            Id : FieldId
            Type : ConcreteTypeHandle
            MarshallingDescriptor : FieldMarshalDescriptor option
        }

    static member ToCliField (this : CliConcreteField) : CliField =
        {
            Offset = this.ConfiguredOffset
            Contents = this.Contents
            Id = this.Id
            Name = this.Name
            Type = this.Type
            MarshallingDescriptor = this.MarshallingDescriptor
        }

/// Field-backed storage preserves named field provenance. The preserved bytes are the full
/// value image used as the base for `ToBytes`: normal field values are still authoritative
/// for represented field ranges, while this byte image preserves padding and other byte
/// ranges not represented by fields. The preserved image must always be exactly the declared
/// `SizeOf` of the containing value type.
and CliFieldBackedStorage =
    {
        Fields : CliConcreteField list
        PreservedBytes : byte[]
    }

and CliValueTypeStorage =
    | Fields of CliFieldBackedStorage
    /// Raw storage is used only for fieldless custom-layout value types.
    | RawBytes of byte[]

/// Where one declared field of a value type lands in that type's *unmanaged* (marshalled)
/// image, as computed by <see cref="CliValueType.TryComputeMarshalLayout"/>.
///
/// This is the per-field detail that <see cref="CliValueType.TryComputeMarshalSize"/> derives
/// and then discards. Anything that needs to *write* the unmanaged image (as opposed to
/// merely sizing it) must consume these placements rather than re-deriving offsets, or the
/// two walks can disagree on an individual field while still agreeing on the total.
///
/// Note the shape assumes one contiguous native range per managed field. That holds for every
/// case we currently support, but it is not obviously universal: CoreCLR's `NFT_DECIMAL` field
/// marshaller, for instance, repositions a `System.Decimal` relative to its managed form. Treat
/// the one-to-one assumption as provisional, and expect to revisit the shape — not merely add a
/// case to the consumer's classification — if a marshaller needs several native ranges.
and MarshalFieldPlacement =
    {
        /// The managed field this placement describes.
        Field : CliField
        /// Byte offset of the field's native form within the unmanaged image.
        NativeOffset : int
        /// Size and alignment the field's native form contributes.
        NativeSize : SizeofResult
    }

and CliValueType =
    private
        {
            /// Do not use directly; use the `.Declared` accessor.
            /// Identifies the declared CLR type of this value (e.g. `System.IntPtr`,
            /// `System.RuntimeTypeHandle`, or a user struct). Used at the eval-stack boundary to
            /// decide primitive-like flattening via `PrimitiveLikeStruct.kind`.
            _Declared : ConcreteTypeHandle
            /// Cached primitive-like classification for `_Declared`. `Some kind` for any value type
            /// whose storage form is a single-field wrapper that the eval stack flattens: the
            /// closed set of BCL wrapper structs (IntPtr, RuntimeTypeHandle, ...) plus every CLR
            /// enum over a fixed-width integer. `None` for user-defined structs and non-primitive
            /// BCL structs. Enum-ness is decided nominally, by the constructing caller, and
            /// arrives as `DeclaredTypeFacts.IsEnum`. Populated at construction
            /// time so the context-free `EvalStackValue.ofCliType` can flatten without threading
            /// `BaseClassTypes`/`AllConcreteTypes` through every push site.
            _PrimitiveLikeKind : PrimitiveLikeKind option
            /// The alignment CoreCLR stamps on the declared type by name, overriding the one its
            /// fields imply; `None` for every type whose demand is derived, which is almost all of
            /// them. Arrives as `DeclaredTypeFacts.NominalAlignment`, and is cached here for the
            /// same reason `_PrimitiveLikeKind` is: answering it needs metadata, and `SizeOf` is
            /// context-free.
            ///
            /// It governs only what a *container* must do to place a field of this type. The
            /// type's own size is still derived (see `SizeOf`), which is faithful: CoreCLR stamps
            /// the alignment after the size has been computed and never recomputes it.
            _NominalAlignment : int option
            /// The instance size and alignment the layout pass produced, cached because it cannot
            /// be recovered from the placements.
            ///
            /// Two facts make recomputation impossible rather than merely wasteful. A declared
            /// `ClassLayout.Size` is a floor *relative to the parent*, so a type can be wider than
            /// its own last field ends -- `[Sequential] PB { long }` -> `[Sequential, Size = 12]
            /// PD { int }` is 20 bytes with its fields ending at 12. And the alignment requirement
            /// mixes each level's own `Pack` with its parent's, which a flat field list has thrown
            /// away.
            ///
            /// `DeclaredTypeFacts.NominalAlignment` is already folded in here, so `SizeOf` is a
            /// field read.
            _InstanceSize : SizeofResult
            _Storage : CliValueTypeStorage
            /// Which field-placement algorithm governs this type, as
            /// `TypeLayoutKind.applied` reports it for the declaring type. Stored alongside
            /// `Layout` because sizing needs both and neither determines the other: `Layout` is
            /// the `ClassLayout` table's `Pack`/`Size`, and this says whether anything reads them.
            LayoutKind : TypeLayoutKind
            Layout : Layout
            /// Marshalling string-encoding hint, derived from the declaring type's
            /// `TypeAttributes.StringFormatMask` (Ansi/Unicode/Auto). Stage 3 of the field-marshal
            /// work consumes this to size `[MarshalAs(ByValTStr)]` fields; today it's plumbing
            /// only and has no effect on runtime behaviour.
            CharSet : CharSet
            /// We track dependency orderings between updates to overlapping fields with a monotonically increasing
            /// timestamp.
            NextTimestamp : uint64
        }

    member this.Declared : ConcreteTypeHandle = this._Declared
    member this.PrimitiveLikeKind : PrimitiveLikeKind option = this._PrimitiveLikeKind

    /// Is an enum with this field shape one whose storage PawPrint flattens?
    ///
    /// True only for enums over the fixed-width integers. ECMA-335 II.14.3 also permits an enum
    /// over `bool`, `char` or a native int — C# cannot declare one but Reflection.Emit can, and
    /// the CLR loads them — and those answer false, so their storage stays a wrapped
    /// `CliValueType`. `IlMachineRuntimeMetadata.unboxMaterialisesFlattened` depends on that:
    /// it decides whether a boxed value materialises flattened, and if this widened to cover them,
    /// `unboxPermitted` would start refusing a legal unbox that works today.
    ///
    /// This is the *structural* half of the enum question, and it is structural on purpose: it
    /// asks which `CliNumericType` cell the value actually holds, which is a fact about the storage
    /// in hand rather than about metadata. The nominal half — is the declared type an enum at all —
    /// cannot be answered from the fields and arrives as `DeclaredTypeFacts.IsEnum`.
    ///
    /// The `value__`/offset-0/single-field conditions are retained as a guard rather than as the
    /// test: for a genuine enum ECMA-335 II.14.3 guarantees them, so failing them means the caller
    /// misidentified the type, and answering "not flattenable" is the safe direction.
    static member private EnumUnderlyingIsFlattenable (fields : CliConcreteField list) : bool =
        match fields with
        | [ f ] when f.Name = "value__" && f.Offset = 0 ->
            match f.Contents with
            | CliType.Numeric numeric ->
                match numeric with
                | CliNumericType.Int8 _
                | CliNumericType.UInt8 _
                | CliNumericType.Int16 _
                | CliNumericType.UInt16 _
                | CliNumericType.Int32 _
                | CliNumericType.Int64 _ -> true
                | CliNumericType.NativeInt _
                | CliNumericType.Float32 _
                | CliNumericType.Float64 _
                | CliNumericType.NativeFloat _ -> false
            | CliType.Bool _
            | CliType.Char _
            | CliType.ObjectRef _
            | CliType.RuntimePointer _
            | CliType.ValueType _ -> false
        | _ -> false

    /// Combine the nominal BCL-wrapper classification with the enum classification.
    /// Returns the BCL kind if `declared` is one of the wrapper structs; otherwise returns
    /// `Some EnumLike` if the declared type is a CLR enum whose underlying integer we flatten;
    /// otherwise `None`.
    ///
    /// Neither half of the enum test implies the other. `isEnum` is
    /// nominal and comes from the caller's metadata; `EnumUnderlyingIsFlattenable` is structural
    /// and reads the storage in hand. Dropping the first classifies `struct Fake { int value__; }`
    /// as an enum (issue #996); dropping the second flattens enums over `bool`/`char`/native int,
    /// which `unboxMaterialisesFlattened` requires stay wrapped.
    static member private ClassifyPrimitiveLike
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (isEnum : bool)
        (fields : CliConcreteField list)
        : PrimitiveLikeKind option
        =
        match PrimitiveLikeStruct.kindFromHandle bct allCt declared with
        | Some k -> Some k
        | None ->
            if isEnum && CliValueType.EnumUnderlyingIsFlattenable fields then
                Some PrimitiveLikeKind.EnumLike
            else
                None

    /// Classify a field for `ComputeAutoLayoutFields`. See `AutoLayoutFieldClass` for the rule
    /// and for why enums and `IntPtr` land on the primitive side of it.
    static member private ClassifyForAutoLayout (contents : CliType) : AutoLayoutFieldClass =
        let asPrimitive (isObjectReference : bool) : AutoLayoutFieldClass =
            let size = (CliType.SizeOf contents).Size

            let log2Size =
                match size with
                | 1 -> 0
                | 2 -> 1
                | 4 -> 2
                | 8 -> 3
                | other ->
                    failwith
                        $"CliValueType.ClassifyForAutoLayout: %O{contents} is a primitive field of %d{other} bytes, but CoreCLR's auto layout only buckets sizes 1, 2, 4 and 8 (MAX_LOG2_PRIMITIVE_FIELD_SIZE = %d{LOG2_NATIVE_INT_SIZE})"

            AutoLayoutFieldClass.Primitive (log2Size, isObjectReference)

        match contents with
        | CliType.ObjectRef _ -> asPrimitive true
        | CliType.Bool _
        | CliType.Char _
        | CliType.Numeric _
        | CliType.RuntimePointer _ -> asPrimitive false
        | CliType.ValueType vt ->
            match vt.PrimitiveLikeKind with
            // These two are exactly the value types whose element type CoreCLR normalises to a
            // primitive: an enum to its underlying integer, `IntPtr`/`UIntPtr` to `ELEMENT_TYPE_I`.
            // The remaining primitive-like kinds are ordinary single-field structs to the type
            // loader, so they are value classes here despite flattening on the eval stack.
            | Some PrimitiveLikeKind.EnumLike
            | Some PrimitiveLikeKind.FlattenToNativeInt -> asPrimitive false
            | Some PrimitiveLikeKind.FlattenToObjectRef
            | Some PrimitiveLikeKind.FlattenToRuntimePointer
            | Some PrimitiveLikeKind.FlattenToManagedPointer
            | None -> AutoLayoutFieldClass.ValueClass

    /// Port of CoreCLR's `MethodTableBuilder::HandleAutoLayout` (methodtablebuilder.cpp:8266),
    /// restricted to the value-type case: no parent instance fields, no 32-bit-only offset bias,
    /// and no inline-array extension.
    ///
    /// CoreCLR routes *every* value type containing GC references through auto layout, whatever
    /// `LayoutKind` the metadata declares (`PlaceInstanceFields`, methodtablebuilder.cpp:8212),
    /// which is why this takes no `Layout`: auto layout ignores both `Pack` and an explicit
    /// `Size`. Bucketing is not cosmetic — a declared-order walk cannot fit `{byte; object; byte}`
    /// into the 16 bytes CoreCLR gives it, because it has no way to make the two bytes adjacent,
    /// and the resulting size is visible to `sizeof` and to array element stride.
    static member private ComputeAutoLayoutFields
        (startOffset : int)
        (fields : CliField list)
        : CliConcreteField list * int
        =
        let classified =
            fields
            |> List.map (fun field -> field, CliValueType.ClassifyForAutoLayout field.Contents)
            |> Array.ofList

        // Object references share the pointer-sized class with other pointer-sized primitives,
        // so they are counted into it here and split back out below.
        let slotsInClass = Array.zeroCreate<int> (LOG2_NATIVE_INT_SIZE + 1)
        let mutable objectReferenceCount = 0

        for _, cls in classified do
            match cls with
            | AutoLayoutFieldClass.Primitive (log2Size, isObjectReference) ->
                slotsInClass.[log2Size] <- slotsInClass.[log2Size] + 1

                if isObjectReference then
                    objectReferenceCount <- objectReferenceCount + 1
            | AutoLayoutFieldClass.ValueClass -> ()

        // Fields the back-fill pass below places out of band, by index into `classified`.
        let backFilled : int option array = Array.create classified.Length None

        let mutable cursor = startOffset

        // "Place small fields first if the parent has a number of field bytes that is not aligned"
        // (methodtablebuilder.cpp:8347-8428). A derived type whose parent ends mid-word tries to
        // fill that gap with its own small fields before starting the largest-first regions, so
        // `class B { byte A; } class D : B { long L; byte C; int N; }` really does put `C` at 1.
        //
        // Only a chain reaches this: with no parent the cursor starts at 0, which is aligned.
        //
        // Note what this pass can *never* move: an object reference. The loop is bounded by
        // `i < MAX_LOG2_PRIMITIVE_FIELD_SIZE` and only ever narrows `i` downwards, so it considers
        // the byte, short and int classes and never the pointer-sized one. CoreCLR has two pieces
        // of machinery guarding exactly that case -- an `i == LOG2SLOT` check that aborts the whole
        // pass rather than reordering GC fields (:8391-8402), and an in-tree `TODO` at :8370-8379
        // about the class scan counting references it may not then use -- and on a 64-bit target
        // both are unreachable, because `LOG2SLOT` is `LOG2_PTRSIZE` = 3 and the loop bound is also
        // 3. They matter only where `LOG2SLOT < MAX_LOG2_PRIMITIVE_FIELD_SIZE`, i.e. on 32-bit.
        // Neither is ported: an unreachable branch is one no test can hold honest.
        if cursor % NATIVE_INT_SIZE <> 0 then
            let mutable i = 0
            let mutable exhausted = false

            while not exhausted && i < LOG2_NATIVE_INT_SIZE do
                if cursor % (1 <<< (i + 1)) = 0 then
                    i <- i + 1
                else

                // Nothing to gain unless some *bigger* field would otherwise be placed first.
                let mutable bigger = i + 1

                while bigger <= LOG2_NATIVE_INT_SIZE && slotsInClass.[bigger] = 0 do
                    bigger <- bigger + 1

                if bigger > LOG2_NATIVE_INT_SIZE then
                    exhausted <- true
                else

                // Fall back to the largest class that still fits in the gap.
                let mutable smaller = i

                while smaller >= 0 && slotsInClass.[smaller] = 0 do
                    smaller <- smaller - 1

                if smaller < 0 then
                    exhausted <- true
                else

                i <- smaller

                // The first unplaced field of that class. One exists: `slotsInClass` counts
                // exactly the unplaced ones, and the scan above chose `i` because that count is
                // non-zero -- so a miss here means the counter and the array have drifted apart,
                // which is a bug in this function rather than a shape to tolerate.
                let mutable chosen = -1
                let mutable candidate = 0

                while chosen < 0 && candidate < classified.Length do
                    if backFilled.[candidate].IsNone then
                        match snd classified.[candidate] with
                        | AutoLayoutFieldClass.Primitive (log2Size, _) when log2Size = i -> chosen <- candidate
                        | _ -> ()

                    candidate <- candidate + 1

                if chosen < 0 then
                    failwith
                        $"CliValueType.ComputeAutoLayoutFields: back-filling wanted a field of size class %d{i}, of which %d{slotsInClass.[i]} are recorded as unplaced, but none was found among the %d{classified.Length} fields"

                cursor <- roundUpToAlignment (1 <<< i) cursor
                backFilled.[chosen] <- Some cursor
                cursor <- cursor + (1 <<< i)
                slotsInClass.[i] <- slotsInClass.[i] - 1
                i <- i + 1

        // Each size class gets one contiguous region, largest class first.
        let nextInClass = Array.zeroCreate<int> (LOG2_NATIVE_INT_SIZE + 1)

        for log2Size in LOG2_NATIVE_INT_SIZE .. -1 .. 0 do
            if slotsInClass.[log2Size] > 0 then
                let slotSize = 1 <<< log2Size
                let start = roundUpToAlignment slotSize cursor
                nextInClass.[log2Size] <- start
                cursor <- start + (slotsInClass.[log2Size] * slotSize)

        // Object references take the front of the pointer-sized region; any non-GC pointer-sized
        // primitives follow them.
        let mutable nextObjectReference = nextInClass.[LOG2_NATIVE_INT_SIZE]

        nextInClass.[LOG2_NATIVE_INT_SIZE] <-
            nextInClass.[LOG2_NATIVE_INT_SIZE] + (objectReferenceCount * NATIVE_INT_SIZE)

        // Value-class fields follow every bucket, in declaration order.
        let mutable nextValueClass = cursor

        let placed =
            classified
            |> Array.mapi (fun index (field, cls) ->
                let size = CliType.SizeOf field.Contents

                let offset =
                    match backFilled.[index] with
                    | Some offset -> offset
                    | None ->

                    match cls with
                    | AutoLayoutFieldClass.Primitive (_, true) ->
                        let offset = nextObjectReference
                        nextObjectReference <- offset + NATIVE_INT_SIZE
                        offset
                    | AutoLayoutFieldClass.Primitive (log2Size, false) ->
                        let offset = nextInClass.[log2Size]
                        nextInClass.[log2Size] <- offset + (1 <<< log2Size)
                        offset
                    | AutoLayoutFieldClass.ValueClass ->
                    // A value class that itself contains GC references is placed at pointer
                    // alignment rather than at its own, possibly wider, alignment
                    // (methodtablebuilder.cpp:8523). That is what keeps a struct holding both a
                    // reference and an `Int128` on an 8-byte rather than 16-byte boundary inside
                    // its enclosing type.
                    //
                    // `Int128` demands 16 (`DeclaredTypeFacts.nominalAlignment`), so a struct
                    // holding both a reference and an `Int128` would land at 16 rather than 8 if
                    // this inherited the field's own alignment. `GcWideOuter` in
                    // `sourcesPure/StructLayoutInt128Alignment.cs` is 40 bytes on real .NET
                    // precisely because of this arm, and would be 48 without it.
                    let alignment =
                        if CliType.ContainsObjectReferences field.Contents then
                            NATIVE_INT_SIZE
                        else
                            size.Alignment

                    let offset = roundUpToAlignment alignment nextValueClass
                    nextValueClass <- offset + size.Size
                    offset

                {
                    Id = field.Id
                    Name = field.Name
                    Contents = field.Contents
                    Offset = offset
                    Size = size.Size
                    Alignment = size.Alignment
                    ConfiguredOffset = field.Offset
                    EditedAtTime = 0UL
                    Type = field.Type
                    MarshallingDescriptor = field.MarshallingDescriptor
                }
            )

        // `dwNumInstanceFieldBytes` before any value-class rounding (methodtablebuilder.cpp:8566):
        // the cursor after the last value class, or after the last primitive region if there are
        // none. Back-filled fields sit in a gap below it and so never extend it. Returned rather
        // than recomputed from the placements because a level that declares no fields of its own
        // must report its parent's size, and `max (offset + size)` over an empty list is 0.
        placed |> List.ofArray, nextValueClass

    /// True when these fields are an `[InlineArray(N)]` type's storage slots rather than a
    /// plain field list: `InlineArrayStorage.expand` mints `FieldId.InlineArrayElement` for
    /// every slot from 1 upwards, so the marker is present exactly when `N >= 2`.
    ///
    /// `N = 1` is *not* an expansion: the list is then the single declared field,
    /// unchanged, and the ordinary one-field layout is already what CoreCLR computes for it.
    static member private IsInlineArrayExpansion (fieldIds : FieldId seq) : bool =
        fieldIds
        |> Seq.exists (fun id ->
            match id with
            | FieldId.InlineArrayElement _ -> true
            | FieldId.Metadata _
            | FieldId.Named _ -> false
        )

    /// The size and alignment CoreCLR gives an `[InlineArray(N)]` type, from the layout its one
    /// element received.
    ///
    /// CoreCLR never lays the slots out together. It lays out the single declared field, sizes
    /// *that* completely — the value-class rounding at the tail of `HandleAutoLayout` on the auto
    /// route (methodtablebuilder.cpp:8574-8607), `AlignSize` on the sequential one
    /// (classlayoutinfo.cpp) — and only then multiplies by N (`PlaceInstanceFields` :8612 and
    /// `HandleSequentialLayout` :8663). The size is therefore N copies of the *rounded* element,
    /// which differs from rounding the run once exactly when an element's size is not already a
    /// multiple of its alignment: three 3-byte elements are 12 bytes on the auto route rather than
    /// 9, and three of a `[StructLayout(Sequential, Size = 5)]` element are 24 rather than 15.
    ///
    /// Note this is the *size* only. Where the elements sit is a separate question with a different
    /// answer — `sizeof(element)` apart, so the last element can end well short of the size; see
    /// the striding in `LayoutLevel`.
    ///
    /// The alignment is the subtler half, and only the auto route has anything to decide. A
    /// sequential type `HasLayout()`, so `MethodTable::GetFieldAlignmentRequirement`
    /// (methodtable.cpp:8853) answers from its `EEClassLayoutInfo` — derived from the one field,
    /// and untouched by the multiplication. An auto type has no layout metadata, so the same
    /// function answers from the *class*: the custom alignment if one was recorded, and otherwise
    /// `min(GetNumInstanceFieldBytes(), TARGET_POINTER_SIZE)`. Both halves of that matter
    /// here, because the recording test is `minAlign != min(elementSize, TARGET_POINTER_SIZE)`
    /// (:8598) and runs *before* the multiplication while the fallback reads the size *after* it:
    ///
    /// * `[Auto, InlineArray(3)] struct { S3 }` — element 3 bytes, `minAlign` 4, so 4 != 3 and the
    ///   custom alignment is recorded. The type is 4-aligned.
    /// * `[Auto, InlineArray(3)] struct { int }` — element 4 bytes, `minAlign` 4, so nothing is
    ///   recorded and the answer is `min(12, 8)` = 8. The type is 8-aligned despite every element
    ///   in it being 4-aligned, which moves it inside a containing struct.
    ///
    /// The fallback is not always a power of two: `[Auto, InlineArray(3)] struct { byte }` reports
    /// 3, and `{ short }` reports 6. Those really are the numbers real .NET computes — it then
    /// fails to *use* such a type, with `InvalidProgramException` from the JIT — so they are
    /// modelled rather than rounded away, and `roundUpToAlignment` is modular arithmetic and copes.
    /// PawPrint has no JIT to refuse them, so a guest that declares one gets the layout CoreCLR
    /// computed instead of the crash CoreCLR would produce; see `docs/divergences.md`.
    static member private InlineArraySize
        (governedByAuto : bool)
        (elementUnrounded : int)
        (elementSize : int)
        (elementAlignment : int)
        (repeat : int)
        : int * int
        =
        let total = elementSize * repeat

        let alignment =
            if not governedByAuto then
                elementAlignment
            elif elementAlignment <> min elementUnrounded NATIVE_INT_SIZE then
                elementAlignment
            else
                min total NATIVE_INT_SIZE

        total, alignment

    /// Whether CoreCLR's auto-layout algorithm governs a type, given the layout kind PawPrint
    /// applies to it (`TypeLayoutKind.applied`) and three facts about its fields.
    ///
    /// Two routes reach auto layout (`MethodTableBuilder::PlaceInstanceFields`,
    /// methodtablebuilder.cpp:8212): the type declares it, or the type declares `Sequential` and
    /// holds GC references, which promotes it. Explicit layout is never promoted.
    ///
    /// Explicit layout is recognised from the fields rather than from the declared kind, and that
    /// is not a shortcut: a *reference* type's field list here is its whole base chain flattened
    /// (issue #994), so it routinely mixes fields governed by different kinds. An explicit-layout
    /// class with no instance fields of its own presents only its sequential base's offset-free
    /// fields, and a sequential class deriving from an explicit-layout base presents only
    /// offset-carrying ones — both load on real .NET (`LayoutKindAcrossInheritance.cs`). So "the
    /// declared kind and the field shape agree" is false as a property of this list, and
    /// the kind matters only for the choice this function exists to make: among fields
    /// that carry no offsets, auto placement or sequential.
    ///
    /// An `[InlineArray(N)]` type is routed by this function like any other, because CoreCLR routes
    /// it like any other: the repeat count is applied to the size the chosen route computes for the
    /// type's *one* declared field (`CliValueType.InlineArraySize`), not to the choice of route.
    ///
    /// `containsReferences` is the *chain's* answer, not the level's: CoreCLR's `hasGCFields` is
    /// `(pParentMT && pParentMT->ContainsGCPointers()) || <own fields hold one>`
    /// (methodtablebuilder.cpp:8179), so a reference anywhere below promotes this level too.
    /// `parentIsNonSequential` is the other half of the same rule and has no counterpart in the
    /// fields at all: a `Sequential` type whose parent is not managed-sequential is promoted
    /// however blittable it is (:8213). `[Explicit] XB` -> `[Sequential] XD` is the shape, and it
    /// is the only route into that arm with no GC reference in sight.
    static member private AutoLayoutGoverns
        (layoutKind : TypeLayoutKind)
        (hasFieldOffsets : bool)
        (containsReferences : bool)
        (parentIsNonSequential : bool)
        : bool
        =
        if hasFieldOffsets then
            false
        else

        match layoutKind with
        | TypeLayoutKind.Auto -> true
        // A declared-`Explicit` type whose fields carry no offsets is the inheritance shape above;
        // it keeps the promotion rule.
        | TypeLayoutKind.Sequential -> containsReferences || parentIsNonSequential
        | TypeLayoutKind.Explicit -> containsReferences

    /// Place one level of a base chain, starting at the parent's instance size, and report what
    /// the *next* level down needs to know.
    ///
    /// This is the whole of the base-chain fix: layout is per-declaring-type, so an inherited
    /// field's offset depends on the type that declares it and on the chain below it, never on
    /// what derives from it (issue #994).
    static member private LayoutLevel
        (parent : ParentLayout option)
        (level : TypeLayoutLevel)
        : CliConcreteField list * ParentLayout
        =
        let layoutKind = level.Facts.LayoutKind
        let layout = level.Facts.Layout
        let fields = level.OwnFields

        let minimumSize, packingSize =
            match layout with
            | Layout.Custom (size = size ; packingSize = packing) ->
                size, if packing = 0 then DEFAULT_PACKING_SIZE else packing
            | Layout.Default -> 0, DEFAULT_PACKING_SIZE

        // Where this level's own fields begin. A parent that was bumped from a computed size of 0
        // up to 1 contributes 0 instead, "but ONLY for inheritance situations"
        // (`TryGetParentLayoutInfo`, classlayoutinfo.cpp:20-45).
        let startOffset =
            match parent with
            | None -> 0
            | Some parent -> if parent.IsZeroSized then 0 else parent.InstanceSize

        let parentReferences =
            match parent with
            | None -> false
            | Some parent -> parent.ContainsReferences

        let parentIsNonSequential =
            match parent with
            | None -> false
            | Some parent -> not parent.IsManagedSequential

        // `TryGetParentLayoutInfo` (classlayoutinfo.cpp:20-45) hands the parent's alignment on only
        // when the parent `IsManagedSequential() || (HasExplicitFieldOffsetLayout() &&
        // IsBlittable())`. "Has layout metadata" is *not* the test: a type that declared
        // `Sequential` but was promoted to auto layout for holding references keeps its layout
        // metadata and still contributes nothing, because auto layout gave it no alignment
        // requirement to give.
        //
        // The blittable-explicit half is not modelled -- PawPrint has no blittability notion -- and
        // is unreachable from here anyway: a sequential or explicit child of an explicit parent is
        // promoted to auto (the parent is not managed-sequential), and auto layout never reads
        // this.
        //
        // As with `containsReferences` above, no test tells `IsManagedSequential` here apart from
        // "has layout metadata", and again it is the rule's shape rather than the corpus: reaching
        // the sequential path from a declared-`Sequential` level *requires* the parent to be
        // managed-sequential, so the two agree wherever this is read. They diverge only for a
        // declared-`Explicit` level over a promoted parent, which needs a three-level chain and a
        // measurement this branch does not have. `IsManagedSequential` is what CoreCLR tests.
        let parentAlignment =
            match parent with
            | None -> 0
            | Some parent ->
                if parent.IsManagedSequential then
                    parent.AlignmentRequirement
                else
                    0

        let seqFields, nonSeqFields =
            fields |> List.partition (fun field -> field.Offset.IsNone)

        // A declared-`Auto` type may not carry `FieldOffset` rows: the router below reads the
        // offsets structurally, so such a type would silently get explicit layout instead of the
        // auto layout its caller asked for. This list is one type's *own* fields, so there is no
        // way for an inherited field to make the two disagree innocently: it
        // means malformed metadata (Roslyn rejects `FieldOffset` outside an explicit-layout type)
        // or a synthetic construction site contradicting itself.
        match nonSeqFields with
        | _ :: _ when layoutKind = TypeLayoutKind.Auto ->
            failwith
                $"CliValueType.LayoutLevel: type declares LayoutKind.Auto but %d{nonSeqFields.Length} of its %d{fields.Length} fields carry a FieldOffset (first: %O{nonSeqFields.Head.Id})"
        | _ -> ()

        // `hasGCFields = (pParentMT && pParentMT->ContainsGCPointers()) || <own>`
        // (methodtablebuilder.cpp:8179).
        //
        // No test kills the `parentReferences` disjunct, and that is a property of the rule rather
        // than a gap in the corpus. For a declared-`Sequential` level it is strictly redundant: a
        // parent holding references was itself promoted to auto layout, so it is not
        // managed-sequential, so `parentIsNonSequential` below already fires. For a
        // declared-`Explicit` level it does decide the route (`ExpD` in `TestBaseChainLayout`
        // takes the auto path because of it) -- but the sequential fallback happens to compute the
        // same size there, because such a parent contributes no alignment either. We follow the
        // rule, not the coincidence.
        let containsReferences =
            parentReferences
            || (fields
                |> List.exists (fun field -> CliType.ContainsObjectReferences field.Contents))

        let governedByAuto =
            CliValueType.AutoLayoutGoverns
                layoutKind
                (not nonSeqFields.IsEmpty)
                containsReferences
                parentIsNonSequential

        // An `[InlineArray(N)]` type's storage slots are N copies of its one declared field
        // (`InlineArrayStorage.expand`), and CoreCLR lays out only the first of them: see
        // `CliValueType.InlineArraySize`. So the fields placed below are the element alone, and the
        // slots are struck from its placement afterwards.
        //
        // The repeat count is read back off the field list rather than carried alongside it. The
        // slots *are* the count, so a second copy of N would be a second thing to keep in step with
        // the first -- and every route into this function, including the eval-stack rewrap in
        // `OfFieldsLike`, would have to carry it.
        let inlineArraySlots : CliField list option =
            if CliValueType.IsInlineArrayExpansion (seqFields |> Seq.map _.Id) then
                Some seqFields
            else
                None

        match inlineArraySlots with
        | None -> ()
        | Some slots ->
            // Everything below assumes the slots are interchangeable copies of one field, which is
            // what `expand` builds and what CoreCLR's single `FieldDesc` means. Same concrete type
            // means same size, which is all the striding needs.
            let element = List.head slots

            match slots |> List.tryFind (fun slot -> slot.Type <> element.Type) with
            | Some odd ->
                failwith
                    $"CliValueType.LayoutLevel: %O{level.Declared} carries %d{slots.Length} inline-array storage slots, but slot %s{odd.Name} has concrete type %O{odd.Type} where the first slot %s{element.Name} has %O{element.Type}; the slots of an inline array are copies of one declared field"
            | None -> ()

            // A value type has no non-trivial parent, and `expand` refuses a declared `ClassSize`
            // (CoreCLR's `IDS_CLASSLOAD_INLINE_ARRAY_EXPLICIT_SIZE`). Both would otherwise silently
            // change what the multiplication is applied to.
            if startOffset <> 0 then
                failwith
                    $"CliValueType.LayoutLevel: %O{level.Declared} carries inline-array storage slots but inherits %d{startOffset} bytes from a base chain; CoreCLR reads `[InlineArray]` only inside the value-type branch of PlaceInstanceFields, so such a type has no non-trivial parent"

            if minimumSize > 0 then
                failwith
                    $"CliValueType.LayoutLevel: %O{level.Declared} carries inline-array storage slots and a declared ClassLayout.Size of %d{minimumSize}; CoreCLR refuses to load such a type (IDS_CLASSLOAD_INLINE_ARRAY_EXPLICIT_SIZE), so `InlineArrayStorage.expand` should already have rejected it"

        // The fields actually placed: an inline array's one element, or every field of an ordinary
        // level. Empty exactly when `seqFields` is, so it can carry the routing match below.
        let placementFields =
            match inlineArraySlots with
            | None -> seqFields
            | Some slots -> [ List.head slots ]

        let placed, unrounded =
            match placementFields, nonSeqFields with
            | _, [] when governedByAuto ->
                // Either the type declares `LayoutKind.Auto`, or it declares `Sequential` and is
                // promoted -- for holding GC references, or for having a parent that is not
                // managed-sequential. Declared field order and any `Pack`/`Size` request are both
                // discarded here; explicit layout is never promoted.
                CliValueType.ComputeAutoLayoutFields startOffset placementFields
            | _, [] ->
                // Sequential layout: declared order, each field at its own alignment capped by
                // `Pack`, continuing from where the parent's fields ended.
                let finalOffset, concreteFields =
                    ((startOffset, []), placementFields)
                    ||> List.fold (fun (currentOffset, acc) field ->
                        let size = CliType.SizeOf field.Contents
                        let alignmentCap = min size.Alignment packingSize
                        let error = currentOffset % alignmentCap

                        let alignedOffset =
                            if error > 0 then
                                currentOffset + (alignmentCap - error)
                            else
                                currentOffset

                        let concreteField =
                            {
                                Id = field.Id
                                Name = field.Name
                                Contents = field.Contents
                                Offset = alignedOffset
                                Size = size.Size
                                Alignment = size.Alignment
                                ConfiguredOffset = field.Offset
                                EditedAtTime = 0UL
                                Type = field.Type
                                MarshallingDescriptor = field.MarshallingDescriptor
                            }

                        alignedOffset + size.Size, concreteField :: acc
                    )

                List.rev concreteFields, finalOffset

            | [], _ :: _ ->
                // Explicit layout. A declared `FieldOffset` is relative to this type's own
                // *instance slice*, so with a non-trivial parent it is biased -- but not by the
                // parent's size, which is what the obvious reading of
                // `ReadOffsetsForExplicitLayout` (classlayoutinfo.cpp) would give. Measured on
                // real .NET, `[Explicit] class D : P { [FieldOffset(0)] int A; }` puts `A` at
                // *twice* the parent's instance size:
                //
                //     parent 4 bytes  -> A@8       parent 8 bytes  -> A@16
                //     parent 16 bytes -> A@32      no parent       -> A@0
                //
                // and a further level below sees twice that again. CoreCLR appears to apply the
                // bias twice: once in `ReadOffsetsForExplicitLayout`, which adds
                // `cbAdjustedParentLayoutSize` to each declared offset, and again in
                // `ValidateExplicitLayout`'s "fixup the offset to include parent as current
                // offsets are relative to instance slice" (methodtablebuilder.cpp:9053-9125),
                // by which point they are already absolute.
                //
                // That is very likely an upstream bug, and reproducing a suspected upstream bug
                // from four data points -- in a shape no guest has ever reached here -- would be
                // guessing. Refuse instead.
                if startOffset <> 0 then
                    failwith
                        $"CliValueType.LayoutLevel: refusing to lay out explicit-layout type %O{level.Declared}, which declares %d{nonSeqFields.Length} field offset(s) of its own and inherits %d{startOffset} bytes from its base chain. Real .NET biases such a type's declared offsets by *twice* the parent's instance size (measured: parent 4 -> first field at 8, parent 8 -> 16, parent 16 -> 32), which looks like a double-application of `cbAdjustedParentLayoutSize` between `ReadOffsetsForExplicitLayout` and `ValidateExplicitLayout`'s fixup (methodtablebuilder.cpp:9053-9125). PawPrint does not model that; see issue #994"

                let concreteFields =
                    nonSeqFields
                    |> List.map (fun field ->
                        let size = CliType.SizeOf field.Contents

                        {
                            Id = field.Id
                            Name = field.Name
                            Contents = field.Contents
                            Offset = field.Offset.Value
                            Size = size.Size
                            Alignment = size.Alignment
                            ConfiguredOffset = field.Offset
                            EditedAtTime = 0UL
                            Type = field.Type
                            MarshallingDescriptor = field.MarshallingDescriptor
                        }
                    )

                let finalOffset =
                    concreteFields
                    |> List.fold (fun maxEnd field -> max maxEnd (field.Offset + field.Size)) startOffset

                concreteFields, finalOffset

            | _ :: _, _ :: _ -> failwith "unexpectedly mixed explicit and automatic layout of fields"

        let size, alignment, isZeroSized =
            if governedByAuto then
                let size, alignment =
                    CliValueType.AutoLayoutSize level.Facts.IsValueType unrounded placed

                size, alignment, false
            else

            // `alignmentRequirement = max(max(1, min(packingSize, parentAlignmentRequirement)),
            // fieldsAlignmentRequirement)` (classlayoutinfo.cpp), shared by the sequential and
            // explicit paths.
            let alignment =
                placed
                |> List.fold (fun maxAlign field -> max maxAlign (min field.Alignment packingSize)) 1
                |> max (min packingSize parentAlignment)

            // A declared `ClassLayout.Size` and the alignment rounding are alternatives, and the
            // declared size is a floor *relative to the parent*:
            // `max(classSizeInMetadata + parentSize, lastFieldEnd)` (:326-341).
            let withFloor =
                if minimumSize > 0 then
                    max (minimumSize + startOffset) unrounded
                else
                    roundUpToAlignment alignment unrounded

            // "The GC requires that all valuetypes containing orefs be sized to a multiple of
            // TARGET_POINTER_SIZE" (`ValidateExplicitLayout`, methodtablebuilder.cpp:9104), which
            // recomputes the size after layout and so applies on top of the floor. Only explicit
            // layout reaches this holding references: a sequential type that does is promoted.
            let sized, alignment =
                if containsReferences then
                    roundUpToAlignment NATIVE_INT_SIZE withFloor, max alignment NATIVE_INT_SIZE
                else
                    withFloor, alignment

            // `SetInstanceBytesSize` (class.h:497) is `size == 0 ? 1 : size`, and a type that was
            // bumped this way is the `IsZeroSized` a derived type must discount.
            if sized = 0 then
                1, alignment, true
            else
                sized, alignment, false

        // Everything above sized the element. Strike the remaining slots off its placement and
        // multiply.
        let placed, size, alignment =
            match inlineArraySlots with
            | None -> placed, size, alignment
            | Some slots ->
                let element =
                    match placed with
                    | [ element ] -> element
                    | _ ->
                        failwith
                            $"CliValueType.LayoutLevel: laying out the single element of inline array %O{level.Declared} produced %d{placed.Length} placements"

                if element.Offset <> 0 then
                    failwith
                        $"CliValueType.LayoutLevel: the single element of inline array %O{level.Declared} was placed at offset %d{element.Offset} rather than 0"

                // `alignment` here is the *type's* -- `minAlign` on the auto route, the layout
                // info's on the sequential one -- not the element field's own demand.
                let total, alignment =
                    CliValueType.InlineArraySize governedByAuto unrounded size alignment slots.Length

                // The slots stride by the element's *own* size, not by the rounded one the total is
                // built from, and the two differ exactly where the rounding bites. Nothing lays
                // these out: CoreCLR has a single `FieldDesc` at offset 0, and an element access is
                // `InlineArrayElementRef` -> `Unsafe.Add(ref Unsafe.As<TBuffer, TElement>(ref
                // buffer), index)`, which is `sizeof(TElement)` arithmetic that never consults the
                // aggregate. So `[Auto, InlineArray(3)] struct { S3 }` really is 12 bytes with its
                // three 3-byte elements at 0, 3 and 6 and three bytes of slack after them --
                // measured on real .NET, and inconsistent-looking upstream rather than here.
                let placed =
                    slots
                    |> List.mapi (fun index slot ->
                        { element with
                            Id = slot.Id
                            Name = slot.Name
                            Offset = index * element.Size
                        }
                    )

                placed, total, alignment

        let next =
            {
                InstanceSize = size
                // `IsManagedSequential` is the kind after promotion, not the declared one.
                IsManagedSequential = layoutKind = TypeLayoutKind.Sequential && not governedByAuto
                AlignmentRequirement = alignment
                IsZeroSized = isZeroSized
                ContainsReferences = containsReferences
            }

        placed, next

    /// The size and alignment CoreCLR's auto layout gives a value class, from the tail of
    /// `MethodTableBuilder::HandleAutoLayout` (methodtablebuilder.cpp:8500-8605).
    ///
    /// `Pack` and `Size` are not parameters because auto layout reads neither.
    ///
    /// <code>
    /// largestAlignmentRequirement = max over fields of
    ///     value class holding references -> TARGET_POINTER_SIZE   (:8523)
    ///     value class otherwise          -> its own alignment     (:8532)
    ///     anything else                  -> TARGET_POINTER_SIZE   (:8554, "non-value-type
    ///                                                              fields always require
    ///                                                              pointer alignment")
    /// size = max 1 (end of the last field)                        (:8572)
    /// minAlign = if size > POINTER then (holdsReferences ? POINTER : largestAlignmentRequirement)
    ///            else the smallest power of two >= size           (:8586)
    /// size = roundUp minAlign size                                (:8602)
    /// </code>
    ///
    /// The reported alignment is `minAlign`. `MethodTable::GetFieldAlignmentRequirement`
    /// (methodtable.cpp:8853) reads the recorded custom alignment when there is one and otherwise
    /// `min(size, POINTER)` — but the custom one is recorded exactly when those two differ
    /// (:8596), so `minAlign` is the answer either way. That first branch of
    /// `GetFieldAlignmentRequirement` — `HasLayout()` — never applies here: a type reaching auto
    /// layout by declaring it has no layout metadata at all (`HasLayoutMetadata` is false for
    /// `IsTdAutoLayout`, methodtablebuilder.cpp:12546), and one reaching it by GC promotion is
    /// sized by this same rule.
    ///
    /// The `largestAlignmentRequirement` value-class arm is the reason a type of all-value-class
    /// fields can stay narrowly aligned: `[Auto] struct { S3 x, y, z; }` over a 3-byte sequential
    /// `S3` is 9 bytes with alignment 1 on real .NET, where the same shape with a single `byte`
    /// field in it would be 16 with alignment 8.
    ///
    /// All of that rounding is inside `if (IsValueClass())` (:8574-8607), so a *reference* type's
    /// instance size is the raw cursor: `class B { byte A; } class M : B { int I; } class D : M {
    /// object O; byte Z; }` really is 17 bytes, and `M` really is 8 rather than 16. That is not a
    /// curiosity -- it is the number a derived type starts its own fields at, so rounding it would
    /// move every inherited field in the chain.
    static member private AutoLayoutSize
        (isValueType : bool)
        (unrounded : int)
        (fields : CliConcreteField list)
        : int * int
        =
        let largestAlignmentRequirement =
            (1, fields)
            ||> List.fold (fun acc field ->
                let required =
                    match CliValueType.ClassifyForAutoLayout field.Contents with
                    | AutoLayoutFieldClass.ValueClass ->
                        if CliType.ContainsObjectReferences field.Contents then
                            NATIVE_INT_SIZE
                        else
                            field.Alignment
                    | AutoLayoutFieldClass.Primitive _ -> NATIVE_INT_SIZE

                max acc required
            )

        let holdsReferences =
            fields
            |> List.exists (fun field -> CliType.ContainsObjectReferences field.Contents)

        if not isValueType then
            unrounded, max 1 largestAlignmentRequirement
        else

        // "Like C++ we enforce that there can be no 0 length structures. Thus for a value class
        // with no fields, we 'pad' the length to be 1" (methodtablebuilder.cpp:8572).
        let unrounded = max 1 unrounded

        let minAlign =
            if unrounded > NATIVE_INT_SIZE then
                if holdsReferences then
                    NATIVE_INT_SIZE
                else
                    largestAlignmentRequirement
            else
                // "if the size is smaller than void* round it up to next power of two"
                let mutable candidate = 1

                while candidate < unrounded do
                    candidate <- candidate * 2

                candidate

        roundUpToAlignment minAlign unrounded, minAlign

    /// The storage for one type's laid-out fields, with the instance size the layout produced.
    ///
    /// The size is passed in rather than recomputed because it is not recoverable from the
    /// placements: a level's declared `ClassLayout.Size` is a floor relative to its parent, so
    /// `[Sequential] PB { long }` -> `[Sequential, Size = 12] PD { int }` makes `PD` 20 bytes
    /// while its fields end at 12. Anything derived from `PD` starts at 20, so the discrepancy is
    /// observable rather than academic.
    static member private StorageFromFields
        (layoutKind : TypeLayoutKind)
        (layout : Layout)
        (size : int)
        (fields : CliConcreteField list)
        : CliValueTypeStorage
        =
        match fields, layoutKind, layout with
        // A fieldless type whose declared `Size` is read gets that many bytes of storage and no
        // field cells. Auto layout does not read `Size`, so such a type is one byte and takes the
        // ordinary field-backed path rather than this one.
        | [], (TypeLayoutKind.Sequential | TypeLayoutKind.Explicit), Layout.Custom (size = declared) when declared > 0 ->
            CliValueTypeStorage.RawBytes (Array.zeroCreate<byte> size)
        | _ ->
            CliValueTypeStorage.Fields
                {
                    Fields = fields
                    PreservedBytes = Array.zeroCreate<byte> size
                }

    static member private FieldStorage (operation : string) (cvt : CliValueType) : CliConcreteField list =
        match cvt._Storage with
        | CliValueTypeStorage.Fields storage -> storage.Fields
        | CliValueTypeStorage.RawBytes bytes ->
            failwith
                $"%s{operation}: raw-backed fieldless value type %O{cvt._Declared} has no fields (%d{bytes.Length} raw bytes)"

    static member private FieldBackedStorage (operation : string) (cvt : CliValueType) : CliFieldBackedStorage =
        match cvt._Storage with
        | CliValueTypeStorage.Fields storage -> storage
        | CliValueTypeStorage.RawBytes bytes ->
            failwith
                $"%s{operation}: raw-backed fieldless value type %O{cvt._Declared} has no fields (%d{bytes.Length} raw bytes)"

    static member private DescribeHandle
        (concreteTypes : AllConcreteTypes option)
        (handle : ConcreteTypeHandle)
        : string
        =
        match
            concreteTypes
            |> Option.bind (fun concreteTypes -> AllConcreteTypes.lookup handle concreteTypes)
        with
        | Some concreteType -> $"%O{concreteType}"
        | None -> $"%O{handle}"

    static member private HexBytes (bytes : byte[]) (start : int) (endExclusive : int) : string =
        if endExclusive <= start then
            "<empty>"
        else
            bytes.[start .. endExclusive - 1]
            |> Array.map (fun b -> b.ToString "X2")
            |> String.concat " "

    static member private UnrepresentedRanges (size : int) (fields : CliConcreteField list) : (int * int) list =
        let represented = Array.zeroCreate<bool> size

        for field in fields do
            let start = max 0 field.Offset
            let endExclusive = min size (field.Offset + field.Size)

            if start < endExclusive then
                for i = start to endExclusive - 1 do
                    represented.[i] <- true

        let rec loop (offset : int) (acc : (int * int) list) : (int * int) list =
            if offset >= size then
                List.rev acc
            elif represented.[offset] then
                loop (offset + 1) acc
            else
                let mutable endExclusive = offset + 1

                while endExclusive < size && not represented.[endExclusive] do
                    endExclusive <- endExclusive + 1

                loop endExclusive ((offset, endExclusive) :: acc)

        loop 0 []

    static member private DescribeUnrepresentedRanges (bytes : byte[]) (fields : CliConcreteField list) : string list =
        match CliValueType.UnrepresentedRanges bytes.Length fields with
        | [] -> [ "unrepresented byte ranges: none" ]
        | ranges ->
            "unrepresented byte ranges:"
            :: (ranges
                |> List.map (fun (start, endExclusive) ->
                    let rangeBytes = CliValueType.HexBytes bytes start endExclusive
                    $"  - [%i{start}, %i{endExclusive}): %s{rangeBytes}"
                ))

    static member DescribeByteLayout (concreteTypes : AllConcreteTypes option) (cvt : CliValueType) : string =
        let declared = CliValueType.DescribeHandle concreteTypes cvt._Declared
        let size = CliValueType.SizeOf cvt
        let byteAddressability = CliValueType.ByteAddressability cvt

        match cvt._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            [
                "value type byte layout:"
                $"declared type: %s{declared}"
                "storage: raw bytes"
                $"size: %i{size.Size} bytes (alignment %i{size.Alignment})"
                $"byte-addressability: %s{byteAddressability.Description}"
                "fields: none"
                "unrepresented byte ranges:"
                $"  - [0, %i{bytes.Length}): %s{CliValueType.HexBytes bytes 0 bytes.Length}"
            ]
            |> String.concat "\n"
        | CliValueTypeStorage.Fields storage ->
            let fieldLines =
                match storage.Fields with
                | [] -> [ "fields: none" ]
                | fields ->
                    "fields:"
                    :: (fields
                        |> List.map (fun field ->
                            let fieldType = CliValueType.DescribeHandle concreteTypes field.Type
                            let endExclusive = field.Offset + field.Size

                            $"  - %s{field.Name}: range=[%i{field.Offset}, %i{endExclusive}), size=%i{field.Size}, type=%s{fieldType}, editedAt=%i{field.EditedAtTime}, value=%O{field.Contents}"
                        ))

            [
                "value type byte layout:"
                $"declared type: %s{declared}"
                "storage: field-backed"
                $"size: %i{size.Size} bytes (alignment %i{size.Alignment})"
                $"preserved byte image: %i{storage.PreservedBytes.Length} bytes"
                $"byte-addressability: %s{byteAddressability.Description}"
            ]
            @ fieldLines
            @ CliValueType.DescribeUnrepresentedRanges storage.PreservedBytes storage.Fields
            |> String.concat "\n"

    static member ToBytes (cvt : CliValueType) : byte[] =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes bytes -> Array.copy bytes
        | CliValueTypeStorage.Fields storage ->
            let expectedSize = CliValueType.SizeOf(cvt).Size

            if storage.PreservedBytes.Length <> expectedSize then
                failwith
                    $"CliValueType.ToBytes: preserved byte image length %i{storage.PreservedBytes.Length} does not match value type size %i{expectedSize} for %O{cvt._Declared}"

            let bytes = Array.copy storage.PreservedBytes

            storage.Fields
            |> List.sortBy _.EditedAtTime
            |> List.iter (fun candidateField ->
                let fieldBytes : byte[] = CliType.ToBytes candidateField.Contents

                for i = 0 to candidateField.Size - 1 do
                    bytes.[candidateField.Offset + i] <- fieldBytes.[i]
            )

            bytes

    static member private CheckByteRange
        (operation : string)
        (offset : int)
        (count : int)
        (length : int)
        (declared : ConcreteTypeHandle)
        : unit
        =
        if offset < 0 then
            failwith $"%s{operation}: byte offset %i{offset} is negative for %O{declared}"

        if count < 0 then
            failwith $"%s{operation}: byte count %i{count} is negative for %O{declared}"

        if count > length - offset then
            let start = int64 offset
            let endExclusive = start + int64 count

            failwith
                $"%s{operation}: byte range [%d{start}, %d{endExclusive}) exceeds %i{length}-byte value type %O{declared}"

    /// The materialised bytes of `[offset, offset + count)`.
    ///
    /// Defined even when the whole value has no byte rendering — `CliType.ToBytes` refuses to
    /// express an object reference or a provenance-carrying native int — provided no such field
    /// overlaps the requested range. Agrees byte for byte with `ToBytes` wherever `ToBytes`
    /// succeeds.
    static member BytesAt (offset : int) (count : int) (cvt : CliValueType) : byte[] =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            CliValueType.CheckByteRange "CliValueType.BytesAt" offset count bytes.Length cvt._Declared

            let result : byte[] = Array.zeroCreate count
            Array.blit bytes offset result 0 count
            result
        | CliValueTypeStorage.Fields storage ->
            let expectedSize = CliValueType.SizeOf(cvt).Size

            if storage.PreservedBytes.Length <> expectedSize then
                failwith
                    $"CliValueType.BytesAt: preserved byte image length %i{storage.PreservedBytes.Length} does not match value type size %i{expectedSize} for %O{cvt._Declared}"

            CliValueType.CheckByteRange "CliValueType.BytesAt" offset count expectedSize cvt._Declared

            let endExclusive = offset + count

            let result : byte[] = Array.zeroCreate count
            Array.blit storage.PreservedBytes offset result 0 count

            // Only fields that overlap the requested range are serialised. A disjoint field
            // cannot affect these bytes by construction, and may have no byte rendering at all,
            // so rendering the whole value first would make a perfectly answerable slice fail
            // because of a field it does not cover. Overlapping fields are replayed in the same
            // `EditedAtTime` order `ToBytes` uses, so the two agree byte for byte wherever
            // `ToBytes` succeeds.
            storage.Fields
            |> List.filter (fun f -> f.Offset < endExclusive && offset < f.Offset + f.Size)
            |> List.sortBy _.EditedAtTime
            |> List.iter (fun candidateField ->
                let fieldBytes : byte[] = CliType.ToBytes candidateField.Contents

                // A field may straddle either end of the slice; copy only the part inside it.
                for i = max candidateField.Offset offset to (min
                                                                (candidateField.Offset + candidateField.Size)
                                                                endExclusive)
                                                            - 1 do
                    result.[i - offset] <- fieldBytes.[i - candidateField.Offset]
            )

            result

    /// The counterpart of <see cref="BytesAt" /> for a value whose bytes are only nameable: a
    /// byte covered by a native int PawPrint models as an identity comes back naming that native
    /// int and the byte's position within it, and every other byte comes back verbatim.
    ///
    /// Agrees with `BytesAt` byte for byte wherever `BytesAt` succeeds.
    static member SymbolicBytesAt (offset : int) (count : int) (cvt : CliValueType) : UInt8Source[] =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes _ -> CliValueType.BytesAt offset count cvt |> Array.map UInt8Source.Verbatim
        | CliValueTypeStorage.Fields storage ->
            let expectedSize = CliValueType.SizeOf(cvt).Size

            if storage.PreservedBytes.Length <> expectedSize then
                failwith
                    $"CliValueType.SymbolicBytesAt: preserved byte image length %i{storage.PreservedBytes.Length} does not match value type size %i{expectedSize} for %O{cvt._Declared}"

            CliValueType.CheckByteRange "CliValueType.SymbolicBytesAt" offset count expectedSize cvt._Declared

            let endExclusive = offset + count

            // Start from the preserved image, exactly as `BytesAt` does: bytes no field covers
            // are ordinary padding and have numbers.
            let result : UInt8Source[] =
                Array.init count (fun i -> UInt8Source.Verbatim storage.PreservedBytes.[offset + i])

            // Same filter and same `EditedAtTime` replay order as `BytesAt`, so the two agree
            // wherever both answer. The only difference is per-field: a field with no byte image
            // of its own contributes named bytes instead of failing the whole slice.
            storage.Fields
            |> List.filter (fun f -> f.Offset < endExclusive && offset < f.Offset + f.Size)
            |> List.sortBy _.EditedAtTime
            |> List.iter (fun candidateField ->
                let fieldBytes : UInt8Source[] =
                    CliType.SymbolicBytesAt 0 candidateField.Size candidateField.Contents

                // A field may straddle either end of the slice; copy only the part inside it.
                for i = max candidateField.Offset offset to (min
                                                                (candidateField.Offset + candidateField.Size)
                                                                endExclusive)
                                                            - 1 do
                    result.[i - offset] <- fieldBytes.[i - candidateField.Offset]
            )

            result

    /// Field-wise counterpart of `CliType.ZeroingChangedAnything`, for aggregates that have no
    /// byte rendering of their own. Any structural difference other than in the field values
    /// (different declared type, different storage form, different field set) counts as a
    /// change; matching fields are compared by the same rule, so a `-0.0` buried inside an
    /// otherwise unrenderable struct is still seen as differing from `0.0`.
    static member internal ZeroingChangedAnything (before : CliValueType) (after : CliValueType) : bool =
        if before._Declared <> after._Declared then
            true
        else

        match before._Storage, after._Storage with
        | CliValueTypeStorage.RawBytes b, CliValueTypeStorage.RawBytes a -> b <> a
        | CliValueTypeStorage.Fields b, CliValueTypeStorage.Fields a ->
            if b.Fields.Length <> a.Fields.Length || b.PreservedBytes <> a.PreservedBytes then
                true
            else
                List.exists2
                    (fun (x : CliConcreteField) (y : CliConcreteField) ->
                        x.Offset <> y.Offset
                        || x.Size <> y.Size
                        || not (FieldId.exactlyEqual x.Id y.Id)
                        || CliType.ZeroingChangedAnything x.Contents y.Contents
                    )
                    b.Fields
                    a.Fields
        | CliValueTypeStorage.RawBytes _, CliValueTypeStorage.Fields _
        | CliValueTypeStorage.Fields _, CliValueTypeStorage.RawBytes _ -> true

    /// Zero the byte range `[offset, offset + count)`. See `CliType.WithZeroedRangeIfChanged`.
    static member WithZeroedRangeIfChanged (offset : int) (count : int) (cvt : CliValueType) : CliValueType option =
        let size = CliValueType.SizeOf(cvt).Size

        // See `CliType.WithZeroedRangeIfChanged`: phrased to avoid overflowing `offset + count`.
        if offset < 0 || count < 0 || offset > size || count > size - offset then
            failwith
                $"CliValueType.WithZeroedRangeIfChanged: range of %d{count} byte(s) at offset %d{offset} is outside the %d{size}-byte value type %O{cvt._Declared}"

        let rangeEnd = offset + count

        match cvt._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            let updated = Array.copy bytes

            for i = offset to rangeEnd - 1 do
                updated.[i] <- 0uy

            if updated = bytes then
                None
            else
                Some
                    { cvt with
                        _Storage = CliValueTypeStorage.RawBytes updated
                    }
        | CliValueTypeStorage.Fields storage ->
            // Walk fields structurally rather than going through `ToBytes`, which is the whole
            // point: `ToBytes` materialises *every* field, so it cannot render a struct that
            // holds a live object reference, even when the requested range covers only plain
            // fields.
            let mutable changed = false

            let updatedFields =
                storage.Fields
                |> List.map (fun field ->
                    let fieldEnd = field.Offset + field.Size

                    if fieldEnd <= offset || field.Offset >= rangeEnd then
                        // Disjoint from the range.
                        field
                    else

                    let updatedContents =
                        if field.Offset >= offset && fieldEnd <= rangeEnd then
                            // Fully covered: zero it whatever its shape, which is what makes a
                            // reference field null without needing to identify it as one.
                            let zeroed = CliType.ZeroLike field.Contents

                            if CliType.ZeroingChangedAnything field.Contents zeroed then
                                Some zeroed
                            else
                                None
                        else
                            // Straddles a range boundary. Re-express the overlap in the field's
                            // own coordinates and let it decide whether it can absorb a partial
                            // zeroing: a nested value type recurses, a primitive takes a byte
                            // write, a reference fails loudly.
                            let localOffset = max 0 (offset - field.Offset)
                            let localEnd = min field.Size (rangeEnd - field.Offset)

                            CliType.WithZeroedRangeIfChanged localOffset (localEnd - localOffset) field.Contents

                    match updatedContents with
                    | None -> field
                    | Some contents ->
                        changed <- true

                        // `EditedAtTime` is left alone. `ToBytes` replays
                        // overlapping fields in timestamp order, so promoting a field to
                        // "newest" changes who wins on *every* byte it covers — including the
                        // bytes outside the requested range, when the field only partially
                        // overlaps that range. A nested 16-byte field aliased by a newer 8-byte
                        // field over its upper half is enough to show it: zeroing the lower
                        // half would otherwise let the nested field's stale upper half
                        // overwrite the alias, changing memory this call was never asked to
                        // touch.
                        //
                        // Keeping the original order is safe in both directions. Inside the
                        // range every intersecting field has had its covered bytes zeroed, so
                        // whichever wins writes zeros; outside it, the order is exactly what it
                        // was, so the bytes are exactly what they were.
                        { field with
                            Contents = contents
                        }
                )

            // The preserved image is a full-size copy of the whole byte image, not just the
            // unrepresented parts, so zeroing the covered range of it does more work than is
            // strictly needed: `ToBytes` overlays the fields back on top, making the
            // field-covered bytes redundant. It is the sole source of truth for *padding*
            // within the range, though, and real memory zeroing clears padding too — so
            // leaving it alone would report stale padding for a cleared range.
            let updatedPreserved = Array.copy storage.PreservedBytes

            for i = offset to rangeEnd - 1 do
                updatedPreserved.[i] <- 0uy

            let preservedChanged = updatedPreserved <> storage.PreservedBytes

            if not changed && not preservedChanged then
                None
            else
                Some
                    { cvt with
                        _Storage =
                            CliValueTypeStorage.Fields
                                {
                                    Fields = updatedFields
                                    PreservedBytes = updatedPreserved
                                }
                    }

    /// The single field of `cvt` whose laid-out extent contains `byteOffset`, when descending
    /// through it is well defined. `None` when the byte is padding of `cvt` itself (no field
    /// covers it), when explicit layout puts two or more fields over it (there is no single one to
    /// descend through — the same refusal `CellPathsExactlyCovering` makes for an aliased range),
    /// or when the field's laid-out extent disagrees with the size of what it now holds.
    ///
    /// That last case is not hypothetical: `WithFieldSetById` replaces `Contents` without
    /// recomputing `Size`, so a field can be left claiming an extent its contents do not fill.
    /// Such a value is already inconsistent — `CliValueType.ToBytes` overlays the field's own
    /// image across its full `Size` and runs off the end of it — so declining is the honest answer
    /// rather than describing bytes that are not there.
    static member private TryDescendableFieldAt
        (byteOffset : int)
        (fields : CliConcreteField list)
        : CliConcreteField option
        =
        match
            fields
            |> List.filter (fun f -> f.Offset <= byteOffset && byteOffset < f.Offset + f.Size)
        with
        | [ f ] when f.Size = CliType.SizeOf(f.Contents).Size -> Some f
        | _ -> None

    /// The maximal run of *padding* bytes containing `byteOffset` — bytes that no field covers, so
    /// no cell names them and the value's byte image is the only thing that holds them — as
    /// `(start, length)` in this value's own coordinates.
    ///
    /// See `CliType.TryPaddingRunAt` for what `None` means and why the notion is needed at all.
    static member TryPaddingRunAt (byteOffset : int) (cvt : CliValueType) : (int * int) option =
        let size = CliValueType.SizeOf(cvt).Size

        if byteOffset < 0 || byteOffset >= size then
            None
        else

        match cvt._Storage with
        // A raw-bytes value *is* its bytes; there is no field structure for anything to be filler
        // between, and the ordinary byte path already serves every offset of it.
        | CliValueTypeStorage.RawBytes _ -> None
        | CliValueTypeStorage.Fields storage ->
            match CliValueType.TryDescendableFieldAt byteOffset storage.Fields with
            | Some f ->
                match CliType.TryPaddingRunAt (byteOffset - f.Offset) f.Contents with
                | None -> None
                | Some (start, length) ->

                // The run is maximal within `f`, but `f` itself may be aliased: explicit layout can
                // put a sibling over part of `f`'s extent, and those bytes are the sibling's
                // content rather than `f`'s filler. `ToBytes` awards them to whichever field wrote
                // last, so answering about them out of `f`'s own preserved image would read and
                // write the wrong storage — a bulk copy would then leave the destination's sibling
                // untouched while believing it had moved those bytes.
                //
                // Clip to the stretch around `byteOffset` that no sibling covers. `byteOffset`
                // itself is never covered by one: `TryDescendableFieldAt` would have seen two
                // containing fields and declined.
                let siblingCovered (b : int) : bool =
                    storage.Fields
                    |> List.exists (fun g ->
                        not (FieldId.exactlyEqual g.Id f.Id) && g.Offset <= b && b < g.Offset + g.Size
                    )

                let runStart = f.Offset + start
                let runEnd = f.Offset + start + length

                let mutable clippedStart = byteOffset

                while clippedStart > runStart && not (siblingCovered (clippedStart - 1)) do
                    clippedStart <- clippedStart - 1

                let mutable clippedEnd = byteOffset + 1

                while clippedEnd < runEnd && not (siblingCovered clippedEnd) do
                    clippedEnd <- clippedEnd + 1

                Some (clippedStart, clippedEnd - clippedStart)
            | None ->

            // Either the byte is padding of this value, or containment was ambiguous. Only the
            // first is a padding run; re-ask whether *any* field covers the byte to tell them
            // apart, since `TryDescendableFieldAt` folds both into `None`.
            let covered (b : int) : bool =
                storage.Fields |> List.exists (fun f -> f.Offset <= b && b < f.Offset + f.Size)

            if covered byteOffset then
                // Some field's extent contains the byte, but no single field could be descended
                // through: two or more overlap it. That is an aliased byte, and refusing it is
                // right for aliased *data*.
                //
                // It is not obviously right for a byte that is padding within every field that
                // covers it — explicit layout overlaying two identical reference-containing
                // structs makes their trailing filler exactly that, and a bulk copy over such an
                // array would want to move it. No such copy can reach here today: a byte-backed
                // value holding references cannot be field-accessed at all, so building the array
                // fails long before the copy. `BulkMoveAcrossOverlappedStructPadding.cs` is parked
                // on that, and is where the question comes back if the representation changes.
                None
            else

            let mutable start = byteOffset

            while start > 0 && not (covered (start - 1)) do
                start <- start - 1

            let mutable endExclusive = byteOffset + 1

            while endExclusive < size && not (covered endExclusive) do
                endExclusive <- endExclusive + 1

            Some (start, endExclusive - start)

    /// The bytes of a range lying wholly inside one padding run. See `CliType.PaddingBytesAt`.
    static member PaddingBytesAt (offset : int) (count : int) (cvt : CliValueType) : byte[] =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes _ ->
            failwith
                $"CliValueType.PaddingBytesAt: raw-bytes-backed %O{cvt._Declared} has no padding; the byte path serves it directly"
        | CliValueTypeStorage.Fields storage ->
            match CliValueType.TryDescendableFieldAt offset storage.Fields with
            | Some f ->
                match f.Contents with
                | CliType.ValueType inner -> CliValueType.PaddingBytesAt (offset - f.Offset) count inner
                | other ->
                    failwith
                        $"CliValueType.PaddingBytesAt: byte offset %d{offset} of %O{cvt._Declared} lies inside field %O{f.Id}, which holds %O{other} rather than padding (this is an interpreter bug: the caller must validate with TryPaddingRunAt first)"
            | None ->
                // `TryPaddingRunAt` has already established that the whole range is uncovered
                // here, and `PreservedBytes` is a full-size image whose uncovered positions
                // `ToBytes` never overlays, so it is authoritative for exactly these bytes.
                let result : byte[] = Array.zeroCreate count
                Array.blit storage.PreservedBytes offset result 0 count
                result

    /// Replace the bytes of a range lying wholly inside one padding run.
    /// See `CliType.WithPaddingBytesAtIfChanged`.
    static member WithPaddingBytesAtIfChanged
        (offset : int)
        (bytes : byte[])
        (cvt : CliValueType)
        : CliValueType option
        =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes _ ->
            failwith
                $"CliValueType.WithPaddingBytesAtIfChanged: raw-bytes-backed %O{cvt._Declared} has no padding; the byte path serves it directly"
        | CliValueTypeStorage.Fields storage ->
            match CliValueType.TryDescendableFieldAt offset storage.Fields with
            | Some f ->
                match f.Contents with
                | CliType.ValueType inner ->
                    CliValueType.WithPaddingBytesAtIfChanged (offset - f.Offset) bytes inner
                    |> Option.map (fun updatedInner ->
                        let updatedFields =
                            storage.Fields
                            |> List.map (fun g ->
                                if FieldId.exactlyEqual g.Id f.Id then
                                    // `EditedAtTime` is left alone, for the same
                                    // reason `WithZeroedRangeIfChanged` leaves it alone: `ToBytes`
                                    // resolves overlapping fields in timestamp order, so promoting
                                    // this one to "newest" would change who wins on bytes outside
                                    // the range this call was asked to touch. Padding is covered by
                                    // no field, so no contest over it exists to be re-decided.
                                    { g with
                                        Contents = CliType.ValueType updatedInner
                                    }
                                else
                                    g
                            )

                        { cvt with
                            _Storage =
                                CliValueTypeStorage.Fields
                                    { storage with
                                        Fields = updatedFields
                                    }
                        }
                    )
                | other ->
                    failwith
                        $"CliValueType.WithPaddingBytesAtIfChanged: byte offset %d{offset} of %O{cvt._Declared} lies inside field %O{f.Id}, which holds %O{other} rather than padding (this is an interpreter bug: the caller must validate with TryPaddingRunAt first)"
            | None ->
                let updated = Array.copy storage.PreservedBytes
                Array.blit bytes 0 updated offset bytes.Length

                if updated = storage.PreservedBytes then
                    None
                else
                    Some
                        { cvt with
                            _Storage =
                                CliValueTypeStorage.Fields
                                    { storage with
                                        PreservedBytes = updated
                                    }
                        }

    /// Return a value with the requested byte range replaced, or `None` if the requested write
    /// would not change the materialised byte image. Returning `None` preserves field provenance
    /// and the next timestamp explicitly; changed writes use the existing value as the
    /// shape/provenance template and canonicalise overlapping-field replay order
    /// the same way `OfBytesLike` does.
    static member WithBytesAtIfChanged (offset : int) (bytes : byte[]) (cvt : CliValueType) : CliValueType option =
        let existing = CliValueType.ToBytes cvt

        CliValueType.CheckByteRange
            "CliValueType.WithBytesAtIfChanged"
            offset
            bytes.Length
            existing.Length
            cvt._Declared

        let mutable identical = true
        let mutable i = 0

        while identical && i < bytes.Length do
            identical <- existing.[offset + i] = bytes.[i]
            i <- i + 1

        if identical then
            None
        else
            Array.blit bytes 0 existing offset bytes.Length
            Some (CliValueType.OfBytesLike cvt existing)

    /// Return a value with the requested byte range replaced. If the requested write would not
    /// change the materialised byte image, this returns `cvt` unchanged.
    static member WithBytesAt (offset : int) (bytes : byte[]) (cvt : CliValueType) : CliValueType =
        match CliValueType.WithBytesAtIfChanged offset bytes cvt with
        | None -> cvt
        | Some updated -> updated

    /// Lay out a whole base chain, base first, and build the storage it produces.
    ///
    /// The last level must be the type being built; every level before it is an ancestor, with the
    /// fields *it* declares. `OfFields` is the one-level case.
    static member OfFieldChain
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (chain : TypeLayoutLevel list)
        : CliValueType
        =
        let outermost =
            match List.tryLast chain with
            | Some level -> level
            | None -> failwith $"CliValueType.OfFieldChain: empty base chain for %O{declared}"

        if outermost.Declared <> declared then
            failwith
                $"CliValueType.OfFieldChain: the outermost level of the chain is %O{outermost.Declared}, but the value being built is declared %O{declared}; the chain must end at the type it lays out"

        let facts = outermost.Facts

        // Fold from the base outwards. Each level is placed from the previous level's instance
        // size, and hands on what the next one needs (`ParentLayout`). A level flagged
        // `IsTrivialParent` -- `System.Object` and `System.ValueType`, and only those -- passes
        // `None` on, which is CoreCLR's `hasNonTrivialParent` and is what stops every plain
        // `[StructLayout(Sequential)] struct` being promoted to auto layout for having a parent
        // that is not managed-sequential.
        let placedByLevel, _, lastLevel =
            ((([] : CliConcreteField list list), (None : ParentLayout option), (None : ParentLayout option)), chain)
            ||> List.fold (fun (acc, parent, _) level ->
                let placed, next = CliValueType.LayoutLevel parent level

                // What the *next* level sees. The outermost level's own result is kept separately
                // as the type's size, because a trivial level passes `None` on and would otherwise
                // discard it.
                let parentForNext = if level.IsTrivialParent then None else Some next

                placed :: acc, parentForNext, Some next
            )

        let fields = placedByLevel |> List.rev |> List.concat

        let instanceSize =
            match lastLevel with
            | Some level -> level
            | None -> failwith $"CliValueType.OfFieldChain: empty base chain for %O{declared}"

        // ECMA-335 II.14.3: an enum has exactly one instance field, named `value__`, at offset 0.
        // Any loadable enum satisfies this, so a violation means `facts` describes some other type
        // than the one whose fields these are -- a caller error, and one worth catching here rather
        // than several opcodes later when the misclassified value is flattened or refused.
        if facts.IsEnum then
            match fields with
            | [ single ] when single.Name = "value__" && single.Offset = 0 -> ()
            | _ ->
                let described =
                    fields
                    |> List.map (fun field -> $"%s{field.Name}@%d{field.Offset}")
                    |> String.concat ", "

                failwith
                    $"CliValueType.OfFields: %O{declared} is described as an enum, but its %d{fields.Length} instance field(s) are [%s{described}] rather than the single `value__` at offset 0 that ECMA-335 II.14.3 requires of one"

        // A stamped alignment replaces the derived one but leaves the size alone: CoreCLR applies
        // it after sizing and never revisits the size (`CheckForSystemTypes`).
        let size =
            match facts.NominalAlignment with
            | None ->
                {
                    Size = instanceSize.InstanceSize
                    Alignment = instanceSize.AlignmentRequirement
                }
            | Some alignment ->
                // Every type CoreCLR stamps happens to be a whole number of its stamp wide, so
                // "round the size to the stamp" and "leave the size alone" agree on all of them and
                // no test could distinguish the two. Rather than leave that a silent coin-flip,
                // require the coincidence: where it holds the choice does not matter, and a stamped
                // type that broke it would need this code to make a decision it has no evidence for.
                if instanceSize.InstanceSize % alignment <> 0 then
                    failwith
                        $"CliValueType.OfFieldChain: %O{declared} carries a nominal alignment of %d{alignment} but its fields derive a size of %d{instanceSize.InstanceSize}, which is not a multiple of it"

                {
                    Size = instanceSize.InstanceSize
                    Alignment = alignment
                }

        {
            _Declared = declared
            _PrimitiveLikeKind = CliValueType.ClassifyPrimitiveLike bct allCt declared facts.IsEnum fields
            _NominalAlignment = facts.NominalAlignment
            _InstanceSize = size
            _Storage = CliValueType.StorageFromFields facts.LayoutKind facts.Layout size.Size fields
            LayoutKind = facts.LayoutKind
            Layout = facts.Layout
            CharSet = facts.CharSet
            NextTimestamp = 1UL
        }

    /// Lay out a type with no inherited instance fields: every value type, and the synthetic
    /// construction sites that build a single type's field block directly.
    static member OfFields
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (facts : DeclaredTypeFacts)
        (f : CliField list)
        : CliValueType
        =
        CliValueType.OfFieldChain
            bct
            allCt
            declared
            [
                {
                    Declared = declared
                    Facts = facts
                    OwnFields = f
                    IsTrivialParent = false
                }
            ]

    /// Rebuild with the same declared type and primitive-like classification as `source`. Used by
    /// the eval-stack rewrap path, which pops an already-classified value and reconstructs its
    /// stored form without needing `BaseClassTypes`/`AllConcreteTypes` in scope.
    /// This intentionally drops preserved bytes: do not call it for values whose padding or
    /// fixed-buffer trailing storage must be preserved.
    ///
    /// Single-level by contract. Everything it reconstructs is a value type off the eval stack, so
    /// there is no base chain to lose -- but nothing in the types enforces that, so it checks: a
    /// multi-level value would silently have its inherited fields re-placed from 0.
    static member OfFieldsLike (source : CliValueType) (layout : Layout) (f : CliField list) : CliValueType =
        if not (CliValueType.IsTightlyPacked source) then
            failwith
                $"CliValueType.OfFieldsLike: refusing to drop preserved bytes for non-tightly-packed value type %O{source.Declared}"

        let placed, next =
            CliValueType.LayoutLevel
                None
                {
                    Declared = source._Declared
                    Facts =
                        {
                            IsValueType = true
                            IsEnum = false
                            NominalAlignment = source._NominalAlignment
                            LayoutKind = source.LayoutKind
                            Layout = layout
                            CharSet = source.CharSet
                        }
                    OwnFields = f
                    IsTrivialParent = false
                }

        {
            _Declared = source._Declared
            _PrimitiveLikeKind = source._PrimitiveLikeKind
            _NominalAlignment = source._NominalAlignment
            _InstanceSize =
                {
                    Size = next.InstanceSize
                    Alignment =
                        match source._NominalAlignment with
                        | Some alignment -> alignment
                        | None -> next.AlignmentRequirement
                }
            _Storage = CliValueType.StorageFromFields source.LayoutKind layout next.InstanceSize placed
            LayoutKind = source.LayoutKind
            Layout = layout
            CharSet = source.CharSet
            NextTimestamp = 1UL
        }

    /// Describe why `field` was not found in `fields`, in enough detail to diagnose the failure
    /// from the message alone.
    ///
    /// The interesting cause of a metadata-keyed miss is that the *same* field definition is
    /// present in storage under a different declaring-type `ConcreteTypeHandle`: the site
    /// performing the access concretized the field's declaring type to a different instantiation
    /// from the one in force when the object's storage was laid out (see
    /// `IlMachineRuntimeMetadata.collectAllInstanceFields`, which keys an inherited field to the
    /// type that *declares* it, versus `ExecutionConcretization.concretizeFieldDeclaringType`,
    /// which keys it to the declaring type named by the access site's token). Callers cannot tell
    /// that apart from "no such field" alone, so say which it is.
    ///
    /// Two near-misses must not be reported as that, because they are different failures:
    ///
    ///  * an inline-array slot index that is out of range shares both its field definition *and*
    ///    its declaring type with the slots that are present, so nothing about the declaring type
    ///    is wrong;
    ///  * a `FieldDefinitionHandle` is a row index scoped to its defining module, so two fields of
    ///    unrelated assemblies can carry equal handles. A single object's storage spans its whole
    ///    base chain and so can span assemblies -- `FSharpException : System.Exception` does.
    ///    Requiring the names to agree as well makes a coincidental row collision vanishingly
    ///    unlikely; this member has no `AllConcreteTypes` with which to confirm the defining
    ///    module outright, so the wording stays hedged and the full identity list is always
    ///    printed for the reader to check.
    static member private DescribeMissingField
        (operation : string)
        (field : FieldId)
        (fields : CliConcreteField list)
        (cvt : CliValueType)
        : string
        =
        let available =
            match fields with
            | [] -> "none"
            | _ -> fields |> List.map (fun f -> $"'%O{f.Id}'") |> String.concat ", "

        let requestedDefinition = FieldId.tryFieldDefinition field
        let requestedDeclaringType = FieldId.tryDeclaringType field

        let sameDefinitionAs (predicate : ConcreteTypeHandle option -> bool) : CliConcreteField list =
            match requestedDefinition with
            | None -> []
            | Some wanted ->
                fields
                |> List.filter (fun f ->
                    FieldId.tryFieldDefinition f.Id = Some wanted
                    && predicate (FieldId.tryDeclaringType f.Id)
                )

        // Same field definition, same declaring type: the declaring type is not what differs.
        // Only the inline-array storage slots can be shaped like this.
        let sameDeclaringType =
            sameDefinitionAs (fun declaringType -> declaringType = requestedDeclaringType)

        // Same field definition and name, different declaring type: the smoking gun.
        let differentDeclaringType =
            sameDefinitionAs (fun declaringType -> declaringType <> requestedDeclaringType)
            |> List.filter (fun f -> f.Id.Name = field.Name)

        let describe (matches : CliConcreteField list) : string =
            matches |> List.map (fun f -> $"'%O{f.Id}'") |> String.concat ", "

        let diagnosis =
            match sameDeclaringType, differentDeclaringType with
            | [], [] -> ""
            | _ :: _, _ ->
                $" Storage does hold other slots of this same field definition on this same declaring type (%s{describe sameDeclaringType}), so the declaring type is not what differs; this is an inline-array slot index that storage does not have."
            | [], _ :: _ ->
                $" The same field definition and name IS present in storage, keyed to a different declaring type: %s{describe differentDeclaringType}. That most likely means the declaring-type instantiation computed at the access site disagrees with the one used when this value's storage was built."

        $"%s{operation}: field '%O{field}' not found on value of declared type %O{cvt._Declared}. Available field identities: %s{available}.%s{diagnosis}"

    static member private FindFieldById (field : FieldId) (cvt : CliValueType) : CliConcreteField =
        let fields = CliValueType.FieldStorage "CliValueType.FindFieldById" cvt

        let exactMatches = fields |> List.filter (fun f -> FieldId.exactlyEqual field f.Id)

        match exactMatches with
        | [ f ] -> f
        | _ :: _ :: _ -> failwith $"Field '%O{field}' matched multiple storage slots exactly"
        | [] ->
            match field with
            | FieldId.Metadata _
            | FieldId.InlineArrayElement _ ->
                failwith (CliValueType.DescribeMissingField "CliValueType.FindFieldById" field fields cvt)
            | FieldId.Named name ->
                let nameMatches = fields |> List.filter (fun f -> f.Name = name)

                match nameMatches with
                | [ f ] -> f
                | [] -> failwith (CliValueType.DescribeMissingField "CliValueType.FindFieldById" field fields cvt)
                | _ :: _ :: _ -> failwith $"Field name '%s{name}' is ambiguous; use metadata field identity"

    /// Returns the offset and size.
    static member GetFieldLayoutById (field : FieldId) (cvt : CliValueType) : int * int =
        let targetField = CliValueType.FindFieldById field cvt

        targetField.Offset, targetField.Size

    /// Returns the offset and size.
    static member GetFieldLayout (field : string) (cvt : CliValueType) : int * int =
        CliValueType.GetFieldLayoutById (FieldId.named field) cvt

    // TODO: use DereferenceFieldAt for the implementation.
    // We should eventually be able to dereference an arbitrary field of a struct
    // as though it were any other field of any other type, to accommodate Unsafe.As.
    static member DereferenceFieldById (field : FieldId) (cvt : CliValueType) : CliType =
        let targetField = CliValueType.FindFieldById field cvt
        let fields = CliValueType.FieldStorage "CliValueType.DereferenceFieldById" cvt

        // Identify all fields that overlap with the target field's memory range
        let targetStart = targetField.Offset
        let targetEnd = targetField.Offset + targetField.Size

        let affectedFields =
            fields
            |> List.filter (fun f ->
                let fieldStart = f.Offset
                let fieldEnd = f.Offset + f.Size
                // Fields overlap if their ranges intersect
                fieldStart < targetEnd && targetStart < fieldEnd
            )

        match affectedFields with
        | [] -> failwith "unexpectedly didn't dereference a field"
        | [ f ] -> f.Contents
        | _ :: _ :: _ ->
            let fieldBytes = CliValueType.BytesAt targetField.Offset targetField.Size cvt

            // `targetField.Contents` is the current value stored in the slot;
            // its shape tells us which primitive flavour to reconstruct. For
            // non-primitive field contents this still falls through to a
            // specific `failwith` inside `OfBytesLike`.
            CliType.OfBytesLike targetField.Contents fieldBytes

    // TODO: use DereferenceFieldAt for the implementation.
    // We should eventually be able to dereference an arbitrary field of a struct
    // as though it were any other field of any other type, to accommodate Unsafe.As.
    static member DereferenceField (field : string) (cvt : CliValueType) : CliType =
        CliValueType.DereferenceFieldById (FieldId.named field) cvt

    static member FieldsAt (offset : int) (cvt : CliValueType) : CliConcreteField list =
        CliValueType.FieldStorage "CliValueType.FieldsAt" cvt
        |> List.filter (fun f -> f.Offset = offset)

    /// Like `FieldsAt`, but returns `[]` for raw-bytes-backed value types instead of failing.
    /// Intended for byte-view dispatch paths that want to *try* a field-precise lookup and
    /// fall through gracefully when the storage carries no fields, without forcing the caller
    /// to peek at the storage discriminator.
    static member TryFieldsAt (offset : int) (cvt : CliValueType) : CliConcreteField list =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes _ -> []
        | CliValueTypeStorage.Fields storage -> storage.Fields |> List.filter (fun f -> f.Offset = offset)

    /// Every field of `cvt`, at whatever offset, returning `[]` for raw-bytes-backed value types
    /// instead of failing. Callers that must reason about the *whole* value — rather than about
    /// what lives at one offset — need this: `TryFieldsAt 0` alone cannot distinguish a struct
    /// whose single field spans it from one that also aliases a second field from a later offset.
    static member TryAllFields (cvt : CliValueType) : CliConcreteField list =
        match cvt._Storage with
        | CliValueTypeStorage.RawBytes _ -> []
        | CliValueTypeStorage.Fields storage -> storage.Fields

    static member DereferenceFieldAt (offset : int) (size : int) (cvt : CliValueType) : CliType =
        let candidates = CliValueType.FieldsAt offset cvt

        match candidates |> List.tryFind (fun f -> f.Size = size) with
        | Some targetField ->
            // Explicit layout can alias the requested range with other fields, and
            // `WithFieldSetById` leaves those siblings' `Contents` stale, recording
            // which write won in `EditedAtTime`. Picking a cell by (offset, size) alone would
            // therefore hand back a value the storage no longer holds.
            //
            // `ToBytes` decides that contest by replaying overlapping fields in `EditedAtTime`
            // order, so the *last* field in that same order owns every byte it covers. When that
            // winner spans the requested range exactly, its cell is authoritative and can be
            // returned directly — which is what keeps provenance the byte image cannot express
            // (runtime pointers, handle-valued native ints, widened native ints) alive across the
            // read. Only a winner that partially covers the range needs the byte image.
            let targetEnd = targetField.Offset + targetField.Size

            let winner =
                CliValueType.FieldStorage "CliValueType.DereferenceFieldAt" cvt
                |> List.filter (fun f -> f.Offset < targetEnd && targetField.Offset < f.Offset + f.Size)
                // Stable, and keyed exactly as `ToBytes` replays: later writes win, and among
                // equal timestamps (e.g. a value type nobody has written to yet) the
                // last-declared field is the one whose bytes land on top.
                |> List.sortBy _.EditedAtTime
                |> List.tryLast

            match winner with
            | Some winner when winner.Offset = targetField.Offset && winner.Size = targetField.Size -> winner.Contents
            | _ ->
                let fieldBytes = CliValueType.BytesAt targetField.Offset targetField.Size cvt
                CliType.OfBytesLike targetField.Contents fieldBytes
        | None ->
            // Storage here is field cells, not bytes, so a request that no single field answers
            // exactly (e.g. viewing `struct { int; int }` as an 8-byte value) has no honest
            // answer to give: say so rather than splicing one together.
            let describeCandidates =
                match candidates with
                | [] -> "no field starts at that offset"
                | _ ->
                    candidates
                    |> List.map (fun f -> $"%s{f.Name} (%d{f.Size} bytes)")
                    |> String.concat ", "
                    |> sprintf "fields starting there: %s"

            failwith
                $"cannot view %O{cvt._Declared} as a %d{size}-byte value at offset %d{offset}: %s{describeCandidates}"

    /// The type's instance size and the alignment it demands of a container.
    ///
    /// A field read rather than a computation: layout is per-declaring-type, so the answer depends
    /// on facts (each level's `Pack`, declared kind and declared `Size`) that the flat field list
    /// this value holds has thrown away. See `_InstanceSize`.
    ///
    /// The `Alignment` here is read by containers, through `CliType.SizeOf` on a field's contents
    /// -- which is exactly the `GetFieldAlignmentRequirement()` call CoreCLR makes at
    /// classlayoutinfo.cpp:112 and methodtablebuilder.cpp:8532. That is the whole propagation
    /// mechanism: no separate walk is needed to make a struct embedding an `Int128` 16-aligned.
    static member SizeOf (vt : CliValueType) : SizeofResult = vt._InstanceSize

    static member ContainsObjectReferences (vt : CliValueType) : bool =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> false
        | CliValueTypeStorage.Fields storage ->
            storage.Fields
            |> List.exists (fun field -> CliType.ContainsObjectReferences field.Contents)

    static member ContainsRuntimePointers (vt : CliValueType) : bool =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> false
        | CliValueTypeStorage.Fields storage ->
            storage.Fields
            |> List.exists (fun field -> CliType.ContainsRuntimePointers field.Contents)

    static member ByteAddressability (vt : CliValueType) : CliByteAddressability =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> CliByteAddressability.ByteAddressable
        | CliValueTypeStorage.Fields storage ->
            let classified =
                storage.Fields
                |> List.choose (fun field ->
                    match CliType.ByteAddressability field.Contents with
                    | CliByteAddressability.ByteAddressable -> None
                    | CliByteAddressability.SymbolicallyAddressable obstruction -> Some (field, obstruction, false)
                    | CliByteAddressability.SymbolicallyAddressable rejection
                    | CliByteAddressability.Rejected rejection -> Some (field, rejection, true)
                )

            // A field with no byte image at all decides the whole value: naming the struct
            // symbolically would promise a `UInt8Source` for every byte, and an object reference
            // has none. Only when every obstruction is a nameable one does the struct become
            // nameable too.
            let firstRejectedField =
                classified
                |> List.tryPick (fun (field, reason, isRejection) -> if isRejection then Some (field, reason) else None)

            let wrapSymbolic (obstruction : CliByteAddressabilityRejection) : CliByteAddressability =
                CliByteAddressability.SymbolicallyAddressable obstruction

            match firstRejectedField with
            | None ->
                match classified with
                | [] -> CliByteAddressability.ByteAddressable
                | (field, obstruction, _) :: _ ->
                    wrapSymbolic (
                        CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField (
                            vt._Declared,
                            field.Id,
                            obstruction
                        )
                    )
            | Some (_, CliByteAddressabilityRejection.ObjectReference)
            | Some (_, CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _) ->
                CliByteAddressability.Rejected (
                    CliByteAddressabilityRejection.ValueTypeContainsObjectReferences vt._Declared
                )
            | Some (_, CliByteAddressabilityRejection.RuntimePointer)
            | Some (_, CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers _) ->
                CliByteAddressability.Rejected (
                    CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers vt._Declared
                )
            | Some (field, rejection) ->
                // Object-reference and pointer containment use the coarse
                // outer-type rejections above. Every other rejection means a
                // field's own byte renderer would fail, so preserve that nested
                // reason instead of collapsing it to a containment predicate.
                CliByteAddressability.Rejected (
                    CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField (
                        vt._Declared,
                        field.Id,
                        rejection
                    )
                )

    static member IsTightlyPacked (vt : CliValueType) : bool =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> false
        | CliValueTypeStorage.Fields storage ->
            let size = CliValueType.SizeOf(vt).Size

            let finalOffset =
                ((Some 0), storage.Fields |> List.sortBy _.Offset)
                ||> List.fold (fun cursor field ->
                    match cursor with
                    | None -> None
                    | Some offset ->
                        if field.Offset = offset then
                            Some (field.Offset + field.Size)
                        else
                            None
                )

            finalOffset = Some size

    static member TryFindMarshalSizeDifference (vt : CliValueType) : string option =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes _ -> None
        | CliValueTypeStorage.Fields storage ->
            storage.Fields
            |> List.tryPick (fun field ->
                CliType.TryFindMarshalSizeDifference field.Contents
                |> Option.map (fun reason -> $"field %s{field.Name}: %s{reason}")
            )

    /// Bytes per character for the declaring type's `CharSet`, used to size
    /// `[MarshalAs(ByValTStr)]` fields. `None` is treated as the runtime default (Ansi). `Auto`
    /// is platform-dependent (Unicode on Windows, Ansi on Unix), so we reject it explicitly
    /// rather than picking a host-dependent answer in a deterministic interpreter.
    static member private CharSetByteSize (charSet : CharSet) : Result<int, MarshalSizeError> =
        match charSet with
        | CharSet.None
        | CharSet.Ansi -> Result.Ok 1
        | CharSet.Unicode -> Result.Ok 2
        | CharSet.Auto ->
            MarshalSizeError.NotImplemented
                "CharSet.Auto is platform-dependent and not yet supported by marshalled-size computation"
            |> Result.Error
        | other ->
            MarshalSizeError.NotImplemented $"unrecognised CharSet %O{other}"
            |> Result.Error

    /// Unmanaged size of a fixed-width scalar `UnmanagedType`. Used both as the per-element
    /// size for `[MarshalAs(ByValArray)]` and as the basis for the compatibility check when a
    /// scalar `UnmanagedType` is supplied directly via `[MarshalAs(...)]`. Only the
    /// unambiguous fixed-width primitive cases are decoded; everything else is rejected so the
    /// caller can decide whether a richer mapping is needed. `Error` (HRESULT) is included
    /// because CoreCLR accepts it on `int`/`uint` fields and it has the same width as `I4`.
    static member private MarshalSizeOfScalar (unmanagedType : UnmanagedType) : Result<SizeofResult, MarshalSizeError> =
        match unmanagedType with
        | UnmanagedType.I1
        | UnmanagedType.U1 ->
            Result.Ok
                {
                    Size = 1
                    Alignment = 1
                }
        | UnmanagedType.I2
        | UnmanagedType.U2 ->
            Result.Ok
                {
                    Size = 2
                    Alignment = 2
                }
        | UnmanagedType.I4
        | UnmanagedType.U4
        | UnmanagedType.R4
        | UnmanagedType.Error ->
            Result.Ok
                {
                    Size = 4
                    Alignment = 4
                }
        | UnmanagedType.I8
        | UnmanagedType.U8
        | UnmanagedType.R8 ->
            Result.Ok
                {
                    Size = 8
                    Alignment = 8
                }
        | UnmanagedType.SysInt
        | UnmanagedType.SysUInt ->
            Result.Ok
                {
                    Size = NATIVE_INT_SIZE
                    Alignment = NATIVE_INT_SIZE
                }
        | other ->
            MarshalSizeError.NotImplemented
                $"UnmanagedType %O{other} is not yet supported by marshalled-size computation"
            |> Result.Error

    /// True iff the given handle refers to a CLI array type. CoreCLR only accepts
    /// `[MarshalAs(ByValArray)]` on array-typed fields, so we use this as the shape guard.
    static member private IsArrayFieldType (handle : ConcreteTypeHandle) : bool =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false

    /// True iff `handle` names the given non-generic corelib type.
    ///
    /// A structural handle answers `false` because `AllConcreteTypes.lookup` has no row for one.
    static member private IsNominallyCorelibType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (target : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match AllConcreteTypes.lookup handle concreteTypes with
        | None -> false
        | Some concreteType ->

        // The corelib-assembly and no-generics test runs before the TypeDef lookup: it is
        // cheaper, and it is what lets a handle from any other assembly answer without resolving
        // at all.
        if
            concreteType.AssemblyFullName = corelib.Corelib.DefinitionFullName
            && concreteType.Generics.IsEmpty
        then
            let typeDef =
                (assemblies.ByDefinitionName concreteType.AssemblyFullName).TypeDefs.[concreteType.Definition.Get]

            TypeInfo.NominallyEqual typeDef target
        else
            false

    /// True iff the given handle refers to `System.String`. CoreCLR only accepts
    /// `[MarshalAs(ByValTStr)]` on string-typed fields, so we use this as the shape guard.
    static member private IsStringFieldType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : bool
        =
        CliValueType.IsNominallyCorelibType concreteTypes assemblies corelib corelib.String handle

    /// True iff `vt`'s declared type is `System.DateTime`. CoreCLR's `MarshalInfo` short-circuits
    /// a DateTime-typed field to `MARSHAL_TYPE_DATE` (8 bytes) at `mlinfo.cpp:1747`, BEFORE the
    /// AutoLayout rejection in the same classifier — so a sequential struct can embed DateTime
    /// even though `DateTime` itself is declared `[StructLayout(LayoutKind.Auto)]`. Callers use
    /// this both to honour the shortcut on the field-size walk and to reject DateTime fields
    /// from the strict-numeric blittable arm of `MarshalNative_TryGetStructMarshalStub` (whose
    /// memmove fast path would otherwise silently emit the managed `_dateData` bytes instead
    /// of the OADate native form).
    static member IsHostKnownDateTime
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : bool
        =
        CliValueType.IsNominallyCorelibType concreteTypes assemblies corelib corelib.DateTime vt._Declared

    /// True iff `vt`'s declared type is `System.Decimal`. CoreCLR's `MarshalInfo` routes a
    /// Decimal-typed *field* through marshal-stub synthesis (`NFT_DECIMAL` in
    /// `fieldmarshaler.cpp`) rather than treating it as memmove-blittable: managed `Decimal`
    /// is 16 bytes with 4-byte field alignment, but native `DECIMAL` is 16 bytes with 8-byte
    /// alignment (its `Lo64` union member is `ULONGLONG`), so a sequential outer struct
    /// containing a `Decimal` field is laid out differently managed vs native. Structurally,
    /// `Decimal` looks like a plain sequential struct of four `Int32` fields, so PawPrint can't
    /// distinguish it without a nominal name match. This predicate is intended for the
    /// **field-level** rejection inside `MarshalNative_TryGetStructMarshalStub`'s classifier;
    /// it does not gate `Marshal.SizeOf<Decimal>()` or top-level
    /// `Marshal.StructureToPtr<decimal>` (where managed and native byte images of standalone
    /// Decimal happen to coincide — `flags` decomposes byte-for-byte to
    /// `wReserved+scale+sign`).
    static member IsHostKnownDecimal
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : bool
        =
        CliValueType.IsNominallyCorelibType concreteTypes assemblies corelib corelib.Decimal vt._Declared

    /// True iff `handle`'s declared type carries `LayoutKind.Auto` in its
    /// `TypeAttributes.LayoutMask`. This is the CoreCLR `HasLayout() == false` predicate:
    /// it covers reference types without `[StructLayout]` (whose default is AutoLayout) as
    /// well as value types explicitly marked `[StructLayout(LayoutKind.Auto)]`.
    /// CoreCLR rejects top-level `Marshal.SizeOf<T>()` and returns FALSE from
    /// `MarshalNative_TryGetStructMarshalStub` for any such type (`fieldmarshaler.cpp:309`,
    /// `marshalnative.cpp:99`).
    /// Returns `false` for synthetic handles (arrays, byrefs, pointers, function pointers) and for
    /// handles whose backing TypeInfo can't be found — those don't have a CLR `LayoutKind` to honour
    /// and callers must classify them through a different path.
    static member IsAutoLayoutHandle
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle concreteTypes with
            | None -> false
            | Some concreteType ->
                match assemblies.TryByDefinitionName concreteType.AssemblyFullName with
                | None -> false
                | Some assy ->
                    match assy.TypeDefs.TryGetValue concreteType.Definition.Get with
                    | false, _ -> false
                    | true, typeDef ->
                        // `LayoutMask = 0x18`; `AutoLayout = 0x00`, `SequentialLayout = 0x08`,
                        // `ExplicitLayout = 0x10`. The zero-valued bits are AutoLayout by ECMA §II.10.1.2.
                        (typeDef.TypeAttributes &&& TypeAttributes.LayoutMask) = TypeAttributes.AutoLayout
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false

    /// True iff `vt`'s declared type carries `LayoutKind.Auto`. Convenience wrapper around
    /// `IsAutoLayoutHandle` for the field/struct marshal-size walk; field-level use is gated
    /// separately so host-known AutoLayout types (DateTime) can still appear as fields via
    /// their dedicated shortcut.
    static member private IsAutoLayout
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (vt : CliValueType)
        : bool
        =
        CliValueType.IsAutoLayoutHandle concreteTypes assemblies vt._Declared

    /// Compute the unmanaged size of a single field, consulting `[MarshalAs(...)]` descriptors
    /// and the declaring type's `CharSet`. Without a descriptor, falls back to the managed
    /// layout size for byte-stable primitives, recurses into nested value types, and rejects
    /// shapes (Bool/Char/ObjectRef) whose unmanaged size deviates from the managed one.
    /// The field's nominal `ConcreteTypeHandle` is consulted to validate `ByValTStr`/`ByValArray`
    /// descriptors against the declared field shape (CoreCLR rejects mismatches).
    static member TryFieldMarshalSize
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (charSet : CharSet)
        (descriptor : FieldMarshalDescriptor option)
        (fieldType : ConcreteTypeHandle)
        (contents : CliType)
        : Result<SizeofResult, MarshalSizeError>
        =
        match descriptor with
        | Some (FieldMarshalDescriptor.ByValTStr sizeConst) ->
            // CoreCLR's `MarshalInfo` rejects ByValTStr unless the managed field is
            // `System.String`. Validate against the declared field type so non-string
            // references (e.g. arbitrary class fields) don't silently get a string-buffer
            // size.
            if not (CliValueType.IsStringFieldType concreteTypes assemblies corelib fieldType) then
                MarshalSizeError.NotMarshalable
                    "[MarshalAs(UnmanagedType.ByValTStr)] is only valid on System.String fields"
                |> Result.Error
            elif sizeConst <= 0 then
                MarshalSizeError.NotMarshalable $"ByValTStr SizeConst=%d{sizeConst} is not positive"
                |> Result.Error
            else
                CliValueType.CharSetByteSize charSet
                |> Result.map (fun bpc ->
                    {
                        Size = sizeConst * bpc
                        Alignment = bpc
                    }
                )
        | Some (FieldMarshalDescriptor.ByValArray (sizeConst, Some elementType)) ->
            // Likewise, ByValArray requires an array-typed field; reject anything else.
            if not (CliValueType.IsArrayFieldType fieldType) then
                MarshalSizeError.NotMarshalable
                    "[MarshalAs(UnmanagedType.ByValArray)] is only valid on array-typed fields"
                |> Result.Error
            elif sizeConst <= 0 then
                MarshalSizeError.NotMarshalable $"ByValArray SizeConst=%d{sizeConst} is not positive"
                |> Result.Error
            else
                CliValueType.MarshalSizeOfScalar elementType
                |> Result.mapError (MarshalSizeError.prefix "ByValArray element type: ")
                |> Result.map (fun elementSize ->
                    {
                        Size = sizeConst * elementSize.Size
                        Alignment = elementSize.Alignment
                    }
                )
        | Some (FieldMarshalDescriptor.ByValArray (_, None)) ->
            MarshalSizeError.NotImplemented "ByValArray descriptor without an explicit element type is not supported"
            |> Result.Error
        | Some (FieldMarshalDescriptor.Other UnmanagedType.Struct) ->
            // `[MarshalAs(UnmanagedType.Struct)]` on a value-type field instructs the
            // marshaller to lay out that struct inline using its own native layout. Recurse
            // into `TryComputeMarshalSize` so nested marshalling annotations on the inner
            // struct's fields contribute correctly to the outer size.
            match contents with
            | CliType.ValueType vt ->
                if CliValueType.IsHostKnownDateTime concreteTypes assemblies corelib vt then
                    Result.Ok
                        {
                            Size = 8
                            Alignment = 8
                        }
                else
                    CliValueType.TryComputeMarshalSize concreteTypes assemblies corelib vt
            | _ ->
                MarshalSizeError.NotMarshalable
                    "[MarshalAs(UnmanagedType.Struct)] is only valid on value-type fields, not reference or primitive contents"
                |> Result.Error
        | Some (FieldMarshalDescriptor.Other unmanagedType) ->
            // CoreCLR's `MarshalInfo` validates a scalar `[MarshalAs]` against the managed
            // field type and rejects width-mismatched pairs (e.g. `[MarshalAs(I1)] int`).
            // Mirror that: only accept scalar descriptors when their declared width matches
            // the field's CLI byte width. Variants whose unmanaged width we don't yet know
            // (`Bool`, `LPStr`, `Currency`, ...) propagate the scalar-size error verbatim.
            CliValueType.MarshalSizeOfScalar unmanagedType
            |> Result.bind (fun descSize ->
                let cliSize = CliType.SizeOf contents

                if cliSize.Size <> descSize.Size then
                    MarshalSizeError.NotMarshalable
                        $"[MarshalAs(%O{unmanagedType})] declares %d{descSize.Size}-byte unmanaged width but managed field has %d{cliSize.Size} bytes"
                    |> Result.Error
                else
                    Result.Ok descSize
            )
        | None ->
            match contents with
            | CliType.Numeric _
            | CliType.RuntimePointer _ -> Result.Ok (CliType.SizeOf contents)
            | CliType.Bool _ ->
                MarshalSizeError.NotImplemented
                    "System.Boolean marshals as a 4-byte BOOL by default, not a 1-byte CLI bool"
                |> Result.Error
            | CliType.Char _ ->
                MarshalSizeError.NotImplemented
                    "System.Char marshalling depends on CharSet and does not always match 2-byte CLI char"
                |> Result.Error
            | CliType.ObjectRef _ ->
                MarshalSizeError.NotImplemented "object references require managed-to-unmanaged marshalling"
                |> Result.Error
            | CliType.ValueType vt ->
                // Mirror CoreCLR's `MarshalInfo::MarshalInfo` (mlinfo.cpp:1747): a DateTime-typed
                // field short-circuits to `MARSHAL_TYPE_DATE` (8 bytes, 8-byte aligned) without
                // recursing into the struct, even though `System.DateTime` itself is AutoLayout.
                if CliValueType.IsHostKnownDateTime concreteTypes assemblies corelib vt then
                    Result.Ok
                        {
                            Size = 8
                            Alignment = 8
                        }
                else
                    CliValueType.TryComputeMarshalSize concreteTypes assemblies corelib vt

    /// Compute the unmanaged size of a value type as `Marshal.SizeOf` would, *and* where each
    /// declared field lands in that unmanaged image. See `TryComputeMarshalSize` for the
    /// size-only entry point, which is this function with the placements dropped.
    ///
    /// Lays fields out using the declaring type's `Layout` (sequential or explicit) and packing,
    /// but with each field sized via `TryFieldMarshalSize` so
    /// `[MarshalAs(ByValTStr/ByValArray)]` fields contribute their unmanaged byte cost rather
    /// than the managed CLI size. Type-system context is required so descriptors that depend on
    /// the field's nominal type (e.g. `ByValTStr` requires `System.String`) can be validated.
    ///
    /// A `RawBytes`-backed value type has no declared fields, so it yields an empty placement
    /// list alongside a non-zero size. That is not "nothing to marshal": consumers that write
    /// the image field by field must reject `RawBytes` storage explicitly rather than reading
    /// an empty list as an empty struct.
    static member TryComputeMarshalLayout
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : Result<SizeofResult * MarshalFieldPlacement list, MarshalSizeError>
        =
        // Mirror CoreCLR's `IsStructMarshalable` (fieldmarshaler.cpp:288): a type with
        // `LayoutKind.Auto` reports `HasLayout() == false`, so `Marshal.SizeOf<T>()` throws an
        // `ArgumentException`. The recursion from `TryFieldMarshalSize` reaches us here too —
        // host-known AutoLayout fields (currently just `System.DateTime`) are intercepted in
        // `TryFieldMarshalSize` before they recurse, so by the time we see an AutoLayout type
        // it really is something we should reject.
        if CliValueType.IsAutoLayout concreteTypes assemblies vt then
            MarshalSizeError.NotMarshalable "type has [StructLayout(LayoutKind.Auto)] and has no native layout"
            |> Result.Error
        else

        match vt._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            Result.Ok (
                {
                    Size = bytes.Length
                    Alignment = 1
                },
                []
            )
        | CliValueTypeStorage.Fields storage ->
            let minimumSize, packingSize =
                match vt.Layout with
                | Layout.Custom (size = size ; packingSize = packing) ->
                    size, if packing = 0 then DEFAULT_PACKING_SIZE else packing
                | Layout.Default -> 0, DEFAULT_PACKING_SIZE

            // CoreCLR's `EEClassNativeLayoutInfo::CollectNativeLayoutFieldMetadataThrowing`
            // (classlayoutinfo.cpp:984-988) bumps a computed native layout size of 0 to 1
            // so the type has a distinct native address. This is universal post-processing
            // and applies whether the zero came from an empty field list, all fields
            // eliding to nothing, or an explicit `Size = 0` on the `[StructLayout]`. Apply
            // here so every concrete return path through the marshal-size walk respects
            // the same invariant.
            let bumpZeroSized (size : SizeofResult) : SizeofResult =
                if size.Size = 0 then
                    { size with
                        Size = 1
                    }
                else
                    size

            // Native layout takes a declared `Size` by exactly the same rule the managed layout
            // does, through the same helper: `CollectNativeLayoutFieldMetadataThrowing` calls
            // `CalculateSizeWithMetadataSize` when the type `HasExplicitSize()` and `AlignSize`
            // otherwise (classlayoutinfo.cpp:939-977). So the floor and the rounding are
            // alternatives here too -- `Marshal.SizeOf` of `[Sequential, Size = 13] { long; int }`
            // is 13, and of the same type with `Size = 4` is 12.
            let computeFinal (currentEnd : int) (maxAlign : int) : SizeofResult =
                let alignment = max maxAlign 1

                let totalSize =
                    if minimumSize > 0 then
                        max minimumSize currentEnd
                    else
                        roundUpToAlignment alignment currentEnd

                bumpZeroSized
                    {
                        Size = totalSize
                        Alignment = alignment
                    }

            let seqFields, nonSeqFields =
                storage.Fields |> List.partition (fun field -> field.ConfiguredOffset.IsNone)

            // Accumulator for both folds: placements so far (reversed), the running
            // offset/extent, and the widest alignment seen. Placements are recorded by the same
            // step that consumes the offset, so the two can never drift apart.
            let placeField
                (field : CliConcreteField)
                (offsetOf : int -> int -> int)
                (acc : Result<MarshalFieldPlacement list * int * int, MarshalSizeError>)
                : Result<MarshalFieldPlacement list * int * int, MarshalSizeError>
                =
                match acc with
                | Result.Error _ -> acc
                | Result.Ok (placed, running, maxAlign) ->
                    match
                        CliValueType.TryFieldMarshalSize
                            concreteTypes
                            assemblies
                            corelib
                            vt.CharSet
                            field.MarshallingDescriptor
                            field.Type
                            field.Contents
                    with
                    | Result.Error err -> Result.Error (MarshalSizeError.prefixField field.Name err)
                    | Result.Ok size ->
                        let alignmentCap = min size.Alignment packingSize
                        let offset = offsetOf running alignmentCap

                        let placement =
                            {
                                Field = CliConcreteField.ToCliField field
                                NativeOffset = offset
                                NativeSize = size
                            }

                        // Sequential layout advances the cursor past this field; explicit layout
                        // instead tracks the furthest extent, because fields may be declared out
                        // of offset order and may overlap.
                        Result.Ok (placement :: placed, max running (offset + size.Size), max maxAlign alignmentCap)

            let finish
                (acc : Result<MarshalFieldPlacement list * int * int, MarshalSizeError>)
                : Result<SizeofResult * MarshalFieldPlacement list, MarshalSizeError>
                =
                acc
                |> Result.map (fun (placed, extent, align) -> computeFinal extent align, List.rev placed)

            match seqFields, nonSeqFields with
            | [], [] ->
                Result.Ok (
                    bumpZeroSized
                        {
                            Size = minimumSize
                            Alignment = 1
                        },
                    []
                )
            | _ :: _, [] ->
                (Result.Ok ([], 0, 0), seqFields)
                ||> List.fold (fun acc field ->
                    placeField
                        field
                        (fun currentOffset alignmentCap ->
                            if alignmentCap = 0 then
                                currentOffset
                            else
                                let err = currentOffset % alignmentCap

                                if err = 0 then
                                    currentOffset
                                else
                                    currentOffset + (alignmentCap - err)
                        )
                        acc
                )
                |> finish
            | [], _ :: _ ->
                (Result.Ok ([], 0, 0), nonSeqFields)
                ||> List.fold (fun acc field -> placeField field (fun _ _ -> field.Offset) acc)
                |> finish
            | _ :: _, _ :: _ ->
                MarshalSizeError.NotMarshalable "unexpectedly mixed explicit and automatic field offsets"
                |> Result.Error

    /// Compute the unmanaged size of a value type as `Marshal.SizeOf` would. This is
    /// `TryComputeMarshalLayout` with the per-field placements dropped; see there for the layout
    /// rules and for why callers that write the unmanaged image must use the placements rather
    /// than deriving their own offsets.
    static member TryComputeMarshalSize
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : Result<SizeofResult, MarshalSizeError>
        =
        CliValueType.TryComputeMarshalLayout concreteTypes assemblies corelib vt
        |> Result.map fst

    /// Sets the value of the specified field, *without* touching any overlapping fields.
    /// `DereferenceField` handles resolving conflicts between overlapping fields.
    static member WithFieldSetById (field : FieldId) (value : CliType) (cvt : CliValueType) : CliValueType =
        let targetField = CliValueType.FindFieldById field cvt

        let storage = CliValueType.FieldBackedStorage "CliValueType.WithFieldSetById" cvt

        {
            _Declared = cvt._Declared
            _PrimitiveLikeKind = cvt._PrimitiveLikeKind
            _NominalAlignment = cvt._NominalAlignment
            _InstanceSize = cvt._InstanceSize
            LayoutKind = cvt.LayoutKind
            Layout = cvt.Layout
            CharSet = cvt.CharSet
            _Storage =
                let updatedFields =
                    storage.Fields
                    |> List.replaceWhere (fun f ->
                        if FieldId.exactlyEqual f.Id targetField.Id then
                            { f with
                                Contents = value
                                EditedAtTime = cvt.NextTimestamp
                            }
                            |> Some
                        else
                            None
                    )

                CliValueTypeStorage.Fields
                    // Preserved bytes remain the prior byte image. `ToBytes` overlays
                    // current field values on top of it so padding and unrepresented ranges survive
                    // field updates without treating this image as authoritative for field ranges.
                    { storage with
                        Fields = updatedFields
                        PreservedBytes = Array.copy storage.PreservedBytes
                    }
            NextTimestamp = cvt.NextTimestamp + 1UL
        }

    /// Sets the value of the specified field, *without* touching any overlapping fields.
    /// `DereferenceField` handles resolving conflicts between overlapping fields.
    static member WithFieldSet (field : string) (value : CliType) (cvt : CliValueType) : CliValueType =
        CliValueType.WithFieldSetById (FieldId.named field) value cvt

    /// Projects the single instance field at offset 0 of a primitive-like struct.
    /// These structs are guaranteed by construction to have exactly one instance field at offset 0
    /// (e.g. `IntPtr._value`, `RuntimeTypeHandle.m_type`, every enum's `value__`); any failure here
    /// indicates a violated invariant, not a caller error. Gated on the classification so it
    /// cannot misfire on user-defined single-field structs.
    static member PrimitiveLikeField (cvt : CliValueType) : CliField =
        if cvt._PrimitiveLikeKind.IsNone then
            failwith $"CliValueType.PrimitiveLikeField: %O{cvt._Declared} is not primitive-like"

        match CliValueType.FieldStorage "CliValueType.PrimitiveLikeField" cvt with
        | [ x ] when x.Offset = 0 -> CliConcreteField.ToCliField x
        | _ ->
            failwith
                $"invariant: primitive-like struct %O{cvt._Declared} must have exactly one instance field at offset 0"

    /// Produce a new value type with `target`'s shape (declared type, primitive-like classification,
    /// field layout, declared offsets) but with each field's contents replaced by the result of
    /// `coerceContents targetContents sourceContents`. Fields are paired positionally; name and
    /// offset must agree between target and source, which holds whenever both value types share the
    /// same declared type.
    ///
    /// Per-field `EditedAtTime` (and `NextTimestamp`) are carried over from `source`: for explicit-
    /// layout unions `CliValueType.ToBytes` replays fields in timestamp order to resolve
    /// overlaps, so losing the source's write ordering would silently change which union member
    /// survives a coercion roundtrip.
    ///
    /// Intended for situations like storing a popped `UserDefinedValueType` back into a typed value
    /// type slot, where each field's value must be coerced into the target's declared shape while
    /// the overall struct layout — and the write-order bookkeeping — is preserved.
    static member CoerceFrom
        (coerceContents : CliType -> CliType -> CliType)
        (target : CliValueType)
        (source : CliValueType)
        : CliValueType
        =
        match target._Storage, source._Storage with
        | CliValueTypeStorage.RawBytes targetBytes, CliValueTypeStorage.RawBytes sourceBytes ->
            if targetBytes.Length <> sourceBytes.Length then
                failwith
                    $"CliValueType.CoerceFrom: raw byte count mismatch between target %O{target._Declared} (%i{targetBytes.Length}) and source %O{source._Declared} (%i{sourceBytes.Length})"

            if target.Layout <> source.Layout then
                failwith
                    $"CliValueType.CoerceFrom: raw layout mismatch between target %O{target._Declared} and source %O{source._Declared}"

            {
                _Declared = target._Declared
                _PrimitiveLikeKind = target._PrimitiveLikeKind
                _NominalAlignment = target._NominalAlignment
                _InstanceSize = target._InstanceSize
                _Storage = CliValueTypeStorage.RawBytes (Array.copy sourceBytes)
                LayoutKind = target.LayoutKind
                Layout = target.Layout
                CharSet = target.CharSet
                NextTimestamp = source.NextTimestamp
            }
        | CliValueTypeStorage.Fields targetStorage, CliValueTypeStorage.Fields sourceStorage ->
            let targetFields = targetStorage.Fields
            let sourceFields = sourceStorage.Fields
            let targetSize = CliValueType.SizeOf(target).Size
            let sourceSize = CliValueType.SizeOf(source).Size

            if targetSize <> sourceSize then
                failwith
                    $"CliValueType.CoerceFrom: field-backed size mismatch between target %O{target._Declared} (%i{targetSize} bytes) and source %O{source._Declared} (%i{sourceSize} bytes)"

            if targetFields.Length <> sourceFields.Length then
                failwith
                    $"CliValueType.CoerceFrom: field count mismatch between target %O{target._Declared} (%i{targetFields.Length}) and source %O{source._Declared} (%i{sourceFields.Length})"

            let merged =
                (targetFields, sourceFields)
                ||> List.map2 (fun tField sField ->
                    if tField.Name <> sField.Name then
                        failwith
                            $"CliValueType.CoerceFrom: name mismatch between target %O{target._Declared} and source %O{source._Declared}: %s{tField.Name} vs %s{sField.Name}"

                    if tField.Offset <> sField.Offset then
                        failwith
                            $"CliValueType.CoerceFrom: offset mismatch for field %s{tField.Name} between target %O{target._Declared} and source %O{source._Declared}: %d{tField.Offset} vs %d{sField.Offset}"

                    { tField with
                        Contents = coerceContents tField.Contents sField.Contents
                        EditedAtTime = sField.EditedAtTime
                    }
                )

            {
                _Declared = target._Declared
                _PrimitiveLikeKind = target._PrimitiveLikeKind
                _NominalAlignment = target._NominalAlignment
                _InstanceSize = target._InstanceSize
                _Storage =
                    CliValueTypeStorage.Fields
                        {
                            Fields = merged
                            PreservedBytes = Array.copy sourceStorage.PreservedBytes
                        }
                LayoutKind = target.LayoutKind
                Layout = target.Layout
                CharSet = target.CharSet
                NextTimestamp = source.NextTimestamp
            }
        | CliValueTypeStorage.RawBytes targetBytes, CliValueTypeStorage.Fields sourceStorage ->
            failwith
                $"CliValueType.CoerceFrom: cannot coerce field-backed source %O{source._Declared} (%i{sourceStorage.Fields.Length} fields) into raw-backed target %O{target._Declared} (%i{targetBytes.Length} bytes)"
        | CliValueTypeStorage.Fields targetStorage, CliValueTypeStorage.RawBytes sourceBytes ->
            failwith
                $"CliValueType.CoerceFrom: cannot coerce raw-backed source %O{source._Declared} (%i{sourceBytes.Length} bytes) into field-backed target %O{target._Declared} (%i{targetStorage.Fields.Length} fields)"

    /// The all-zero value of the same shape as `template`. See `CliType.ZeroLike`.
    ///
    /// Structural rather than byte-driven, so it is total over field shapes that have no byte
    /// rendering (a struct holding an unmanaged pointer, e.g. `struct S { int N; int* P; }`,
    /// is a legitimate element type for a bulk zeroing: raw pointers are not GC-tracked, so
    /// such an array reports `ContainsGCPointers = false`).
    static member ZeroLike (template : CliValueType) : CliValueType =
        match template._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            { template with
                _Storage = CliValueTypeStorage.RawBytes (Array.zeroCreate bytes.Length)
            }
        | CliValueTypeStorage.Fields storage ->
            // Field write timestamps are replayed in declaration order, as `OfBytesLike` does: a
            // zeroed struct has no meaningful overlapping-field write history to preserve, and
            // every order produces the same all-zero result anyway.
            let fields =
                storage.Fields
                |> List.mapi (fun index field ->
                    { field with
                        Contents = CliType.ZeroLike field.Contents
                        EditedAtTime = uint64 index
                    }
                )

            { template with
                _Storage =
                    CliValueTypeStorage.Fields
                        {
                            Fields = fields
                            PreservedBytes = Array.zeroCreate (CliValueType.SizeOf template).Size
                        }
                NextTimestamp = max 1UL (uint64 fields.Length)
            }

    /// Reconstruct a value type from preserved bytes using `template` for field layout and field
    /// shapes. Preserved bytes do not encode original overlapping-field write history, so the
    /// recovered value uses declaration-order replay as its canonical write order.
    static member OfBytesLike (template : CliValueType) (bytes : byte[]) : CliValueType =
        let rec cliTypeOfBytesLike (template : CliType) (bytes : byte[]) : CliType =
            match template with
            | CliType.ValueType vt -> valueTypeOfBytesLike vt bytes |> CliType.ValueType
            | _ -> CliType.OfBytesLike template bytes

        and valueTypeOfBytesLike (template : CliValueType) (bytes : byte[]) : CliValueType =
            match template._Storage with
            | CliValueTypeStorage.RawBytes templateBytes ->
                if bytes.Length <> templateBytes.Length then
                    failwith
                        $"CliValueType.OfBytesLike: byte count mismatch for raw-backed value type %O{template._Declared}; expected %i{templateBytes.Length}, got %i{bytes.Length}"

                {
                    _Declared = template._Declared
                    _PrimitiveLikeKind = template._PrimitiveLikeKind
                    _NominalAlignment = template._NominalAlignment
                    _InstanceSize = template._InstanceSize
                    _Storage = CliValueTypeStorage.RawBytes (Array.copy bytes)
                    LayoutKind = template.LayoutKind
                    Layout = template.Layout
                    CharSet = template.CharSet
                    NextTimestamp = template.NextTimestamp
                }
            | CliValueTypeStorage.Fields storage ->
                let expected = CliValueType.SizeOf(template).Size

                if bytes.Length <> expected then
                    failwith
                        $"CliValueType.OfBytesLike: byte count mismatch for field-backed value type %O{template._Declared}; expected %i{expected}, got %i{bytes.Length}"

                let fields =
                    storage.Fields
                    |> List.mapi (fun index field ->
                        if field.Offset < 0 then
                            failwith
                                $"CliValueType.OfBytesLike: field %s{field.Name} in %O{template._Declared} has negative offset %i{field.Offset}"

                        if field.Size < 0 then
                            failwith
                                $"CliValueType.OfBytesLike: field %s{field.Name} in %O{template._Declared} has negative size %i{field.Size}"

                        let fieldEnd = field.Offset + field.Size

                        if fieldEnd > bytes.Length then
                            failwith
                                $"CliValueType.OfBytesLike: field %s{field.Name} in %O{template._Declared} spans bytes [%i{field.Offset}, %i{fieldEnd}) beyond %i{bytes.Length}-byte input"

                        let fieldBytes = Array.zeroCreate<byte> field.Size
                        Array.blit bytes field.Offset fieldBytes 0 field.Size

                        let contents = cliTypeOfBytesLike field.Contents fieldBytes

                        { field with
                            Contents = contents
                            EditedAtTime = uint64 index
                        }
                    )

                let result =
                    {
                        _Declared = template._Declared
                        _PrimitiveLikeKind = template._PrimitiveLikeKind
                        _NominalAlignment = template._NominalAlignment
                        _InstanceSize = template._InstanceSize
                        _Storage =
                            CliValueTypeStorage.Fields
                                {
                                    Fields = fields
                                    PreservedBytes = Array.copy bytes
                                }
                        LayoutKind = template.LayoutKind
                        Layout = template.Layout
                        CharSet = template.CharSet
                        NextTimestamp = max 1UL (uint64 fields.Length)
                    }

                result

        valueTypeOfBytesLike template bytes

/// `[InlineArray(N)]` makes a value type N repeats of its single declared instance field. CoreCLR
/// keeps exactly one `FieldDesc` and instead multiplies the type's instance size by N *after*
/// laying the one field out (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612
/// for the auto-layout route, :8663 for sequential, :8696 for explicit).
///
/// PawPrint's value storage is field-cell based rather than a byte block, so the N-1 implicit
/// repeats need cells of their own. Expanding the declared field into N identical `CliField`s makes
/// the ordinary layout algorithms produce CoreCLR's answer: because every copy is identical, laying
/// N of them out in sequence and rounding the total is the same thing as laying one out, rounding
/// it, and multiplying by N.
[<RequireQualifiedAccess>]
module InlineArrayStorage =
    /// The repeat count that actually governs a type's storage, given the count its metadata
    /// declares and whether the type is a value type.
    ///
    /// `[InlineArray(N)]` means nothing on a reference type. CoreCLR reads the attribute only
    /// inside the `IsValueClass()` branch of `MethodTableBuilder::PlaceInstanceFields`
    /// (methodtablebuilder.cpp:1738), so a class carrying it loads with its declared fields and none
    /// of the inline-array rules — not the "exactly one instance field" rule, and not the repeat.
    /// C# cannot emit that (the attribute is `AttributeTargets.Struct`), but hand-written IL can,
    /// and honouring it would either give such a class synthetic storage or make us reject a type
    /// CoreCLR accepts.
    ///
    /// `TypeInfo.InlineArrayLength` stays a faithful record of what the metadata says;
    /// deciding when that record is inert is this function's job.
    let effectiveLength (isValueType : bool) (declared : int option) : int option =
        if isValueType then declared else None

    /// Expand a value type's declared instance fields into its *storage slots*.
    ///
    /// `None` (the overwhelmingly common case) is the identity. `Some n` reproduces every condition
    /// CoreCLR checks alongside the repeat count, all of which raise `TypeLoadException` there:
    /// exactly one declared instance field (`IDS_CLASSLOAD_INLINE_ARRAY_FIELD_COUNT`,
    /// methodtablebuilder.cpp:1751), no explicit field offset (`..._EXPLICIT`, :1767), and no
    /// declared `ClassSize` (`..._EXPLICIT_SIZE`, :1773). The last is a distinct condition from the
    /// second — `[StructLayout(Sequential, Size = X)]` sets `ClassSize` without making the layout
    /// explicit — and it matters here even though `Layout.Custom`'s size is only ever a floor:
    /// running a type CoreCLR refuses to load means guessing at semantics the guest never asked
    /// for, so we fail loudly, as with the other two. Only hand-crafted IL can reach any of them.
    ///
    /// Slot 0 is returned unchanged — it *is* the declared field, at offset 0, exactly as CoreCLR's
    /// single `FieldDesc` is. Slots 1 and up get a distinct identity and a distinct storage name;
    /// see `FieldId.InlineArrayElement`.
    let expand
        (describeType : unit -> string)
        (layout : Layout)
        (inlineArrayLength : int option)
        (fields : CliField list)
        : CliField list
        =
        match inlineArrayLength with
        | None -> fields
        | Some repeat ->

        if repeat <= 0 then
            failwith
                $"[InlineArray(%d{repeat})] on %s{describeType ()}: the repeat count must be positive (CoreCLR's IDS_CLASSLOAD_INLINE_ARRAY_LENGTH)"

        match layout with
        | Layout.Custom (size = size) when size > 0 ->
            failwith
                $"[InlineArray(%d{repeat})] on %s{describeType ()}: the type declares an explicit size of %d{size} bytes, but an inline array may not declare one (CoreCLR's IDS_CLASSLOAD_INLINE_ARRAY_EXPLICIT_SIZE)"
        | Layout.Custom _
        | Layout.Default -> ()

        match fields with
        | [ single ] ->
            if single.Offset.IsSome then
                failwith
                    $"[InlineArray(%d{repeat})] on %s{describeType ()}: field %s{single.Name} carries an explicit offset, but an inline array may not use explicit layout (CoreCLR's IDS_CLASSLOAD_INLINE_ARRAY_EXPLICIT)"

            // CoreCLR rejects the type when the multiplied instance size overflows a field offset
            // (`extendedSize > FIELD_OFFSET_LAST_REAL_OFFSET`, methodtablebuilder.cpp:8616 and
            // :8669; the limit is `(1 <<< 27) - 8`, field.h:16 and :27). Check it here, *before*
            // materialising the slots: `[InlineArray(1_000_000_000)]` would otherwise ask for a
            // billion `CliField` records and take the interpreter down with an OOM — a silent kill
            // in place of the loud type-load rejection the guest should have seen.
            //
            // The product is computed from the element's own size rather than from the padded slot
            // size the layout will actually use, so it can only ever under-estimate; that makes
            // this strictly more permissive than CoreCLR, which is the safe direction. It is
            // computed in `int64` because the `int` product is exactly what would overflow.
            let elementSize = int64 (CliType.SizeOf single.Contents).Size
            let extendedSize = int64 repeat * elementSize

            if extendedSize > FIELD_OFFSET_LAST_REAL_OFFSET then
                failwith
                    $"[InlineArray(%d{repeat})] on %s{describeType ()}: %d{repeat} slots of %d{elementSize} bytes is %d{extendedSize} bytes, which exceeds the %d{FIELD_OFFSET_LAST_REAL_OFFSET}-byte limit on a field offset (CoreCLR's IDS_CLASSLOAD_FIELDTOOLARGE)"

            match single.Id with
            | FieldId.Metadata (declaringType, fieldHandle, name) ->
                List.init
                    repeat
                    (fun index ->
                        if index = 0 then
                            single
                        else
                            { single with
                                Id = FieldId.inlineArrayElement declaringType fieldHandle name index
                                Name = FieldId.inlineArrayElementName name index
                            }
                    )
            | other ->
                failwith
                    $"[InlineArray(%d{repeat})] on %s{describeType ()}: its single instance field has non-metadata identity %O{other}, so its storage repeats cannot be identified"
        | fields ->
            failwith
                $"[InlineArray(%d{repeat})] on %s{describeType ()}: declares %d{List.length fields} instance fields, but an inline array must declare exactly one (CoreCLR's IDS_CLASSLOAD_INLINE_ARRAY_FIELD_COUNT)"

[<RequireQualifiedAccess>]
module CliType =
    /// If `ty` is a primitive-like wrapper (IntPtr, RuntimeTypeHandle, an enum, ...) at rest,
    /// return the contents of its single underlying field; otherwise return `ty` unchanged.
    /// Used by consumers that read stored primitive-like fields and need to see the flattened
    /// primitive form (e.g. `RuntimeType.m_handle` as a `NativeInt (TypeHandlePtr ...)`).
    let unwrapPrimitiveLike (ty : CliType) : CliType =
        match ty with
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsSome -> (CliValueType.PrimitiveLikeField vt).Contents
        | _ -> ty

    /// Repeatedly unwrap primitive-like wrappers. This is needed at native
    /// method boundaries where CoreLib wraps a runtime pointer in more than one
    /// single-field value type, for example `RuntimeFieldHandleInternal` around
    /// `IntPtr`.
    let rec unwrapPrimitiveLikeDeep (ty : CliType) : CliType =
        match ty with
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsSome ->
            CliValueType.PrimitiveLikeField vt |> _.Contents |> unwrapPrimitiveLikeDeep
        | _ -> ty

    /// In fact any non-zero value will do for True, but we'll use 1
    let ofBool (b : bool) : CliType = CliType.Bool (if b then 1uy else 0uy)

    let ofChar (c : char) : CliType =
        CliType.Char (byte (int c / 256), byte (int c % 256))

    let ofManagedObject (ptr : ManagedHeapAddress) : CliType = CliType.ObjectRef (Some ptr)

    let sizeOf (ty : CliType) : int = CliType.SizeOf(ty).Size

    let containsObjectReferences (ty : CliType) : bool = CliType.ContainsObjectReferences ty

    /// Reconstruct a primitive `CliType` from its byte encoding, using
    /// `template` only for its shape (which primitive flavour to produce).
    /// Delegates to `CliType.OfBytesLike`; see the static member for details.
    let ofBytesLike (template : CliType) (bytes : byte[]) : CliType = CliType.OfBytesLike template bytes

    let ofSymbolicBytesLike (template : CliType) (bytes : UInt8Source[]) : CliType =
        CliType.OfSymbolicBytesLike template bytes

    let zeroOfPrimitive
        (concreteTypes : AllConcreteTypes)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (primitiveType : PrimitiveType)
        : CliType
        =
        match primitiveType with
        | PrimitiveType.Boolean -> CliType.Bool 0uy
        | PrimitiveType.Char -> CliType.Char (0uy, 0uy)
        | PrimitiveType.SByte -> CliType.Numeric (CliNumericType.Int8 0y)
        | PrimitiveType.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
        | PrimitiveType.Int16 -> CliType.Numeric (CliNumericType.Int16 0s)
        | PrimitiveType.UInt16 -> CliType.Numeric (CliNumericType.UInt16 0us)
        | PrimitiveType.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
        | PrimitiveType.UInt32 ->
            // uint32 doesn't exist; the spec has them stored on the stack as if signed, with two's complement wraparound
            CliType.Numeric (CliNumericType.Int32 0)
        | PrimitiveType.Int64 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | PrimitiveType.UInt64 ->
            // uint64 doesn't exist; the spec has them stored on the stack as if signed, with two's complement wraparound
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
        | PrimitiveType.Single -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | PrimitiveType.Double -> CliType.Numeric (CliNumericType.Float64 0.0)
        | PrimitiveType.String -> CliType.ObjectRef None
        | PrimitiveType.TypedReference -> failwith "todo"
        | PrimitiveType.IntPtr ->
            let intPtrHandle =
                AllConcreteTypes.findExistingNonGenericConcreteType concreteTypes corelib.IntPtr.Identity
                |> Option.get

            let valueField =
                corelib.IntPtr.Fields
                |> List.filter (fun field -> field.Name = "_value" && not field.IsStatic)
                |> List.exactlyOne

            {
                Id = FieldId.metadata intPtrHandle valueField.Handle valueField.Name
                Name = valueField.Name
                Contents =
                    CliType.Numeric (
                        CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
                    )
                Offset = None
                Type = intPtrHandle
                MarshallingDescriptor = valueField.MarshallingDescriptor
            }
            |> List.singleton
            |> CliValueType.OfFields
                corelib
                concreteTypes
                intPtrHandle
                (DeclaredTypeFacts.ofCorelibType corelib corelib.IntPtr)
            |> CliType.ValueType
        | PrimitiveType.UIntPtr ->
            let uintPtrHandle =
                AllConcreteTypes.findExistingNonGenericConcreteType concreteTypes corelib.UIntPtr.Identity
                |> Option.get

            let valueField =
                corelib.UIntPtr.Fields
                |> List.filter (fun field -> field.Name = "_value" && not field.IsStatic)
                |> List.exactlyOne

            {
                Id = FieldId.metadata uintPtrHandle valueField.Handle valueField.Name
                Name = valueField.Name
                Contents =
                    CliType.Numeric (
                        CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
                    )
                Offset = None
                Type = uintPtrHandle
                MarshallingDescriptor = valueField.MarshallingDescriptor
            }
            |> List.singleton
            |> CliValueType.OfFields
                corelib
                concreteTypes
                uintPtrHandle
                (DeclaredTypeFacts.ofCorelibType corelib corelib.UIntPtr)
            |> CliType.ValueType
        | PrimitiveType.Object -> CliType.ObjectRef None

    /// The zero value of the given type, as `initobj`/`newarr`/a fresh local sees it.
    ///
    /// This is PawPrint's type-layout builder, and like CoreCLR's MethodTable builder it needs
    /// the transitive closure of the type's *field* types, not just the type itself: laying out
    /// `struct S { Dep.External E; }` requires reading `Dep`, however the guest arrived at `S`.
    /// Hence the loader: the assembly holding a field's type is routinely one that nothing has
    /// yet had a reason to name, so a walk that could only read already-loaded assemblies would
    /// fail on perfectly ordinary programs (issue #868). The updated `LoadedAssemblies` comes
    /// back out alongside the updated `AllConcreteTypes`, and callers must thread *both*: the
    /// returned `CliType` embeds `ConcreteTypeHandle`s minted during the walk, which dangle if
    /// the registry is dropped.
    let rec zeroOf
        (loadAssembly : IAssemblyLoad)
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * AllConcreteTypes * LoadedAssemblies
        =
        zeroOfWithVisited loadAssembly concreteTypes assemblies corelib handle Set.empty

    and zeroOfWithVisited
        (loadAssembly : IAssemblyLoad)
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes * LoadedAssemblies
        =

        // Handle constructed types first
        match handle with
        | ConcreteTypeHandle.Byref _ ->
            // Byref types are managed references - the zero value is a null reference
            CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), concreteTypes, assemblies

        | ConcreteTypeHandle.Pointer _ ->
            // Pointer types are unmanaged pointers - the zero value is a null pointer
            CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), concreteTypes, assemblies

        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Array types are reference types - the zero value is null
            CliType.ObjectRef None, concreteTypes, assemblies

        | ConcreteTypeHandle.FunctionPointer _ ->
            // Function pointers are stored in a native-int slot: a non-null fnptr
            // is NativeIntSource.FunctionPointer carrying a MethodInfo, and the
            // null fnptr is the same shape with the canonical zero source.
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)), concreteTypes, assemblies

        | ConcreteTypeHandle.Concrete _ ->
            // This is a concrete type - look it up in the mapping
            let concreteType =
                match AllConcreteTypes.lookup handle concreteTypes with
                | Some ct -> ct
                | None -> failwithf "ConcreteTypeHandle %A not found in AllConcreteTypes" handle

            // Get the type definition from the assembly
            let assembly = assemblies.ByDefinitionName concreteType.AssemblyFullName
            let typeDef = assembly.TypeDefs.[concreteType.Definition.Get]

            // Check if it's a primitive type by comparing with corelib types FIRST
            if
                concreteType.AssemblyFullName = corelib.Corelib.DefinitionFullName
                && concreteType.Generics.IsEmpty
            then
                // Check against known primitive types
                if TypeInfo.NominallyEqual typeDef corelib.Boolean then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Boolean, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Char then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Char, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.SByte then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.SByte, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Byte then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Byte, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Int16 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int16, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.UInt16 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt16, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Int32 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int32, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.UInt32 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt32, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Int64 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int64, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.UInt64 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt64, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Single then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Single, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Double then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Double, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.String then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.String, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Object then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Object, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.IntPtr then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.IntPtr, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.UIntPtr then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UIntPtr, concreteTypes, assemblies
                elif TypeInfo.NominallyEqual typeDef corelib.Array then
                    // Arrays are reference types
                    CliType.ObjectRef None, concreteTypes, assemblies

                // Not a known primitive, now check for cycles
                // We're in a cycle - return a default zero value for the type
                // Value types can't be self-referential unless they are specifically known to the
                // runtime - for example, System.Byte is a value type with a single field,
                // of type System.Byte.
                // Since we check for (nominal) equality against all such types in the first branch,
                // this code path is only hit with reference types.
                else if Set.contains handle visited then
                    CliType.ObjectRef None, concreteTypes, assemblies
                else
                    let visited = Set.add handle visited
                    // Not a known primitive, check if it's a value type or reference type
                    determineZeroForCustomType
                        loadAssembly
                        concreteTypes
                        assemblies
                        corelib
                        handle
                        concreteType
                        typeDef
                        visited

            // Not from corelib or has generics
            // This is an array type, so null is appropriate
            else if
                concreteType.AssemblyFullName = corelib.Corelib.DefinitionFullName
                && TypeInfo.NominallyEqual typeDef corelib.Array
                && concreteType.Generics.Length = 1
            then
                CliType.ObjectRef None, concreteTypes, assemblies

            // Custom type - now check for cycles
            // We're in a cycle - return a default zero value for the type.
            // Value types can't be self-referential unless they are specifically known to the
            // runtime - for example, System.Byte is a value type with a single field,
            // of type System.Byte.
            // Since we check for (nominal) equality against all such types in the first branch,
            // this code path is only hit with reference types.
            else if Set.contains handle visited then
                CliType.ObjectRef None, concreteTypes, assemblies
            else
                let visited = Set.add handle visited
                // Custom type - need to determine if it's a value type or reference type
                determineZeroForCustomType
                    loadAssembly
                    concreteTypes
                    assemblies
                    corelib
                    handle
                    concreteType
                    typeDef
                    visited

    and private determineZeroForCustomType
        (loadAssembly : IAssemblyLoad)
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteType : ConcreteType<ConcreteTypeHandle>)
        (typeDef : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes * LoadedAssemblies
        =

        // `isValueType` walks the base-type chain and cannot load, so discharge its precondition
        // first. This matters precisely on the paths that made this function need a loader at
        // all: a field type we have only just read in may itself derive from a class in a third
        // assembly, and nothing before this point had any reason to bind that reference.
        let assemblies =
            Concretization.ensureTypeDefinitionBaseAssembliesLoaded
                loadAssembly
                assemblies
                (assemblies.ByDefinitionName concreteType.AssemblyFullName)
                concreteType.Definition.Get

        let isValueType = DumpedAssembly.isValueType corelib assemblies typeDef

        if isValueType then
            // It's a value type - need to create zero values for all non-static fields
            let mutable currentConcreteTypes = concreteTypes
            let mutable currentAssemblies = assemblies

            let vt =
                typeDef.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                |> List.map (fun field ->
                    // Need to concretize the field type with the concrete type's generics
                    let fieldTypeDefn = field.Signature

                    let fieldHandle, updatedConcreteTypes, updatedAssemblies =
                        concretizeFieldType
                            loadAssembly
                            currentConcreteTypes
                            currentAssemblies
                            corelib
                            concreteType
                            fieldTypeDefn

                    currentConcreteTypes <- updatedConcreteTypes
                    currentAssemblies <- updatedAssemblies

                    let fieldZero, updatedConcreteTypes2, updatedAssemblies2 =
                        zeroOfWithVisited
                            loadAssembly
                            currentConcreteTypes
                            currentAssemblies
                            corelib
                            fieldHandle
                            visited

                    currentConcreteTypes <- updatedConcreteTypes2
                    currentAssemblies <- updatedAssemblies2

                    {
                        Id = FieldId.metadata handle field.Handle field.Name
                        Name = field.Name
                        Contents = fieldZero
                        Offset = field.Offset
                        Type = fieldHandle
                        MarshallingDescriptor = field.MarshallingDescriptor
                    }
                )
                |> InlineArrayStorage.expand
                    (fun () -> $"%s{typeDef.Namespace}.%s{typeDef.Name}")
                    typeDef.Layout
                    (InlineArrayStorage.effectiveLength isValueType typeDef.InlineArrayLength)
                |> CliValueType.OfFields
                    corelib
                    currentConcreteTypes
                    handle
                    (DeclaredTypeFacts.ofTypeInfo corelib currentAssemblies typeDef)

            CliType.ValueType vt, currentConcreteTypes, currentAssemblies
        else
            // It's a reference type
            CliType.ObjectRef None, concreteTypes, assemblies

    and private concretizeFieldType
        (loadAssembly : IAssemblyLoad)
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (declaringType : ConcreteType<ConcreteTypeHandle>)
        (fieldType : TypeDefn)
        : ConcreteTypeHandle * AllConcreteTypes * LoadedAssemblies
        =

        // Create a concretization context
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = concreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = corelib
            }

        // The field type might reference generic parameters of the declaring type
        let methodGenerics = ImmutableArray.Empty // Fields don't have method generics

        let handle, newCtx =
            TypeConcretization.concretizeType
                ctx
                loadAssembly
                declaringType.AssemblyFullName
                declaringType.Generics
                methodGenerics
                fieldType

        handle, newCtx.ConcreteTypes, newCtx.LoadedAssemblies

    let withFieldSet (field : string) (value : CliType) (c : CliType) : CliType =
        match c with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.WithFieldSet field value cvt |> CliType.ValueType

    let withFieldSetById (field : FieldId) (value : CliType) (c : CliType) : CliType =
        match c with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.WithFieldSetById field value cvt |> CliType.ValueType

    let getField (field : string) (value : CliType) : CliType =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.DereferenceField field cvt

    let getFieldById (field : FieldId) (value : CliType) : CliType =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.DereferenceFieldById field cvt

    /// Read the cell a `CellPathsExactlyCovering` path names. An empty path is the value itself.
    let rec getCellAtPath (path : FieldId list) (value : CliType) : CliType =
        match path with
        | [] -> value
        | field :: rest -> getCellAtPath rest (getFieldById field value)

    /// Replace the cell a `CellPathsExactlyCovering` path names, rebuilding each enclosing value on
    /// the way back out so that nothing outside the cell's own extent is disturbed. An empty path
    /// replaces the value itself.
    let rec withCellAtPathSet (path : FieldId list) (cell : CliType) (value : CliType) : CliType =
        match path with
        | [] -> cell
        | field :: rest ->
            let child = getFieldById field value
            withFieldSetById field (withCellAtPathSet rest cell child) value

    /// Returns the offset and size.
    let getFieldLayout (field : string) (value : CliType) : int * int =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.GetFieldLayout field cvt

    /// Returns the offset and size.
    let getFieldLayoutById (field : FieldId) (value : CliType) : int * int =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.GetFieldLayoutById field cvt

    /// Returns None if there isn't *exactly* one field that starts there. This rules out some valid programs.
    let getFieldAt (offset : int) (value : CliType) : CliConcreteField option =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.FieldsAt offset cvt |> List.tryExactlyOne
