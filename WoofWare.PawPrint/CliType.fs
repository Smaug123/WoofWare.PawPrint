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
    /// CoreCLR genuinely rejects this shape from being marshalled as an unmanaged structure.
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
        | CliByteAddressabilityRejection.Int64SourceNotByteAddressable source ->
            $"int64 with non-byte-addressable provenance %O{source}"
        | CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _ ->
            "value type containing object references"
        | CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers _ -> "value type containing runtime pointers"
        | CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField (_, field, rejection) ->
            $"value type containing non-byte-addressable field %O{field}: %s{rejection.Description}"

type CliByteAddressability =
    | ByteAddressable
    | Rejected of CliByteAddressabilityRejection

    member this.Description : string =
        match this with
        | CliByteAddressability.ByteAddressable -> "byte-addressable"
        | CliByteAddressability.Rejected rejection -> $"rejected: %s{rejection.Description}"

[<RequireQualifiedAccess>]
module private ByteAddressabilityClassifier =
    let nativeIntSource (source : NativeIntSource) : CliByteAddressability =
        match source with
        | NativeIntSource.Verbatim _
        | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> CliByteAddressability.ByteAddressable
        | NativeIntSource.ManagedPointer _
        | NativeIntSource.FunctionPointer _
        | NativeIntSource.TypeHandlePtr _
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
        | NativeIntSource.WaitHandlePtr _
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
        | CliNumericType.UInt8 _
        | CliNumericType.UInt16 _
        | CliNumericType.Float32 _
        | CliNumericType.Float64 _ -> CliByteAddressability.ByteAddressable
        | CliNumericType.Int64 source -> int64Source source
        | CliNumericType.NativeInt source -> nativeIntSource source

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
    /// This is *not* a CLI type as such. I don't actually know its status. A value type is represented simply
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
        | CliType.Numeric (CliNumericType.UInt8 _) -> CliType.Numeric (CliNumericType.UInt8 bytes.[0])
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
                | CliNumericType.UInt8 _ -> CliNumericType.UInt8 0uy
                | CliNumericType.Int16 _ -> CliNumericType.Int16 0s
                | CliNumericType.UInt16 _ -> CliNumericType.UInt16 0us
                | CliNumericType.Int32 _ -> CliNumericType.Int32 0
                | CliNumericType.Int64 _ -> CliNumericType.Int64 (Int64Source.Verbatim 0L)
                | CliNumericType.Float32 _ -> CliNumericType.Float32 0.0f
                | CliNumericType.Float64 _ -> CliNumericType.Float64 0.0
                | CliNumericType.NativeFloat _ -> CliNumericType.NativeFloat 0.0
                // Provenance is deliberately dropped: the result is the numeric zero, and a
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

    /// Did zeroing actually change anything? Deliberately not `=`: structural equality on
    /// floats follows IEEE, so it calls `-0.0` equal to `0.0` even though they differ in every
    /// byte that matters. Zeroing a cell holding `-0.0` really does change memory, and
    /// reporting "unchanged" would leave the sign bit set. Where a byte rendering exists it is
    /// the ground truth; where it does not (references, provenance-carrying native ints) there
    /// is no such subtlety and structural equality is exactly right.
    static member internal ZeroingChangedAnything (before : CliType) (after : CliType) : bool =
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

    /// Return a byte-addressable CLI value with the requested byte range replaced, or `None` if
    /// the materialised byte image would be unchanged. Value types delegate to
    /// `CliValueType.WithBytesAtIfChanged`, so represented padding and overlapping-field
    /// provenance stay within the value-layout model.
    static member WithBytesAtIfChanged (offset : int) (bytes : byte[]) (value : CliType) : CliType option =
        match CliType.ByteAddressability value with
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
            /// enum (detected structurally by the reserved `value__` field at offset 0). `None`
            /// for user-defined structs and non-primitive BCL structs. Populated at construction
            /// time so the context-free `EvalStackValue.ofCliType` can flatten without threading
            /// `BaseClassTypes`/`AllConcreteTypes` through every push site.
            _PrimitiveLikeKind : PrimitiveLikeKind option
            _Storage : CliValueTypeStorage
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

    /// Structural detection of CLR enums: exactly one instance field at offset 0 named `value__`
    /// with an integral underlying type. The `value__` name is CLR-reserved for enums, so this
    /// matches the nominal "has base type `System.Enum`" check without threading assembly lookup
    /// through every construction site.
    static member private IsEnumStructural (fields : CliConcreteField list) : bool =
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

    /// Combine the nominal BCL-wrapper classification with the structural enum detection.
    /// Returns the BCL kind if `declared` is one of the wrapper structs; otherwise returns
    /// `Some EnumLike` if `fields` has the structural shape of a CLR enum; otherwise `None`.
    static member private ClassifyPrimitiveLike
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (fields : CliConcreteField list)
        : PrimitiveLikeKind option
        =
        match PrimitiveLikeStruct.kindFromHandle bct allCt declared with
        | Some k -> Some k
        | None ->
            if CliValueType.IsEnumStructural fields then
                Some PrimitiveLikeKind.EnumLike
            else
                None

    static member private ComputeConcreteFields (layout : Layout) (fields : CliField list) : CliConcreteField list =
        // Minimum size only matters for `sizeof` computation
        let _minimumSize, packingSize =
            match layout with
            | Layout.Custom (size = size ; packingSize = packing) ->
                size, if packing = 0 then DEFAULT_STRUCT_ALIGNMENT else packing
            | Layout.Default -> 0, DEFAULT_STRUCT_ALIGNMENT

        let seqFields, nonSeqFields =
            fields |> List.partition (fun field -> field.Offset.IsNone)

        match seqFields, nonSeqFields with
        | [], [] -> []
        | _ :: _, [] ->
            // Sequential layout: compute offsets respecting alignment
            let _, concreteFields =
                ((0, []), seqFields)
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

            List.rev concreteFields

        | [], _ :: _ ->
            // Explicit layout: use provided offsets
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

        | _ :: _, _ :: _ -> failwith "unexpectedly mixed explicit and automatic layout of fields"

    static member private SizeOfFieldStorage (layout : Layout) (fields : CliConcreteField list) : SizeofResult =
        let minimumSize, packingSize =
            match layout with
            | Layout.Custom (size = size ; packingSize = packing) ->
                size, if packing = 0 then DEFAULT_STRUCT_ALIGNMENT else packing
            | Layout.Default -> 0, DEFAULT_STRUCT_ALIGNMENT

        if fields.IsEmpty then
            {
                Size = minimumSize
                Alignment = 1
            }
        else
            let finalOffset, alignment =
                fields
                |> List.fold
                    (fun (maxEnd, maxAlign) field ->
                        let fieldEnd = field.Offset + field.Size
                        let alignmentCap = min field.Alignment packingSize
                        max maxEnd fieldEnd, max maxAlign alignmentCap
                    )
                    (0, 0)

            let error = finalOffset % alignment

            let size =
                if error = 0 then
                    finalOffset
                else
                    finalOffset + (alignment - error)

            {
                Size = max size minimumSize
                Alignment = alignment
            }

    static member private StorageFromFields (layout : Layout) (fields : CliConcreteField list) : CliValueTypeStorage =
        match fields, layout with
        | [], Layout.Custom (size = size) when size > 0 -> CliValueTypeStorage.RawBytes (Array.zeroCreate<byte> size)
        | _ ->
            let size = CliValueType.SizeOfFieldStorage layout fields

            CliValueTypeStorage.Fields
                {
                    Fields = fields
                    PreservedBytes = Array.zeroCreate<byte> size.Size
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
    /// Only fields that overlap the requested range are serialised. A disjoint field cannot
    /// affect these bytes by construction, and may have no byte rendering at all — `CliType.ToBytes`
    /// refuses to express an object reference or a provenance-carrying native int — so rendering
    /// the whole value first would make a perfectly answerable slice fail because of a field it
    /// does not cover. Overlapping fields are replayed in the same `EditedAtTime` order `ToBytes`
    /// uses, so the two agree byte for byte wherever `ToBytes` succeeds.
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

    /// Return a value with the requested byte range replaced, or `None` if the requested write
    /// would not change the materialised byte image. Returning `None` preserves field provenance
    /// and the next timestamp explicitly; changed writes use the existing value as the
    /// shape/provenance template and intentionally canonicalise overlapping-field replay order
    /// the same way `OfBytesLike` does.
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
    ///
    /// Walks fields structurally rather than going through `ToBytes`, which is the whole point:
    /// `ToBytes` materialises *every* field, so it cannot render a struct that holds a live
    /// object reference, even when the requested range covers only plain fields.
    ///
    /// Field write timestamps are deliberately preserved, not refreshed. `ToBytes` replays
    /// overlapping fields in timestamp order, and a field that only partially overlaps the
    /// requested range extends outside it, so promoting it to "newest" would let its untouched
    /// bytes win over a sibling and change memory outside the range. Keeping the original order
    /// is safe both ways: inside the range every intersecting field has had its covered bytes
    /// zeroed, so whichever wins writes zeros, and outside it nothing about the order changed.
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

                        // `EditedAtTime` is deliberately left alone. `ToBytes` replays
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

    static member OfFields
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (layout : Layout)
        (charSet : CharSet)
        (f : CliField list)
        : CliValueType
        =
        let fields = CliValueType.ComputeConcreteFields layout f

        {
            _Declared = declared
            _PrimitiveLikeKind = CliValueType.ClassifyPrimitiveLike bct allCt declared fields
            _Storage = CliValueType.StorageFromFields layout fields
            Layout = layout
            CharSet = charSet
            NextTimestamp = 1UL
        }

    /// Rebuild with the same declared type and primitive-like classification as `source`. Used by
    /// the eval-stack rewrap path, which pops an already-classified value and reconstructs its
    /// stored form without needing `BaseClassTypes`/`AllConcreteTypes` in scope.
    /// This intentionally drops preserved bytes: do not call it for values whose padding or
    /// fixed-buffer trailing storage must be preserved.
    static member OfFieldsLike (source : CliValueType) (layout : Layout) (f : CliField list) : CliValueType =
        if not (CliValueType.IsTightlyPacked source) then
            failwith
                $"CliValueType.OfFieldsLike: refusing to drop preserved bytes for non-tightly-packed value type %O{source.Declared}"

        let fields = CliValueType.ComputeConcreteFields layout f

        {
            _Declared = source._Declared
            _PrimitiveLikeKind = source._PrimitiveLikeKind
            _Storage = CliValueType.StorageFromFields layout fields
            Layout = layout
            CharSet = source.CharSet
            NextTimestamp = 1UL
        }

    static member private FindFieldById (field : FieldId) (cvt : CliValueType) : CliConcreteField =
        let fields = CliValueType.FieldStorage "CliValueType.FindFieldById" cvt

        let exactMatches = fields |> List.filter (fun f -> FieldId.exactlyEqual field f.Id)

        match exactMatches with
        | [ f ] -> f
        | _ :: _ :: _ -> failwith $"Field '%O{field}' matched multiple storage slots exactly"
        | [] ->
            match field with
            | FieldId.Metadata _ -> failwith $"Field '%O{field}' not found"
            | FieldId.Named name ->
                let nameMatches = fields |> List.filter (fun f -> f.Name = name)

                match nameMatches with
                | [ f ] -> f
                | [] -> failwith $"Field '%O{field}' not found"
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

    static member DereferenceFieldAt (offset : int) (size : int) (cvt : CliValueType) : CliType =
        let candidates = CliValueType.FieldsAt offset cvt

        match candidates |> List.tryFind (fun f -> f.Size = size) with
        | Some targetField ->
            // Explicit layout can alias the requested range with other fields, and
            // `WithFieldSetById` deliberately leaves those siblings' `Contents` stale, recording
            // which write won in `EditedAtTime`. Picking a cell by (offset, size) alone would
            // therefore hand back a value the storage no longer holds.
            //
            // `ToBytes` decides that contest by replaying overlapping fields in `EditedAtTime`
            // order, so the *last* field in that same order owns every byte it covers. When that
            // winner spans the requested range exactly, its cell is authoritative and can be
            // returned directly — which is what keeps provenance the byte image cannot express
            // (runtime pointers, handle-valued native ints, widened native ints) alive across the
            // read. Only a winner that partially covers the range genuinely needs the byte image.
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

    static member SizeOf (vt : CliValueType) : SizeofResult =
        match vt._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            {
                Size = bytes.Length
                Alignment = 1
            }
        | CliValueTypeStorage.Fields storage -> CliValueType.SizeOfFieldStorage vt.Layout storage.Fields

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
            let firstRejectedField =
                storage.Fields
                |> List.tryPick (fun field ->
                    match CliType.ByteAddressability field.Contents with
                    | CliByteAddressability.ByteAddressable -> None
                    | CliByteAddressability.Rejected rejection -> Some (field, rejection)
                )

            match firstRejectedField with
            | None -> CliByteAddressability.ByteAddressable
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
                // Object/reference containment keeps the historical outer-type
                // rejection above. Every other rejection means a field's own
                // byte renderer would fail, so preserve that nested reason
                // instead of collapsing it to a coarse containment predicate.
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

    /// True iff the given handle refers to `System.String`. CoreCLR only accepts
    /// `[MarshalAs(ByValTStr)]` on string-typed fields, so we use this as the shape guard.
    static member private IsStringFieldType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle concreteTypes with
            | None -> false
            | Some concreteType ->
                if
                    concreteType.Assembly.FullName = corelib.Corelib.Name.FullName
                    && concreteType.Generics.IsEmpty
                then
                    let typeDef =
                        assemblies.[concreteType.Assembly].TypeDefs.[concreteType.Definition.Get]

                    TypeInfo.NominallyEqual typeDef corelib.String
                else
                    false
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false

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
        match vt._Declared with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup vt._Declared concreteTypes with
            | None -> false
            | Some concreteType ->
                if
                    concreteType.Assembly.FullName = corelib.Corelib.Name.FullName
                    && concreteType.Generics.IsEmpty
                then
                    let typeDef =
                        assemblies.[concreteType.Assembly].TypeDefs.[concreteType.Definition.Get]

                    TypeInfo.NominallyEqual typeDef corelib.DateTime
                else
                    false
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false

    /// True iff `vt`'s declared type is `System.Decimal`. CoreCLR's `MarshalInfo` routes a
    /// Decimal-typed *field* through marshal-stub synthesis (`NFT_DECIMAL` in
    /// `fieldmarshaler.cpp`) rather than treating it as memmove-blittable: managed `Decimal`
    /// is 16 bytes with 4-byte field alignment, but native `DECIMAL` is 16 bytes with 8-byte
    /// alignment (its `Lo64` union member is `ULONGLONG`), so a sequential outer struct
    /// containing a `Decimal` field is laid out differently managed vs native. Structurally,
    /// `Decimal` looks like a plain sequential struct of four `Int32` fields, so PawPrint can't
    /// distinguish it without a nominal name match. This predicate is intended for the
    /// **field-level** rejection inside `MarshalNative_TryGetStructMarshalStub`'s classifier;
    /// it deliberately does not gate `Marshal.SizeOf<Decimal>()` or top-level
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
        match vt._Declared with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup vt._Declared concreteTypes with
            | None -> false
            | Some concreteType ->
                if
                    concreteType.Assembly.FullName = corelib.Corelib.Name.FullName
                    && concreteType.Generics.IsEmpty
                then
                    let typeDef =
                        assemblies.[concreteType.Assembly].TypeDefs.[concreteType.Definition.Get]

                    TypeInfo.NominallyEqual typeDef corelib.Decimal
                else
                    false
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false

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
                match assemblies.TryByDefinition concreteType.Assembly with
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

    /// Compute the unmanaged size of a value type as `Marshal.SizeOf` would. Lays fields out
    /// using the declaring type's `Layout` (sequential or explicit) and packing, but with each
    /// field sized via `TryFieldMarshalSize` so `[MarshalAs(ByValTStr/ByValArray)]` fields
    /// contribute their unmanaged byte cost rather than the managed CLI size. Type-system
    /// context is required so descriptors that depend on the field's nominal type (e.g.
    /// `ByValTStr` requires `System.String`) can be validated.
    static member TryComputeMarshalSize
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        : Result<SizeofResult, MarshalSizeError>
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
            Result.Ok
                {
                    Size = bytes.Length
                    Alignment = 1
                }
        | CliValueTypeStorage.Fields storage ->
            let minimumSize, packingSize =
                match vt.Layout with
                | Layout.Custom (size = size ; packingSize = packing) ->
                    size, if packing = 0 then DEFAULT_STRUCT_ALIGNMENT else packing
                | Layout.Default -> 0, DEFAULT_STRUCT_ALIGNMENT

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

            let computeFinal (currentEnd : int) (maxAlign : int) : SizeofResult =
                let alignment = max maxAlign 1
                let error = currentEnd % alignment

                let totalSize =
                    if error = 0 then
                        currentEnd
                    else
                        currentEnd + (alignment - error)

                bumpZeroSized
                    {
                        Size = max totalSize minimumSize
                        Alignment = alignment
                    }

            let seqFields, nonSeqFields =
                storage.Fields |> List.partition (fun field -> field.ConfiguredOffset.IsNone)

            match seqFields, nonSeqFields with
            | [], [] ->
                Result.Ok (
                    bumpZeroSized
                        {
                            Size = minimumSize
                            Alignment = 1
                        }
                )
            | _ :: _, [] ->
                (Result.Ok (0, 0), seqFields)
                ||> List.fold (fun acc field ->
                    match acc with
                    | Result.Error _ -> acc
                    | Result.Ok (currentOffset, maxAlign) ->
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

                            let alignedOffset =
                                if alignmentCap = 0 then
                                    currentOffset
                                else
                                    let err = currentOffset % alignmentCap

                                    if err = 0 then
                                        currentOffset
                                    else
                                        currentOffset + (alignmentCap - err)

                            Result.Ok (alignedOffset + size.Size, max maxAlign alignmentCap)
                )
                |> Result.map (fun (off, align) -> computeFinal off align)
            | [], _ :: _ ->
                (Result.Ok (0, 0), nonSeqFields)
                ||> List.fold (fun acc field ->
                    match acc with
                    | Result.Error _ -> acc
                    | Result.Ok (maxEnd, maxAlign) ->
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
                            let fieldEnd = field.Offset + size.Size
                            Result.Ok (max maxEnd fieldEnd, max maxAlign alignmentCap)
                )
                |> Result.map (fun (off, align) -> computeFinal off align)
            | _ :: _, _ :: _ ->
                MarshalSizeError.NotMarshalable "unexpectedly mixed explicit and automatic field offsets"
                |> Result.Error

    /// Sets the value of the specified field, *without* touching any overlapping fields.
    /// `DereferenceField` handles resolving conflicts between overlapping fields.
    static member WithFieldSetById (field : FieldId) (value : CliType) (cvt : CliValueType) : CliValueType =
        let targetField = CliValueType.FindFieldById field cvt

        let storage = CliValueType.FieldBackedStorage "CliValueType.WithFieldSetById" cvt

        {
            _Declared = cvt._Declared
            _PrimitiveLikeKind = cvt._PrimitiveLikeKind
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
                    // Preserved bytes intentionally remain the prior byte image. `ToBytes` overlays
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
                _Storage = CliValueTypeStorage.RawBytes (Array.copy sourceBytes)
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
                _Storage =
                    CliValueTypeStorage.Fields
                        {
                            Fields = merged
                            PreservedBytes = Array.copy sourceStorage.PreservedBytes
                        }
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
    ///
    /// Field write timestamps are replayed in declaration order, as `OfBytesLike` does: a
    /// zeroed struct has no meaningful overlapping-field write history to preserve, and every
    /// order produces the same all-zero result anyway.
    static member ZeroLike (template : CliValueType) : CliValueType =
        match template._Storage with
        | CliValueTypeStorage.RawBytes bytes ->
            { template with
                _Storage = CliValueTypeStorage.RawBytes (Array.zeroCreate bytes.Length)
            }
        | CliValueTypeStorage.Fields storage ->
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
                    _Storage = CliValueTypeStorage.RawBytes (Array.copy bytes)
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
                        _Storage =
                            CliValueTypeStorage.Fields
                                {
                                    Fields = fields
                                    PreservedBytes = Array.copy bytes
                                }
                        Layout = template.Layout
                        CharSet = template.CharSet
                        NextTimestamp = max 1UL (uint64 fields.Length)
                    }

                result

        valueTypeOfBytesLike template bytes

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
        | PrimitiveType.Byte -> CliType.Numeric (CliNumericType.UInt8 0uy)
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
                Layout.Default
                (CharSetMetadata.ofTypeAttributes corelib.IntPtr.TypeAttributes)
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
                Layout.Default
                (CharSetMetadata.ofTypeAttributes corelib.UIntPtr.TypeAttributes)
            |> CliType.ValueType
        | PrimitiveType.Object -> CliType.ObjectRef None

    let rec zeroOf
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * AllConcreteTypes
        =
        zeroOfWithVisited concreteTypes assemblies corelib handle Set.empty

    and zeroOfWithVisited
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes
        =

        // Handle constructed types first
        match handle with
        | ConcreteTypeHandle.Byref _ ->
            // Byref types are managed references - the zero value is a null reference
            CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), concreteTypes

        | ConcreteTypeHandle.Pointer _ ->
            // Pointer types are unmanaged pointers - the zero value is a null pointer
            CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), concreteTypes

        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Array types are reference types - the zero value is null
            CliType.ObjectRef None, concreteTypes

        | ConcreteTypeHandle.FunctionPointer _ ->
            // Function pointers are stored in a native-int slot: a non-null fnptr
            // is NativeIntSource.FunctionPointer carrying a MethodInfo, and the
            // null fnptr is the same shape with the canonical zero source.
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)), concreteTypes

        | ConcreteTypeHandle.Concrete _ ->
            // This is a concrete type - look it up in the mapping
            let concreteType =
                match AllConcreteTypes.lookup handle concreteTypes with
                | Some ct -> ct
                | None -> failwithf "ConcreteTypeHandle %A not found in AllConcreteTypes" handle

            // Get the type definition from the assembly
            let assembly = assemblies.[concreteType.Assembly]
            let typeDef = assembly.TypeDefs.[concreteType.Definition.Get]

            // Check if it's a primitive type by comparing with corelib types FIRST
            if
                concreteType.Assembly.FullName = corelib.Corelib.Name.FullName
                && concreteType.Generics.IsEmpty
            then
                // Check against known primitive types
                if TypeInfo.NominallyEqual typeDef corelib.Boolean then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Boolean, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Char then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Char, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.SByte then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.SByte, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Byte then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Byte, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int16 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int16, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt16 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt16, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int32 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int32, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt32 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt32, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int64 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Int64, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt64 then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UInt64, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Single then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Single, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Double then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Double, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.String then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.String, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Object then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.Object, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.IntPtr then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.IntPtr, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UIntPtr then
                    zeroOfPrimitive concreteTypes corelib PrimitiveType.UIntPtr, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Array then
                    // Arrays are reference types
                    CliType.ObjectRef None, concreteTypes

                // Not a known primitive, now check for cycles
                // We're in a cycle - return a default zero value for the type
                // Value types can't be self-referential unless they are specifically known to the
                // runtime - for example, System.Byte is a value type with a single field,
                // of type System.Byte.
                // Since we check for (nominal) equality against all such types in the first branch,
                // this code path is only hit with reference types.
                else if Set.contains handle visited then
                    CliType.ObjectRef None, concreteTypes
                else
                    let visited = Set.add handle visited
                    // Not a known primitive, check if it's a value type or reference type
                    determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited

            // Not from corelib or has generics
            // This is an array type, so null is appropriate
            else if
                concreteType.Assembly.FullName = corelib.Corelib.Name.FullName
                && TypeInfo.NominallyEqual typeDef corelib.Array
                && concreteType.Generics.Length = 1
            then
                CliType.ObjectRef None, concreteTypes

            // Custom type - now check for cycles
            // We're in a cycle - return a default zero value for the type.
            // Value types can't be self-referential unless they are specifically known to the
            // runtime - for example, System.Byte is a value type with a single field,
            // of type System.Byte.
            // Since we check for (nominal) equality against all such types in the first branch,
            // this code path is only hit with reference types.
            else if Set.contains handle visited then
                CliType.ObjectRef None, concreteTypes
            else
                let visited = Set.add handle visited
                // Custom type - need to determine if it's a value type or reference type
                determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited

    and private determineZeroForCustomType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteType : ConcreteType<ConcreteTypeHandle>)
        (typeDef : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes
        =

        let isValueType = DumpedAssembly.isValueType corelib assemblies typeDef

        if isValueType then
            // It's a value type - need to create zero values for all non-static fields
            let mutable currentConcreteTypes = concreteTypes

            let vt =
                typeDef.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                |> List.map (fun field ->
                    // Need to concretize the field type with the concrete type's generics
                    let fieldTypeDefn = field.Signature

                    let fieldHandle, updatedConcreteTypes =
                        concretizeFieldType currentConcreteTypes assemblies corelib concreteType fieldTypeDefn

                    currentConcreteTypes <- updatedConcreteTypes

                    let fieldZero, updatedConcreteTypes2 =
                        zeroOfWithVisited currentConcreteTypes assemblies corelib fieldHandle visited

                    currentConcreteTypes <- updatedConcreteTypes2

                    {
                        Id = FieldId.metadata handle field.Handle field.Name
                        Name = field.Name
                        Contents = fieldZero
                        Offset = field.Offset
                        Type = fieldHandle
                        MarshallingDescriptor = field.MarshallingDescriptor
                    }
                )
                |> CliValueType.OfFields
                    corelib
                    currentConcreteTypes
                    handle
                    typeDef.Layout
                    (CharSetMetadata.ofTypeAttributes typeDef.TypeAttributes)

            CliType.ValueType vt, currentConcreteTypes
        else
            // It's a reference type
            CliType.ObjectRef None, concreteTypes

    and private concretizeFieldType
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (declaringType : ConcreteType<ConcreteTypeHandle>)
        (fieldType : TypeDefn)
        : ConcreteTypeHandle * AllConcreteTypes
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

        let loadAssembly = IAssemblyLoad.alreadyLoadedOnly

        let handle, newCtx =
            TypeConcretization.concretizeType
                ctx
                loadAssembly
                declaringType.Assembly
                declaringType.Generics
                methodGenerics
                fieldType

        handle, newCtx.ConcreteTypes

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
