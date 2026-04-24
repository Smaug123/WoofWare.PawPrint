namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open Checked

/// Source:
/// Table I.6: Data Types Directly Supported by the CLI
type CliSupportedObject =
    /// Can be assigned the null value 0
    /// This is the 'O' type.
    | ObjectReference of ManagedHeapAddress option
    /// This is the '&' type. It can point to managed or unmanaged memory.
    /// TODO: the contents of this are therefore wrong
    | PointerType of ManagedHeapAddress option
    | Int8 of int8
    | UInt8 of uint8
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Float32 of float32
    | Float64 of float
    | NativeInt of int64
    | NativeUint of uint64

/// Defined in III.1.1
type BasicCliType =
    | ObjectReference of ManagedHeapAddress option
    | PointerType of ManagedHeapAddress option
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | NativeFloat of float

/// The root storage location that a managed pointer points into.
[<NoComparison>]
type ByrefRoot =
    /// Address of a local variable slot on the stack.
    | LocalVariable of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a method argument slot on the stack.
    | Argument of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a whole value stored in heap-backed storage.
    /// Used for boxed value-type storage and constructor `this` for value types.
    | HeapValue of obj : ManagedHeapAddress
    /// Address of a named field within a heap-allocated object.
    /// Created by `ldflda` on an ObjectRef.
    | HeapObjectField of obj : ManagedHeapAddress * fieldName : string
    /// Address of an indexed element within a heap-allocated array.
    /// Created by `ldelema`.
    | ArrayElement of arr : ManagedHeapAddress * index : int

/// A navigation step applied after reaching the byref root.
[<NoComparison>]
type ByrefProjection =
    /// Navigate to a named field within the current value.
    /// Created by `ldflda` on an existing managed pointer.
    | Field of fieldName : string
    /// Reinterpret the pointed-to value as a different type.
    /// Created by `Unsafe.As`.
    | ReinterpretAs of ConcreteType<ConcreteTypeHandle>
    /// Byte offset accumulated under a trailing `ReinterpretAs` projection by
    /// pointer arithmetic. Only appears as the final element of the projection
    /// list, and only when immediately preceded by a `ReinterpretAs`. Interior
    /// code relies on this invariant: any `ByteOffset` found elsewhere in the
    /// list is a bug.
    | ByteOffset of byteOffset : int

/// A managed pointer (byref / CLI `&` type).
/// Points at a storage location, not at an object.
[<NoComparison>]
type ManagedPointerSource =
    | Null
    | Byref of root : ByrefRoot * projections : ByrefProjection list

    override this.ToString () =
        let formatProj acc proj =
            match proj with
            | ByrefProjection.Field name -> $"<field %s{name} of {acc}>"
            | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
            | ByrefProjection.ByteOffset n -> $"<{acc} + %d{n} bytes>"

        match this with
        | ManagedPointerSource.Null -> "<null managed pointer>"
        | ManagedPointerSource.Byref (root, projs) ->
            let rootStr =
                match root with
                | ByrefRoot.LocalVariable (source, method, var) ->
                    $"<variable %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.Argument (source, method, var) ->
                    $"<argument %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.HeapValue addr -> $"<heap value %O{addr}>"
                | ByrefRoot.HeapObjectField (addr, fieldName) -> $"<field %s{fieldName} of heap object %O{addr}>"
                | ByrefRoot.ArrayElement (arr, index) -> $"<element %i{index} of array %O{arr}>"

            projs |> List.fold formatProj rootStr

[<RequireQualifiedAccess>]
module ManagedPointerSource =
    let appendProjection (projection : ByrefProjection) (src : ManagedPointerSource) : ManagedPointerSource =
        match src with
        | ManagedPointerSource.Null -> failwith "cannot project from null managed pointer"
        | ManagedPointerSource.Byref (root, projs) ->
            // ReinterpretAs is address-preserving: it changes only the type view, not the byte offset.
            // So consecutive ReinterpretAs projections collapse to the most recent one; any trailing
            // ByteOffset (an accumulated cursor under a prior reinterpret) is reset along with the
            // reinterpret it qualified.
            let newProjs =
                match projection, List.rev projs with
                | ByrefProjection.ReinterpretAs _,
                  ByrefProjection.ByteOffset n :: (ByrefProjection.ReinterpretAs _) :: revRest ->
                    // Replacing the type view leaves the byte cursor alone: the
                    // reinterpret is address-preserving, so the caller is still
                    // at the same byte. Preserve the `ByteOffset` under the new
                    // reinterpret.
                    List.rev revRest @ [ projection ; ByrefProjection.ByteOffset n ]
                | ByrefProjection.ReinterpretAs _, (ByrefProjection.ReinterpretAs _) :: revRest ->
                    List.rev revRest @ [ projection ]
                | ByrefProjection.ByteOffset n, ByrefProjection.ByteOffset m :: revRest ->
                    if n = -m then
                        List.rev revRest
                    else
                        List.rev revRest @ [ ByrefProjection.ByteOffset (m + n) ]
                | ByrefProjection.ByteOffset 0, _ -> projs
                | ByrefProjection.ByteOffset _, ByrefProjection.ReinterpretAs _ :: _ -> projs @ [ projection ]
                | ByrefProjection.ByteOffset n, _ ->
                    failwith
                        $"cannot append ByteOffset %d{n} to projection list without a trailing ReinterpretAs: %O{src}"
                | _ -> projs @ [ projection ]

            ManagedPointerSource.Byref (root, newProjs)

    /// Fold whole-cell byte offsets of an array-rooted byref into the cell index,
    /// keeping the remaining in-cell offset in `[0, cellSize)`. `cellSizeOf`
    /// resolves an array heap address to its stored element byte size. Callers
    /// invoke this after any arithmetic that appends a `ByteOffset` so that
    /// byrefs which denote the same byte location share one structural form —
    /// keeping `Unsafe.AreSame`, `ceq`, and `stripTrailingReinterprets`
    /// equality-based.
    let normaliseArrayByteOffset
        (cellSizeOf : ManagedHeapAddress -> int)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        match src with
        | ManagedPointerSource.Null -> src
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs ty :: rest ->
                let cellSize = cellSizeOf arr

                if cellSize <= 0 then
                    src
                else
                    // Floor-division so negatives land in `[0, cellSize)`.
                    let cellAdvance =
                        let q = n / cellSize
                        let r = n - q * cellSize
                        if r < 0 then q - 1 else q

                    let newOffset = n - cellAdvance * cellSize
                    let newCell = i + cellAdvance
                    let prefix = List.rev rest

                    let tail =
                        if newOffset = 0 then
                            [ ByrefProjection.ReinterpretAs ty ]
                        else
                            [ ByrefProjection.ReinterpretAs ty ; ByrefProjection.ByteOffset newOffset ]

                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, newCell), prefix @ tail)
            | _ -> src
        | _ -> src

    /// Drop any trailing address-preserving `ReinterpretAs` projections so that two
    /// byrefs reaching the same byte location by different type-view paths compare
    /// equal. A `ReinterpretAs` followed by a `Field` must stay: field resolution
    /// depends on the reinterpreted type's layout, so it is no longer purely
    /// address-preserving in that case. A trailing `ByteOffset` DOES change the
    /// byte address and is preserved; a trailing `ByteOffset 0` is stripped as a
    /// no-op, and the reinterpret it qualified then becomes strippable.
    let rec stripTrailingReinterprets (src : ManagedPointerSource) : ManagedPointerSource =
        match src with
        | ManagedPointerSource.Null -> src
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset 0 :: revRest ->
                stripTrailingReinterprets (ManagedPointerSource.Byref (root, List.rev revRest))
            | ByrefProjection.ReinterpretAs _ :: revRest ->
                stripTrailingReinterprets (ManagedPointerSource.Byref (root, List.rev revRest))
            | _ -> src

    /// True when a byref source carries a non-trailing `ReinterpretAs`
    /// projection (i.e. a reinterpret followed by a Field). Such projections
    /// would need a bytewise layout comparison — `ref a.X` vs
    /// `ref Unsafe.As<A,B>(ref a).X` can alias despite having different
    /// projection chains — and we don't yet model that. Callers that compare
    /// byrefs structurally use this to refuse the comparison rather than
    /// silently returning a potentially-wrong answer.
    let hasNonTrailingReinterpret (src : ManagedPointerSource) : bool =
        match src with
        | ManagedPointerSource.Null -> false
        | ManagedPointerSource.Byref (_, projs) ->
            let stripped =
                projs
                |> List.rev
                |> List.skipWhile (fun p ->
                    match p with
                    | ByrefProjection.ReinterpretAs _
                    | ByrefProjection.ByteOffset _ -> true
                    | _ -> false
                )

            stripped
            |> List.exists (fun p ->
                match p with
                | ByrefProjection.ReinterpretAs _ -> true
                | _ -> false
            )

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource

[<RequireQualifiedAccess>]
type NativeIntSource =
    | Verbatim of int64
    | ManagedPointer of ManagedPointerSource
    | FunctionPointer of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    | TypeHandlePtr of ConcreteTypeHandle
    | MethodTablePtr of ConcreteTypeHandle
    | FieldHandlePtr of int64
    | AssemblyHandle of string
    /// Synthetic byte delta returned by `Unsafe.ByteOffset` or managed-pointer
    /// subtraction for two byrefs into distinct arrays. We don't model heap
    /// addresses as integers, so the value is a deterministic sentinel large
    /// enough to defeat the unsigned overlap check `(nuint)offset < len` used by
    /// Memmove. The tag exists so downstream arithmetic (add/sub with anything
    /// non-zero) fails loudly rather than silently composing into a wrong answer;
    /// comparisons and Conv.U/Conv.I treat the payload as if it were a regular
    /// `Verbatim`.
    | SyntheticCrossArrayOffset of int64

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.ManagedPointer ptr -> $"<managed pointer {ptr}>"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"
        | NativeIntSource.TypeHandlePtr ptr -> $"<type ID %O{ptr}>"
        | NativeIntSource.MethodTablePtr ptr -> $"<method table for type %O{ptr}>"
        | NativeIntSource.FieldHandlePtr ptr -> $"<field ID %O{ptr}>"
        | NativeIntSource.AssemblyHandle name -> $"<assembly %s{name}>"
        | NativeIntSource.SyntheticCrossArrayOffset i -> $"<synthetic cross-array byte offset %i{i}>"

[<RequireQualifiedAccess>]
module NativeIntSource =
    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.SyntheticCrossArrayOffset i -> i = 0L
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.AssemblyHandle _ -> false
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> true
            | _ -> false

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.SyntheticCrossArrayOffset i -> i >= 0L
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.AssemblyHandle _ -> true
        | NativeIntSource.ManagedPointer _ -> true

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | NativeIntSource.SyntheticCrossArrayOffset a, NativeIntSource.Verbatim b -> a < b
        | NativeIntSource.Verbatim a, NativeIntSource.SyntheticCrossArrayOffset b -> a < b
        | NativeIntSource.SyntheticCrossArrayOffset a, NativeIntSource.SyntheticCrossArrayOffset b -> a < b
        | _, _ -> failwith "TODO"


/// Defined in III.1.1.1
type CliNumericType =
    | Int32 of int32
    | Int64 of int64
    /// The real CLR just represents these as native ints, but we track their provenance.
    | NativeInt of NativeIntSource
    | NativeFloat of float
    | Int8 of int8
    | Int16 of int16
    | UInt8 of uint8
    | UInt16 of uint16
    | Float32 of float32
    | Float64 of float

    static member SizeOf (t : CliNumericType) : int =
        match t with
        | CliNumericType.Int32 _ -> 4
        | CliNumericType.Int64 _ -> 8
        | CliNumericType.NativeInt _ -> 8
        | CliNumericType.NativeFloat _ -> 8
        | CliNumericType.Int8 _ -> 1
        | CliNumericType.Int16 _ -> 2
        | CliNumericType.UInt8 _ -> 1
        | CliNumericType.UInt16 _ -> 2
        | CliNumericType.Float32 _ -> 4
        | CliNumericType.Float64 _ -> 8

    static member ToBytes (t : CliNumericType) : byte[] =
        match t with
        | CliNumericType.Int32 i -> BitConverter.GetBytes i
        | CliNumericType.Int64 i -> BitConverter.GetBytes i
        | CliNumericType.NativeInt src ->
            match src with
            | NativeIntSource.Verbatim i -> BitConverter.GetBytes i
            | NativeIntSource.SyntheticCrossArrayOffset i -> BitConverter.GetBytes i
            | NativeIntSource.ManagedPointer src ->
                match src with
                | ManagedPointerSource.Null -> BitConverter.GetBytes 0L
                | _ -> failwith "refusing to express pointer as bytes"
            | NativeIntSource.FieldHandlePtr _ -> failwith "refusing to express FieldHandlePtr as bytes"
            | NativeIntSource.FunctionPointer _ -> failwith "refusing to express FunctionPointer as bytes"
            | NativeIntSource.TypeHandlePtr _ -> failwith "refusing to express TypeHandlePtr as bytes"
            | NativeIntSource.MethodTablePtr _ -> failwith "refusing to express MethodTablePtr as bytes"
            | NativeIntSource.AssemblyHandle _ -> failwith "refusing to express AssemblyHandle as bytes"
        | CliNumericType.NativeFloat f -> BitConverter.GetBytes f
        // Overload resolution for sbyte/byte silently picks
        // `BitConverter.GetBytes(System.Half)` (2 bytes) in net8/net9; build
        // the single-byte result explicitly to stay faithful to CLR layout.
        // Route a negative sbyte through int32 + mask to preserve bit
        // pattern without hitting the checked-conversion throw.
        | CliNumericType.Int8 i -> [| byte (int i &&& 0xFF) |]
        | CliNumericType.Int16 i -> BitConverter.GetBytes i
        | CliNumericType.UInt8 i -> [| i |]
        | CliNumericType.UInt16 i -> BitConverter.GetBytes i
        | CliNumericType.Float32 i -> BitConverter.GetBytes i
        | CliNumericType.Float64 i -> BitConverter.GetBytes i

type CliRuntimePointer =
    | Verbatim of int64
    | FieldRegistryHandle of int64
    | MethodTablePtr of ConcreteTypeHandle
    | Managed of ManagedPointerSource

type SizeofResult =
    {
        Alignment : int
        Size : int
    }

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
    /// little-endian host. Structs, object refs, runtime pointers etc. are
    /// out of scope for this helper and fall through to a specific `failwith`.
    static member OfBytesLike (template : CliType) (bytes : byte[]) : CliType =
        let expected = CliType.SizeOf(template).Size

        if bytes.Length <> expected then
            failwith
                $"CliType.OfBytesLike: byte count mismatch — template %O{template} expects %d{expected} bytes, got %d{bytes.Length}"

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
            CliType.Numeric (CliNumericType.Int64 (BitConverter.ToInt64 (bytes, 0)))
        | CliType.Numeric (CliNumericType.Float32 _) ->
            CliType.Numeric (CliNumericType.Float32 (BitConverter.ToSingle (bytes, 0)))
        | CliType.Numeric (CliNumericType.Float64 _) ->
            CliType.Numeric (CliNumericType.Float64 (BitConverter.ToDouble (bytes, 0)))
        | CliType.Numeric (CliNumericType.NativeFloat _) ->
            CliType.Numeric (CliNumericType.NativeFloat (BitConverter.ToDouble (bytes, 0)))
        | CliType.Numeric (CliNumericType.NativeInt _) ->
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (BitConverter.ToInt64 (bytes, 0))))
        | CliType.ObjectRef _
        | CliType.RuntimePointer _
        | CliType.ValueType _ ->
            failwith
                $"TODO: CliType.OfBytesLike: non-primitive template %O{template} (bytes reconstruction for non-primitive storage not yet modelled)"

and CliField =
    {
        Name : string
        Contents : CliType
        /// "None" for "no explicit offset specified"; we expect most offsets to be None.
        Offset : int option
        Type : ConcreteTypeHandle
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
            Type : ConcreteTypeHandle
        }

    static member ToCliField (this : CliConcreteField) : CliField =
        {
            Offset = this.ConfiguredOffset
            Contents = this.Contents
            Name = this.Name
            Type = this.Type
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
            /// enum (detected structurally by the reserved `value__` field at offset 0). `None`
            /// for user-defined structs and non-primitive BCL structs. Populated at construction
            /// time so the context-free `EvalStackValue.ofCliType` can flatten without threading
            /// `BaseClassTypes`/`AllConcreteTypes` through every push site.
            _PrimitiveLikeKind : PrimitiveLikeKind option
            _Fields : CliConcreteField list
            Layout : Layout
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
                            Name = field.Name
                            Contents = field.Contents
                            Offset = alignedOffset
                            Size = size.Size
                            Alignment = size.Alignment
                            ConfiguredOffset = field.Offset
                            EditedAtTime = 0UL
                            Type = field.Type
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
                    Name = field.Name
                    Contents = field.Contents
                    Offset = field.Offset.Value
                    Size = size.Size
                    Alignment = size.Alignment
                    ConfiguredOffset = field.Offset
                    EditedAtTime = 0UL
                    Type = field.Type
                }
            )

        | _ :: _, _ :: _ -> failwith "unexpectedly mixed explicit and automatic layout of fields"

    static member ToBytes (cvt : CliValueType) : byte[] =
        let bytes = Array.zeroCreate<byte> (CliValueType.SizeOf(cvt).Size)

        cvt._Fields
        |> List.sortBy _.EditedAtTime
        |> List.iter (fun candidateField ->
            let fieldBytes : byte[] = CliType.ToBytes candidateField.Contents

            for i = 0 to candidateField.Size - 1 do
                bytes.[candidateField.Offset + i] <- fieldBytes.[i]
        )

        bytes

    static member OfFields
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (declared : ConcreteTypeHandle)
        (layout : Layout)
        (f : CliField list)
        : CliValueType
        =
        let fields = CliValueType.ComputeConcreteFields layout f

        {
            _Declared = declared
            _PrimitiveLikeKind = CliValueType.ClassifyPrimitiveLike bct allCt declared fields
            _Fields = fields
            Layout = layout
            NextTimestamp = 1UL
        }

    /// Rebuild with the same declared type and primitive-like classification as `source`. Used by
    /// the eval-stack rewrap path, which pops an already-classified value and reconstructs its
    /// stored form without needing `BaseClassTypes`/`AllConcreteTypes` in scope.
    static member OfFieldsLike (source : CliValueType) (layout : Layout) (f : CliField list) : CliValueType =
        let fields = CliValueType.ComputeConcreteFields layout f

        {
            _Declared = source._Declared
            _PrimitiveLikeKind = source._PrimitiveLikeKind
            _Fields = fields
            Layout = layout
            NextTimestamp = 1UL
        }

    static member AddField (f : CliField) (vt : CliValueType) : CliValueType =
        // Recompute all fields with the new one added
        // TODO: the existence of this function at all is rather dubious, but it's there
        // at the moment to support delegate types.
        // The whole function is just a bodge and it will hopefully go away soon; I just don't know how.
        let prevFields = vt._Fields |> List.map (fun f -> f.Name, f) |> Map.ofList

        let allFields =
            f
            :: (vt._Fields
                |> List.map (fun cf ->
                    {
                        Name = cf.Name
                        Contents = cf.Contents
                        Offset =
                            match vt.Layout with
                            | Layout.Default -> None
                            | Layout.Custom _ -> Some cf.Offset
                        Type = cf.Type
                    }
                ))

        let newFields =
            CliValueType.ComputeConcreteFields vt.Layout allFields
            |> List.map (fun field ->
                match Map.tryFind field.Name prevFields with
                | Some prev ->
                    { field with
                        EditedAtTime = prev.EditedAtTime
                    }
                | None ->
                    { field with
                        EditedAtTime = vt.NextTimestamp
                    }
            )

        {
            _Declared = vt._Declared
            _PrimitiveLikeKind = vt._PrimitiveLikeKind
            _Fields = newFields
            Layout = vt.Layout
            NextTimestamp = vt.NextTimestamp + 1UL
        }

    /// Returns the offset and size.
    static member GetFieldLayout (field : string) (cvt : CliValueType) : int * int =
        let targetField =
            cvt._Fields
            |> List.tryFind (fun f -> f.Name = field)
            |> Option.defaultWith (fun () -> failwithf $"Field '%s{field}' not found")

        targetField.Offset, targetField.Size

    // TODO: use DereferenceFieldAt for the implementation.
    // We should eventually be able to dereference an arbitrary field of a struct
    // as though it were any other field of any other type, to accommodate Unsafe.As.
    static member DereferenceField (field : string) (cvt : CliValueType) : CliType =
        let targetField =
            cvt._Fields
            |> List.tryFind (fun f -> f.Name = field)
            |> Option.defaultWith (fun () -> failwithf $"Field '%s{field}' not found")

        // Identify all fields that overlap with the target field's memory range
        let targetStart = targetField.Offset
        let targetEnd = targetField.Offset + targetField.Size

        let affectedFields =
            cvt._Fields
            |> List.filter (fun f ->
                let fieldStart = f.Offset
                let fieldEnd = f.Offset + f.Size
                // Fields overlap if their ranges intersect
                fieldStart < targetEnd && targetStart < fieldEnd
            )

        match affectedFields with
        | [] -> failwith "unexpectedly didn't dereference a field"
        | [ f ] -> f.Contents
        | fields ->
            let bytes = CliValueType.ToBytes cvt

            let fieldBytes =
                bytes.[targetField.Offset .. targetField.Offset + targetField.Size - 1]

            // `targetField.Contents` is the current value stored in the slot;
            // its shape tells us which primitive flavour to reconstruct. For
            // non-primitive field contents this still falls through to a
            // specific `failwith` inside `OfBytesLike`.
            CliType.OfBytesLike targetField.Contents fieldBytes

    static member FieldsAt (offset : int) (cvt : CliValueType) : CliConcreteField list =
        cvt._Fields |> List.filter (fun f -> f.Offset = offset)

    static member DereferenceFieldAt (offset : int) (size : int) (cvt : CliValueType) : CliType =
        let targetField =
            CliValueType.FieldsAt offset cvt |> List.tryFind (fun f -> f.Size = size)

        match targetField with
        | None -> failwith "TODO: couldn't find the field"
        | Some f -> f.Contents

    static member SizeOf (vt : CliValueType) : SizeofResult =
        let minimumSize, packingSize =
            match vt.Layout with
            | Layout.Custom (size = size ; packingSize = packing) ->
                size, if packing = 0 then DEFAULT_STRUCT_ALIGNMENT else packing
            | Layout.Default -> 0, DEFAULT_STRUCT_ALIGNMENT

        if vt._Fields.IsEmpty then
            {
                Size = minimumSize
                Alignment = 1
            }
        else
            // Now we can just use the precomputed offsets and sizes
            let finalOffset, alignment =
                vt._Fields
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

    static member ContainsObjectReferences (vt : CliValueType) : bool =
        vt._Fields
        |> List.exists (fun field -> CliType.ContainsObjectReferences field.Contents)

    /// Sets the value of the specified field, *without* touching any overlapping fields.
    /// `DereferenceField` handles resolving conflicts between overlapping fields.
    static member WithFieldSet (field : string) (value : CliType) (cvt : CliValueType) : CliValueType =
        {
            _Declared = cvt._Declared
            _PrimitiveLikeKind = cvt._PrimitiveLikeKind
            Layout = cvt.Layout
            _Fields =
                cvt._Fields
                |> List.replaceWhere (fun f ->
                    if f.Name = field then
                        { f with
                            Contents = value
                            EditedAtTime = cvt.NextTimestamp
                        }
                        |> Some
                    else
                        None
                )
            NextTimestamp = cvt.NextTimestamp + 1UL
        }

    /// Projects the single instance field at offset 0 of a primitive-like struct.
    /// These structs are guaranteed by construction to have exactly one instance field at offset 0
    /// (e.g. `IntPtr._value`, `RuntimeTypeHandle.m_type`, every enum's `value__`); any failure here
    /// indicates a violated invariant, not a caller error. Gated on the classification so it
    /// cannot misfire on user-defined single-field structs.
    static member PrimitiveLikeField (cvt : CliValueType) : CliField =
        if cvt._PrimitiveLikeKind.IsNone then
            failwith $"CliValueType.PrimitiveLikeField: %O{cvt._Declared} is not primitive-like"

        match cvt._Fields with
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
        if target._Fields.Length <> source._Fields.Length then
            failwith
                $"CliValueType.CoerceFrom: field count mismatch between target %O{target._Declared} (%i{target._Fields.Length}) and source %O{source._Declared} (%i{source._Fields.Length})"

        let merged =
            (target._Fields, source._Fields)
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
            _Fields = merged
            Layout = target.Layout
            NextTimestamp = source.NextTimestamp
        }

type CliTypeResolutionResult =
    | Resolved of CliType
    | FirstLoad of WoofWare.PawPrint.AssemblyReference

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
        | PrimitiveType.Int64 -> CliType.Numeric (CliNumericType.Int64 0L)
        | PrimitiveType.UInt64 ->
            // uint64 doesn't exist; the spec has them stored on the stack as if signed, with two's complement wraparound
            CliType.Numeric (CliNumericType.Int64 0L)
        | PrimitiveType.Single -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | PrimitiveType.Double -> CliType.Numeric (CliNumericType.Float64 0.0)
        | PrimitiveType.String -> CliType.ObjectRef None
        | PrimitiveType.TypedReference -> failwith "todo"
        | PrimitiveType.IntPtr ->
            let intPtrHandle =
                AllConcreteTypes.findExistingNonGenericConcreteType concreteTypes corelib.IntPtr.Identity
                |> Option.get

            {
                Name = "_value"
                Contents =
                    CliType.Numeric (
                        CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
                    )
                Offset = None
                Type = intPtrHandle
            }
            |> List.singleton
            |> CliValueType.OfFields corelib concreteTypes intPtrHandle Layout.Default
            |> CliType.ValueType
        | PrimitiveType.UIntPtr ->
            let uintPtrHandle =
                AllConcreteTypes.findExistingNonGenericConcreteType concreteTypes corelib.UIntPtr.Identity
                |> Option.get

            {
                Name = "_value"
                Contents =
                    CliType.Numeric (
                        CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
                    )
                Offset = None
                Type = uintPtrHandle
            }
            |> List.singleton
            |> CliValueType.OfFields corelib concreteTypes uintPtrHandle Layout.Default
            |> CliType.ValueType
        | PrimitiveType.Object -> CliType.ObjectRef None

    let rec zeroOf
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * AllConcreteTypes
        =
        zeroOfWithVisited concreteTypes assemblies corelib handle Set.empty

    and zeroOfWithVisited
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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

        | ConcreteTypeHandle.Concrete _ ->
            // This is a concrete type - look it up in the mapping
            let concreteType =
                match AllConcreteTypes.lookup handle concreteTypes with
                | Some ct -> ct
                | None -> failwithf "ConcreteTypeHandle %A not found in AllConcreteTypes" handle

            // Get the type definition from the assembly
            let assembly = assemblies.[concreteType.Assembly.FullName]
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
                else if

                    // Not a known primitive, now check for cycles
                    Set.contains handle visited
                then
                    // We're in a cycle - return a default zero value for the type
                    // Value types can't be self-referential unless they are specifically known to the
                    // runtime - for example, System.Byte is a value type with a single field,
                    // of type System.Byte.
                    // Since we check for (nominal) equality against all such types in the first branch,
                    // this code path is only hit with reference types.
                    CliType.ObjectRef None, concreteTypes
                else
                    let visited = Set.add handle visited
                    // Not a known primitive, check if it's a value type or reference type
                    determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited
            else if

                // Not from corelib or has generics
                concreteType.Assembly = corelib.Corelib.Name
                && typeDef = corelib.Array
                && concreteType.Generics.Length = 1
            then
                // This is an array type, so null is appropriate
                CliType.ObjectRef None, concreteTypes
            else if

                // Custom type - now check for cycles
                Set.contains handle visited
            then
                // We're in a cycle - return a default zero value for the type.
                // Value types can't be self-referential unless they are specifically known to the
                // runtime - for example, System.Byte is a value type with a single field,
                // of type System.Byte.
                // Since we check for (nominal) equality against all such types in the first branch,
                // this code path is only hit with reference types.
                CliType.ObjectRef None, concreteTypes
            else
                let visited = Set.add handle visited
                // Custom type - need to determine if it's a value type or reference type
                determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited

    and private determineZeroForCustomType
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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
                        Name = field.Name
                        Contents = fieldZero
                        Offset = field.Offset
                        Type = fieldHandle
                    }
                )
                |> CliValueType.OfFields corelib currentConcreteTypes handle typeDef.Layout

            CliType.ValueType vt, currentConcreteTypes
        else
            // It's a reference type
            CliType.ObjectRef None, concreteTypes

    and private concretizeFieldType
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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

        let loadAssembly =
            { new IAssemblyLoad with
                member _.LoadAssembly loaded assyName ref =
                    match loaded.TryGetValue assyName.FullName with
                    | true, currentAssy ->
                        let targetAssyRef = currentAssy.AssemblyReferences.[ref]

                        match loaded.TryGetValue targetAssyRef.Name.FullName with
                        | true, targetAssy -> loaded, targetAssy
                        | false, _ ->
                            failwithf
                                "Assembly %s not loaded when trying to resolve reference"
                                targetAssyRef.Name.FullName
                    | false, _ ->
                        failwithf "Current assembly %s not loaded when trying to resolve reference" assyName.FullName
            }

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

    let getField (field : string) (value : CliType) : CliType =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.DereferenceField field cvt

    /// Returns the offset and size.
    let getFieldLayout (field : string) (value : CliType) : int * int =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.GetFieldLayout field cvt

    /// Returns None if there isn't *exactly* one field that starts there. This rules out some valid programs.
    let getFieldAt (offset : int) (value : CliType) : CliConcreteField option =
        match value with
        | CliType.Numeric cliNumericType -> failwith "todo"
        | CliType.Bool b -> failwith "todo"
        | CliType.Char (high, low) -> failwith "todo"
        | CliType.ObjectRef managedHeapAddressOption -> failwith "todo"
        | CliType.RuntimePointer cliRuntimePointer -> failwith "todo"
        | CliType.ValueType cvt -> CliValueType.FieldsAt offset cvt |> List.tryExactlyOne
