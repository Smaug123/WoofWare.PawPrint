namespace WoofWare.PawPrint

open System
open Checked

/// Identifies which PE image byte range a pointer describes.
[<RequireQualifiedAccess>]
type PeByteRangePointerSource =
    /// Static data declared by a field RVA, as used by RuntimeHelpers.InitializeArray.
    | FieldRva of field : ComparableFieldDefinitionHandle
    /// Managed resource ranges point at the resource payload bytes, after the
    /// ECMA-335 4-byte length prefix; PeByteRangePointer.Size is the decoded
    /// payload length, not including that prefix.
    | ManagedResource of resourceName : string
    /// The COR signature blob for a field definition (ECMA II.23.2.4),
    /// stored in the metadata stream's #Blob heap rather than in a section.
    /// `PeByteRangePointer.Size` is the blob's length; the RVA field has no
    /// PE-section meaning for this variant and is set to 0. Bytes are read
    /// via the assembly's `MetadataReader.GetBlobBytes` rather than through
    /// `PeReader.GetSectionData`.
    | FieldSignatureBlob of field : ComparableFieldDefinitionHandle

type PeByteRangePointer =
    {
        AssemblyFullName : string
        Source : PeByteRangePointerSource
        RelativeVirtualAddress : int
        Size : int
    }

    override this.ToString () : string =
        let source =
            match this.Source with
            | PeByteRangePointerSource.FieldRva field -> $"field %O{field.Get}"
            | PeByteRangePointerSource.ManagedResource resourceName -> $"managed resource %s{resourceName}"
            | PeByteRangePointerSource.FieldSignatureBlob field -> $"field signature blob for %O{field.Get}"

        $"<PE data %s{this.AssemblyFullName} %s{source} at %d{this.RelativeVirtualAddress} size %d{this.Size}>"

/// Identity of the target of a `RuntimeTypeHandle`. The target may be a fully
/// closed concrete type, an open generic type definition (e.g. `Box<>`), or a
/// generic parameter (`T` / `U`) belonging to a type or method.
[<RequireQualifiedAccess>]
type RuntimeTypeHandleTarget =
    | Closed of ConcreteTypeHandle
    | OpenGenericTypeDefinition of ResolvedTypeIdentity
    /// A generic type parameter (e.g. T in IEquatable<T>), identified by its declaring
    /// type and zero-based position. Surfaced through reflection as a RuntimeType with
    /// IsGenericParameter = true.
    | GenericParameter of declaringType : ResolvedTypeIdentity * position : int
    /// A generic method parameter (e.g. TResult in TResult Foo<TResult>()), identified by
    /// its declaring type, declaring method, and zero-based position within the method's
    /// generic parameter list. Surfaced through reflection as a RuntimeType with
    /// IsGenericParameter = true and DeclaringMethod non-null.
    | MethodGenericParameter of
        declaringType : ResolvedTypeIdentity *
        declaringMethod : ComparableMethodDefinitionHandle *
        position : int

    override this.ToString () : string =
        match this with
        | RuntimeTypeHandleTarget.Closed handle -> string handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            $"open generic definition %s{identity.Assembly.Name}/%O{identity.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            $"generic parameter #%i{position} of %s{declaringType.Assembly.Name}/%O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            $"method generic parameter #%i{position} of method %O{declaringMethod.Get} on %s{declaringType.Assembly.Name}/%O{declaringType.TypeDefinition.Get}"

/// The root storage location that a managed pointer points into.
[<NoComparison>]
type ByrefRoot =
    /// Address of a local variable slot on the stack.
    | LocalVariable of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a method argument slot on the stack.
    | Argument of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a byte in a localloc block owned by a method frame.
    | StackMemoryByte of sourceThread : ThreadId * methodFrame : FrameId * block : StackMemoryBlockId * byteOffset : int
    /// Address of a byte in a native-heap block allocated by
    /// `Marshal.AllocHGlobal` / `NativeMemory.Alloc`. Lifetime is explicitly
    /// controlled by `NativeMemory.Free` / `Marshal.FreeHGlobal`; reads or
    /// writes through this root after the block has been freed fail loudly.
    | NativeMemoryByte of block : NativeMemoryBlockId * byteOffset : int
    /// Address of a whole value stored in heap-backed storage.
    /// Used for boxed value-type storage and constructor `this` for value types.
    | HeapValue of obj : ManagedHeapAddress
    /// Address of a named field within a heap-allocated object.
    /// Created by `ldflda` on an ObjectRef.
    | HeapObjectField of obj : ManagedHeapAddress * field : FieldId
    /// Address of an indexed element within a heap-allocated array.
    /// Created by `ldelema`.
    | ArrayElement of arr : ManagedHeapAddress * index : int
    /// Address of a read-only byte range stored in a PE image.
    | PeByteRange of PeByteRangePointer
    /// Address of a static field slot in the interpreter's static storage map.
    ///
    /// `owner` is fixed when the byref is constructed (`ldsflda`), not resolved from whichever
    /// thread later dereferences it. That matches .NET: `ldsflda` on a `[ThreadStatic]` field
    /// resolves to a concrete per-thread address when it executes, and the resulting managed
    /// pointer is a plain address, so a byref taken on thread A and dereferenced on thread B
    /// still addresses A's slot.
    | StaticField of declaringType : ConcreteTypeHandle * field : ComparableFieldDefinitionHandle * owner : StaticOwner
    /// Address of a UTF-16 character within a heap-allocated string's trailing
    /// character data. Created by `ldflda` on `String._firstChar`.
    | StringCharAt of str : ManagedHeapAddress * charIndex : int
    /// Address of the cell caching a type's canonical `RuntimeType`. Reads return
    /// the RuntimeType registered in `IlMachineState.TypeHandles`; pre-allocation
    /// at byref construction is what makes the read total.
    ///
    /// CoreCLR stores this cache in one of two structures depending on the shape
    /// of the type, and managed code reaches it by two different `ldflda`s:
    /// `MethodTableAuxiliaryData::ExposedClassObjectRaw` for MethodTable-backed
    /// types (`Closed` instantiations like `Box<int>`, and
    /// `OpenGenericTypeDefinition`s like `Box<>`), and `TypeDesc::_exposedClassObject`
    /// for TypeDesc-backed ones (byref / pointer / function-pointer /
    /// generic-parameter). Both cache the same RuntimeType for the same target, so
    /// they are one cell here rather than two: two roots would give one logical
    /// location two identities and break byref equality between them.
    | ExposedClassObject of declaringType : RuntimeTypeHandleTarget

/// Identity of a byte-addressable storage container. Offsets within the
/// container are tracked separately.
///
/// `HeapObject` and `HeapObjectField` are not literally "byte arrays" — they
/// identify a managed heap allocation (or a particular class field within
/// one) as the shared origin for byte-offset reasoning. Two byrefs into the
/// same boxed value reach through the same `HeapObject addr` regardless of
/// which interior field they project; two byrefs through the same class
/// field reach through the same `HeapObjectField (addr, field)`. Disjoint
/// fields of the same class instance get distinct `HeapObjectField` keys,
/// and a boxed value and a class-field byref cannot coexist on the same
/// address (each heap allocation has a single object kind), so the two
/// heap kinds never need to be reconciled against each other.
[<RequireQualifiedAccess>]
type ByteStorageIdentity =
    | Array of ManagedHeapAddress
    | String of ManagedHeapAddress
    | PeByteRange of PeByteRangePointer
    /// Two threads' slots of the same `[ThreadStatic]` field are separate storage, so the
    /// owner is part of the identity; see `ByrefRoot.StaticField`.
    | StaticField of ConcreteTypeHandle * ComparableFieldDefinitionHandle * StaticOwner
    | StackMemory of ThreadId * FrameId * StackMemoryBlockId
    | StackLocal of ThreadId * FrameId * uint16
    | StackArgument of ThreadId * FrameId * uint16
    | NativeMemory of NativeMemoryBlockId
    | HeapObject of ManagedHeapAddress
    | HeapObjectField of ManagedHeapAddress * FieldId

[<RequireQualifiedAccess>]
module ByteStorageIdentity =
    let private rank (identity : ByteStorageIdentity) : int =
        match identity with
        | ByteStorageIdentity.Array _ -> 0
        | ByteStorageIdentity.String _ -> 1
        | ByteStorageIdentity.PeByteRange _ -> 2
        | ByteStorageIdentity.StaticField _ -> 3
        | ByteStorageIdentity.StackMemory _ -> 4
        | ByteStorageIdentity.StackLocal _ -> 5
        | ByteStorageIdentity.StackArgument _ -> 6
        | ByteStorageIdentity.NativeMemory _ -> 7
        | ByteStorageIdentity.HeapObject _ -> 8
        | ByteStorageIdentity.HeapObjectField _ -> 9

    let compare (left : ByteStorageIdentity) (right : ByteStorageIdentity) : int =
        match left, right with
        | ByteStorageIdentity.Array left, ByteStorageIdentity.Array right -> Operators.compare left right
        | ByteStorageIdentity.String left, ByteStorageIdentity.String right -> Operators.compare left right
        | ByteStorageIdentity.PeByteRange left, ByteStorageIdentity.PeByteRange right -> Operators.compare left right
        | ByteStorageIdentity.StaticField (leftType, leftField, leftOwner),
          ByteStorageIdentity.StaticField (rightType, rightField, rightOwner) ->
            Operators.compare (leftType, leftField, leftOwner) (rightType, rightField, rightOwner)
        | ByteStorageIdentity.StackMemory (leftThread, leftFrame, leftBlock),
          ByteStorageIdentity.StackMemory (rightThread, rightFrame, rightBlock) ->
            Operators.compare (leftThread, leftFrame, leftBlock) (rightThread, rightFrame, rightBlock)
        | ByteStorageIdentity.StackLocal (leftThread, leftFrame, leftLocal),
          ByteStorageIdentity.StackLocal (rightThread, rightFrame, rightLocal) ->
            Operators.compare (leftThread, leftFrame, leftLocal) (rightThread, rightFrame, rightLocal)
        | ByteStorageIdentity.StackArgument (leftThread, leftFrame, leftArgument),
          ByteStorageIdentity.StackArgument (rightThread, rightFrame, rightArgument) ->
            Operators.compare (leftThread, leftFrame, leftArgument) (rightThread, rightFrame, rightArgument)
        | ByteStorageIdentity.NativeMemory left, ByteStorageIdentity.NativeMemory right -> Operators.compare left right
        | ByteStorageIdentity.HeapObject left, ByteStorageIdentity.HeapObject right -> Operators.compare left right
        | ByteStorageIdentity.HeapObjectField (leftAddr, leftField),
          ByteStorageIdentity.HeapObjectField (rightAddr, rightField) ->
            Operators.compare (leftAddr, leftField) (rightAddr, rightField)
        | _ -> Operators.compare (rank left) (rank right)

/// A navigation step applied after reaching the byref root.
[<NoComparison>]
type ByrefProjection =
    /// Navigate to a named field within the current value.
    /// Created by `ldflda` on an existing managed pointer.
    | Field of field : FieldId
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
    /// A fake non-null managed reference whose only meaningful content is the
    /// raw `int64` bit pattern that produced it. BCL code in
    /// `MemoryMarshal.GetNonNullPinnableReference` synthesises one of these
    /// via `Unsafe.AsRef<T>((void*)1)` when a `Span<T>` is empty, so the
    /// subsequent `fixed (T* p = &ref)` pins to a non-null `T*` (the
    /// downstream native API is documented to tolerate any address for a
    /// zero-length buffer, as long as it isn't null). The reference must
    /// never be dereferenced; the only legitimate operations are
    /// round-tripping back through `conv.u`/`conv.i` to a `Verbatim`,
    /// `Unsafe.IsNullRef` (true iff `bits = 0L`, but normalised away at
    /// construction so this case never carries zero), and structural
    /// equality against another managed reference.
    | NativeIntPlaceholder of bits : int64

    override this.ToString () =
        let formatProj acc proj =
            match proj with
            | ByrefProjection.Field field -> $"<field %O{field} of {acc}>"
            | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
            | ByrefProjection.ByteOffset n -> $"<{acc} + %d{n} bytes>"

        match this with
        | ManagedPointerSource.Null -> "<null managed pointer>"
        | ManagedPointerSource.NativeIntPlaceholder bits -> $"<fake non-null byref @ 0x%x{bits}>"
        | ManagedPointerSource.Byref (root, projs) ->
            let rootStr =
                match root with
                | ByrefRoot.LocalVariable (source, method, var) ->
                    $"<variable %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.Argument (source, method, var) ->
                    $"<argument %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.StackMemoryByte (source, method, block, byteOffset) ->
                    $"<byte %d{byteOffset} of %O{block} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.NativeMemoryByte (block, byteOffset) -> $"<byte %d{byteOffset} of %O{block}>"
                | ByrefRoot.HeapValue addr -> $"<heap value %O{addr}>"
                | ByrefRoot.HeapObjectField (addr, field) -> $"<field %O{field} of heap object %O{addr}>"
                | ByrefRoot.ArrayElement (arr, index) -> $"<element %i{index} of array %O{arr}>"
                | ByrefRoot.PeByteRange peByteRange -> $"%O{peByteRange}"
                | ByrefRoot.StaticField (declaringType, field, owner) ->
                    $"<static field %O{field.Get} of type %O{declaringType} in %O{owner}>"
                | ByrefRoot.StringCharAt (str, charIndex) -> $"<char %i{charIndex} of string %O{str}>"
                | ByrefRoot.ExposedClassObject declaringType -> $"<cached RuntimeType cell for type %O{declaringType}>"

            projs |> List.fold formatProj rootStr

/// State-dependent information needed to canonicalise byte cursors.
///
/// A byte cursor is the trailing `ByrefProjection.ByteOffset` carried under a
/// trailing `ByrefProjection.ReinterpretAs`. It records that a byref has been
/// reinterpreted as a byte-addressed view and then moved by some number of
/// bytes from the typed root cell. For example, a cursor four bytes after
/// `arr[0]` may be structurally equivalent to `arr[1]` with no residual cursor
/// when the array element size is four.
///
/// Canonicalisation folds whole-cell cursor movement into roots that already
/// have an index or byte offset: array element indices, string character
/// indices, and localloc byte offsets. Any remaining cursor is kept as the
/// in-cell byte offset. Array element sizes are state-dependent and must be
/// supplied by callers with heap/type access; string and localloc strides are
/// fixed by the pointer root itself.
type ByteOffsetNormalisationContext =
    private
    | KnownArrayElementSizes of Map<ManagedHeapAddress, int>
    | NonArrayRootsOnly
    | FixedStrideRootsOnly

[<RequireQualifiedAccess>]
module ByteOffsetNormalisationContext =
    let withArrayElementSize (array : ManagedHeapAddress) (elementSize : int) : ByteOffsetNormalisationContext =
        ByteOffsetNormalisationContext.KnownArrayElementSizes (Map.ofList [ array, elementSize ])

    let withArrayElementSizes (arrayElementSizes : (ManagedHeapAddress * int) list) : ByteOffsetNormalisationContext =
        ByteOffsetNormalisationContext.KnownArrayElementSizes (Map.ofList arrayElementSizes)

    let nonArrayRootsOnly : ByteOffsetNormalisationContext =
        ByteOffsetNormalisationContext.NonArrayRootsOnly

    let fixedStrideRootsOnly : ByteOffsetNormalisationContext =
        ByteOffsetNormalisationContext.FixedStrideRootsOnly

    let internal tryGetArrayElementSize
        (context : ByteOffsetNormalisationContext)
        (array : ManagedHeapAddress)
        : int option
        =
        match context with
        | ByteOffsetNormalisationContext.KnownArrayElementSizes sizes ->
            match Map.tryFind array sizes with
            | Some elementSize -> Some elementSize
            | None ->
                failwith $"array byref %O{array} reached byte-offset normalisation without a recorded element size"
        | ByteOffsetNormalisationContext.NonArrayRootsOnly ->
            failwith $"byte-offset normalisation for non-array roots unexpectedly reached array byref %O{array}"
        | ByteOffsetNormalisationContext.FixedStrideRootsOnly -> None

/// A managed pointer whose trailing byte cursor has been canonicalised for the
/// roots supported by ByteOffsetNormalisationContext. APIs that compare byrefs
/// structurally should require this wrapper rather than accepting a raw
/// ManagedPointerSource.
[<NoComparison>]
type NormalisedManagedPointerSource = private | NormalisedManagedPointerSource of ManagedPointerSource

[<RequireQualifiedAccess>]
module ManagedPointerSource =
    /// A *bit-pattern byref* is one that carries a raw native-int value rather
    /// than referring to any storage: the `Unsafe.AsRef<T>((void*)bits)`
    /// placeholder, and `Null`, which is simply the bit pattern 0. Arithmetic
    /// on these is plain int64 bit arithmetic — there is no root to walk and
    /// no int32 offset model involved, so callers must handle them before
    /// decomposing a byref into root-plus-offset.
    ///
    /// Contrast `tryStableAddressBits`, which deliberately also answers for
    /// symbolic byrefs so that alignment masks can see their low bits.
    let tryBitPatternBits (src : ManagedPointerSource) : int64 voption =
        match src with
        | ManagedPointerSource.Null -> ValueSome 0L
        | ManagedPointerSource.NativeIntPlaceholder bits -> ValueSome bits
        | ManagedPointerSource.Byref _ -> ValueNone

    /// Inverse of <see cref="tryBitPatternBits"/>. Zero normalises back to
    /// `Null` so the placeholder invariant ("never carries zero") holds and
    /// `Unsafe.IsNullRef` agrees with the CLR's bit-pattern definition.
    let ofBitPattern (bits : int64) : ManagedPointerSource =
        if bits = 0L then
            ManagedPointerSource.Null
        else
            ManagedPointerSource.NativeIntPlaceholder bits

    let internal tryGetArrayRoot (src : ManagedPointerSource) : ManagedHeapAddress option =
        match src with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (array, _), _) -> Some array
        | _ -> None

    /// If both byrefs reach the same storage modulo a byte cursor — i.e. they
    /// share a root and identical prefix projections, differing only in the
    /// trailing byte cursor under a final `ReinterpretAs` — return how far
    /// `src2` is past `src1` in bytes (negative if behind). Returns `None`
    /// for distinct roots, projections that aren't pure byte cursors, or any
    /// shape we can't unambiguously decompose. The result is a relative byte
    /// delta only; callers must not interpret it as an absolute address.
    let tryByteOffsetWithinSameRoot (src1 : ManagedPointerSource) (src2 : ManagedPointerSource) : int64 option =
        let splitTrailingByteCursor (projs : ByrefProjection list) : (ByrefProjection list * int64) option =
            // Returns (prefix, trailing byte offset). The prefix excludes the
            // final ReinterpretAs and any ByteOffset attached to it. A bare
            // empty list (no trailing reinterpret) counts as offset 0; a
            // trailing Field is not a pure byte cursor and yields None.
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revRest ->
                Some (List.rev revRest, int64 n)
            | ByrefProjection.ReinterpretAs _ :: revRest -> Some (List.rev revRest, 0L)
            | [] -> Some ([], 0L)
            | _ -> None

        match src1, src2 with
        | ManagedPointerSource.Null, ManagedPointerSource.Null -> Some 0L
        | ManagedPointerSource.NativeIntPlaceholder b1, ManagedPointerSource.NativeIntPlaceholder b2 -> Some (b2 - b1)
        | ManagedPointerSource.Byref (root1, projs1), ManagedPointerSource.Byref (root2, projs2) when root1 = root2 ->
            match splitTrailingByteCursor projs1, splitTrailingByteCursor projs2 with
            | Some (prefix1, offset1), Some (prefix2, offset2) when prefix1 = prefix2 -> Some (offset2 - offset1)
            | _ -> None
        | _ -> None

    /// Validate the byref-projection list invariant for a byref reaching
    /// `tryByteAddressDeltaSign`'s array fallback: `ByteOffset` only appears
    /// as the final element preceded by `ReinterpretAs`, and is non-negative
    /// (the construction-site canonicaliser establishes this via
    /// floor-division in `normaliseTrailingByteOffset`). Throws on violation —
    /// a malformed projection list signals a construction-site bug, and
    /// silently returning a possibly-wrong delta sign would mask it.
    let private validateByrefProjectionsAreCanonical
        (src : ManagedPointerSource)
        (projs : ByrefProjection list)
        : unit
        =
        let rec walk (preceding : ByrefProjection option) (rest : ByrefProjection list) : unit =
            match rest with
            | [] -> ()
            | [ ByrefProjection.ByteOffset n ] ->
                match preceding with
                | Some (ByrefProjection.ReinterpretAs _) ->
                    if n < 0 then
                        failwith
                            $"ManagedPointerSource: trailing byte cursor must be non-negative under canonical form, got %d{n} in %O{src}"
                | _ ->
                    failwith
                        $"ManagedPointerSource: trailing ByteOffset %d{n} must be preceded by ReinterpretAs in %O{src}"
            | ByrefProjection.ByteOffset n :: _ ->
                failwith $"ManagedPointerSource: ByteOffset %d{n} appears at a non-trailing position in %O{src}"
            | proj :: tail -> walk (Some proj) tail

        walk None projs

    /// Returns the sign of `addr(src2) - addr(src1)` — i.e. negative when
    /// src2 sits at a lower byte address than src1, positive when higher,
    /// zero when equal. The convention matches `tryByteOffsetWithinSameRoot`,
    /// which returns the same delta byte-accurately.
    ///
    /// Strictly weaker than `tryByteOffsetWithinSameRoot`: the magnitude is
    /// not meaningful, only the sign. Use this for pointer comparisons
    /// (`cgt.un`, `clt.un`) where only order matters; use
    /// `tryByteOffsetWithinSameRoot` whenever the actual byte delta is
    /// required.
    ///
    /// Precondition: byrefs reaching the array-index fallback path must be
    /// in canonical projection-list form — `ByteOffset` only as the trailing
    /// element under `ReinterpretAs`, and non-negative. Throws on violation
    /// rather than silently returning a wrong sign. The `< cellSize` half of
    /// the canonical bound (which we cannot verify here without heap access)
    /// is established at construction by floor-division in
    /// `normaliseTrailingByteOffset`.
    let tryByteAddressDeltaSign (src1 : ManagedPointerSource) (src2 : ManagedPointerSource) : int option =
        let splitTrailingByteCursor (projs : ByrefProjection list) : (ByrefProjection list * int64) option =
            // Mirrors `tryByteOffsetWithinSameRoot.splitTrailingByteCursor`:
            // returns (prefix, trailing byte cursor) when projections are a
            // pure byte cursor under an optional final ReinterpretAs, and
            // None when the projection chain ends in a Field or other shape
            // we can't decompose into a single byte cursor.
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revRest ->
                Some (List.rev revRest, int64 n)
            | ByrefProjection.ReinterpretAs _ :: revRest -> Some (List.rev revRest, 0L)
            | [] -> Some ([], 0L)
            | _ -> None

        match tryByteOffsetWithinSameRoot src1 src2 with
        | Some n -> Some (compare n 0L)
        | None ->
            // Same array, possibly different element index: element size is
            // strictly positive and each pointer's byte effect relative to
            // its cell start lies in `[0, cellSize)` (residuals via
            // floor-division; field offsets by layout). Hence
            // `compare idx2 idx1` agrees with the sign of the byte address
            // delta `addr(src2) - addr(src1)` whenever the indices differ.
            // When the indices match, `tryByteOffsetWithinSameRoot` would
            // already have answered.
            match src1, src2 with
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr1, idx1), projs1),
              ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr2, idx2), projs2) when arr1 = arr2 && idx1 <> idx2 ->
                validateByrefProjectionsAreCanonical src1 projs1
                validateByrefProjectionsAreCanonical src2 projs2
                Some (compare idx2 idx1)
            // Same native-memory block, different root byte offsets:
            // `tryByteOffsetWithinSameRoot` only catches identical roots,
            // but `NativeMemoryByte (block, n)` produced by pointer
            // arithmetic varies `n` while the block is fixed. The total
            // byte address inside the block is `rootOffset + trailing
            // cursor`, so once the prefix projections match (so the prefix
            // contributes equally on both sides) the sign of
            // `(rootOffset2 + cursor2) - (rootOffset1 + cursor1)` is the
            // sign of the byte address delta. Cross-block comparisons stay
            // None — those have no defensible ordering.
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block1, rootOffset1), projs1),
              ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block2, rootOffset2), projs2) when block1 = block2 ->
                match splitTrailingByteCursor projs1, splitTrailingByteCursor projs2 with
                | Some (prefix1, cursor1), Some (prefix2, cursor2) when prefix1 = prefix2 ->
                    let addr1 = int64 rootOffset1 + cursor1
                    let addr2 = int64 rootOffset2 + cursor2
                    Some (compare addr2 addr1)
                | _ -> None
            | _ -> None

    /// Returns deterministic low address bits for byrefs that have a stable
    /// synthetic address model. For PE byte ranges this is `RVA + byteOffset`,
    /// not a real loaded module address; callers may use it only for low-bit
    /// alignment masks where the unknown image base contributes zero low bits.
    let tryStableAddressBits (src : ManagedPointerSource) : int64 option =
        match src with
        | ManagedPointerSource.Null -> Some 0L
        | ManagedPointerSource.NativeIntPlaceholder bits -> Some bits
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (_, _, _, rootByteOffset), projs) ->
            let rec loop (byteOffset : int) (projs : ByrefProjection list) : int64 option =
                match projs with
                | [] -> Some (int64 rootByteOffset + int64 byteOffset)
                | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
                | ByrefProjection.ByteOffset n :: rest -> loop (byteOffset + n) rest
                | ByrefProjection.Field _ :: _ -> None

            loop 0 projs
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (_, rootByteOffset), projs) ->
            // Native-heap blocks are modelled as being allocated at unknown
            // (but well-aligned) base addresses; only the in-block byte offset
            // contributes to the low bits visible to alignment masks.
            let rec loop (byteOffset : int) (projs : ByrefProjection list) : int64 option =
                match projs with
                | [] -> Some (int64 rootByteOffset + int64 byteOffset)
                | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
                | ByrefProjection.ByteOffset n :: rest -> loop (byteOffset + n) rest
                | ByrefProjection.Field _ :: _ -> None

            loop 0 projs
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
            let rec loop (byteOffset : int) (projs : ByrefProjection list) : int64 option =
                match projs with
                | [] -> Some (int64 peByteRange.RelativeVirtualAddress + int64 byteOffset)
                | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
                | ByrefProjection.ByteOffset n :: rest -> loop (byteOffset + n) rest
                | ByrefProjection.Field _ :: _ -> None

            loop 0 projs
        | ManagedPointerSource.Byref _ -> None

    /// How many low bits of a byref's *container start* address the real runtime
    /// guarantees to be zero, for the roots whose container alignment PawPrint is
    /// willing to claim. `None` means "no claim": nothing may be said about that
    /// byref's low address bits.
    ///
    /// This is the alignment half of the byref model — a container whose address is
    /// unknown, plus a known in-container byte offset (`tryStableAddressBits` and
    /// its array/string counterpart in `NullaryIlOp`). Together they let
    /// `TaggedPointerBits.bitAndOffsetFromAlignedBase` answer the alignment masks
    /// managed code writes, without inventing an address. Every number below is a
    /// guarantee the runtime makes, not an observation of a particular run, and
    /// each is deliberately conservative: claiming *fewer* bits only ever refuses
    /// more questions.
    ///
    /// All of these assume the 64-bit object layout, which is the only one PawPrint
    /// models (`NativeIntSource` is 64-bit throughout).
    let tryContainerAlignmentBits (src : ManagedPointerSource) : int option =
        match src with
        // Not "unknown base plus offset": these have exact, fully-known bit
        // patterns, and callers must use those rather than an alignment claim.
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> None
        // The GC allocates objects 8-byte aligned on 64-bit, and an SZARRAY's
        // element data begins after a 16-byte header (`MethodTable*` plus a 4-byte
        // component count and 4 bytes of padding). Multi-dimensional arrays add two
        // `int`s of bounds per rank, i.e. a further multiple of 8. So array data
        // starts 8-byte aligned in every case.
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement _, _) -> Some 3
        // A string's character data begins at object + 12 (`MethodTable*` plus a
        // 4-byte length), so from an 8-byte-aligned object it is 4-byte aligned —
        // and no better. This is the one container where the obvious 8-byte guess
        // would be wrong.
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt _, _) -> Some 2
        // The stack pointer is kept 16-byte aligned on both x64 and arm64, and the
        // JIT rounds a `localloc` up to the stack alignment, so a localloc block
        // starts at least 8-byte aligned.
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte _, _) -> Some 3
        // `NativeMemory.Alloc` / `Marshal.AllocHGlobal` bottom out in `malloc`,
        // which returns storage aligned for any fundamental type — 16 bytes on
        // 64-bit targets.
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte _, _) -> Some 3
        // A PE image is mapped at a page-aligned base and its sections at their
        // RVAs, so the low bits of an RVA are the low bits of the mapped address.
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange _, _) -> Some 3
        // Object fields, static fields, stack slots and the synthetic roots have no
        // stable in-container offset either (see `tryStableAddressBits` and
        // `NullaryIlOp.tryManagedPointerAddressBits`), so there is nothing to pair
        // an alignment claim with.
        | ManagedPointerSource.Byref _ -> None

    let appendProjection (projection : ByrefProjection) (src : ManagedPointerSource) : ManagedPointerSource =
        match src with
        | ManagedPointerSource.Null -> failwith "cannot project from null managed pointer"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith $"cannot project from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
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

    /// Apply an address-preserving change of type view to a managed pointer.
    /// `Unsafe.As<TFrom, TTo>` never dereferences its argument, so unlike the
    /// general `appendProjection` this is total: it is defined on the two
    /// non-anchored pointer forms as well.
    ///
    /// An anchored byref gains a `ReinterpretAs` projection. A null byref, and
    /// the `Unsafe.AsRef<T>((void*)bits)` bit-pattern placeholder, are returned
    /// unchanged — neither denotes storage whose type view could change, and
    /// both are pure addresses that the reinterpret leaves alone. The BCL relies
    /// on this: the bitwise-equatable path of `SequenceEqual`/`StartsWith`/
    /// `EndsWith` reinterprets `MemoryMarshal.GetReference(span)` through
    /// `Unsafe.As<T, byte>` *before* it checks the length, so a `default` span
    /// (or `ReadOnlySpan<T>.Empty`, which is `default`) reaches here with a null
    /// byref that is never subsequently dereferenced.
    ///
    /// Producing a null byref stays safe because every read and write path
    /// rejects `ManagedPointerSource.Null` loudly (see the `readManagedByref`
    /// and `writeManagedByref` families in `IlMachineManagedByref`), which is
    /// the NullReferenceException the real runtime would raise.
    let reinterpretAs (target : ConcreteType<ConcreteTypeHandle>) (src : ManagedPointerSource) : ManagedPointerSource =
        match src with
        | ManagedPointerSource.Null
        | ManagedPointerSource.NativeIntPlaceholder _ -> src
        | ManagedPointerSource.Byref _ -> appendProjection (ByrefProjection.ReinterpretAs target) src

    let private normaliseTrailingByteOffset
        (tryGetCellSize : ByrefRoot -> int option)
        (advanceRoot : ByrefRoot -> int -> ByrefRoot option)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        match src with
        | ManagedPointerSource.Null -> src
        | ManagedPointerSource.NativeIntPlaceholder _ -> src
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs, tryGetCellSize root with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs ty :: rest, Some cellSize when cellSize > 0 ->
                // Floor-division so negatives land in `[0, cellSize)`.
                let cellAdvance =
                    let q = n / cellSize
                    let r = n - q * cellSize
                    if r < 0 then q - 1 else q

                match advanceRoot root cellAdvance with
                | None -> src
                | Some newRoot ->
                    let newOffset = n - cellAdvance * cellSize
                    let prefix = List.rev rest

                    let tail =
                        if newOffset = 0 then
                            [ ByrefProjection.ReinterpretAs ty ]
                        else
                            [ ByrefProjection.ReinterpretAs ty ; ByrefProjection.ByteOffset newOffset ]

                    ManagedPointerSource.Byref (newRoot, prefix @ tail)
            | _ -> src

    /// Fold whole-cell byte offsets of an array-rooted byref into the cell
    /// index, keeping the remaining in-cell offset in `[0, cellSize)`.
    /// Public callers should use the byte-view construction helpers or
    /// `normaliseForComparison`, rather than normalising arbitrary pointers.
    let private normaliseArrayByteOffset
        (context : ByteOffsetNormalisationContext)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        normaliseTrailingByteOffset
            (function
            | ByrefRoot.ArrayElement (arr, _) -> ByteOffsetNormalisationContext.tryGetArrayElementSize context arr
            | _ -> None)
            (fun root cellAdvance ->
                match root with
                | ByrefRoot.ArrayElement (arr, i) -> Some (ByrefRoot.ArrayElement (arr, i + cellAdvance))
                | _ -> None
            )
            src

    /// Fold whole-character byte offsets of a string-character byref into the
    /// character index. This is the string/trailing-data analogue of
    /// `normaliseArrayByteOffset`: UTF-16 character cells are two bytes wide,
    /// and equivalent byte addresses should have one structural representation.
    let private normaliseStringByteOffset (src : ManagedPointerSource) : ManagedPointerSource =
        normaliseTrailingByteOffset
            (function
            | ByrefRoot.StringCharAt _ -> Some 2
            | _ -> None)
            (fun root cellAdvance ->
                match root with
                | ByrefRoot.StringCharAt (str, charIndex) ->
                    Some (ByrefRoot.StringCharAt (str, charIndex + cellAdvance))
                | _ -> None
            )
            src

    /// Fold byte offsets of a localloc byte byref into the root byte offset.
    let private normaliseStackMemoryByteOffset (src : ManagedPointerSource) : ManagedPointerSource =
        normaliseTrailingByteOffset
            (function
            | ByrefRoot.StackMemoryByte _ -> Some 1
            | _ -> None)
            (fun root cellAdvance ->
                match root with
                | ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset) ->
                    Some (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + cellAdvance))
                | _ -> None
            )
            src

    /// Fold byte offsets of a native-heap byte byref into the root byte offset.
    let private normaliseNativeMemoryByteOffset (src : ManagedPointerSource) : ManagedPointerSource =
        normaliseTrailingByteOffset
            (function
            | ByrefRoot.NativeMemoryByte _ -> Some 1
            | _ -> None)
            (fun root cellAdvance ->
                match root with
                | ByrefRoot.NativeMemoryByte (block, byteOffset) ->
                    Some (ByrefRoot.NativeMemoryByte (block, byteOffset + cellAdvance))
                | _ -> None
            )
            src

    /// Canonicalise any trailing byte cursor that can be folded into the root.
    /// Prefer this over calling the root-specific normalisers directly.
    let private normaliseByteOffset
        (context : ByteOffsetNormalisationContext)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        src
        |> normaliseStackMemoryByteOffset
        |> normaliseNativeMemoryByteOffset
        |> normaliseArrayByteOffset context
        |> normaliseStringByteOffset

    /// Reinterpret a byref as a byte-addressed view, advance it by a byte count,
    /// and canonicalise any whole-cell movement into the byref root.
    let addByteOffsetUnderReinterpret
        (context : ByteOffsetNormalisationContext)
        (reinterpretAs : ConcreteType<ConcreteTypeHandle>)
        (byteOffset : int)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        src
        |> appendProjection (ByrefProjection.ReinterpretAs reinterpretAs)
        |> appendProjection (ByrefProjection.ByteOffset byteOffset)
        |> normaliseByteOffset context

    /// Advance an existing byte-addressed view and canonicalise any whole-cell
    /// movement into the byref root.
    let addByteOffsetToByteView
        (context : ByteOffsetNormalisationContext)
        (byteOffset : int)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        src
        |> appendProjection (ByrefProjection.ByteOffset byteOffset)
        |> normaliseByteOffset context

    let normaliseForComparison
        (context : ByteOffsetNormalisationContext)
        (src : ManagedPointerSource)
        : NormalisedManagedPointerSource
        =
        normaliseByteOffset context src |> NormalisedManagedPointerSource

    /// Use only at boundaries which cannot access the normalisation context but
    /// receive byrefs from constructors that already canonicalise byte cursors.
    /// This can validate fixed-stride roots, but array residuals still rely on
    /// the construction site having normalised with a real element-size context.
    let unsafeAssumeNormalisedForComparison (src : ManagedPointerSource) : NormalisedManagedPointerSource =
        let fixedRootNormalised =
            normaliseByteOffset ByteOffsetNormalisationContext.fixedStrideRootsOnly src

        if fixedRootNormalised <> src then
            failwith $"unsafeAssumeNormalisedForComparison received a non-normalised fixed-stride byref: %O{src}"

        NormalisedManagedPointerSource src

    let private stripTrailingReinterpretsRaw (src : ManagedPointerSource) : ManagedPointerSource =
        let rec go (src : ManagedPointerSource) : ManagedPointerSource =
            match src with
            | ManagedPointerSource.Null -> src
            | ManagedPointerSource.NativeIntPlaceholder _ -> src
            | ManagedPointerSource.Byref (root, projs) ->
                match List.rev projs with
                | ByrefProjection.ByteOffset 0 :: revRest -> go (ManagedPointerSource.Byref (root, List.rev revRest))
                | ByrefProjection.ReinterpretAs _ :: revRest -> go (ManagedPointerSource.Byref (root, List.rev revRest))
                | _ -> src

        go src

    /// Drop any trailing address-preserving `ReinterpretAs` projections so that two
    /// byrefs reaching the same byte location by different type-view paths compare
    /// equal. A `ReinterpretAs` followed by a `Field` must stay: field resolution
    /// depends on the reinterpreted type's layout, so it is no longer purely
    /// address-preserving in that case. A trailing `ByteOffset` DOES change the
    /// byte address and is preserved; a trailing `ByteOffset 0` is stripped as a
    /// no-op, and the reinterpret it qualified then becomes strippable.
    let stripTrailingReinterprets (src : NormalisedManagedPointerSource) : ManagedPointerSource =
        let (NormalisedManagedPointerSource src) = src
        stripTrailingReinterpretsRaw src

    /// True when a byref source carries a non-trailing `ReinterpretAs`
    /// projection (i.e. a reinterpret followed by a Field). Such projections
    /// would need a bytewise layout comparison — `ref a.X` vs
    /// `ref Unsafe.As<A,B>(ref a).X` can alias despite having different
    /// projection chains — and we don't yet model that. Callers that compare
    /// byrefs structurally use this to refuse the comparison rather than
    /// silently returning a potentially-wrong answer.
    let hasNonTrailingReinterpret (src : NormalisedManagedPointerSource) : bool =
        let (NormalisedManagedPointerSource src) = src

        match src with
        | ManagedPointerSource.Null -> false
        | ManagedPointerSource.NativeIntPlaceholder _ -> false
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

    /// CEQ semantics for two normalised byref sources. Trailing address-
    /// preserving `ReinterpretAs` projections are stripped before comparison,
    /// so `Unsafe.As`-style type-view changes don't break identity. A non-
    /// trailing `ReinterpretAs` (e.g. reinterpret-then-field) would need a
    /// bytewise layout comparison, which we don't model — fail loudly rather
    /// than silently returning a wrong answer. `context` is folded into the
    /// failure message so callers can identify which boundary refused.
    let ceqNormalised
        (context : string)
        (p1 : NormalisedManagedPointerSource)
        (p2 : NormalisedManagedPointerSource)
        : bool
        =
        let (NormalisedManagedPointerSource raw1) = p1
        let (NormalisedManagedPointerSource raw2) = p2

        if hasNonTrailingReinterpret p1 || hasNonTrailingReinterpret p2 then
            failwith
                $"TODO (CEQ): %s{context} with `ReinterpretAs` followed by `Field` needs a bytewise layout comparison; got %O{raw1} vs %O{raw2}"

        stripTrailingReinterprets p1 = stripTrailingReinterprets p2

[<RequireQualifiedAccess>]
module NormalisedManagedPointerSource =
    let value (NormalisedManagedPointerSource src) : ManagedPointerSource = src
