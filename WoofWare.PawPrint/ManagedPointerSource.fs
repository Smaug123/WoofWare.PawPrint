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

        $"<PE data %s{this.AssemblyFullName} %s{source} at %d{this.RelativeVirtualAddress} size %d{this.Size}>"

/// The root storage location that a managed pointer points into.
[<NoComparison>]
type ByrefRoot =
    /// Address of a local variable slot on the stack.
    | LocalVariable of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a method argument slot on the stack.
    | Argument of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a byte in a localloc block owned by a method frame.
    | LocalMemoryByte of sourceThread : ThreadId * methodFrame : FrameId * block : LocallocBlockId * byteOffset : int
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
    | StaticField of declaringType : ConcreteTypeHandle * field : ComparableFieldDefinitionHandle
    /// Address of a UTF-16 character within a heap-allocated string's trailing
    /// character data. Created by `ldflda` on `String._firstChar`.
    | StringCharAt of str : ManagedHeapAddress * charIndex : int

/// Identity of a byte-addressable storage container. Offsets within the
/// container are tracked separately.
[<RequireQualifiedAccess>]
type ByteStorageIdentity =
    | Array of ManagedHeapAddress
    | String of ManagedHeapAddress
    | PeByteRange of PeByteRangePointer
    | StaticField of ConcreteTypeHandle * ComparableFieldDefinitionHandle
    | LocalMemory of ThreadId * FrameId * LocallocBlockId
    | StackLocal of ThreadId * FrameId * uint16
    | StackArgument of ThreadId * FrameId * uint16

[<RequireQualifiedAccess>]
module ByteStorageIdentity =
    let private rank (identity : ByteStorageIdentity) : int =
        match identity with
        | ByteStorageIdentity.Array _ -> 0
        | ByteStorageIdentity.String _ -> 1
        | ByteStorageIdentity.PeByteRange _ -> 2
        | ByteStorageIdentity.StaticField _ -> 3
        | ByteStorageIdentity.LocalMemory _ -> 4
        | ByteStorageIdentity.StackLocal _ -> 5
        | ByteStorageIdentity.StackArgument _ -> 6

    let compare (left : ByteStorageIdentity) (right : ByteStorageIdentity) : int =
        match left, right with
        | ByteStorageIdentity.Array left, ByteStorageIdentity.Array right -> Operators.compare left right
        | ByteStorageIdentity.String left, ByteStorageIdentity.String right -> Operators.compare left right
        | ByteStorageIdentity.PeByteRange left, ByteStorageIdentity.PeByteRange right -> Operators.compare left right
        | ByteStorageIdentity.StaticField (leftType, leftField), ByteStorageIdentity.StaticField (rightType, rightField) ->
            Operators.compare (leftType, leftField) (rightType, rightField)
        | ByteStorageIdentity.LocalMemory (leftThread, leftFrame, leftBlock),
          ByteStorageIdentity.LocalMemory (rightThread, rightFrame, rightBlock) ->
            Operators.compare (leftThread, leftFrame, leftBlock) (rightThread, rightFrame, rightBlock)
        | ByteStorageIdentity.StackLocal (leftThread, leftFrame, leftLocal),
          ByteStorageIdentity.StackLocal (rightThread, rightFrame, rightLocal) ->
            Operators.compare (leftThread, leftFrame, leftLocal) (rightThread, rightFrame, rightLocal)
        | ByteStorageIdentity.StackArgument (leftThread, leftFrame, leftArgument),
          ByteStorageIdentity.StackArgument (rightThread, rightFrame, rightArgument) ->
            Operators.compare (leftThread, leftFrame, leftArgument) (rightThread, rightFrame, rightArgument)
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

    override this.ToString () =
        let formatProj acc proj =
            match proj with
            | ByrefProjection.Field field -> $"<field %O{field} of {acc}>"
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
                | ByrefRoot.LocalMemoryByte (source, method, block, byteOffset) ->
                    $"<byte %d{byteOffset} of %O{block} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.HeapValue addr -> $"<heap value %O{addr}>"
                | ByrefRoot.HeapObjectField (addr, field) -> $"<field %O{field} of heap object %O{addr}>"
                | ByrefRoot.ArrayElement (arr, index) -> $"<element %i{index} of array %O{arr}>"
                | ByrefRoot.PeByteRange peByteRange -> $"%O{peByteRange}"
                | ByrefRoot.StaticField (declaringType, field) ->
                    $"<static field %O{field.Get} of type %O{declaringType}>"
                | ByrefRoot.StringCharAt (str, charIndex) -> $"<char %i{charIndex} of string %O{str}>"

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
    let internal tryGetArrayRoot (src : ManagedPointerSource) : ManagedHeapAddress option =
        match src with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (array, _), _) -> Some array
        | _ -> None

    let internal tryProjectionByteOffset (rootByteOffset : int64) (projs : ByrefProjection list) : int64 option =
        ((Some rootByteOffset), projs)
        ||> List.fold (fun (offset : int64 option) (projection : ByrefProjection) ->
            match offset with
            | None -> None
            | Some offset ->
                match projection with
                | ByrefProjection.ReinterpretAs _ -> Some offset
                | ByrefProjection.ByteOffset n -> Some (offset + int64<int> n)
                | ByrefProjection.Field _ -> None
        )

    /// Returns deterministic low address bits for byrefs that have a stable
    /// synthetic address model. For PE byte ranges this is `RVA + byteOffset`,
    /// not a real loaded module address; callers may use it only for low-bit
    /// alignment masks where the unknown image base contributes zero low bits.
    /// Use IlMachineState.tryManagedPointerIntegerAddress when an operation
    /// needs full synthetic address bits for integer conversion or comparison.
    let tryStableAddressBits (src : ManagedPointerSource) : int64 option =
        match src with
        | ManagedPointerSource.Null -> Some 0L
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (_, _, _, rootByteOffset), projs) ->
            tryProjectionByteOffset (int64<int> rootByteOffset) projs
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
            tryProjectionByteOffset (int64<int> peByteRange.RelativeVirtualAddress) projs
        | ManagedPointerSource.Byref _ -> None

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

    let private normaliseTrailingByteOffset
        (tryGetCellSize : ByrefRoot -> int option)
        (advanceRoot : ByrefRoot -> int -> ByrefRoot option)
        (src : ManagedPointerSource)
        : ManagedPointerSource
        =
        match src with
        | ManagedPointerSource.Null -> src
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
    let private normaliseLocalMemoryByteOffset (src : ManagedPointerSource) : ManagedPointerSource =
        normaliseTrailingByteOffset
            (function
            | ByrefRoot.LocalMemoryByte _ -> Some 1
            | _ -> None)
            (fun root cellAdvance ->
                match root with
                | ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset) ->
                    Some (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset + cellAdvance))
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
        |> normaliseLocalMemoryByteOffset
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
module NormalisedManagedPointerSource =
    let value (NormalisedManagedPointerSource src) : ManagedPointerSource = src
