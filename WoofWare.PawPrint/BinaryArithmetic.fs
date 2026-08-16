namespace WoofWare.PawPrint

#nowarn "42"

type private FieldContainer =
    | HeapObject of ManagedHeapAddress
    | ByrefContainer of ManagedPointerSource

/// A *symbolic* byref decomposed into the storage it refers to plus an
/// offset. Bit-pattern byrefs (`Null`, `NativeIntPlaceholder`) are not
/// representable here: they refer to no storage, so callers must peel them
/// off with `ManagedPointerSource.tryBitPatternBits` before decomposing.
type private ArithmeticTarget =
    | StackMemoryTarget of ThreadId * FrameId * StackMemoryBlockId * int
    | NativeMemoryTarget of NativeMemoryBlockId * int
    | ArrayTarget of ManagedHeapAddress * int
    | StringTarget of ManagedHeapAddress * int
    | FieldTarget of FieldContainer * FieldId
    /// A byref that addresses one whole typed storage slot — `&local`, `&arg`,
    /// `&staticField`, or a box's interior pointer — rather than a byte, an
    /// element, or a field. A zero offset leaves such a byref exactly as it is;
    /// any other offset turns it into a byte cursor over the slot, because
    /// moving an address must not also choose a type view (the access width
    /// belongs to the later `ldind`/`stind`).
    | WholeValueTarget of ptr : ManagedPointerSource
    /// A byref ending in `ReinterpretAs T [; ByteOffset n]`. Pointer arithmetic
    /// walks the byte cursor rather than the underlying storage. `prefixProjs`
    /// is whatever came before the reinterpret.
    | ByteViewTarget of
        root : ByrefRoot *
        prefixProjs : ByrefProjection list *
        reinterpretTy : ConcreteType<ConcreteTypeHandle> *
        byteOffset : int

[<RequireQualifiedAccess>]
module private ArithmeticTarget =

    let decompose (ptr : ManagedPointerSource) : ArithmeticTarget =
        match ptr with
        | ManagedPointerSource.Null ->
            failwith
                "refusing to decompose the null byref into root-plus-offset; it is the bit pattern 0, and callers must handle bit-pattern byrefs before decomposing"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"refusing to do pointer arithmetic on fake non-null byref @ 0x%x{bits}; the placeholder must never be advanced"
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
            ArithmeticTarget.StackMemoryTarget (thread, frame, block, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), []) ->
            ArithmeticTarget.NativeMemoryTarget (block, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            ArithmeticTarget.ArrayTarget (arr, index)
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            ArithmeticTarget.StringTarget (str, charIndex)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field), []) ->
            ArithmeticTarget.FieldTarget (FieldContainer.HeapObject addr, field)
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.Field field :: revRest ->
                let parentPtr = ManagedPointerSource.Byref (root, List.rev revRest)
                ArithmeticTarget.FieldTarget (FieldContainer.ByrefContainer parentPtr, field)
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs ty :: revRest ->
                ArithmeticTarget.ByteViewTarget (root, List.rev revRest, ty, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: {ptr} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs ty :: revRest ->
                ArithmeticTarget.ByteViewTarget (root, List.rev revRest, ty, 0)
            | [] ->
                match root with
                | ByrefRoot.LocalVariable _
                | ByrefRoot.Argument _
                | ByrefRoot.StaticField _
                | ByrefRoot.HeapValue _ -> ArithmeticTarget.WholeValueTarget ptr
                | ByrefRoot.PeByteRange range ->
                    // A PE byte range is byte-addressed, so offsetting it wants a byte cursor
                    // rather than field resolution. Nothing asks for it yet, and inventing a
                    // field-shaped answer for byte-shaped storage would be a lie.
                    failwith
                        $"refusing to do pointer arithmetic on the whole PE byte range %O{range}: offsetting a byte-addressed range needs a byte cursor, which no caller has needed yet"
                | ByrefRoot.ExposedClassObject target ->
                    // One objref cache cell, with no interior structure to offset into.
                    failwith
                        $"refusing to do pointer arithmetic on the RuntimeType cache cell for %O{target}: the cell holds a single object reference and has no interior to address"
                | ByrefRoot.StackMemoryByte _
                | ByrefRoot.NativeMemoryByte _
                | ByrefRoot.ArrayElement _
                | ByrefRoot.StringCharAt _
                | ByrefRoot.HeapObjectField _ ->
                    failwith
                        $"byref %O{ptr} has a byte-, element- or field-addressed root and no projections, which the arms above already match (this is an interpreter bug)"

    let getFieldContainerValue
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (container : FieldContainer)
        : CliType
        =
        match container with
        | FieldContainer.HeapObject addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | FieldContainer.ByrefContainer ptr -> IlMachineState.readManagedByref baseClassTypes state ptr

/// Whether an arithmetic operation wraps on overflow (`add`, `sub`) or traps
/// (`add.ovf`, `sub.ovf`). This is only observable in the pointer helpers at
/// the `NativeIntPlaceholder` arms: everywhere else our pointers are symbolic
/// (a root plus offsets), and the offsets have no machine-width bit pattern
/// that could overflow. A placeholder's payload, by contrast, is a genuine
/// native-int bit pattern from `(void*)bits`, so the checked forms must trap on
/// it exactly as the CLR would.
[<RequireQualifiedAccess>]
type private OverflowBehaviour =
    | Wrap
    | Trap

type IArithmeticOperation =
    abstract Int32Int32 : int32 -> int32 -> int32
    abstract Int32NativeInt : int32 -> nativeint -> nativeint
    abstract NativeIntInt32 : nativeint -> int32 -> nativeint
    abstract Int64Int64 : int64 -> int64 -> int64
    abstract FloatFloat : float -> float -> float
    abstract NativeIntNativeInt : nativeint -> nativeint -> nativeint

    /// This int64 return type should be wrapped in NativeIntSource or Int64Source, for example, as soon
    /// as you obtain it.
    abstract CrossArrayOffsets : SyntheticCrossArrayOffset -> SyntheticCrossArrayOffset -> int64

    /// `int op &`. The offset is a native int (int64 on this 64-bit
    /// interpreter): a bit-pattern byref can legitimately be offset by more
    /// than an int32 holds, and it is the symbolic byref path — whose offsets
    /// really are int32 — that narrows and fails.
    abstract Int64ManagedPtr :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        int64 ->
        ManagedPointerSource ->
            Choice<ManagedPointerSource, int64>

    /// `& op int`. See <see cref="Int64ManagedPtr"/> for the offset width.
    abstract ManagedPtrInt64 :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        ManagedPointerSource ->
        int64 ->
            Choice<ManagedPointerSource, int64>

    abstract ManagedPtrManagedPtr :
        BaseClassTypes<DumpedAssembly> ->
        IlMachineState ->
        ManagedPointerSource ->
        ManagedPointerSource ->
            Choice<ManagedPointerSource, NativeIntSource>

    abstract Name : string

[<RequireQualifiedAccess>]
module ArithmeticOperation =
    let private verbatimInt64 (value : int64) : NativeIntSource = NativeIntSource.Verbatim value

    /// Arithmetic on a `NativeIntPlaceholder`'s bit pattern. `Trap` raises
    /// OverflowException, which the opcode handlers turn into a guest
    /// `System.OverflowException`.
    let private addPlaceholderBits (behaviour : OverflowBehaviour) (a : int64) (b : int64) : int64 =
        match behaviour with
        | OverflowBehaviour.Wrap -> a + b
        | OverflowBehaviour.Trap -> Checked.(+) a b

    /// See <see cref="addPlaceholderBits"/>.
    let private subPlaceholderBits (behaviour : OverflowBehaviour) (a : int64) (b : int64) : int64 =
        match behaviour with
        | OverflowBehaviour.Wrap -> a - b
        | OverflowBehaviour.Trap -> Checked.(-) a b

    /// Symbolic byrefs are a root plus an int32 offset (an array index, a
    /// field byte offset), so an offset that does not fit cannot be
    /// applied to one. Bit-pattern byrefs must be peeled off before this is
    /// reached — their offsets are native-int wide.
    ///
    /// 64-bit assumption: on 32-bit, the BCL's wraparound idiom intentionally
    /// produces oversize int64 offsets and relies on a subsequent `conv.u`
    /// truncating mod 2^32. We don't model that, so an oversize offset onto a
    /// symbolic byref is an error rather than a truncation.
    let private narrowSymbolicOffset (v : int64) : int32 =
        if v > int64<int32> System.Int32.MaxValue || v < int64<int32> System.Int32.MinValue then
            failwith
                $"managed pointer arithmetic: offset %d{v} does not fit the int32 symbolic byref offset model (array indices and field byte offsets are int32); only bit-pattern byrefs accept native-int-wide offsets"

        int32<int64> v

    let private checkedAddInt32 (context : string) (a : int) (b : int) : int =
        let result = int64 a + int64 b

        if result > int64 System.Int32.MaxValue || result < int64 System.Int32.MinValue then
            failwith $"managed pointer arithmetic (%s{context}) overflowed int32 offset model: %d{a} + %d{b}"

        int result

    let private charConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Char

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"System.Char concrete handle %O{handle} was not registered")

    let private byteConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"System.Byte concrete handle %O{handle} was not registered")

    let private crossArrayPointerDelta
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr1 : ManagedHeapAddress)
        (index1 : int)
        (byteOffset1 : int)
        (arr2 : ManagedHeapAddress)
        (index2 : int)
        (byteOffset2 : int)
        : NativeIntSource
        =
        if arr1 = arr2 then
            failwith "crossArrayPointerDelta called for two byrefs into the same array"

        let position1 =
            ManagedPointerByteView.arrayBytePosition state arr1 index1 (int64 byteOffset1)

        let position2 =
            ManagedPointerByteView.arrayBytePosition state arr2 index2 (int64 byteOffset2)

        NativeIntSource.syntheticCrossStorageByteOffset
            (ByteStorageIdentity.Array arr2)
            position2
            (ByteStorageIdentity.Array arr1)
            position1

    let private subtractArrayByteLocations
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr1 : ManagedHeapAddress)
        (index1 : int)
        (offset1 : int)
        (arr2 : ManagedHeapAddress)
        (index2 : int)
        (offset2 : int)
        : NativeIntSource
        =
        if arr1 <> arr2 then
            // Distinct PawPrint arrays have no real byte distance. Keep the
            // result tagged so later arithmetic cannot silently compose it.
            crossArrayPointerDelta baseClassTypes state arr1 index1 offset1 arr2 index2 offset2
        else
            let elementSize = ManagedPointerByteView.arrayElementSize state arr1

            let cellDelta = (int64 index1 - int64 index2) * int64 elementSize
            let byteDelta = cellDelta + int64 (offset1 - offset2)

            verbatimInt64 byteDelta

    let private addOffsetToManagedPtr
        (behaviour : OverflowBehaviour)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (offset : int64)
        (ptr : ManagedPointerSource)
        : Choice<ManagedPointerSource, int64>
        =
        match ManagedPointerSource.tryBitPatternBits ptr with
        | ValueSome bits ->
            // `(void*)bits + v = (void*)(bits + v)`. GetNonNullPinnableReference
            // produces an empty span whose pointer is the placeholder; callers
            // then form an end pointer by adding `length * elementSize` (which is
            // zero for an empty span, but in general arithmetic on the bits is
            // legitimate as long as no dereference occurs). No narrowing here:
            // the bits are a native int, so the offset may be wider than int32.
            addPlaceholderBits behaviour bits offset
            |> ManagedPointerSource.ofBitPattern
            |> Choice1Of2
        | ValueNone ->

        let v = narrowSymbolicOffset offset

        match ArithmeticTarget.decompose ptr with
        | ArithmeticTarget.StackMemoryTarget (thread, frame, block, byteOffset) ->
            let byteOffset = checkedAddInt32 "localloc byte offset" byteOffset v

            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), [])
            |> Choice1Of2
        | ArithmeticTarget.NativeMemoryTarget (block, byteOffset) ->
            let byteOffset = checkedAddInt32 "native memory byte offset" byteOffset v

            ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), [])
            |> Choice1Of2
        | ArithmeticTarget.ArrayTarget (arr, index) ->
            let index = checkedAddInt32 "array index" index v

            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), [])
            |> Choice1Of2
        | ArithmeticTarget.StringTarget (str, charIndex) ->
            let charType = charConcreteType baseClassTypes state

            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), [])
            |> ManagedPointerSource.addByteOffsetUnderReinterpret
                ByteOffsetNormalisationContext.nonArrayRootsOnly
                charType
                v
            |> Choice1Of2
        | ArithmeticTarget.FieldTarget (container, field) ->
            let obj = ArithmeticTarget.getFieldContainerValue baseClassTypes state container

            let offset, _ = CliType.getFieldLayoutById field obj
            let offset = checkedAddInt32 "field byte offset" offset v

            match CliType.getFieldAt offset obj with
            | None ->
                match container with
                | FieldContainer.HeapObject addr ->
                    let byteType = byteConcreteType baseClassTypes state

                    ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
                    |> ManagedPointerByteView.addByteOffset state byteType offset
                    |> Choice1Of2
                | FieldContainer.ByrefContainer parentPtr ->
                    let byteType = byteConcreteType baseClassTypes state

                    parentPtr
                    |> ManagedPointerByteView.addByteOffset state byteType offset
                    |> Choice1Of2
            | Some field ->
                let newField = CliConcreteField.ToCliField(field).Id

                let newPtr =
                    match container with
                    | FieldContainer.HeapObject addr ->
                        ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, newField), [])
                    | FieldContainer.ByrefContainer parentPtr ->
                        ManagedPointerSource.appendProjection (ByrefProjection.Field newField) parentPtr

                Choice1Of2 newPtr
        | ArithmeticTarget.WholeValueTarget ptr ->
            if v = 0 then
                // `p + 0` is `p`, structurally and not merely by address: one byte location gets
                // one structural form, which is the same canonicalisation the non-zero branch
                // below performs when it folds whole cells into the root. A byref's access width
                // comes from the access rather than from the slot (see `writeManagedByrefCore`),
                // so nothing downstream depends on which spelling the guest produced.
                Choice1Of2 ptr
            else

            // A byte cursor, never a resolved field. Advancing a raw pointer moves an address; it
            // does not choose a type view, and the access width only arrives later with the
            // `ldind`/`stind` that dereferences it. Resolving to whichever field begins at this
            // offset would decide that width early and get it wrong two ways:
            //   * `*(int*)((byte*)&v + 4)` over four `byte` fields at offsets 4..7 would read one
            //     byte instead of spanning all four;
            //   * `(p + n) - n` would come back as the field at offset 0 rather than as `p`, so a
            //     round trip would compare unequal to where it started.
            // Both are silent wrong answers rather than loud failures. The byte cursor normalises
            // `ByteOffset 0` away on the way back, so the round trip really does return.
            //
            // A slot whose value has no byte image produces a cursor that fails when it is read or
            // written rather than here; that is the byte-scatter walks' report to make, and a loud
            // failure there beats a plausible answer here.
            let byteType = byteConcreteType baseClassTypes state

            ptr |> ManagedPointerByteView.addByteOffset state byteType v |> Choice1Of2
        | ArithmeticTarget.ByteViewTarget _ ->
            // Walk the byte cursor under the trailing reinterpret. The reinterpret
            // stays (it's the type view the caller set up); the byte offset
            // accumulates. A zero result drops the ByteOffset so stripping
            // behaviour and byref equality continue to normalise.
            // Fold whole cells into the root when possible: two byrefs
            // denoting the same byte location must share one structural form,
            // else equality (Unsafe.AreSame, ceq) spuriously returns false
            // when the cursor lands on another cell boundary.
            ptr |> ManagedPointerByteView.addByteOffsetToByteView state v |> Choice1Of2

    let private mulOffsetManagedPtr
        (state : IlMachineState)
        (v : int64)
        (ptr : ManagedPointerSource)
        : Choice<ManagedPointerSource, int64>
        =
        if v = 0L then
            Choice2Of2 0L
        elif v = 1L then
            Choice1Of2 ptr
        else

        match ptr with
        | ManagedPointerSource.Null -> Choice2Of2 0L
        | _ -> failwith "refusing to multiply pointers"

    let add =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add" a b : int32 #)
            member _.Int64Int64 a b = (# "add" a b : int64 #)
            member _.FloatFloat a b = (# "add" a b : float #)
            member _.NativeIntNativeInt a b = (# "add" a b : nativeint #)
            member _.Int32NativeInt a b = (# "add" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "add" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                if a = SyntheticCrossArrayOffset.negate b then
                    0L
                else
                    failwith "refusing to add SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int64ManagedPtr baseClassTypes state val1 ptr2 =
                addOffsetToManagedPtr OverflowBehaviour.Wrap baseClassTypes state val1 ptr2

            member _.ManagedPtrInt64 baseClassTypes state ptr1 val2 =
                addOffsetToManagedPtr OverflowBehaviour.Wrap baseClassTypes state val2 ptr1

            member _.Name = "add"
        }

    let addOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "add.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "add.ovf" a b : int64 #)
            member _.FloatFloat a b = (# "add.ovf" a b : float #)
            member _.NativeIntNativeInt a b = (# "add.ovf" a b : nativeint #)
            member _.Int32NativeInt a b = (# "add.ovf" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "add.ovf" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                if a = SyntheticCrossArrayOffset.negate b then
                    0L
                else
                    failwith "refusing to add_ovf SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice1Of2 ptr2
                | _, ManagedPointerSource.Null -> Choice1Of2 ptr1
                | _, _ -> failwith "refusing to add two managed pointers"

            member _.Int64ManagedPtr baseClassTypes state val1 ptr2 =
                addOffsetToManagedPtr OverflowBehaviour.Trap baseClassTypes state val1 ptr2

            member _.ManagedPtrInt64 baseClassTypes state ptr1 val2 =
                addOffsetToManagedPtr OverflowBehaviour.Trap baseClassTypes state val2 ptr1

            member _.Name = "add.ovf"
        }

    /// Whether both pointers reach into the *same* argument slot.
    ///
    /// Argument-rooted pointers are otherwise refused outright by subtraction: two different
    /// argument slots are separate storage with no byte distance between them, and an argument
    /// paired with anything else has none either. Two pointers into one argument slot do have a
    /// distance, which is what `&arg + n` produces, so that pair is let through to the
    /// ordinary decomposition.
    let private sameArgumentRoot (ptr1 : ManagedPointerSource) (ptr2 : ManagedPointerSource) : bool =
        match ptr1, ptr2 with
        | ManagedPointerSource.Byref (ByrefRoot.Argument (thread1, frame1, index1), _),
          ManagedPointerSource.Byref (ByrefRoot.Argument (thread2, frame2, index2), _) ->
            thread1 = thread2 && frame1 = frame2 && index1 = index2
        | _ -> false

    /// Pointer subtraction is shared between `sub` and `sub.ovf`: ECMA-335
    /// gives both the same `& - int -> &` and `& - & -> native int`
    /// signatures. `behaviour` is only consulted for the bit-pattern
    /// placeholder arms; every other arm is symbolic, and the int32 offset
    /// model's own limits are enforced by `checkedAddInt32`, which fails
    /// loudly (an interpreter limitation) rather than throwing
    /// OverflowException into the guest.
    let private subManagedPtrManagedPtr
        (behaviour : OverflowBehaviour)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr1 : ManagedPointerSource)
        (ptr2 : ManagedPointerSource)
        : Choice<ManagedPointerSource, NativeIntSource>
        =
        match ManagedPointerSource.tryBitPatternBits ptr1, ManagedPointerSource.tryBitPatternBits ptr2 with
        // `& - & -> native int`. Both sides are raw bit patterns here
        // (`Unsafe.AsRef<T>((void*)bits)`, or `Null` at bits = 0), so the
        // difference is plain bit subtraction: GetNonNullPinnableReference
        // uses `endPtr - startPtr` to recover an empty span's byte length,
        // which is 0 when the two share a bit pattern. `0 - Int64.MinValue`
        // overflows, so the checked form traps here. This must precede the
        // generic `Null`-on-the-right arm below, which would otherwise
        // return the left byref for `placeholder - Null`.
        | ValueSome bits1, ValueSome bits2 -> subPlaceholderBits behaviour bits1 bits2 |> verbatimInt64 |> Choice2Of2
        | _ ->

        match ptr1, ptr2 with
        // Subtracting the zero bit pattern from a symbolic byref leaves it
        // where it was.
        | ptr1, ManagedPointerSource.Null -> Choice1Of2 ptr1
        | ManagedPointerSource.Null, _ -> failwith "refusing to create negative pointer"
        | ManagedPointerSource.NativeIntPlaceholder _, _
        | _, ManagedPointerSource.NativeIntPlaceholder _ ->
            failwith $"refusing to subtract through fake non-null byref placeholder: %O{ptr1} and %O{ptr2}"
        | ManagedPointerSource.Byref (ByrefRoot.Argument _, _), _
        | _, ManagedPointerSource.Byref (ByrefRoot.Argument _, _) when not (sameArgumentRoot ptr1 ptr2) ->
            failwith $"refusing to operate on pointers to arguments: %O{ptr1} and %O{ptr2}"
        | ManagedPointerSource.Byref _, ManagedPointerSource.Byref _ ->
            match ArithmeticTarget.decompose ptr1, ArithmeticTarget.decompose ptr2 with
            | ArithmeticTarget.StackMemoryTarget (thread1, frame1, block1, byteOffset1),
              ArithmeticTarget.StackMemoryTarget (thread2, frame2, block2, byteOffset2) ->
                if thread1 = thread2 && frame1 = frame2 && block1 = block2 then
                    int64 byteOffset1 - int64 byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.StackMemory (thread2, frame2, block2))
                        (int64 byteOffset2)
                        (ByteStorageIdentity.StackMemory (thread1, frame1, block1))
                        (int64 byteOffset1)
                    |> Choice2Of2
            | ArithmeticTarget.NativeMemoryTarget (block1, byteOffset1),
              ArithmeticTarget.NativeMemoryTarget (block2, byteOffset2) ->
                if block1 = block2 then
                    int64 byteOffset1 - int64 byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.NativeMemory block2)
                        (int64 byteOffset2)
                        (ByteStorageIdentity.NativeMemory block1)
                        (int64 byteOffset1)
                    |> Choice2Of2
            | ArithmeticTarget.ArrayTarget (arr1, index1), ArithmeticTarget.ArrayTarget (arr2, index2) ->
                subtractArrayByteLocations baseClassTypes state arr1 index1 0 arr2 index2 0
                |> Choice2Of2
            | ArithmeticTarget.StringTarget (str1, index1), ArithmeticTarget.StringTarget (str2, index2) ->
                if str1 <> str2 then
                    failwith $"refusing to subtract character pointers into different strings: %O{str1} vs %O{str2}"

                (int64 index1 - int64 index2) * 2L |> verbatimInt64 |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.ArrayElement (arr1, index1), prefix1, _, offset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.ArrayElement (arr2, index2), prefix2, _, offset2) when
                prefix1 = prefix2
                ->
                subtractArrayByteLocations baseClassTypes state arr1 index1 offset1 arr2 index2 offset2
                |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.StackMemoryByte (thread1, frame1, block1, rootOffset1),
                                               prefix1,
                                               _,
                                               offset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.StackMemoryByte (thread2, frame2, block2, rootOffset2),
                                               prefix2,
                                               _,
                                               offset2) when prefix1 = prefix2 ->
                let byteOffset1 = int64 rootOffset1 + int64 offset1
                let byteOffset2 = int64 rootOffset2 + int64 offset2

                if thread1 = thread2 && frame1 = frame2 && block1 = block2 then
                    byteOffset1 - byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.StackMemory (thread2, frame2, block2))
                        byteOffset2
                        (ByteStorageIdentity.StackMemory (thread1, frame1, block1))
                        byteOffset1
                    |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.StackMemoryByte (thread1, frame1, block1, rootOffset1),
                                               [],
                                               _,
                                               offset1),
              ArithmeticTarget.StackMemoryTarget (thread2, frame2, block2, byteOffset2) ->
                let byteOffset1 = int64 rootOffset1 + int64 offset1

                if thread1 = thread2 && frame1 = frame2 && block1 = block2 then
                    byteOffset1 - int64 byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.StackMemory (thread2, frame2, block2))
                        (int64 byteOffset2)
                        (ByteStorageIdentity.StackMemory (thread1, frame1, block1))
                        byteOffset1
                    |> Choice2Of2
            | ArithmeticTarget.StackMemoryTarget (thread1, frame1, block1, byteOffset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.StackMemoryByte (thread2, frame2, block2, rootOffset2),
                                               [],
                                               _,
                                               offset2) ->
                let byteOffset2 = int64 rootOffset2 + int64 offset2

                if thread1 = thread2 && frame1 = frame2 && block1 = block2 then
                    int64 byteOffset1 - byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.StackMemory (thread2, frame2, block2))
                        byteOffset2
                        (ByteStorageIdentity.StackMemory (thread1, frame1, block1))
                        (int64 byteOffset1)
                    |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.NativeMemoryByte (block1, rootOffset1), prefix1, _, offset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.NativeMemoryByte (block2, rootOffset2), prefix2, _, offset2) when
                prefix1 = prefix2
                ->
                let byteOffset1 = int64 rootOffset1 + int64 offset1
                let byteOffset2 = int64 rootOffset2 + int64 offset2

                if block1 = block2 then
                    byteOffset1 - byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.NativeMemory block2)
                        byteOffset2
                        (ByteStorageIdentity.NativeMemory block1)
                        byteOffset1
                    |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.NativeMemoryByte (block1, rootOffset1), [], _, offset1),
              ArithmeticTarget.NativeMemoryTarget (block2, byteOffset2) ->
                let byteOffset1 = int64 rootOffset1 + int64 offset1

                if block1 = block2 then
                    byteOffset1 - int64 byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.NativeMemory block2)
                        (int64 byteOffset2)
                        (ByteStorageIdentity.NativeMemory block1)
                        byteOffset1
                    |> Choice2Of2
            | ArithmeticTarget.NativeMemoryTarget (block1, byteOffset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.NativeMemoryByte (block2, rootOffset2), [], _, offset2) ->
                let byteOffset2 = int64 rootOffset2 + int64 offset2

                if block1 = block2 then
                    int64 byteOffset1 - byteOffset2 |> verbatimInt64 |> Choice2Of2
                else
                    NativeIntSource.syntheticCrossStorageByteOffset
                        (ByteStorageIdentity.NativeMemory block2)
                        byteOffset2
                        (ByteStorageIdentity.NativeMemory block1)
                        (int64 byteOffset1)
                    |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.StringCharAt (str1, index1), prefix1, _, offset1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.StringCharAt (str2, index2), prefix2, _, offset2) when
                prefix1 = prefix2
                ->
                if str1 <> str2 then
                    failwith
                        $"refusing to subtract character byte-view pointers into different strings: %O{str1} vs %O{str2}"

                ((int64 index1 * 2L + int64 offset1) - (int64 index2 * 2L + int64 offset2))
                |> verbatimInt64
                |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (ByrefRoot.StringCharAt (str1, index1), [], _, offset1),
              ArithmeticTarget.StringTarget (str2, index2) ->
                if str1 <> str2 then
                    failwith
                        $"refusing to subtract character byte-view pointer from pointer into different string: %O{str1} vs %O{str2}"

                ((int64 index1 * 2L + int64 offset1) - int64 index2 * 2L)
                |> verbatimInt64
                |> Choice2Of2
            | ArithmeticTarget.StringTarget (str1, index1),
              ArithmeticTarget.ByteViewTarget (ByrefRoot.StringCharAt (str2, index2), [], _, offset2) ->
                if str1 <> str2 then
                    failwith
                        $"refusing to subtract character pointer from byte-view pointer into different string: %O{str1} vs %O{str2}"

                (int64 index1 * 2L - (int64 index2 * 2L + int64 offset2))
                |> verbatimInt64
                |> Choice2Of2
            | ArithmeticTarget.FieldTarget (container1, field1), ArithmeticTarget.FieldTarget (container2, field2) ->
                if container1 <> container2 then
                    failwith
                        $"refusing to subtract pointers to fields of different containers: %O{container1} vs %O{container2}"

                let obj1 = ArithmeticTarget.getFieldContainerValue baseClassTypes state container1
                let obj2 = ArithmeticTarget.getFieldContainerValue baseClassTypes state container2

                let offset1, _ = CliType.getFieldLayoutById field1 obj1
                let offset2, _ = CliType.getFieldLayoutById field2 obj2

                int64 offset1 - int64 offset2 |> verbatimInt64 |> Choice2Of2
            | ArithmeticTarget.ByteViewTarget (root1, prefix1, _, off1),
              ArithmeticTarget.ByteViewTarget (root2, prefix2, _, off2) when root1 = root2 && prefix1 = prefix2 ->
                // Same underlying storage; subtraction is the byte-offset
                // delta regardless of which `ReinterpretAs` type was used
                // on each side (the view is address-preserving).
                int64 off1 - int64 off2 |> verbatimInt64 |> Choice2Of2
            // A whole-slot byref and a byte cursor over that same slot. `&slot + n` produces
            // exactly this pair, so `int* q = p + n; q - p;` reaches here; without these arms
            // advancing a whole-slot pointer would work while measuring the advance would not.
            // The slot's own address is byte offset zero, so the delta is just the cursor's.
            | ArithmeticTarget.ByteViewTarget (root1, [], _, off1), ArithmeticTarget.WholeValueTarget slot2 when
                ManagedPointerSource.Byref (root1, []) = slot2
                ->
                int64 off1 |> verbatimInt64 |> Choice2Of2
            | ArithmeticTarget.WholeValueTarget slot1, ArithmeticTarget.ByteViewTarget (root2, [], _, off2) when
                slot1 = ManagedPointerSource.Byref (root2, [])
                ->
                -(int64 off2) |> verbatimInt64 |> Choice2Of2
            | ArithmeticTarget.WholeValueTarget slot1, ArithmeticTarget.WholeValueTarget slot2 ->
                if slot1 <> slot2 then
                    // Two distinct slots have no byte distance to report: locals, arguments,
                    // statics and boxes are separate storage here, not offsets into one address
                    // space. Refuse rather than invent a number.
                    failwith
                        $"refusing to subtract pointers to two distinct whole storage slots: %O{slot1} vs %O{slot2}"

                verbatimInt64 0L |> Choice2Of2
            | ArithmeticTarget.StackMemoryTarget _, _
            | _, ArithmeticTarget.StackMemoryTarget _ ->
                failwith $"refusing to subtract localloc byte pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
            | ArithmeticTarget.NativeMemoryTarget _, _
            | _, ArithmeticTarget.NativeMemoryTarget _ ->
                failwith
                    $"refusing to subtract native memory byte pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
            | ArithmeticTarget.ArrayTarget _, _
            | _, ArithmeticTarget.ArrayTarget _ ->
                failwith $"refusing to subtract array element pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
            | ArithmeticTarget.StringTarget _, _
            | _, ArithmeticTarget.StringTarget _ ->
                failwith
                    $"refusing to subtract string character pointer from incompatible pointer: %O{ptr1} vs %O{ptr2}"
            | target1, target2 ->
                failwith
                    $"TODO: subtracting incompatible managed pointer targets is not implemented: %O{target1} vs %O{target2} (%O{ptr1} vs %O{ptr2})"

    /// `int - &` is not in the ECMA-335 table, so this only tolerates the
    /// degenerate null case, where the byref contributes the bit pattern 0 and
    /// the result is just the integer.
    let private subOffsetManagedPtr (val1 : int64) (ptr2 : ManagedPointerSource) : Choice<ManagedPointerSource, int64> =
        match ptr2 with
        | ManagedPointerSource.Null -> Choice2Of2 val1
        | _ -> failwith "refusing to subtract a pointer"

    let private subOffsetFromManagedPtr
        (behaviour : OverflowBehaviour)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr1 : ManagedPointerSource)
        (offset : int64)
        : Choice<ManagedPointerSource, int64>
        =
        match ManagedPointerSource.tryBitPatternBits ptr1 with
        | ValueSome bits ->
            // Subtract directly rather than routing through the negation
            // below: `bits - Int64.MinValue` must trap under a checked op
            // rather than silently wrapping through `-offset`.
            subPlaceholderBits behaviour bits offset
            |> ManagedPointerSource.ofBitPattern
            |> Choice1Of2
        | ValueNone ->

        if offset = System.Int64.MinValue then
            failwith
                "managed pointer subtraction by Int64.MinValue is not representable: negating it overflows native int"

        addOffsetToManagedPtr behaviour baseClassTypes state (-offset) ptr1

    let sub =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "sub" a b : int32 #)
            member _.Int64Int64 a b = (# "sub" a b : int64 #)
            member _.FloatFloat a b = (# "sub" a b : float #)
            member _.NativeIntNativeInt a b = (# "sub" a b : nativeint #)
            member _.Int32NativeInt a b = (# "sub" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "sub" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                if a = b then
                    0L
                else
                    failwith "refusing to sub SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr baseClassTypes state ptr1 ptr2 =
                subManagedPtrManagedPtr OverflowBehaviour.Wrap baseClassTypes state ptr1 ptr2

            member _.Int64ManagedPtr _ _ val1 ptr2 = subOffsetManagedPtr val1 ptr2

            member _.ManagedPtrInt64 baseClassTypes state ptr1 val2 =
                subOffsetFromManagedPtr OverflowBehaviour.Wrap baseClassTypes state ptr1 val2

            member _.Name = "sub"
        }

    let subOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "sub.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "sub.ovf" a b : int64 #)

            // ECMA-335 III.3.68: sub.ovf takes int32, int64, native int and &.
            // Floats have no overflow trap, so a verifier would reject float
            // operands here; reaching this arm means the IL was malformed.
            member _.FloatFloat a b =
                failwith $"refusing to sub.ovf float values: %f{a} and %f{b}"

            member _.NativeIntNativeInt a b = (# "sub.ovf" a b : nativeint #)
            member _.Int32NativeInt a b = (# "sub.ovf" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "sub.ovf" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                if a = b then
                    0L
                else
                    failwith "refusing to sub_ovf SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr baseClassTypes state ptr1 ptr2 =
                subManagedPtrManagedPtr OverflowBehaviour.Trap baseClassTypes state ptr1 ptr2

            member _.Int64ManagedPtr _ _ val1 ptr2 = subOffsetManagedPtr val1 ptr2

            member _.ManagedPtrInt64 baseClassTypes state ptr1 val2 =
                subOffsetFromManagedPtr OverflowBehaviour.Trap baseClassTypes state ptr1 val2

            member _.Name = "sub.ovf"
        }

    let mul =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul" a b : int32 #)
            member _.Int64Int64 a b = (# "mul" a b : int64 #)
            member _.FloatFloat a b = (# "mul" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul" a b : nativeint #)
            member _.Int32NativeInt a b = (# "mul" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "mul" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to mul SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, ManagedPointerSource.Null -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int64ManagedPtr _ state a ptr = mulOffsetManagedPtr state a ptr
            member _.ManagedPtrInt64 _ state ptr a = mulOffsetManagedPtr state a ptr

            member _.Name = "mul"
        }

    let rem =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "rem" a b : int32 #)
            member _.Int64Int64 a b = (# "rem" a b : int64 #)
            member _.FloatFloat a b = (# "rem" a b : float #)
            member _.NativeIntNativeInt a b = (# "rem" a b : nativeint #)
            member _.Int32NativeInt a b = (# "rem" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "rem" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to rem SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 = failwith "refusing to rem pointers"

            member _.Int64ManagedPtr _ _ a ptr = failwith "refusing to rem pointer"

            member _.ManagedPtrInt64 _ _ ptr a = failwith "refusing to rem pointer"

            member _.Name = "rem"
        }

    let remUn =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "rem.un" a b : int32 #)
            member _.Int64Int64 a b = (# "rem.un" a b : int64 #)

            member _.FloatFloat a b =
                failwith $"refusing to rem.un float values: %f{a} and %f{b}"

            member _.NativeIntNativeInt a b = (# "rem.un" a b : nativeint #)
            member _.Int32NativeInt a b = (# "rem.un" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "rem.un" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to rem_un SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 = failwith "refusing to rem.un pointers"

            member _.Int64ManagedPtr _ _ a ptr = failwith "refusing to rem.un pointer"

            member _.ManagedPtrInt64 _ _ ptr a = failwith "refusing to rem.un pointer"

            member _.Name = "rem.un"
        }

    let mulOvf =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul.ovf" a b : int32 #)
            member _.Int64Int64 a b = (# "mul.ovf" a b : int64 #)
            member _.FloatFloat a b = (# "mul.ovf" a b : float #)
            member _.NativeIntNativeInt a b = (# "mul.ovf" a b : nativeint #)
            member _.Int32NativeInt a b = (# "mul.ovf" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "mul.ovf" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to mul_ovf SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, ManagedPointerSource.Null -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to multiply two managed pointers"

            member _.Int64ManagedPtr _ state a ptr = mulOffsetManagedPtr state a ptr
            member _.ManagedPtrInt64 _ state a ptr = mulOffsetManagedPtr state ptr a

            member _.Name = "mul_ovf"
        }

    let mulOvfUn =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "mul.ovf.un" a b : int32 #)
            member _.Int64Int64 a b = (# "mul.ovf.un" a b : int64 #)

            member _.FloatFloat a b =
                failwith $"refusing to mul.ovf.un float values: %f{a} and %f{b}"

            member _.NativeIntNativeInt a b = (# "mul.ovf.un" a b : nativeint #)
            member _.Int32NativeInt a b = (# "mul.ovf.un" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "mul.ovf.un" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to mul_ovf_un SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                failwith $"refusing to mul.ovf.un two managed pointers: %O{ptr1} and %O{ptr2}"

            member _.Int64ManagedPtr _ state a ptr = mulOffsetManagedPtr state a ptr
            member _.ManagedPtrInt64 _ state a ptr = mulOffsetManagedPtr state ptr a

            member _.Name = "mul.ovf.un"
        }

    let div =
        { new IArithmeticOperation with
            member _.Int32Int32 a b = (# "div" a b : int32 #)
            member _.Int64Int64 a b = (# "div" a b : int64 #)
            member _.FloatFloat a b = (# "div" a b : float #)
            member _.NativeIntNativeInt a b = (# "div" a b : nativeint #)
            member _.Int32NativeInt a b = (# "div" a b : nativeint #)
            member _.NativeIntInt32 a b = (# "div" a b : nativeint #)

            member _.CrossArrayOffsets a b =
                failwith "refusing to div SyntheticCrossArrayOffsets"

            member _.ManagedPtrManagedPtr _ _ ptr1 ptr2 =
                match ptr1, ptr2 with
                | ManagedPointerSource.Null, _ -> Choice2Of2 (NativeIntSource.Verbatim 0L)
                | _, _ -> failwith "refusing to divide two managed pointers"

            member _.Int64ManagedPtr _ _ a ptr =
                if a = 0L then
                    Choice2Of2 0L
                else
                    failwith "refusing to divide pointers"

            member _.ManagedPtrInt64 _ _ ptr a =
                if a = 1L then
                    Choice1Of2 ptr
                else
                    failwith "refusing to divide a pointer"

            member _.Name = "div"
        }

[<RequireQualifiedAccess>]
module BinaryArithmetic =
    /// Apply a binary arithmetic operation. Returns the result together with
    /// the (possibly updated) machine state — the WidenedNativeInt arms that
    /// materialise synthesised pointer-hash bits register new
    /// `PointerHashState` assignments on `state`; every other arm returns
    /// `state` unchanged. Callers MUST use the returned state, not the input
    /// state, when pushing the result.
    let execute
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : IArithmeticOperation)
        (state : IlMachineState)
        (val1 : EvalStackValue)
        (val2 : EvalStackValue)
        : EvalStackValue * IlMachineState
        =
        let managedPtrManagedPtr (ptr1 : ManagedPointerSource) (ptr2 : ManagedPointerSource) : EvalStackValue =
            match op.ManagedPtrManagedPtr baseClassTypes state ptr1 ptr2 with
            | Choice1Of2 ptr -> EvalStackValue.ManagedPointer ptr
            | Choice2Of2 offset -> EvalStackValue.NativeInt offset

        let managedPtrChoiceAsNativeInt (result : Choice<ManagedPointerSource, int64>) : EvalStackValue =
            match result with
            | Choice1Of2 ptr -> EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
            | Choice2Of2 i -> EvalStackValue.NativeInt (NativeIntSource.Verbatim i)

        let managedPtrManagedPtrAsNativeInt (result : Choice<ManagedPointerSource, NativeIntSource>) : EvalStackValue =
            match result with
            | Choice1Of2 ptr -> EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
            | Choice2Of2 offset -> EvalStackValue.NativeInt offset

        // The offset stays native-int wide here. Narrowing to the int32
        // symbolic offset model is the *pointer* path's business
        // (`narrowSymbolicOffset`), because only a symbolic byref has an
        // int32 offset; a bit-pattern byref's offset is a native int and may
        // legitimately exceed int32.
        let nativeIntOffsetForPointerArithmetic (src : NativeIntSource) : int64 =
            match src with
            | NativeIntSource.Verbatim n -> n
            // A null byref used as an offset is the bit pattern 0.
            | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0L
            | v ->
                failwith
                    $"managed pointer arithmetic (%s{op.Name}): refusing to use non-verbatim native int %O{v} as pointer offset"

        let widenedManagedPtrChoiceAsInt64
            (signed : bool)
            (result : Choice<ManagedPointerSource, int64>)
            : EvalStackValue
            =
            match result with
            | Choice1Of2 ptr ->
                EvalStackValue.Int64 (Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ptr) signed)
            | Choice2Of2 i -> EvalStackValue.Int64 (Int64Source.Verbatim i)

        let materialise (src : NativeIntSource) (counters : PointerHashState) : int64 * PointerHashState =
            PointerHashSynthesis.materialiseHashBits $"BinaryArithmetic.%s{op.Name}" src counters

        let withState (esv : EvalStackValue) : EvalStackValue * IlMachineState = esv, state

        // see table at https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.opcodes.add?view=net-9.0
        match val1, val2 with
        | EvalStackValue.Int32 (Int32Source.Verbatim val1), EvalStackValue.Int32 (Int32Source.Verbatim val2) ->
            op.Int32Int32 val1 val2
            |> Int32Source.Verbatim
            |> EvalStackValue.Int32
            |> withState
        | EvalStackValue.Int32 (Int32Source.Verbatim val1),
          EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val2) ->
            op.Int64ManagedPtr baseClassTypes state (int64<int32> val1) val2
            |> managedPtrChoiceAsNativeInt
            |> withState
        | EvalStackValue.Int32 (Int32Source.Verbatim val1), EvalStackValue.NativeInt val2 ->
            let val2 =
                match val2 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            op.Int32NativeInt val1 val2
            |> int64<nativeint>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
            |> withState
        | EvalStackValue.Int32 (Int32Source.Verbatim val1), EvalStackValue.ManagedPointer val2 ->
            match op.Int64ManagedPtr baseClassTypes state (int64<int32> val1) val2 with
            | Choice1Of2 v -> EvalStackValue.ManagedPointer v |> withState
            | Choice2Of2 i ->
                // The numeric arms of `int op &` only ever hand back the input
                // integer or zero, so this cannot overflow; assert rather than
                // silently truncating if that ever changes.
                if i > int64<int32> System.Int32.MaxValue || i < int64<int32> System.Int32.MinValue then
                    failwith
                        $"managed pointer arithmetic (%s{op.Name}): int32 operand yielded out-of-range numeric result %d{i}"

                EvalStackValue.Int32 (Int32Source.Verbatim (int32<int64> i)) |> withState
        | EvalStackValue.Int32 (Int32Source.Verbatim val1), EvalStackValue.ObjectRef val2 ->
            failwith "" |> EvalStackValue.ObjectRef |> withState
        | EvalStackValue.Int32 _, EvalStackValue.NullObjectRef -> failwith ""
        | EvalStackValue.Int64 (Int64Source.Verbatim val1), EvalStackValue.Int64 (Int64Source.Verbatim val2) ->
            op.Int64Int64 val1 val2
            |> Int64Source.Verbatim
            |> EvalStackValue.Int64
            |> withState
        // Arithmetic on synthesised pointer-hash bits stays in the hash domain:
        // the bits are not a real numeric quantity, but the bit-mixing pipeline
        // (e.g. `hash * 11400714819323198485ul` in CastCache.KeyToBucket) needs
        // arithmetic ops to combine them. Keep the OpaqueHashBits tag so the
        // result can't round-trip back to a pointer via `conv.u`/`conv.i`.
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits val1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits val2) ->
            op.Int64Int64 val1 val2
            |> Int64Source.OpaqueHashBits
            |> EvalStackValue.Int64
            |> withState
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits val1), EvalStackValue.Int64 (Int64Source.Verbatim val2) ->
            op.Int64Int64 val1 val2
            |> Int64Source.OpaqueHashBits
            |> EvalStackValue.Int64
            |> withState
        | EvalStackValue.Int64 (Int64Source.Verbatim val1), EvalStackValue.Int64 (Int64Source.OpaqueHashBits val2) ->
            op.Int64Int64 val1 val2
            |> Int64Source.OpaqueHashBits
            |> EvalStackValue.Int64
            |> withState
        | EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset val1),
          EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset val2) ->
            op.CrossArrayOffsets val1 val2
            |> Int64Source.Verbatim
            |> EvalStackValue.Int64
            |> withState
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer val1, signed)),
          EvalStackValue.Int64 (Int64Source.Verbatim val2) ->
            op.ManagedPtrInt64 baseClassTypes state val1 val2
            |> widenedManagedPtrChoiceAsInt64 signed
            |> withState
        | EvalStackValue.Int64 (Int64Source.Verbatim val1),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer val2, signed)) ->
            op.Int64ManagedPtr baseClassTypes state val1 val2
            |> widenedManagedPtrChoiceAsInt64 signed
            |> withState
        // Arithmetic on a widened non-managed-pointer source materialises the
        // synthesised hash bits up-front, so a pointer-hash expression starting
        // with arithmetic (e.g. `(ulong)handle * C` for the CastCache golden-
        // ratio mix) doesn't fall through to "invalid operation". The
        // ManagedPointer arms above match first, so `src` here is always one
        // of the non-managed pointer shapes that `materialiseHashBits`
        // accepts (TypeHandlePtr, MethodTablePtr, function/method/field
        // handles, etc.). Result is tagged OpaqueHashBits so it can't
        // round-trip back to a pointer via `conv.u` / `conv.i`.
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)), EvalStackValue.Int64 (Int64Source.Verbatim val2) ->
            let val1, counters = materialise src state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }

            op.Int64Int64 val1 val2 |> Int64Source.OpaqueHashBits |> EvalStackValue.Int64, state
        | EvalStackValue.Int64 (Int64Source.Verbatim val1), EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            let val2, counters = materialise src state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }

            op.Int64Int64 val1 val2 |> Int64Source.OpaqueHashBits |> EvalStackValue.Int64, state
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)),
          EvalStackValue.Int64 (Int64Source.OpaqueHashBits val2) ->
            let val1, counters = materialise src state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }

            op.Int64Int64 val1 val2 |> Int64Source.OpaqueHashBits |> EvalStackValue.Int64, state
        | EvalStackValue.Int64 (Int64Source.OpaqueHashBits val1),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, _)) ->
            let val2, counters = materialise src state.PointerHashState

            let state =
                { state with
                    PointerHashState = counters
                }

            op.Int64Int64 val1 val2 |> Int64Source.OpaqueHashBits |> EvalStackValue.Int64, state
        | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src1, _)),
          EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src2, _)) ->
            // Mixing managed-pointer arithmetic with non-pointer hash bits in
            // the same op is unsupported — pointer × pointer arithmetic on
            // bare nativeints is itself rare, and falls through to the
            // existing "invalid operation" failwith if either side is a
            // managed pointer.
            match src1, src2 with
            | NativeIntSource.ManagedPointer _, _
            | _, NativeIntSource.ManagedPointer _ ->
                failwith
                    $"TODO: BinaryArithmetic %s{op.Name} on (WidenedNativeInt %O{src1}) and (WidenedNativeInt %O{src2}): one side is a managed pointer, the other isn't"
            | _ ->
                let val1, counters = materialise src1 state.PointerHashState
                let val2, counters = materialise src2 counters

                let state =
                    { state with
                        PointerHashState = counters
                    }

                op.Int64Int64 val1 val2 |> Int64Source.OpaqueHashBits |> EvalStackValue.Int64, state
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val1),
          EvalStackValue.Int32 (Int32Source.Verbatim val2) ->
            op.ManagedPtrInt64 baseClassTypes state val1 (int64<int32> val2)
            |> managedPtrChoiceAsNativeInt
            |> withState
        | EvalStackValue.NativeInt val1, EvalStackValue.Int32 (Int32Source.Verbatim val2) ->
            let val1 =
                match val1 with
                | NativeIntSource.Verbatim n -> nativeint<int64> n
                | v -> failwith $"refusing to operate on non-verbatim native int %O{v}"

            op.NativeIntInt32 val1 val2
            |> int64<nativeint>
            |> NativeIntSource.Verbatim
            |> EvalStackValue.NativeInt
            |> withState
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val1),
          EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val2) ->
            op.ManagedPtrManagedPtr baseClassTypes state val1 val2
            |> managedPtrManagedPtrAsNativeInt
            |> withState
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val1), EvalStackValue.NativeInt val2 ->
            let val2 = nativeIntOffsetForPointerArithmetic val2

            op.ManagedPtrInt64 baseClassTypes state val1 val2
            |> managedPtrChoiceAsNativeInt
            |> withState
        | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val2) ->
            let val1 = nativeIntOffsetForPointerArithmetic val1

            op.Int64ManagedPtr baseClassTypes state val1 val2
            |> managedPtrChoiceAsNativeInt
            |> withState
        | EvalStackValue.NativeInt val1, EvalStackValue.NativeInt val2 ->
            match val1, val2 with
            | NativeIntSource.SyntheticCrossArrayOffset val1, NativeIntSource.SyntheticCrossArrayOffset val2 ->
                op.CrossArrayOffsets val1 val2
                |> NativeIntSource.Verbatim
                |> EvalStackValue.NativeInt
                |> withState
            | NativeIntSource.Verbatim val1, NativeIntSource.Verbatim val2 ->
                let val1 = nativeint<int64> val1
                let val2 = nativeint<int64> val2

                op.NativeIntNativeInt val1 val2
                |> int64<nativeint>
                |> NativeIntSource.Verbatim
                |> EvalStackValue.NativeInt
                |> withState
            | val1, val2 -> failwith $"refusing to operate %s{op.Name} on non-verbatim native ints %O{val1}, %O{val2}"

        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val1), EvalStackValue.ManagedPointer val2 ->
            op.ManagedPtrManagedPtr baseClassTypes state val1 val2
            |> managedPtrManagedPtrAsNativeInt
            |> withState
        | EvalStackValue.NativeInt val1, EvalStackValue.ManagedPointer val2 ->
            let val1 = nativeIntOffsetForPointerArithmetic val1

            match op.Int64ManagedPtr baseClassTypes state val1 val2 with
            | Choice1Of2 v -> EvalStackValue.ManagedPointer v |> withState
            | Choice2Of2 i -> EvalStackValue.NativeInt (NativeIntSource.Verbatim i) |> withState
        | EvalStackValue.NativeInt val1, EvalStackValue.ObjectRef val2 ->
            failwith "" |> EvalStackValue.ObjectRef |> withState
        | EvalStackValue.NativeInt _, EvalStackValue.NullObjectRef -> failwith ""
        | EvalStackValue.Float val1, EvalStackValue.Float val2 ->
            op.FloatFloat val1 val2 |> EvalStackValue.Float |> withState
        | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt (NativeIntSource.ManagedPointer val2) ->
            match op.ManagedPtrManagedPtr baseClassTypes state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result |> withState
            | Choice2Of2 result -> EvalStackValue.NativeInt result |> withState
        | EvalStackValue.ManagedPointer val1, EvalStackValue.NativeInt val2 ->
            let val2 = nativeIntOffsetForPointerArithmetic val2

            match op.ManagedPtrInt64 baseClassTypes state val1 val2 with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result |> withState
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim result) |> withState
        | EvalStackValue.ObjectRef val1, EvalStackValue.NativeInt val2 ->
            failwith "" |> EvalStackValue.ObjectRef |> withState
        | EvalStackValue.NullObjectRef, EvalStackValue.NativeInt _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.Int32 (Int32Source.Verbatim val2) ->
            match op.ManagedPtrInt64 baseClassTypes state val1 (int64<int32> val2) with
            | Choice1Of2 result -> EvalStackValue.ManagedPointer result |> withState
            | Choice2Of2 result -> EvalStackValue.NativeInt (NativeIntSource.Verbatim result) |> withState
        | EvalStackValue.ObjectRef val1, EvalStackValue.Int32 (Int32Source.Verbatim val2) ->
            failwith "" |> EvalStackValue.ObjectRef |> withState
        | EvalStackValue.NullObjectRef, EvalStackValue.Int32 _ -> failwith ""
        | EvalStackValue.ManagedPointer val1, EvalStackValue.ManagedPointer val2 ->
            managedPtrManagedPtr val1 val2 |> withState
        | val1, val2 -> failwith $"invalid %s{op.Name} operation: {val1} and {val2}"
