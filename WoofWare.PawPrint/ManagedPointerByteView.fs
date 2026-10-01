namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module ManagedPointerByteView =
    let private arrayElementHandleOfShape : ArrayShape -> ConcreteTypeHandle =
        ArrayElementType.ofShape

    /// The handle naming what the cells of `arr` hold. Callers that need to know *why*
    /// `anchorByteViewIfPlainArrayByref` declined an array byref can ask this: it declines exactly
    /// the `Byref`, `Pointer` and `FunctionPointer` handles.
    let arrayElementHandle (state : IlMachineState) (arr : ManagedHeapAddress) : ConcreteTypeHandle =
        arrayElementHandleOfShape (ManagedHeap.getArrayShape arr state.ManagedHeap)

    /// The byte stride between cells of the array at `arr`, recorded at
    /// allocation (`ArrayShape.ElementStride`).
    let arrayElementSize (state : IlMachineState) (arr : ManagedHeapAddress) : int =
        ManagedHeap.getArrayElementStride arr state.ManagedHeap

    let arrayBytePosition
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int64)
        : int64
        =
        int64 index * int64 (arrayElementSize state arr) + byteOffset

    let normalisationContextForPointer
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ByteOffsetNormalisationContext
        =
        match ManagedPointerSource.tryGetArrayRoot ptr with
        | Some arr -> ByteOffsetNormalisationContext.withArrayElementSize arr (arrayElementSize state arr)
        | None -> ByteOffsetNormalisationContext.nonArrayRootsOnly

    let normalisationContextForPointers
        (state : IlMachineState)
        (ptrs : ManagedPointerSource list)
        : ByteOffsetNormalisationContext
        =
        let arrayElementSizes =
            ptrs
            |> List.choose ManagedPointerSource.tryGetArrayRoot
            |> List.distinct
            |> List.map (fun arr -> arr, arrayElementSize state arr)

        if List.isEmpty arrayElementSizes then
            ByteOffsetNormalisationContext.nonArrayRootsOnly
        else
            ByteOffsetNormalisationContext.withArrayElementSizes arrayElementSizes

    let addByteOffset
        (state : IlMachineState)
        (viewType : ConcreteTypeHandle)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let normalisation = normalisationContextForPointer state ptr

        ManagedPointerSource.addByteOffsetUnderReinterpret normalisation viewType byteOffset ptr

    let addByteOffsetToByteView
        (state : IlMachineState)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let normalisation = normalisationContextForPointer state ptr

        ManagedPointerSource.addByteOffsetToByteView normalisation byteOffset ptr

    /// Anchor a byte-view on a plain byref (array-element or string-char), naming the type that
    /// later reads and writes through the pointer should view its target as. Apply at the
    /// byref-to-native-pointer transition (`Conv_U`, `Conv_I`), and wherever else a plain byref
    /// is about to become a byte cursor — `BinaryArithmetic`'s array arm does it too, so that the
    /// cursor `add`/`sub` leaves behind names the same view type a `fixed` block would have.
    ///
    /// The anchor does not decide the *stride*. `add` and `sub` against a byref are byte
    /// arithmetic whether or not it carries one (ECMA-335 §III.1.5), and `BinaryArithmetic`
    /// divides by the element stride itself. What the anchor decides is which branch the
    /// `Unsafe.*` intrinsics take — `IntrinsicHelpers.offsetManagedPointerByElements` looks for a
    /// byte-view tail to tell a byte cursor from an element walk — and which shape the
    /// cell-aligned read and write short-circuits match on.
    ///
    /// Reference-typed element arrays (e.g. `object[]`) and jagged arrays
    /// (e.g. `object[][]`) are anchored too: cell-aligned typed reads and
    /// writes preserve identity, and mid-cell access still fails, which is
    /// correct — reference cells aren't byte-addressable.
    ///
    /// Byrefs into arrays whose element handle is a pointer/byref/fnptr
    /// (e.g. `int*[]`, `delegate*<...>[]`) are left un-anchored, because the byte-view machinery
    /// is not extended over pointer cells; arithmetic on them is byte-strided all the
    /// same. A byref whose declared pointee really is `byte` does not need this
    /// anchor at all and can be anchored unconditionally — see
    /// `anchorByteStrideOverArrayData` below.
    let anchorByteViewIfPlainArrayByref
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        match ptr with
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.ArrayElement (arr, _)
                                         Projections = []
                                     } ->
            let handle =
                arrayElementHandleOfShape (ManagedHeap.getArrayShape arr state.ManagedHeap)

            match handle with
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // Reference-typed elements (e.g. `object[]`, and jagged arrays' array-typed
                // elements) are anchored too, so that the C#
                // `fixed (object* p = arr) { p[k] = ...; }` pattern — which lowers to a
                // byref-to-native-pointer transition followed by `sizeof object; add;
                // stind.ref` — writes through a pointer whose view type is the cell's own
                // type. The cells themselves are
                // non-byte-addressable (`ObjectRef`); cell-aligned typed reads
                // route through `readArrayBytesAs`'s shape-matching
                // short-circuit and cell-aligned typed writes route through
                // `tryWriteArrayElementPrecise`, both of which preserve
                // identity. Mid-cell access would still fail at the
                // byte-scatter walks.
                addByteOffset state handle 0 ptr
            // Pointer/byref/fnptr element handles carry non-byte-addressable
            // pointer provenance, and the byte-view machinery is not extended
            // over them today. Leaving the byref un-anchored means `Conv_U`
            // merely transports it onto the native-int eval stack, which is
            // what the legal-IL `ldelema ptr[int32]; conv.u` shape
            // needs, without forcing the byte-addressability promise
            // we cannot keep. Arithmetic on the result is still byte-strided,
            // because that does not depend on the anchor.
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> ptr
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.StringCharAt _
                                         Projections = []
                                     } ->
            // Anchor with `System.Char` so that the C#
            // `fixed (char* p = &MemoryMarshal.GetReference(span))` pattern,
            // followed by a byte-stride `Unsafe.Add<byte>`, takes the
            // byte-cursor branch in
            // `IntrinsicHelpers.offsetManagedPointerByElements` rather than
            // the element-stride branch that demands a matching char cell
            // size.
            match
                AllConcreteTypes.findExistingNonGenericConcreteType
                    state.TypeSystem.ConcreteTypes
                    baseClassTypes.Char.Identity
            with
            | Some charType -> addByteOffset state charType 0 ptr
            | None -> ptr
        | _ -> ptr

    /// Anchor a byte-stride view on an array byref whose declared pointee really is `byte`,
    /// i.e. a `ref byte` rather than a `ref T` — the shape
    /// `MemoryMarshal.GetArrayDataReference(Array)` returns.
    ///
    /// Distinct from `anchorByteViewIfPlainArrayByref` above, which preserves the *element's*
    /// CLI shape as the reinterpret target because its callers (`Conv_U`/`Conv_I`) are
    /// transporting a `ref T` onto the native-int stack and want the cell-aligned typed
    /// read/write short-circuits to keep matching on that shape. Here the byref is declared
    /// over bytes, so `System.Byte` is the honest target and no shape surrogate is needed.
    ///
    /// Consequently this is total over element handles, including the pointer/byref/fnptr
    /// elements the shape-preserving anchor declines: byte *stride* is well defined for those
    /// (it is recorded on the array at allocation and read back by `arrayElementSize`,
    /// independent of the reinterpret target), even though byte-granular *dereference* of
    /// such a cell is not
    /// modelled and still fails loudly at the access. That distinction matters: the arithmetic
    /// is perfectly well defined, so failing there would be rejecting legal IL.
    ///
    /// Non-array byrefs pass through unchanged; the only caller hands in an array-element
    /// byref it has just constructed.
    let anchorByteStrideOverArrayData
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        match ptr with
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.ArrayElement _
                                         Projections = []
                                     } ->
            let byteType =
                AllConcreteTypes.findExistingNonGenericConcreteType
                    state.TypeSystem.ConcreteTypes
                    baseClassTypes.Byte.Identity
                |> Option.defaultWith (fun () ->
                    failwith "anchorByteStrideOverArrayData: System.Byte is not concretized"
                )

            addByteOffset state byteType 0 ptr
        | _ -> ptr
