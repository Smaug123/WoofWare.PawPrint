namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module ManagedPointerByteView =
    let private arrayElementHandle : ArrayShape -> ConcreteTypeHandle =
        ArrayElementType.ofShape

    /// The byte stride between cells of the array at `arr`, recorded at
    /// allocation (`ArrayShape.ElementStride`).
    let arrayElementSize (state : IlMachineState) (arr : ManagedHeapAddress) : int =
        ManagedHeap.getArrayElementStride arr state.ManagedHeap

    /// The looked-up concrete element type of the given array, when the element
    /// is a registered concrete type. Returns `None` when the element handle is
    /// structural (`Pointer`, `Byref`, `OneDimArrayZero`, `Array`,
    /// `FunctionPointer`) — those handles are intentionally not present in the
    /// `AllConcreteTypes` index.
    let arrayElementConcreteType
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        : ConcreteType<ConcreteTypeHandle> option
        =
        let handle = arrayElementHandle (ManagedHeap.getArrayShape arr state.ManagedHeap)

        AllConcreteTypes.lookup handle state.ConcreteTypes

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
        (viewType : ConcreteType<ConcreteTypeHandle>)
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

    /// Anchor a byte-view on a plain byref (array-element or string-char) so
    /// subsequent pointer arithmetic uses byte stride (ECMA-335 §III.1.5:
    /// native-pointer +/- int is byte arithmetic). Plain byrefs without this
    /// anchor keep element-stride semantics, matching `Unsafe.Add<T>`
    /// intrinsic behaviour. Apply at the byref-to-native-pointer transition
    /// (`Conv_U`, `Conv_I`).
    ///
    /// Reference-typed element arrays (e.g. `object[]`) and jagged arrays
    /// (e.g. `object[][]`) are anchored too: cell-aligned typed reads and
    /// writes preserve identity, and mid-cell access still fails, which is
    /// correct — reference cells aren't byte-addressable.
    ///
    /// Byrefs into arrays whose element handle is a pointer/byref/fnptr
    /// (e.g. `int*[]`, `delegate*<...>[]`) are left un-anchored: subsequent
    /// pointer arithmetic on them still uses element-stride semantics. A
    /// byref whose declared pointee really is `byte` does not need this
    /// anchor at all and can be anchored unconditionally — see
    /// `anchorByteStrideOverArrayData` below.
    let anchorByteViewIfPlainArrayByref
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let tryObjectConcreteType () : ConcreteType<ConcreteTypeHandle> option =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Object.Identity
            |> Option.bind (fun handle -> AllConcreteTypes.lookup handle state.ConcreteTypes)

        let tryCharConcreteType () : ConcreteType<ConcreteTypeHandle> option =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Char.Identity
            |> Option.bind (fun handle -> AllConcreteTypes.lookup handle state.ConcreteTypes)

        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), []) ->
            let handle = arrayElementHandle (ManagedHeap.getArrayShape arr state.ManagedHeap)

            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                // Reference-typed elements (e.g. `object[]`) are anchored too:
                // the C# `fixed (object* p = arr) { p[k] = ...; }` pattern
                // lowers to a byref-to-native-pointer transition followed by
                // `sizeof object; add; stind.ref`, so without the anchor the
                // trailing `add` would be element-stride and produce
                // out-of-bounds element indices. The cells themselves are
                // non-byte-addressable (`ObjectRef`); cell-aligned typed reads
                // route through `readArrayBytesAs`'s shape-matching
                // short-circuit and cell-aligned typed writes route through
                // `tryWriteArrayElementPrecise`, both of which preserve
                // identity. Mid-cell access would still fail at the
                // byte-scatter walks.
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some elementType -> addByteOffset state elementType 0 ptr
                | None -> ptr
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // Jagged-array element handles are structural — not registered
                // in `AllConcreteTypes` (they're synthetic, derived from the
                // inner element type) — but the cells are array references;
                // the byte-view's reinterpret target only needs to carry the
                // `ObjectRef` shape, and `System.Object` is the universal
                // surrogate for that shape. The byte-stride context still
                // comes from `arrayElementSize` (which derives the stride from
                // the array's element type, independent of the reinterpret
                // target), and the cell-aligned read/write short-circuits
                // preserve identity exactly as for `object[]`.
                match tryObjectConcreteType () with
                | Some objectType -> addByteOffset state objectType 0 ptr
                | None -> ptr
            // Pointer/byref/fnptr element handles carry non-byte-addressable
            // pointer provenance and no `ObjectRef`-shaped surrogate type,
            // so the byte-view machinery cannot be safely extended over
            // them today. Leaving the byref un-anchored means `Conv_U`
            // merely transports it onto the native-int eval stack, which is
            // what the legal-IL `ldelema ptr[int32]; conv.u` shape
            // needs, without forcing the byte-addressability promise
            // we cannot keep.
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> ptr
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt _, []) ->
            // Anchor with `System.Char` so that the C#
            // `fixed (char* p = &MemoryMarshal.GetReference(span))` pattern,
            // followed by a byte-stride `Unsafe.Add<byte>`, takes the
            // byte-cursor branch in
            // `IntrinsicHelpers.offsetManagedPointerByElements` rather than
            // the element-stride branch that demands a matching char cell
            // size.
            match tryCharConcreteType () with
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
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement _, []) ->
            let byteType =
                AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
                |> Option.bind (fun handle -> AllConcreteTypes.lookup handle state.ConcreteTypes)
                |> Option.defaultWith (fun () ->
                    failwith "anchorByteStrideOverArrayData: System.Byte is not concretized"
                )

            addByteOffset state byteType 0 ptr
        | _ -> ptr
