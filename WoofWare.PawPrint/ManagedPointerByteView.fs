namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module ManagedPointerByteView =
    let private arrayElementHandle (arrObj : AllocatedArray) : ConcreteTypeHandle =
        match arrObj.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith $"array object has non-array concrete type: %O{arrObj.ConcreteType}"

    let arrayElementSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        : int
        =
        let obj = state.ManagedHeap.Arrays.[arr]

        if obj.Length > 0 then
            CliType.sizeOf obj.Elements.[0]
        else
            let zero, _ =
                CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes (arrayElementHandle obj)

            CliType.sizeOf zero

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
        let obj = state.ManagedHeap.Arrays.[arr]
        let handle = arrayElementHandle obj
        AllConcreteTypes.lookup handle state.ConcreteTypes

    let arrayBytePosition
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int64)
        : int64
        =
        int64 index * int64 (arrayElementSize baseClassTypes state arr) + byteOffset

    let normalisationContextForPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ByteOffsetNormalisationContext
        =
        match ManagedPointerSource.tryGetArrayRoot ptr with
        | Some arr ->
            ByteOffsetNormalisationContext.withArrayElementSize arr (arrayElementSize baseClassTypes state arr)
        | None -> ByteOffsetNormalisationContext.nonArrayRootsOnly

    let normalisationContextForPointers
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptrs : ManagedPointerSource list)
        : ByteOffsetNormalisationContext
        =
        let arrayElementSizes =
            ptrs
            |> List.choose ManagedPointerSource.tryGetArrayRoot
            |> List.distinct
            |> List.map (fun arr -> arr, arrayElementSize baseClassTypes state arr)

        if List.isEmpty arrayElementSizes then
            ByteOffsetNormalisationContext.nonArrayRootsOnly
        else
            ByteOffsetNormalisationContext.withArrayElementSizes arrayElementSizes

    let addByteOffset
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (viewType : ConcreteType<ConcreteTypeHandle>)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let normalisation = normalisationContextForPointer baseClassTypes state ptr

        ManagedPointerSource.addByteOffsetUnderReinterpret normalisation viewType byteOffset ptr

    let addByteOffsetToByteView
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let normalisation = normalisationContextForPointer baseClassTypes state ptr

        ManagedPointerSource.addByteOffsetToByteView normalisation byteOffset ptr

    /// Anchor a byte-view on a plain `ArrayElement` byref so subsequent pointer
    /// arithmetic uses byte stride (ECMA-335 §III.1.5: native-pointer +/- int is
    /// byte arithmetic). Plain byrefs without this anchor keep element-stride
    /// semantics, matching `Unsafe.Add<T>` intrinsic behaviour. Apply at the
    /// byref-to-native-pointer transition (`Conv_U`, `Conv_I`).
    ///
    /// Reference-typed element arrays (e.g. `object[]`) are anchored too: the
    /// C# `fixed (object* p = arr) { p[k] = ...; }` pattern lowers to a
    /// byref-to-native-pointer transition followed by `sizeof object; add;
    /// stind.ref`, so without the anchor the trailing `add` would be
    /// element-stride and produce out-of-bounds element indices. The cells
    /// themselves are non-byte-addressable (`ObjectRef`); cell-aligned typed
    /// reads route through `readArrayBytesAs`'s shape-matching short-circuit
    /// and cell-aligned typed writes route through
    /// `tryWriteArrayElementPrecise`, both of which preserve identity.
    /// Mid-cell access would still fail at the byte-scatter walks, which is
    /// correct — reference cells aren't byte-addressable.
    ///
    /// Jagged arrays (e.g. `object[][]`) have a structural element handle
    /// (`OneDimArrayZero` or `Array`); those handles are not registered in
    /// `AllConcreteTypes` (they're synthetic, derived from the inner element
    /// type), but the cells are array references — `ObjectRef`-shaped, like
    /// `object[]`. Anchor those with `System.Object` as the reinterpret
    /// target: it carries the same CLI shape, the byte-stride context still
    /// comes from `arrayElementSize` (which uses cell `CliType.sizeOf`,
    /// independent of the reinterpret target), and the cell-aligned
    /// read/write short-circuits preserve identity exactly as for `object[]`.
    ///
    /// Pointer/byref/fnptr element handles (e.g. `int*[]`,
    /// `delegate*<...>[]`) carry non-byte-addressable pointer provenance and
    /// no `ObjectRef`-shaped surrogate type, so the byte-view machinery
    /// cannot be safely extended over them today. Leave such byrefs
    /// un-anchored: `Conv_U` merely transports them onto the native-int eval
    /// stack, which is what the Codex-flagged `ldelema ptr[int32]; conv.u`
    /// legal-IL shape needs, without forcing the byte-addressability promise
    /// we cannot keep. Subsequent pointer arithmetic on the structural-element
    /// byref will still use element-stride semantics — extending byte-stride
    /// support to pointer-element arrays is future work.
    let anchorByteViewIfPlainArrayByref
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let tryObjectConcreteType () : ConcreteType<ConcreteTypeHandle> option =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Object.Identity
            |> Option.bind (fun handle -> AllConcreteTypes.lookup handle state.ConcreteTypes)

        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), []) ->
            let arrObj = state.ManagedHeap.Arrays.[arr]
            let handle = arrayElementHandle arrObj

            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some elementType -> addByteOffset baseClassTypes state elementType 0 ptr
                | None -> ptr
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // Jagged-array cells are array references; the byte-view's
                // reinterpret target only needs to carry the `ObjectRef` shape,
                // and `System.Object` is the universal surrogate for that shape.
                match tryObjectConcreteType () with
                | Some objectType -> addByteOffset baseClassTypes state objectType 0 ptr
                | None -> ptr
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> ptr
        | _ -> ptr
