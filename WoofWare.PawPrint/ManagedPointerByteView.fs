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

    /// The looked-up concrete element type of the given array.
    let arrayElementConcreteType
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        : ConcreteType<ConcreteTypeHandle>
        =
        let obj = state.ManagedHeap.Arrays.[arr]
        let handle = arrayElementHandle obj

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () ->
            failwith $"array element concrete type %O{handle} was not registered for array %O{arr}"
        )

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
    let anchorByteViewIfPlainArrayByref
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), []) ->
            let elementType = arrayElementConcreteType state arr
            addByteOffset baseClassTypes state elementType 0 ptr
        | _ -> ptr
