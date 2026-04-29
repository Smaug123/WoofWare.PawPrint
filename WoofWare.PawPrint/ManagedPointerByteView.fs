namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module ManagedPointerByteView =
    let private arrayElementHandle (arrObj : AllocatedArray) : ConcreteTypeHandle =
        match arrObj.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> failwith $"array object has non-array concrete type: %O{arrObj.ConcreteType}"

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

    let arrayBytePosition
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int64)
        : int64
        =
        int64 index * int64 (arrayElementSize baseClassTypes state arr) + byteOffset

    let addByteOffset
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (viewType : ConcreteType<ConcreteTypeHandle>)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        ptr
        |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs viewType)
        |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset byteOffset)
        |> ManagedPointerSource.normaliseLocalMemoryByteOffset
        |> ManagedPointerSource.normaliseArrayByteOffset (arrayElementSize baseClassTypes state)
        |> ManagedPointerSource.normaliseStringByteOffset
