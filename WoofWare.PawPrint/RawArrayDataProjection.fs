namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal RawArrayDataProjection =
    // Project CoreLib's RawArrayData layout view over PawPrint's structured AllocatedArray storage.
    // PawPrint carries CLI uint32 fields as Int32 while preserving the low 32 bits; see PrimitiveType.UInt32.
    let private uint32Field (value : uint32) : CliType =
        CliType.Numeric (CliNumericType.Int32 (int32 value))

    let private isRawArrayDataField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.DeclaringType.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
        && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
        && field.DeclaringType.Name = "RawArrayData"
        && field.DeclaringType.Generics.IsEmpty

    /// Both callers want only the dimensions — one reads `Length`, the other merely
    /// asserts that the address really is an array — so this hands back a shape, from
    /// which no cell is reachable.
    let private arrayOrFail (addr : ManagedHeapAddress) (state : IlMachineState) : ArrayShape =
        match ManagedHeap.tryGetArrayShape addr state.ManagedHeap with
        | Some arr -> arr
        | None -> failwith $"RawArrayData projection expected array object at %O{addr}"

    let private byteConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        match AllConcreteTypes.lookup byteHandle state.ConcreteTypes with
        | Some byteType -> byteType
        | None -> failwith "RawArrayData projection could not find System.Byte in AllConcreteTypes"

    let tryProjectField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : CliType option
        =
        if not (isRawArrayDataField baseClassTypes field) then
            None
        else
            let arr = arrayOrFail addr state

            match field.Name with
            | "Length" -> Some (uint32Field (uint32 arr.Length))
            | "Data" ->
                failwith
                    $"TODO: RawArrayData::Data value load for array object %O{addr}; this is the shape emitted by reading Unsafe.As<RawArrayData>(array).Data, but only ldflda address projection is implemented"
            | _ ->
                failwith
                    $"TODO: RawArrayData field projection for System.Runtime.CompilerServices.RawArrayData::{field.Name}"

    let tryProjectFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        if not (isRawArrayDataField baseClassTypes field) then
            None
        else
            let arr = arrayOrFail addr state

            match field.Name with
            | "Data" ->
                // CoreLib exposes RawArrayData.Data as a byte view even for reference arrays. Keep the
                // array root in the byref so future copy/write-barrier code can preserve ObjectRef
                // provenance instead of treating reference cells as raw integers.
                let byteView = ByrefProjection.ReinterpretAs (byteConcreteType baseClassTypes state)

                match arr.ConcreteType with
                | ConcreteTypeHandle.OneDimArrayZero _ ->
                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (addr, 0), [ byteView ])
                    |> Some
                | ConcreteTypeHandle.Array (_, rank) ->
                    // On a multi-dimensional array `Data` is not element 0 but the start of the
                    // bounds block that precedes it (see `MultiDimArrayBounds`). CoreLib reads the
                    // lengths and lower bounds from there and then steps forward by the block's
                    // size to reach the elements, so the byref is element 0 less that size: the
                    // step cancels the offset exactly, and a read before element 0 is served from
                    // the block by `readArrayBytesAs`.
                    ManagedPointerSource.Byref (
                        ByrefRoot.ArrayElement (addr, 0),
                        [
                            byteView
                            ByrefProjection.ByteOffset (-(MultiDimArrayBounds.sizeInBytes rank))
                        ]
                    )
                    |> Some
                | other ->
                    failwith
                        $"RawArrayData::Data projection encountered array at %O{addr} whose ConcreteType is not an array handle: %O{other}"
            | "Length" ->
                failwith
                    $"TODO: RawArrayData::Length address projection for array object %O{addr}; this is the shape emitted by taking a byref to Unsafe.As<RawArrayData>(array).Length, but only ldfld value projection is implemented"
            | _ ->
                failwith
                    $"TODO: RawArrayData field address projection for System.Runtime.CompilerServices.RawArrayData::{field.Name}"
