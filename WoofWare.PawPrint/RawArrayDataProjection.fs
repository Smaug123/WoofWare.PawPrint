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
        field.DeclaringType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
        && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
        && field.DeclaringType.Name = "RawArrayData"
        && field.DeclaringType.Generics.IsEmpty

    let private arrayOrFail (addr : ManagedHeapAddress) (state : IlMachineState) : AllocatedArray =
        match state.ManagedHeap.Arrays.TryGetValue addr with
        | true, arr -> arr
        | false, _ -> failwith $"RawArrayData projection expected array object at %O{addr}"

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
            let _arr = arrayOrFail addr state

            match field.Name with
            | "Data" ->
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (addr, 0),
                    [ ByrefProjection.ReinterpretAs (byteConcreteType baseClassTypes state) ]
                )
                |> Some
            | "Length" ->
                failwith
                    $"TODO: RawArrayData::Length address projection for array object %O{addr}; this is the shape emitted by taking a byref to Unsafe.As<RawArrayData>(array).Length, but only ldfld value projection is implemented"
            | _ ->
                failwith
                    $"TODO: RawArrayData field address projection for System.Runtime.CompilerServices.RawArrayData::{field.Name}"
