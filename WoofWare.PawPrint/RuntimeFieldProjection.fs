namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal RuntimeFieldProjection =
    /// Field address projections for runtime-managed layouts. These are fields
    /// whose metadata names an ordinary field, but whose address in the real CLR
    /// is a view over structured runtime storage rather than a standalone object
    /// field cell. Keep new trailing-data cases here so IL op execution stays
    /// generic.
    let private isCorelibType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (namespaceName : string)
        (typeName : string)
        : bool
        =
        field.DeclaringType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
        && field.DeclaringType.Namespace = namespaceName
        && field.DeclaringType.Name = typeName
        && field.DeclaringType.Generics.IsEmpty

    let private tryProjectStringTrailingDataFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        : ManagedPointerSource option
        =
        if
            field.Name = "_firstChar"
            && isCorelibType baseClassTypes field "System" "String"
        then
            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (addr, 0), []) |> Some
        else
            None

    let private byteConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        match AllConcreteTypes.lookup byteHandle state.ConcreteTypes with
        | Some byteType -> byteType
        | None -> failwith "RawData projection could not find System.Byte in AllConcreteTypes"

    let private isRawDataField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        isCorelibType baseClassTypes field "System.Runtime.CompilerServices" "RawData"

    let private requireBoxedValueType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : unit
        =
        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
        | false, _ -> failwith $"RawData::Data projection expected boxed value type object at %O{addr}, got array"
        | true, obj ->
            let concrete =
                AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"RawData::Data projection found unregistered concrete type %O{obj.ConcreteType}"
                )

            let assembly =
                state.LoadedAssembly concrete.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"RawData::Data projection needs loaded assembly %O{concrete.Assembly}"
                )

            let typeDef = assembly.TypeDefs.[concrete.Definition.Get]

            if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeDef) then
                failwith $"RawData::Data projection expected boxed value type at %O{addr}, got %O{obj.ConcreteType}"

    let private tryProjectRawDataFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        if not (isRawDataField baseClassTypes field) then
            None
        else
            match field.Name with
            | "Data" ->
                requireBoxedValueType baseClassTypes addr state

                // The projection establishes the runtime storage identity only.
                // Payload byte-view safety, including object-reference and layout
                // checks, is enforced when the byref is read or written.
                ManagedPointerSource.Byref (
                    ByrefRoot.HeapValue addr,
                    [ ByrefProjection.ReinterpretAs (byteConcreteType baseClassTypes state) ]
                )
                |> Some
            | _ ->
                failwith
                    $"TODO: RawData field address projection for System.Runtime.CompilerServices.RawData::{field.Name}"

    let tryProjectFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        match RawArrayDataProjection.tryProjectFieldAddress baseClassTypes field addr state with
        | Some projection -> Some projection
        | None ->
            match tryProjectRawDataFieldAddress baseClassTypes field addr state with
            | Some projection -> Some projection
            | None -> tryProjectStringTrailingDataFieldAddress baseClassTypes field addr
