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

    let tryProjectFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        match RawArrayDataProjection.tryProjectFieldAddress baseClassTypes field addr state with
        | Some projection -> Some projection
        | None -> tryProjectStringTrailingDataFieldAddress baseClassTypes field addr
