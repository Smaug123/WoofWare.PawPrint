namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module FieldIdentity =
    let fieldId (declaringTypeHandle : ConcreteTypeHandle) (field : FieldInfo<'typeGeneric, 'fieldGeneric>) : FieldId =
        FieldId.metadata declaringTypeHandle field.Handle field.Name

    let cliField
        (declaringTypeHandle : ConcreteTypeHandle)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (contents : CliType)
        (fieldTypeHandle : ConcreteTypeHandle)
        : CliField
        =
        {
            Id = fieldId declaringTypeHandle field
            Name = field.Name
            Contents = contents
            Offset = field.Offset
            Type = fieldTypeHandle
        }

    let private requiredOwnFieldMatching
        (kind : string)
        (predicate : FieldInfo<GenericParamFromMetadata, TypeDefn> -> bool)
        (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldName : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        let matches =
            declaringType.Fields
            |> List.filter (fun field -> field.Name = fieldName && predicate field)

        match matches with
        | [ field ] -> field
        | [] ->
            failwith
                $"%s{declaringType.Namespace}.%s{declaringType.Name} does not declare expected %s{kind} field '%s{fieldName}'"
        | _ ->
            failwith
                $"%s{declaringType.Namespace}.%s{declaringType.Name} declares multiple %s{kind} fields named '%s{fieldName}'"

    let requiredOwnInstanceField
        (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldName : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        requiredOwnFieldMatching "instance" (fun field -> not field.IsStatic) declaringType fieldName

    let requiredOwnStaticField
        (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldName : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        requiredOwnFieldMatching "static" (fun field -> field.IsStatic) declaringType fieldName

    let requiredNonGenericInstanceFieldId
        (allConcreteTypes : AllConcreteTypes)
        (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldName : string)
        : FieldId
        =
        let declaringTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes declaringType

        requiredOwnInstanceField declaringType fieldName |> fieldId declaringTypeHandle
