namespace WoofWare.PawPrint

open System.Reflection.Metadata

[<RequireQualifiedAccess>]
type FieldId =
    | Metadata of declaringType : ConcreteTypeHandle * field : ComparableFieldDefinitionHandle * name : string
    | Named of name : string

    member this.Name : string =
        match this with
        | FieldId.Metadata (name = name) -> name
        | FieldId.Named name -> name

    override this.ToString () : string =
        match this with
        | FieldId.Metadata (declaringType, field, name) -> $"%O{declaringType}::%s{name} (%O{field.Get})"
        | FieldId.Named name -> name

[<RequireQualifiedAccess>]
module FieldId =
    let metadata (declaringType : ConcreteTypeHandle) (field : FieldDefinitionHandle) (name : string) : FieldId =
        FieldId.Metadata (declaringType, ComparableFieldDefinitionHandle.Make field, name)

    let named (name : string) : FieldId = FieldId.Named name

    let exactlyEqual (left : FieldId) (right : FieldId) : bool =
        match left, right with
        | FieldId.Metadata (leftType, leftField, _), FieldId.Metadata (rightType, rightField, _) ->
            leftType = rightType && leftField = rightField
        | FieldId.Named leftName, FieldId.Named rightName -> leftName = rightName
        | FieldId.Metadata _, FieldId.Named _
        | FieldId.Named _, FieldId.Metadata _ -> false
