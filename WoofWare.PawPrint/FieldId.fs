namespace WoofWare.PawPrint

open System.Reflection.Metadata

[<RequireQualifiedAccess>]
type FieldId =
    | Metadata of declaringType : ConcreteTypeHandle * field : ComparableFieldDefinitionHandle * name : string
    | Named of name : string
    /// The `index`th implicit repeat (`index >= 1`) of the single declared instance field of an
    /// `[InlineArray(N)]` value type.
    ///
    /// CoreCLR has exactly one `FieldDesc` for that field and instead multiplies the type's
    /// instance size by `N` (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612);
    /// the repeats are storage, not fields, and reflection never sees them. PawPrint's value storage
    /// is field-cell based, so each repeat needs a cell of its own, and each cell needs an identity.
    /// Slot 0 deliberately keeps `FieldId.Metadata`, so `ldfld`/`ldflda`/`Marshal.OffsetOf` and
    /// every other metadata-driven lookup resolve to it with no special-casing; only slots 1 and up
    /// are named this way.
    ///
    /// `name` is the *storage* name (`_item[1]`), deliberately distinct from the declared field's,
    /// so that `CliValueType`'s name-keyed lookup fallback still resolves the declared name uniquely
    /// to slot 0 rather than reporting it ambiguous.
    | InlineArrayElement of
        declaringType : ConcreteTypeHandle *
        field : ComparableFieldDefinitionHandle *
        name : string *
        index : int

    member this.Name : string =
        match this with
        | FieldId.Metadata (name = name) -> name
        | FieldId.Named name -> name
        | FieldId.InlineArrayElement (name = name) -> name

    override this.ToString () : string =
        match this with
        | FieldId.Metadata (declaringType, field, name) -> $"%O{declaringType}::%s{name} (%O{field.Get})"
        | FieldId.Named name -> name
        | FieldId.InlineArrayElement (declaringType, field, name, index) ->
            $"%O{declaringType}::%s{name} (inline-array slot %d{index} of %O{field.Get})"

[<RequireQualifiedAccess>]
module FieldId =
    let metadata (declaringType : ConcreteTypeHandle) (field : FieldDefinitionHandle) (name : string) : FieldId =
        FieldId.Metadata (declaringType, ComparableFieldDefinitionHandle.Make field, name)

    /// The storage name of inline-array slot `index` of a field declared as `name`. Slot 0 keeps
    /// the declared name (it *is* the declared field); later slots are suffixed so they cannot
    /// collide with it in name-keyed lookups.
    let inlineArrayElementName (name : string) (index : int) : string =
        if index = 0 then name else $"%s{name}[%d{index}]"

    /// The identity of inline-array storage slot `index` of the field declared by
    /// `field`/`name` on `declaringType`. Slot 0 is the declared field itself, so it is
    /// `FieldId.Metadata`; see `FieldId.InlineArrayElement` for why later slots are not.
    let inlineArrayElement
        (declaringType : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (name : string)
        (index : int)
        : FieldId
        =
        if index < 0 then
            failwith $"FieldId.inlineArrayElement: negative inline-array slot index %d{index} for %s{name}"

        if index = 0 then
            FieldId.Metadata (declaringType, field, name)
        else
            FieldId.InlineArrayElement (declaringType, field, inlineArrayElementName name index, index)

    let named (name : string) : FieldId = FieldId.Named name

    /// The metadata field definition this identity names, for the two cases that have one.
    /// `Named` is a name-keyed identity with no metadata behind it, so it has none.
    let tryFieldDefinition (field : FieldId) : ComparableFieldDefinitionHandle option =
        match field with
        | FieldId.Metadata (field = handle)
        | FieldId.InlineArrayElement (field = handle) -> Some handle
        | FieldId.Named _ -> None

    /// The declaring type this identity is keyed to, for the two cases that have one.
    let tryDeclaringType (field : FieldId) : ConcreteTypeHandle option =
        match field with
        | FieldId.Metadata (declaringType = declaringType)
        | FieldId.InlineArrayElement (declaringType = declaringType) -> Some declaringType
        | FieldId.Named _ -> None

    let exactlyEqual (left : FieldId) (right : FieldId) : bool =
        match left, right with
        | FieldId.Metadata (leftType, leftField, _), FieldId.Metadata (rightType, rightField, _) ->
            leftType = rightType && leftField = rightField
        | FieldId.Named leftName, FieldId.Named rightName -> leftName = rightName
        | FieldId.InlineArrayElement (leftType, leftField, _, leftIndex),
          FieldId.InlineArrayElement (rightType, rightField, _, rightIndex) ->
            leftType = rightType && leftField = rightField && leftIndex = rightIndex
        // A slot-`k` repeat is never the declared field: slot 0 is emitted as `Metadata`, so an
        // `InlineArrayElement` always has `index >= 1` and addresses storage the declared field
        // does not.
        | FieldId.Metadata _, (FieldId.Named _ | FieldId.InlineArrayElement _)
        | FieldId.Named _, (FieldId.Metadata _ | FieldId.InlineArrayElement _)
        | FieldId.InlineArrayElement _, (FieldId.Metadata _ | FieldId.Named _) -> false
