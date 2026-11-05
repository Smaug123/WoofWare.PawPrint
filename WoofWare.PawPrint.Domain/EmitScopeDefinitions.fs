namespace WoofWare.PawPrint

open System.Reflection

/// <summary>
/// The outcome of asking CoreCLR's emitter to define a type in a dynamic module's scope
/// (<c>RegMeta::_DefineTypeDef</c>).
/// </summary>
[<RequireQualifiedAccess>]
type TypeDefDefinition =
    /// A new <c>TypeDef</c> row, and the scope that holds it.
    | Defined of TypeDefRid * EmitScope
    /// <summary>
    /// The scope already has this row, with the same namespace and name, so nothing is added: the
    /// emitter returns <c>META_S_DUPLICATE</c>, for which <c>TypeBuilder_DefineType</c> raises
    /// <c>ArgumentException</c>.
    /// </summary>
    | Duplicate of TypeDefRid

/// <summary>
/// The definitions CoreCLR's emitter adds to a dynamic module's scope, each checked for a duplicate
/// first, as the emitter checks for duplicate <c>TypeDef</c>s in a dynamic module.
/// </summary>
[<RequireQualifiedAccess>]
module EmitScopeDefinitions =
    /// <summary>
    /// The top-level <c>TypeDef</c> row with this namespace and name, compared ordinally, as CoreCLR's
    /// <c>ImportHelper::FindTypeDefByName</c> finds it with no enclosing type: the first row whose
    /// visibility is not nested.
    /// </summary>
    let tryFindTopLevelTypeDef (ns : string) (name : string) (scope : EmitScope) : TypeDefRid option =
        failwith "TODO: EmitScopeDefinitions.tryFindTopLevelTypeDef"

    /// <summary>
    /// Define a type that no other type encloses, as <c>RegMeta::DefineTypeDef</c> does: the full name
    /// is divided by <c>EmitScopeReferences.splitTypeName</c>, and if <c>tryFindTopLevelTypeDef</c>
    /// finds a row with that namespace and name, nothing is added. Otherwise a new row holds the
    /// name, then the namespace, interned in that order; the flags without the reserved bits
    /// (<c>tdReservedMask</c>), which the emitter keeps for itself; and the parent, if there is one.
    /// One <c>InterfaceImpl</c> row follows for each of <paramref name="interfaces"/>, in order, a
    /// repeated interface included.
    /// </summary>
    let defineTopLevelTypeDef
        (fullName : string)
        (flags : TypeAttributes)
        (extends : TypeDefOrRef option)
        (interfaces : TypeDefOrRef list)
        (scope : EmitScope)
        : TypeDefDefinition
        =
        failwith "TODO: EmitScopeDefinitions.defineTopLevelTypeDef"
