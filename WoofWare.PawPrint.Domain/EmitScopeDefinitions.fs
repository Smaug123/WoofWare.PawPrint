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
    /// `tdReservedMask`: `tdRTSpecialName` and `tdHasSecurity`, which only the emitter sets.
    let private reservedFlags : TypeAttributes = enum<TypeAttributes> 0x00040800

    /// <summary>
    /// The top-level <c>TypeDef</c> row with this namespace and name, compared ordinally, as CoreCLR's
    /// <c>ImportHelper::FindTypeDefByName</c> finds it with no enclosing type: the first row whose
    /// visibility is not nested.
    /// </summary>
    let tryFindTopLevelTypeDef (ns : string) (name : string) (scope : EmitScope) : TypeDefRid option =
        // Equal strings are one heap entry, so a row matches exactly when it holds these offsets.
        match EmitScope.tryStringOffset ns scope, EmitScope.tryStringOffset name scope with
        | Some ns, Some name ->
            EmitScope.typeDefs scope
            |> Seq.tryFindIndex (fun row ->
                // `IsTdNested`: a row of any nested visibility is not a top-level type.
                (row.Flags &&& TypeAttributes.VisibilityMask) < TypeAttributes.NestedPublic
                && row.Name = name
                && row.Namespace = ns
            )
            |> Option.map (fun index -> TypeDefRid (index + 1))
        | _ -> None

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
        let ns, name = EmitScopeReferences.splitTypeName fullName

        match tryFindTopLevelTypeDef ns name scope with
        | Some existing -> TypeDefDefinition.Duplicate existing
        | None ->
            let name, scope = EmitScope.internString name scope
            let ns, scope = EmitScope.internString ns scope

            let typeDef, scope =
                EmitScope.addTypeDef
                    {
                        Flags = flags &&& ~~~reservedFlags
                        Name = name
                        Namespace = ns
                        Extends = extends
                    }
                    scope

            let scope =
                (scope, interfaces)
                ||> List.fold (fun scope target ->
                    EmitScope.addInterfaceImpl
                        {
                            Class = typeDef
                            Interface = target
                        }
                        scope
                )

            TypeDefDefinition.Defined (typeDef, scope)
