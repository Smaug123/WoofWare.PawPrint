namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
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
/// The definitions CoreCLR's emitter adds to a dynamic module's scope: types, each checked for a
/// duplicate first (<c>MDDupTypeDef</c>), and methods, which are not (<c>MDDupMethodDef</c> is not set).
/// </summary>
[<RequireQualifiedAccess>]
module EmitScopeDefinitions =
    /// `tdReservedMask`: `tdRTSpecialName` and `tdHasSecurity`, which only the emitter sets.
    let private reservedFlags : TypeAttributes = enum<TypeAttributes> 0x00040800

    /// `mdReservedMask`: `mdRTSpecialName`, `mdHasSecurity` and `mdRequireSecObject`, which only the
    /// emitter sets.
    let private reservedMethodFlags : MethodAttributes = enum<MethodAttributes> 0xd000

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

    /// <summary>
    /// Define a method of <paramref name="owner"/>, as <c>RegMeta::DefineMethod</c> does in a dynamic
    /// module, whose emitter does not check for a duplicate <c>MethodDef</c> (<c>MDDupMethodDef</c>
    /// is not set): a new row, at the next RID, however many of its type's methods share its name and
    /// signature.
    /// </summary>
    /// <remarks>
    /// The row keeps the low 16 bits of <paramref name="flags"/>, the column's width, without the
    /// reserved bits (<c>mdReservedMask</c>), which the emitter keeps for itself. A method named
    /// <c>.ctor</c> or <c>.cctor</c>, or whose name starts with <c>_VtblGap</c>, compared ordinally,
    /// gains <c>RTSpecialName</c> and <c>SpecialName</c>. The row has no body, and its implementation
    /// flags are <c>IL</c>, until later calls fill them in.
    /// </remarks>
    let defineMethod
        (owner : TypeDefRid)
        (name : string)
        (signature : ImmutableArray<byte>)
        (flags : MethodAttributes)
        (scope : EmitScope)
        : MethodDefRid * EmitScope
        =
        let special =
            name = ".ctor"
            || name = ".cctor"
            || name.StartsWith ("_VtblGap", StringComparison.Ordinal)

        let flags =
            (flags &&& ~~~reservedMethodFlags &&& enum<MethodAttributes> 0xFFFF)
            ||| (if special then
                     MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName
                 else
                     enum<MethodAttributes> 0)

        let name, scope = EmitScope.internString name scope
        let signature, scope = EmitScope.internBlob signature scope

        EmitScope.addMethodDef
            {
                Owner = owner
                Body = None
                ImplFlags = MethodImplAttributes.IL
                Flags = flags
                Name = name
                Signature = signature
            }
            scope

    /// <summary>
    /// The body <c>TypeBuilder_SetMethodIL</c> writes for a method with no exception handlers: the
    /// header <c>COR_ILMETHOD::Size</c> picks, then the code. A tiny header (ECMA-335 II.25.4.2) when
    /// the method zeroes no locals, has no local signature, a max stack of at most 8 and fewer than 64
    /// bytes of code; otherwise a fat one (II.25.4.3) holding all four.
    /// </summary>
    let methodBody
        (initLocals : bool)
        (maxStack : uint16)
        (code : ImmutableArray<byte>)
        (localSignature : StandAloneSigRid option)
        : ImmutableArray<byte>
        =
        if code.IsDefault then
            invalidArg (nameof code) "a method body's code must be an initialised array"

        let builder = ImmutableArray.CreateBuilder<byte> ()

        if not initLocals && localSignature.IsNone && maxStack <= 8us && code.Length < 64 then
            // `CorILMethod_TinyFormat`, with the code size in the top six bits.
            builder.Add (byte (0x02 ||| (code.Length <<< 2)))
        else
            // `CorILMethod_FatFormat`, `CorILMethod_InitLocals` if asked for, and the header's size of
            // three 4-byte words in the top nibble.
            let flags = 0x03 ||| (if initLocals then 0x10 else 0) ||| (3 <<< 12)

            let localSignatureToken =
                match localSignature with
                | Some rid -> 0x11000000 ||| rid.Rid
                | None -> 0

            let little (value : int) (width : int) =
                for i in 0 .. width - 1 do
                    builder.Add (byte (value >>> (8 * i)))

            little flags 2
            little (int maxStack) 2
            little code.Length 4
            little localSignatureToken 4

        builder.AddRange code
        builder.ToImmutable ()

    /// <summary>
    /// Set a method's implementation flags, as <c>RegMeta::SetMethodImplFlags</c> does: the low 16
    /// bits of <paramref name="flags"/>, the column's width, replacing whatever the row held.
    /// </summary>
    let setMethodImplFlags (method : MethodDefRid) (flags : MethodImplAttributes) (scope : EmitScope) : EmitScope =
        EmitScope.setMethodImplFlags method (flags &&& enum<MethodImplAttributes> 0xFFFF) scope
