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

/// What an exception clause's handler is (ECMA-335 II.19), as <c>TypeBuilder_SetMethodIL</c> writes the
/// clause's flags and its class token or filter offset.
[<RequireQualifiedAccess>]
type ExceptionClauseHandler =
    /// Catches exceptions assignable to the type this token names.
    | Catch of classToken : int
    /// Runs the filter that starts at this offset to decide whether to handle the exception.
    | Filter of filterOffset : int
    /// Runs however the try block is left. CoreCLR writes its class token as <c>mdTypeRefNil</c>.
    | Finally
    /// Runs when the try block is left by an exception. CoreCLR writes the class token the managed
    /// caller passes, which is 0.
    | Fault of classToken : int

/// An exception clause of a method body: its handler, and the try block and handler it covers, each
/// as an offset into the code and a length.
type ExceptionClause =
    {
        Handler : ExceptionClauseHandler
        TryOffset : int
        TryLength : int
        HandlerOffset : int
        HandlerLength : int
    }

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
    /// Record that <paramref name="cls"/> implements <paramref name="target"/>, as
    /// <c>RegMeta::AddInterfaceImpl</c> does: unless the table already has a row with this class and
    /// interface (<c>ImportHelper::FindInterfaceImpl</c>), which it looks for whether or not the emitter
    /// checks for duplicates elsewhere, a new row. <c>DefineType</c>'s interfaces are added without
    /// that search.
    /// </summary>
    let addInterfaceImpl (cls : TypeDefRid) (target : TypeDefOrRef) (scope : EmitScope) : EmitScope =
        let existing =
            EmitScope.interfaceImpls scope
            |> Seq.exists (fun row -> row.Class = cls && row.Interface = target)

        if existing then
            scope
        else
            EmitScope.addInterfaceImpl
                {
                    Class = cls
                    Interface = target
                }
                scope

    /// `fdRTSpecialName`, `fdHasFieldMarshal` and `fdHasDefault`: the bits of `fdReservedMask` that
    /// `RegMeta::DefineField` clears from the flags it is given, keeping `fdHasFieldRVA`.
    let private reservedFieldFlags : FieldAttributes = enum<FieldAttributes> 0x9400

    /// <summary>
    /// Define a field of <paramref name="owner"/>, as <c>RegMeta::DefineField</c> does in a dynamic
    /// module, whose emitter does not check for a duplicate <c>Field</c> (<c>MDDupFieldDef</c> is not
    /// set): a new row, at the next RID.
    /// </summary>
    /// <remarks>
    /// The row keeps the low 16 bits of <paramref name="flags"/>, the column's width, without
    /// <c>RTSpecialName</c>, <c>HasFieldMarshal</c> and <c>HasDefault</c>, which the emitter keeps for
    /// itself. A field named exactly <c>value__</c> gains <c>RTSpecialName</c> and <c>SpecialName</c>.
    /// </remarks>
    /// <exception cref="ArgumentException">
    /// <paramref name="flags"/> has <c>HasFieldRVA</c>, for which the emitter adds a <c>FieldRVA</c>
    /// row, which a scope does not hold.
    /// </exception>
    let defineField
        (owner : TypeDefRid)
        (name : string)
        (signature : ImmutableArray<byte>)
        (flags : FieldAttributes)
        (scope : EmitScope)
        : FieldRid * EmitScope
        =
        if flags.HasFlag FieldAttributes.HasFieldRVA then
            invalidArg
                (nameof flags)
                $"EmitScopeDefinitions.defineField: %s{name} has HasFieldRVA, for which the emitter adds a FieldRVA row, which a scope does not hold"

        let flags =
            (flags &&& ~~~reservedFieldFlags &&& enum<FieldAttributes> 0xFFFF)
            ||| (if name = "value__" then
                     FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName
                 else
                     enum<FieldAttributes> 0)

        let name, scope = EmitScope.internString name scope
        let signature, scope = EmitScope.internBlob signature scope

        EmitScope.addField
            {
                Owner = owner
                Flags = flags
                Name = name
                Signature = signature
            }
            scope

    /// <summary>
    /// The body <c>TypeBuilder_SetMethodIL</c> writes: the header <c>COR_ILMETHOD::Size</c> picks, the
    /// code, and a section of the exception clauses if there are any. A tiny header (ECMA-335 II.25.4.2)
    /// when the method zeroes no locals, has no local signature, a max stack of at most 8, fewer than 64
    /// bytes of code and no clauses; otherwise a fat one (II.25.4.3) holding all four. The section
    /// (II.25.4.5) follows the code at the next 4-byte boundary, and is small, as
    /// <c>ExceptionHandlingSize</c> picks, when it holds at most 20 clauses whose offsets fit 16 bits and
    /// whose lengths fit 8; otherwise fat.
    /// </summary>
    let methodBody
        (initLocals : bool)
        (maxStack : uint16)
        (code : ImmutableArray<byte>)
        (localSignature : StandAloneSigRid option)
        (clauses : ExceptionClause list)
        : ImmutableArray<byte>
        =
        if code.IsDefault then
            invalidArg (nameof code) "a method body's code must be an initialised array"

        let builder = ImmutableArray.CreateBuilder<byte> ()

        let little (value : int) (width : int) =
            for i in 0 .. width - 1 do
                builder.Add (byte (value >>> (8 * i)))

        let moreSections = not clauses.IsEmpty

        if
            not initLocals
            && localSignature.IsNone
            && maxStack <= 8us
            && code.Length < 64
            && not moreSections
        then
            // `CorILMethod_TinyFormat`, with the code size in the top six bits.
            builder.Add (byte (0x02 ||| (code.Length <<< 2)))
        else
            // `CorILMethod_FatFormat`, `CorILMethod_InitLocals` if asked for, `CorILMethod_MoreSects`
            // if there are clauses, and the header's size of three 4-byte words in the top nibble.
            let flags =
                0x03
                ||| (if initLocals then 0x10 else 0)
                ||| (if moreSections then 0x08 else 0)
                ||| (3 <<< 12)

            let localSignatureToken =
                match localSignature with
                | Some rid -> 0x11000000 ||| rid.Rid
                | None -> 0

            little flags 2
            little (int maxStack) 2
            little code.Length 4
            little localSignatureToken 4

        builder.AddRange code

        if moreSections then
            // The header is 12 bytes, so aligning the code's end aligns the section.
            while builder.Count % 4 <> 0 do
                builder.Add 0uy

            let flagsAndClassToken (clause : ExceptionClause) : int * int =
                match clause.Handler with
                | ExceptionClauseHandler.Catch token -> 0, token
                | ExceptionClauseHandler.Filter offset -> 1, offset
                // `mdTypeRefNil`.
                | ExceptionClauseHandler.Finally -> 2, 0x01000000
                | ExceptionClauseHandler.Fault token -> 4, token

            let small =
                clauses.Length <= 20
                && clauses
                   |> List.forall (fun c ->
                       c.TryOffset <= 0xFFFF
                       && c.TryLength <= 0xFF
                       && c.HandlerOffset <= 0xFFFF
                       && c.HandlerLength <= 0xFF
                   )

            if small then
                // `CorILMethod_Sect_EHTable`, the data size in one byte, and two reserved bytes.
                builder.Add 0x01uy
                builder.Add (byte (4 + 12 * clauses.Length))
                little 0 2

                for clause in clauses do
                    let flags, classToken = flagsAndClassToken clause
                    little flags 2
                    little clause.TryOffset 2
                    little clause.TryLength 1
                    little clause.HandlerOffset 2
                    little clause.HandlerLength 1
                    little classToken 4
            else
                // `CorILMethod_Sect_EHTable` with `CorILMethod_Sect_FatFormat`, and the data size in
                // three bytes.
                builder.Add 0x41uy
                little (4 + 24 * clauses.Length) 3

                for clause in clauses do
                    let flags, classToken = flagsAndClassToken clause
                    little flags 4
                    little clause.TryOffset 4
                    little clause.TryLength 4
                    little clause.HandlerOffset 4
                    little clause.HandlerLength 4
                    little classToken 4

        builder.ToImmutable ()

    /// <summary>
    /// Set a method's implementation flags, as <c>RegMeta::SetMethodImplFlags</c> does: the low 16
    /// bits of <paramref name="flags"/>, the column's width, replacing whatever the row held.
    /// </summary>
    let setMethodImplFlags (method : MethodDefRid) (flags : MethodImplAttributes) (scope : EmitScope) : EmitScope =
        EmitScope.setMethodImplFlags method (flags &&& enum<MethodImplAttributes> 0xFFFF) scope
