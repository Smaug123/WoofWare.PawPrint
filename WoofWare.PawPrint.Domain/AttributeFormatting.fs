namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Helpers for rendering custom attribute applications in the comment-prefixed
/// IlDump style. The "Attribute" suffix on attribute type names is stripped
/// (so <c>SerializableAttribute</c> renders as <c>[Serializable]</c>); blobs
/// whose ctor signature the reader can't decode fall back to a raw hex dump.
/// </summary>
[<RequireQualifiedAccess>]
module AttributeFormatting =

    /// <summary>
    /// Returns the custom attributes applied to <paramref name="parent"/> via the
    /// assembly's <c>CustomAttributesByParentToken</c> index, materialised from the
    /// <c>Attributes</c> dictionary. Attribute order matches the metadata order.
    /// </summary>
    let attributesFor (assembly : DumpedAssembly) (parent : MetadataToken) : CustomAttribute list =
        let parentRawToken = MetadataToken.toInt parent

        match assembly.CustomAttributesByParentToken.TryGetValue parentRawToken with
        | false, _ -> []
        | true, attrTokens ->
            attrTokens
            |> Seq.choose (fun t ->
                let rowNum = t &&& 0x00FFFFFF
                let handle = MetadataTokens.CustomAttributeHandle rowNum

                match assembly.Attributes.TryGetValue handle with
                | true, attr -> Some attr
                | false, _ -> None
            )
            |> List.ofSeq

    /// Strip a trailing "Attribute" suffix from a simple type name. The bare
    /// name "Attribute" is preserved (no infinite-strip).
    let private stripAttributeSuffix (name : string) : string =
        let suffix = "Attribute"

        if name.EndsWith (suffix, StringComparison.Ordinal) && name.Length > suffix.Length then
            name.Substring (0, name.Length - suffix.Length)
        else
            name

    /// Strip a CLI generic arity suffix (e.g. <c>`1</c>) from the end of a
    /// simple name. Only digit sequences after a single backtick are removed;
    /// anything else is returned unchanged.
    let private stripGenericArity (name : string) : string =
        let idx = name.LastIndexOf '`'

        if idx > 0 && idx < name.Length - 1 then
            let rest = name.Substring (idx + 1)

            if rest |> Seq.forall Char.IsDigit then
                name.Substring (0, idx)
            else
                name
        else
            name

    /// Apply <c>stripAttributeSuffix</c> to the last name segment of a qualified
    /// type name (segments are separated by '.' or '/'). The namespace prefix
    /// and any outer-type prefix are preserved unchanged.
    let private stripAttributeSuffixOnLastSegment (qualified : string) : string =
        let lastSlash = qualified.LastIndexOf '/'
        let lastDot = qualified.LastIndexOf '.'
        let sep = max lastSlash lastDot

        if sep < 0 then
            stripAttributeSuffix qualified
        else
            let head = qualified.Substring (0, sep + 1)
            let tail = qualified.Substring (sep + 1)
            head + stripAttributeSuffix tail

    /// Apply <c>stripGenericArity</c> followed by <c>stripAttributeSuffix</c>
    /// to the last name segment of a qualified type name. This is the rendering
    /// the human reader wants for a generic attribute: <c>MyAttribute`1</c>
    /// becomes <c>My</c>, ready for an explicit <c>&lt;args&gt;</c> suffix.
    let private prettifyGenericAttributeBase (qualified : string) : string =
        let lastSlash = qualified.LastIndexOf '/'
        let lastDot = qualified.LastIndexOf '.'
        let sep = max lastSlash lastDot

        let head, tail =
            if sep < 0 then
                "", qualified
            else
                qualified.Substring (0, sep + 1), qualified.Substring (sep + 1)

        head + (tail |> stripGenericArity |> stripAttributeSuffix)

    /// <summary>
    /// Render the display name of an attribute type expressed as a
    /// <see cref="TypeDefn"/>: typically the signature of a
    /// <c>TypeSpecification</c> used as the parent of a constructor
    /// <c>MemberReference</c>.
    /// </summary>
    /// <remarks>
    /// An attribute application names a closed type, so nothing binds a generic parameter here:
    /// any that survives into the blob is rendered positionally.
    /// </remarks>
    let private renderAttributeTypeFromTypeDefn (assembly : DumpedAssembly) (td : TypeDefn) : string =
        IlFormatting.renderTypeDefnAsName assembly GenericScope.unknown td

    /// <summary>
    /// The display name of the attribute type, i.e. the type whose constructor
    /// <paramref name="attr"/>.Constructor points at. The conventional trailing
    /// <c>Attribute</c> suffix is stripped from the simple name; the namespace
    /// (and any outer-type prefix for nested attribute types) is retained.
    /// Generic attributes whose ctor parent is a <c>TypeSpecification</c> are
    /// rendered as <c>Base&lt;args&gt;</c>, with the suffix stripped from
    /// <c>Base</c> rather than from the full <c>Base&lt;args&gt;</c> string.
    /// </summary>
    let attributeTypeName (assembly : DumpedAssembly) (attr : CustomAttribute) : string =
        match attr.Constructor with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, m ->
                let typeHandle = m.DeclaringType.Definition.Get

                match assembly.TypeDefs.TryGetValue typeHandle with
                | true, td ->
                    IlFormatting.qualifyTypeName assembly.TypeDefs td
                    |> stripAttributeSuffixOnLastSegment
                | false, _ -> $"TypeDef(%O{typeHandle})"
            | false, _ -> $"MethodDef(%O{handle})"
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, m ->
                match m.Parent with
                | MetadataToken.TypeSpecification tsHandle ->
                    match assembly.TypeSpecs.TryGetValue tsHandle with
                    | true, ts ->
                        match ts.Signature with
                        | TypeDefn.GenericInstantiation (generic, args) ->
                            // Strip the CLI arity marker AND the "Attribute" suffix from
                            // the generic *head*: an explicit "<args>" follows, so e.g.
                            // "MyGenericAttribute`1" becomes "MyGeneric", which we then
                            // combine with the rendered args.
                            let baseName =
                                renderAttributeTypeFromTypeDefn assembly generic |> prettifyGenericAttributeBase

                            let argsStr =
                                args |> Seq.map (renderAttributeTypeFromTypeDefn assembly) |> String.concat ", "

                            sprintf "%s<%s>" baseName argsStr
                        | other ->
                            renderAttributeTypeFromTypeDefn assembly other
                            |> stripAttributeSuffixOnLastSegment
                    | false, _ -> $"TypeSpec(%O{tsHandle})"
                | _ ->
                    // An attribute's ctor parent names a closed type; nothing is in scope to
                    // bind a generic parameter.
                    IlFormatting.formatMetadataToken assembly GenericScope.unknown m.Parent
                    |> stripAttributeSuffixOnLastSegment
            | false, _ -> $"MemberRef(%O{handle})"
        | other -> sprintf "%O" other

    /// Substitute occurrences of <c>GenericTypeParameter idx</c> in
    /// <paramref name="td"/> with the corresponding type from
    /// <paramref name="typeArgs"/>. Out-of-range indices are left unchanged so
    /// the consumer can still distinguish "no substitution possible" from a
    /// successful resolution. <c>GenericMethodParameter</c> is *not* touched —
    /// those are bound by the enclosing method, not the type spec.
    let rec private substituteTypeArgs (typeArgs : TypeDefn list) (td : TypeDefn) : TypeDefn =
        match td with
        | TypeDefn.GenericTypeParameter idx when idx >= 0 && idx < List.length typeArgs -> List.item idx typeArgs
        | TypeDefn.Array (elt, rank) -> TypeDefn.Array (substituteTypeArgs typeArgs elt, rank)
        | TypeDefn.OneDimensionalArrayLowerBoundZero elt ->
            TypeDefn.OneDimensionalArrayLowerBoundZero (substituteTypeArgs typeArgs elt)
        | TypeDefn.Pinned t -> TypeDefn.Pinned (substituteTypeArgs typeArgs t)
        | TypeDefn.Pointer t -> TypeDefn.Pointer (substituteTypeArgs typeArgs t)
        | TypeDefn.Byref t -> TypeDefn.Byref (substituteTypeArgs typeArgs t)
        | TypeDefn.Modified m ->
            TypeDefn.Modified
                {
                    Unmodified = substituteTypeArgs typeArgs m.Unmodified
                    Modifier = substituteTypeArgs typeArgs m.Modifier
                    IsRequired = m.IsRequired
                }
        | TypeDefn.GenericInstantiation (generic, args) ->
            let generic' = substituteTypeArgs typeArgs generic

            let args' =
                args
                |> Seq.map (substituteTypeArgs typeArgs)
                |> System.Collections.Immutable.ImmutableArray.CreateRange

            TypeDefn.GenericInstantiation (generic', args')
        | _ -> td

    /// Resolve the parameter types declared on the attribute's constructor, so
    /// that the blob reader can be invoked. Returns <c>None</c> if the ctor
    /// token is not a kind we know how to look up (e.g. MethodSpec) or the
    /// member-ref signature is a field rather than a method. When the parent
    /// of a MemberRef is a <c>TypeSpecification</c> holding a closed generic,
    /// the TypeSpec's args are substituted into the ctor's parameter types so
    /// a parameter declared as <c>T</c> resolves to its concrete instantiation
    /// before the blob decoder runs.
    let private tryConstructorParamTypes (assembly : DumpedAssembly) (attr : CustomAttribute) : TypeDefn list option =
        match attr.Constructor with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, m -> Some m.RawSignature.ParameterTypes
            | false, _ -> None
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, m ->
                match m.Signature with
                | MemberSignature.Method ms ->
                    let paramTypes =
                        match m.Parent with
                        | MetadataToken.TypeSpecification tsHandle ->
                            match assembly.TypeSpecs.TryGetValue tsHandle with
                            | true, ts ->
                                match ts.Signature with
                                | TypeDefn.GenericInstantiation (_, typeArgs) ->
                                    let argList = List.ofSeq typeArgs
                                    ms.ParameterTypes |> List.map (substituteTypeArgs argList)
                                | _ -> ms.ParameterTypes
                            | false, _ -> ms.ParameterTypes
                        | _ -> ms.ParameterTypes

                    Some paramTypes
                | MemberSignature.Field _ -> None
            | false, _ -> None
        | _ -> None

    /// <summary>
    /// Render a single decoded fixed arg in a form suitable for inclusion in a
    /// <c>[Attr(...)]</c> application. Strings and chars are escaped via
    /// <see cref="IlFormatting.escapeStringLiteral"/>; SZARRAYs render as
    /// brace-delimited element lists; the null variants of String and Array
    /// both render as <c>null</c>.
    /// </summary>
    let rec formatFixedArg (arg : CustomAttribFixedArg) : string =
        match arg with
        | CustomAttribFixedArg.Bool b -> if b then "true" else "false"
        | CustomAttribFixedArg.Char c ->
            // escapeStringLiteral handles backslash and the usual whitespace/null escapes,
            // but is targeted at double-quoted strings and so leaves '\'' as-is. Escape it
            // here so a single-quote char doesn't render as the ambiguous '''.
            let escaped =
                if c = '\'' then
                    "\\'"
                else
                    IlFormatting.escapeStringLiteral (string c)

            sprintf "'%s'" escaped
        | CustomAttribFixedArg.I1 v -> sprintf "%dy" v
        | CustomAttribFixedArg.U1 v -> sprintf "%duy" v
        | CustomAttribFixedArg.I2 v -> sprintf "%ds" v
        | CustomAttribFixedArg.U2 v -> sprintf "%dus" v
        | CustomAttribFixedArg.I4 v -> sprintf "%d" v
        | CustomAttribFixedArg.U4 v -> sprintf "%du" v
        | CustomAttribFixedArg.I8 v -> sprintf "%dL" v
        | CustomAttribFixedArg.U8 v -> sprintf "%duL" v
        | CustomAttribFixedArg.R4 v ->
            // "R" round-trips the metadata-stored bit pattern; "%g"'s default
            // precision silently truncates values like 1.234567f.
            sprintf "%sf" (v.ToString ("R", System.Globalization.CultureInfo.InvariantCulture))
        | CustomAttribFixedArg.R8 v -> v.ToString ("R", System.Globalization.CultureInfo.InvariantCulture)
        | CustomAttribFixedArg.String None -> "null"
        | CustomAttribFixedArg.String (Some s) -> sprintf "\"%s\"" (IlFormatting.escapeStringLiteral s)
        | CustomAttribFixedArg.Array None -> "null"
        | CustomAttribFixedArg.Array (Some elts) ->
            let inner = elts |> List.map formatFixedArg |> String.concat ", "
            sprintf "{ %s }" inner

    /// Dump the attribute's raw <c>Value</c> blob as space-separated hex bytes.
    /// Used when the structured decoder can't make progress.
    let private formatBlobAsHex (attr : CustomAttribute) : string =
        attr.Value |> Seq.map (sprintf "%02X") |> String.concat " "

    /// Decoded form of a blob: positional args and the raw <c>NumNamed</c>
    /// count (so callers can surface its presence without us having to decode
    /// the named-args section in this PR).
    type private DecodedBlob =
        {
            Args : CustomAttribFixedArg list
            NumNamed : uint16
        }

    let private tryDecodeBlob (assembly : DumpedAssembly) (attr : CustomAttribute) : Result<DecodedBlob, string> =
        match tryConstructorParamTypes assembly attr with
        | None -> Error "unresolved ctor"
        | Some paramTypes ->
            match CustomAttribute.readFixedArgs paramTypes attr.Value with
            | Error msg -> Error msg
            | Ok (args, offset) ->
                let numNamed =
                    let remaining = attr.Value.Length - offset

                    if remaining >= 2 then
                        (uint16 attr.Value.[offset]) ||| ((uint16 attr.Value.[offset + 1]) <<< 8)
                    else
                        0us

                Ok
                    {
                        Args = args
                        NumNamed = numNamed
                    }

    /// <summary>
    /// Render an attribute as <c>[Name(args) /* +N named */]</c>. Empty
    /// positional argument lists collapse to <c>[Name]</c>; when there are no
    /// positional args but named args are present, the comment trails the
    /// name directly. Blobs whose ctor signature is unresolved or whose
    /// decode fails fall back to a parenthesised hex dump.
    /// </summary>
    let formatAttributeApplication (assembly : DumpedAssembly) (attr : CustomAttribute) : string =
        let name = attributeTypeName assembly attr

        match tryDecodeBlob assembly attr with
        | Error _ ->
            if attr.Value.Length = 0 then
                sprintf "[%s]" name
            else
                sprintf "[%s(/* blob: %s */)]" name (formatBlobAsHex attr)
        | Ok decoded ->
            let argsClause =
                if List.isEmpty decoded.Args then
                    ""
                else
                    let argsStr = decoded.Args |> List.map formatFixedArg |> String.concat ", "

                    sprintf "(%s)" argsStr

            let namedSuffix =
                if decoded.NumNamed = 0us then
                    ""
                else
                    sprintf " /* +%d named */" decoded.NumNamed

            sprintf "[%s%s%s]" name argsClause namedSuffix

    /// <summary>
    /// Comment-prefixed header for a TypeDef in attribute-dump mode,
    /// including a <c>&lt;T, U&gt;</c> clause when the type is generic.
    /// </summary>
    let typeHeader (assembly : DumpedAssembly) (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
        let qualified = IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo
        let generics = IlFormatting.formatGenericsClause typeInfo.Generics
        sprintf "// type %s%s" qualified generics

    /// <summary>
    /// Comment-prefixed header for a MethodDef in attribute-dump mode. The
    /// owning type's qualified name is supplied by the caller (so a type-level
    /// walk doesn't re-resolve it per method).
    /// </summary>
    let methodHeader
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let staticStr = if method.IsStatic then "static " else ""
        let generics = IlFormatting.formatGenericsClause method.Generics

        // The method's own declaration binds the parameters its signature mentions.
        let scope = GenericScope.ofMethod method

        let paramTypes =
            method.RawSignature.ParameterTypes
            |> List.map (IlFormatting.renderTypeDefn assembly scope)
            |> String.concat ", "

        let returnType =
            IlFormatting.renderMethodReturnType assembly scope method.RawSignature.ReturnType

        sprintf "// method %s::%s%s%s(%s) : %s" qualifiedTypeName staticStr method.Name generics paramTypes returnType

    /// <summary>
    /// Comment-prefixed header for a FieldDef. Static-ness is rendered in the
    /// same position as <see cref="methodHeader"/> renders it. A trailing
    /// <c>@ 0xNN</c> reports the field's offset for explicitly-laid-out types;
    /// fields whose offset the runtime chooses carry no such suffix.
    /// </summary>
    let fieldHeader
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let staticStr =
            if field.Attributes.HasFlag System.Reflection.FieldAttributes.Static then
                "static "
            else
                ""

        let offsetStr =
            match field.Offset with
            | None -> ""
            | Some offset -> sprintf " @ 0x%X" offset

        // A field's signature is written against its declaring type's parameters; a field
        // declaration binds no method parameters.
        let signature =
            IlFormatting.renderTypeDefn assembly (GenericScope.ofDeclaringType field.DeclaringType) field.Signature

        sprintf "// field %s::%s%s : %s%s" qualifiedTypeName staticStr field.Name signature offsetStr

    /// Comment-prefixed header for an EventDef.
    let eventHeader (qualifiedTypeName : string) (event : EventDefn) : string =
        sprintf "// event %s::%s" qualifiedTypeName event.Name

    /// Comment-prefixed header for a PropertyDef. PropertyDef has no domain
    /// type, so the name is supplied by the IlDump caller after reading the
    /// metadata reader's PropertyDefinitions table.
    let propertyHeader (qualifiedTypeName : string) (propertyName : string) : string =
        sprintf "// property %s::%s" qualifiedTypeName propertyName

    /// Comment-prefixed header for the assembly manifest row. The simple
    /// assembly name is used (rather than the full versioned name) so the
    /// header matches the style of the other comment-prefixed headers.
    let assemblyHeader (assembly : DumpedAssembly) : string =
        sprintf "// assembly %s" assembly.Name.Name

    /// Comment-prefixed header for the module definition. The module name is
    /// supplied by the caller after reading the metadata reader's
    /// ModuleDefinition row.
    let moduleHeader (moduleName : string) : string = sprintf "// module %s" moduleName

    /// <summary>
    /// Render an owner header followed by one indented <c>[Attr(args)]</c>
    /// line per attribute, or an empty list if the owner has no attributes.
    /// The header is always a comment-prefixed line so it interleaves with
    /// the existing IL-dump format.
    /// </summary>
    let renderOwnerLines (assembly : DumpedAssembly) (header : string) (parent : MetadataToken) : string list =
        let attrs = attributesFor assembly parent

        match attrs with
        | [] -> []
        | _ ->
            [
                yield header
                for attr in attrs do
                    yield sprintf "//   %s" (formatAttributeApplication assembly attr)
            ]
