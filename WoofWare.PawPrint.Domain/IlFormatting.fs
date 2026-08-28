namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// What the generic-parameter indices in a <see cref="TypeDefn"/> mean at the point where it is
/// being rendered.
/// </summary>
/// <remarks>
/// <para>
/// A signature's <c>GenericTypeParameter 0</c> is only meaningful relative to whatever *binds*
/// it, which is not always the code the reader is looking at: the signature of a member
/// referenced from inside a method is written in the scope of the type that declares that
/// member, not in the scope of the calling method. So the binding is passed in explicitly, and
/// callers must supply the scope that binds the indices in the signature they are
/// rendering.
/// </para>
/// <para>
/// A missing entry is not an error: an index with no name renders positionally, ILDasm-style,
/// as <c>!0</c> / <c>!!0</c>. <see cref="GenericScope.unknown"/> — "nothing is known to be
/// bound here" — is therefore always a safe answer, and is the right one wherever we cannot
/// prove which declaration binds the indices.
/// </para>
/// </remarks>
type GenericScope =
    {
        /// Names of the generic parameters bound by the enclosing *type*, keyed by the sequence
        /// number a <c>TypeDefn.GenericTypeParameter</c> would carry.
        TypeParameters : Map<int, string>

        /// Names of the generic parameters bound by the enclosing *method*, keyed by the sequence
        /// number a <c>TypeDefn.GenericMethodParameter</c> would carry.
        MethodParameters : Map<int, string>
    }

[<RequireQualifiedAccess>]
module GenericScope =
    /// No generic parameter is known to be bound here, so every index renders positionally.
    let unknown : GenericScope =
        {
            TypeParameters = Map.empty
            MethodParameters = Map.empty
        }

    /// Index the parameters by their declared sequence number rather than by their position in
    /// the array: the sequence number is what a `TypeDefn` index refers to.
    let private names (parameters : GenericParamFromMetadata ImmutableArray) : Map<int, string> =
        parameters
        |> Seq.map (fun ((param : WoofWare.PawPrint.GenericParameter), _) -> param.SequenceNumber, param.Name)
        |> Map.ofSeq

    /// The scope in which the signatures of the members `typeInfo` declares are written. Their
    /// method parameters are bound per-member, so they are not in scope here.
    let ofType (typeInfo : TypeInfo<GenericParamFromMetadata, 'field>) : GenericScope =
        {
            TypeParameters = names typeInfo.Generics
            MethodParameters = Map.empty
        }

    /// The scope in which the members `declaringType` declares are written; as
    /// <see cref="ofType"/>, but for a type reached through a member rather than through the
    /// TypeDef index.
    let ofDeclaringType (declaringType : ConcreteType<GenericParamFromMetadata>) : GenericScope =
        {
            TypeParameters = names declaringType.Generics
            MethodParameters = Map.empty
        }

    /// The scope in which `method`'s signature, locals and IL body are written.
    let ofMethod
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, 'vars>)
        : GenericScope
        =
        {
            TypeParameters = names method.DeclaringTypeGenerics
            MethodParameters = names method.Generics
        }

[<RequireQualifiedAccess>]
module IlFormatting =
    let qualifyTypeName
        (typeDefs : IReadOnlyDictionary<TypeDefinitionHandle, TypeInfo<GenericParamFromMetadata, TypeDefn>>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let rec buildNesting (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string list =
            if ti.DeclaringType.IsNil then
                if String.IsNullOrEmpty ti.Namespace then
                    [ ti.Name ]
                else
                    [ $"%s{ti.Namespace}.%s{ti.Name}" ]
            else
                match typeDefs.TryGetValue ti.DeclaringType with
                | true, parent -> ti.Name :: buildNesting parent
                | false, _ -> [ ti.Name ]

        buildNesting typeInfo |> List.rev |> String.concat "/"

    /// <summary>
    /// Format a TypeRef as <c>Namespace.Name</c>, walking the <c>ResolutionScope</c> chain to
    /// prefix any outer types (separated by <c>/</c>). The leading namespace is omitted for
    /// nested types, which carry it on their outermost enclosing type instead.
    /// </summary>
    let qualifyTypeRef (assembly : DumpedAssembly) (typeRef : TypeRef) : string =
        let rec qualify (r : TypeRef) : string =
            match r.ResolutionScope with
            | TypeRefResolutionScope.TypeRef parentHandle ->
                match assembly.TypeRefs.TryGetValue parentHandle with
                | true, parent -> $"%s{qualify parent}/%s{r.Name}"
                | false, _ -> r.Name
            | _ ->
                if String.IsNullOrEmpty r.Namespace then
                    r.Name
                else
                    $"%s{r.Namespace}.%s{r.Name}"

        qualify typeRef

    /// <summary>
    /// The display name of the type identified by a TypeDef row. When the row belongs to the
    /// assembly being dumped we can name it outright; otherwise we have not read the defining
    /// assembly, so we report the assembly and the row's token — enough for the reader to run
    /// ildump against it — rather than inventing a name we do not have.
    /// </summary>
    let private renderTypeDefinition (assembly : DumpedAssembly) (identity : ResolvedTypeIdentity) : string =
        let handle = identity.TypeDefinition.Get
        let entityHandle : EntityHandle = TypeDefinitionHandle.op_Implicit handle
        let token = MetadataTokens.GetToken entityHandle

        if identity.AssemblyFullName = assembly.Name.FullName then
            match assembly.TypeDefs.TryGetValue handle with
            | true, typeInfo -> qualifyTypeName assembly.TypeDefs typeInfo
            | false, _ -> $"TypeDef(0x%08X{token})"
        else
            $"[%s{AssemblyDefinitionName.simpleName identity.AssemblyFullName}]TypeDef(0x%08X{token})"

    /// <summary>
    /// How a type reached through a TypeRef is spelled.
    /// </summary>
    /// <remarks>
    /// This is the only axis on which a disassembly rendering and a display name differ, so it
    /// is the only thing the two entry points below vary. Everything else about them is then
    /// identical by construction — separate walks would drift apart.
    /// </remarks>
    type private TypeRefStyle =
        /// <c>ref[System.Object]</c>: a disassembly distinguishes a reference from a definition.
        | Tagged
        /// <c>System.Object</c>: a display name names a type rather than describing one.
        | Bare

    let rec private renderTypeDefnWith
        (style : TypeRefStyle)
        (assembly : DumpedAssembly)
        (scope : GenericScope)
        (typeDefn : TypeDefn)
        : string
        =
        let recurse : TypeDefn -> string = renderTypeDefnWith style assembly scope

        match typeDefn with
        | TypeDefn.PrimitiveType primitiveType -> $"%O{primitiveType}"
        | TypeDefn.Array (elt, rank) -> $"arr[%s{recurse elt} ; rank=%i{rank}]"
        | TypeDefn.Pinned typeDefn -> $"pinned[%s{recurse typeDefn}]"
        | TypeDefn.Pointer typeDefn -> $"ptr[%s{recurse typeDefn}]"
        | TypeDefn.Byref typeDefn -> $"byref[%s{recurse typeDefn}]"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> $"arr[%s{recurse elements}]"
        | TypeDefn.Modified m ->
            let req = if m.IsRequired then "modreq" else "modopt"

            $"modified[%s{recurse m.Unmodified} ; %s{req}=%s{recurse m.Modifier}]"
        | TypeDefn.FromReference (typeRef, _) ->
            let name = qualifyTypeRef assembly typeRef

            match style with
            | TypeRefStyle.Tagged -> $"ref[%s{name}]"
            | TypeRefStyle.Bare -> name
        | TypeDefn.FromDefinition (identity, _) -> renderTypeDefinition assembly identity
        | TypeDefn.GenericInstantiation (generic, args) ->
            let args = args |> Seq.map recurse |> String.concat ", "
            $"%s{recurse generic}<%s{args}>"
        | TypeDefn.FunctionPointer typeMethodSignature ->
            let args =
                typeMethodSignature.ParameterTypes |> List.map recurse |> String.concat " -> "

            let ret =
                renderMethodReturnTypeWith style assembly scope typeMethodSignature.ReturnType

            $"*(%s{args} -> %s{ret})"
        | TypeDefn.GenericTypeParameter index ->
            match Map.tryFind index scope.TypeParameters with
            | Some name -> $"!%s{name}"
            | None -> $"!%i{index}"
        | TypeDefn.GenericMethodParameter index ->
            match Map.tryFind index scope.MethodParameters with
            | Some name -> $"!!%s{name}"
            | None -> $"!!%i{index}"
        | TypeDefn.Void -> "void"

    and private renderMethodReturnTypeWith
        (style : TypeRefStyle)
        (assembly : DumpedAssembly)
        (scope : GenericScope)
        (ret : MethodReturnType<TypeDefn>)
        : string
        =
        match ret with
        | MethodReturnType.Void -> "void"
        | MethodReturnType.Returns ty -> renderTypeDefnWith style assembly scope ty

    /// <summary>
    /// Render a <see cref="TypeDefn"/> for a human reading a disassembly.
    /// </summary>
    /// <remarks>
    /// This deliberately does not call <c>TypeDefn.ToString</c>, which is a Domain-layer debug
    /// rendering: it collapses a TypeDef to <c>&lt;type defined in Foo&gt;</c> and the generic
    /// parameters to <c>&lt;type param 0&gt;</c>, none of which a reader can act on. Every case
    /// is therefore handled here, and every nested type goes back through this function.
    /// Generic parameters are named when <paramref name="scope"/> says what binds them, and
    /// render positionally (<c>!0</c>, <c>!!0</c>) otherwise.
    /// </remarks>
    let renderTypeDefn (assembly : DumpedAssembly) (scope : GenericScope) (typeDefn : TypeDefn) : string =
        renderTypeDefnWith TypeRefStyle.Tagged assembly scope typeDefn

    /// As <see cref="renderTypeDefn"/>, for the return shape of a signature. `void` is the
    /// absence of a return value rather than a type, so it has no scope-dependent rendering.
    let renderMethodReturnType
        (assembly : DumpedAssembly)
        (scope : GenericScope)
        (ret : MethodReturnType<TypeDefn>)
        : string
        =
        renderMethodReturnTypeWith TypeRefStyle.Tagged assembly scope ret

    /// <summary>
    /// As <see cref="renderTypeDefn"/>, but naming types rather than describing them: a
    /// referenced type renders as a bare qualified name, at every depth.
    /// </summary>
    /// <remarks>
    /// This is what an attribute application wants. <c>[Lib.MyGeneric&lt;Lib.ArgType&gt;]</c>
    /// is a name, so the <c>ref[...]</c> tagging would be the wrong register; it would also
    /// hide the last name segment from the arity and "Attribute" suffix stripping the caller
    /// applies to the generic head.
    /// </remarks>
    let renderTypeDefnAsName (assembly : DumpedAssembly) (scope : GenericScope) (typeDefn : TypeDefn) : string =
        renderTypeDefnWith TypeRefStyle.Bare assembly scope typeDefn

    /// Render the generic-parameter clause shown after a type or method name
    /// (e.g. <c>&lt;T, U&gt;</c>). Returns <c>""</c> if there are no generics.
    let formatGenericsClause (generics : GenericParamFromMetadata seq) : string =
        let names =
            generics
            |> Seq.map (fun ((param : WoofWare.PawPrint.GenericParameter), _) -> param.Name)
            |> List.ofSeq

        if List.isEmpty names then
            ""
        else
            let joined = String.concat ", " names
            $"<%s{joined}>"

    let private formatMemberSignature
        (assembly : DumpedAssembly)
        (scope : GenericScope)
        (signature : MemberSignature)
        : string
        =
        match signature with
        | MemberSignature.Method m ->
            let paramTypes =
                m.ParameterTypes
                |> List.map (renderTypeDefn assembly scope)
                |> String.concat ", "

            $"(%s{paramTypes}) : %s{renderMethodReturnType assembly scope m.ReturnType}"
        | MemberSignature.Field f -> $" : %s{renderTypeDefn assembly scope f}"

    /// <summary>
    /// The scope in which the signature of a member whose parent is <paramref name="parent"/> is
    /// written: that of the type which declares the member.
    /// </summary>
    /// <remarks>
    /// A member reference's signature is emitted by the compiler against the *declaring type's*
    /// generic parameters, so it must not be read in the scope of the method which references it:
    /// inside <c>List&lt;T&gt;.ConvertAll&lt;TOutput&gt;</c>, the field
    /// <c>List&lt;!!TOutput&gt;::_items</c> still declares its type as <c>!0[]</c>, and that
    /// <c>!0</c> is <c>List</c>'s own <c>T</c>. Method parameters are never in scope here: a
    /// member reference's <c>!!n</c> binds to the *referenced* method's own parameters, whose
    /// names live with that method's declaration rather than at the reference.
    /// </remarks>
    let private memberSignatureScope (assembly : DumpedAssembly) (parent : MetadataToken) : GenericScope =
        let ofTypeDefHandle (handle : TypeDefinitionHandle) : GenericScope =
            match assembly.TypeDefs.TryGetValue handle with
            | true, typeInfo -> GenericScope.ofType typeInfo
            | false, _ -> GenericScope.unknown

        match parent with
        | MetadataToken.TypeDefinition handle -> ofTypeDefHandle handle
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, typeSpec ->
                // The parent is typically a closed instantiation such as `List<!!0>`; the names
                // we want are the ones on the generic definition at its head.
                match typeSpec.Signature with
                | TypeDefn.GenericInstantiation (TypeDefn.FromDefinition (identity, _), _)
                | TypeDefn.FromDefinition (identity, _) ->
                    if identity.AssemblyFullName = assembly.Name.FullName then
                        ofTypeDefHandle identity.TypeDefinition.Get
                    else
                        GenericScope.unknown
                | _ -> GenericScope.unknown
            | false, _ -> GenericScope.unknown
        | _ -> GenericScope.unknown

    /// <summary>
    /// Render the target of a metadata token. <paramref name="scope"/> is the scope of the code
    /// containing the token — it binds the generic arguments written *at* the token, such as a
    /// MethodSpec's type arguments or a TypeSpec's instantiation. It does not bind the generic
    /// parameters inside a referenced member's own signature; see
    /// <see cref="memberSignatureScope"/>.
    /// </summary>
    let rec formatMetadataToken (assembly : DumpedAssembly) (scope : GenericScope) (token : MetadataToken) : string =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, m ->
                // renderTypeDefinition also covers the case where the declaring type is somehow
                // absent from the TypeDef index; ConcreteType's own ToString would render each
                // of its generic parameters as a raw metadata record.
                let typeName =
                    renderTypeDefinition
                        assembly
                        (MethodOwner.requireDeclaringType "rendering a MethodDef token" m.Owner).Identity

                $"%s{typeName}::%s{m.Name}"
            | false, _ -> $"MethodDef(%O{handle})"
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, m ->
                let parentStr = formatMetadataToken assembly scope m.Parent

                let sigStr =
                    formatMemberSignature assembly (memberSignatureScope assembly m.Parent) m.Signature

                $"%s{parentStr}::%s{m.PrettyName}%s{sigStr}"
            | false, _ -> $"MemberRef(%O{handle})"
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                // The type arguments are written at the call site, so they are read in the
                // scope of the code containing this token.
                let args =
                    spec.Signature |> Seq.map (renderTypeDefn assembly scope) |> String.concat ", "

                match spec.Method with
                | MetadataToken.MemberReference memberHandle ->
                    match assembly.Members.TryGetValue memberHandle with
                    | true, m ->
                        let parentStr = formatMetadataToken assembly scope m.Parent

                        let sigStr =
                            formatMemberSignature assembly (memberSignatureScope assembly m.Parent) m.Signature

                        $"%s{parentStr}::%s{m.PrettyName}<%s{args}>%s{sigStr}"
                    | false, _ -> $"MemberRef(%O{memberHandle})<%s{args}>"
                | other ->
                    let methodName = formatMetadataToken assembly scope other
                    $"%s{methodName}<%s{args}>"
            | false, _ -> $"MethodSpec(%O{handle})"
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, tr -> qualifyTypeRef assembly tr
            | false, _ -> $"TypeRef(%O{handle})"
        | MetadataToken.TypeDefinition handle ->
            match assembly.TypeDefs.TryGetValue handle with
            | true, td -> qualifyTypeName assembly.TypeDefs td
            | false, _ -> $"TypeDef(%O{handle})"
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            // A TypeSpec appearing as an instruction's operand is written at that instruction,
            // so its generic arguments are bound by the enclosing code.
            | true, ts -> renderTypeDefn assembly scope ts.Signature
            | false, _ -> $"TypeSpec(%O{handle})"
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, f ->
                let typeName = renderTypeDefinition assembly f.DeclaringType.Identity

                $"%s{typeName}::%s{f.Name}"
            | false, _ -> $"FieldDef(%O{handle})"
        | other -> $"%O{other}"

    let escapeStringLiteral (s : string) : string =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")
            .Replace ("\0", "\\0")

    /// <summary>
    /// Render one instruction. <paramref name="scope"/> is the scope of the method whose body
    /// this instruction belongs to; pass <see cref="GenericScope.unknown"/> if that method is
    /// not to hand, and generic parameters render positionally.
    /// </summary>
    let formatIlOp (assembly : DumpedAssembly) (scope : GenericScope) (ilOp : IlOp) (offset : int) : string =
        match ilOp with
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata token) ->
            let tokenStr = formatMetadataToken assembly scope token.Token
            $"    IL_%04X{offset}: %-20O{op} %s{tokenStr}"
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope scopeIndex) ->
            // As for `ldstr` below: the entry lives in the guest heap, which this formatter cannot
            // reach, and is read when the instruction executes rather than when the body was
            // decoded.
            $"    IL_%04X{offset}: %-20O{op} DynamicScope[%d{scopeIndex}]"
        | IlOp.UnaryStringToken (op, StringOperand.FromMetadata token) ->
            let str = assembly.Strings token.Token |> escapeStringLiteral
            $"    IL_%04X{offset}: %-20O{op} \"%s{str}\""
        | IlOp.UnaryStringToken (op, StringOperand.FromDynamicScope scopeIndex) ->
            // No value to show: it lives in the guest heap, which this formatter has no access to,
            // and is read when the instruction executes rather than when the body was decoded.
            $"    IL_%04X{offset}: %-20O{op} DynamicScope[%d{scopeIndex}]"
        | _ -> IlOp.Format ilOp offset

    /// <remarks>
    /// This is specialised to the metadata flavour of <see cref="MethodInfo"/> — the one the
    /// TypeDef index holds — because the whole point is to render the method's types by the
    /// names its own declaration gives them, which only that flavour carries.
    /// </remarks>
    let formatMethodLines
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string list
        =
        let staticStr = if method.IsStatic then "static " else ""

        // The method's own declaration binds both its type's and its own generic parameters,
        // so everything printed below can name them.
        let scope = GenericScope.ofMethod method

        let generics = formatGenericsClause method.Generics

        let paramTypes =
            method.Signature.ParameterTypes
            |> List.map (renderTypeDefn assembly scope)
            |> String.concat ", "

        let returnType = renderMethodReturnType assembly scope method.Signature.ReturnType

        let header =
            $"// %s{qualifiedTypeName}::%s{staticStr}%s{method.Name}%s{generics}(%s{paramTypes}) : %s{returnType}"

        match method.Body with
        | MethodBody.InternalCall -> [ header ; "  // No IL body (InternalCall)" ]
        | MethodBody.PInvoke -> [ header ; "  // No IL body (P/Invoke)" ]
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateCtor ->
            [ header ; "  // No IL body (runtime-provided delegate .ctor)" ]
        | MethodBody.RuntimeProvided RuntimeBehaviour.DelegateInvoke ->
            [ header ; "  // No IL body (runtime-provided delegate Invoke)" ]
        | MethodBody.RuntimeProvided RuntimeBehaviour.StructMarshalStub ->
            [ header ; "  // No IL body (runtime-provided struct-marshal stub)" ]
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, targetName, hasTypeNameOverrides)) ->
            let nameStr =
                match targetName with
                | Some n -> $"\"{n}\""
                | None -> "<attributed method name>"

            let overrides =
                if hasTypeNameOverrides then
                    ", types named by [UnsafeAccessorType]"
                else
                    ""

            [
                header
                $"  // No IL body (runtime-provided UnsafeAccessor: {kind}, target={nameStr}{overrides})"
            ]
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessorInvalidKind raw) ->
            [
                header
                $"  // No IL body (runtime-provided UnsafeAccessor naming no kind: {raw})"
            ]
        | MethodBody.RuntimeProvided (RuntimeBehaviour.Unrecognised name) ->
            [ header ; $"  // No IL body (runtime-provided, unclassified: {name})" ]
        | MethodBody.Abstract -> [ header ; "  // No IL body (abstract)" ]
        | MethodBody.Il instructions ->
            let localLines =
                match instructions.LocalVars with
                | None -> []
                | Some locals when locals.Length = 0 -> []
                | Some locals ->
                    let initStr = if instructions.LocalsInit then " init" else ""

                    [
                        yield $"  .locals%s{initStr}"

                        for i = 0 to locals.Length - 1 do
                            yield $"    [%d{i}] %s{renderTypeDefn assembly scope locals.[i]}"
                    ]

            let instructionLines =
                instructions.Instructions
                |> List.map (fun (ilOp, offset) -> formatIlOp assembly scope ilOp offset)

            [ yield header ; yield! localLines ; yield! instructionLines ]
