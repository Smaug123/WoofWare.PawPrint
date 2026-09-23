namespace WoofWare.PawPrint

open System
open System.Globalization
open System.Security.Cryptography
open System.Text

/// The identity of a method's IL body: sixteen lowercase hex digits of the SHA-256 of
/// `IlBodyFingerprint.canonicalText`.
///
/// A body with different instructions, operands, locals, exception regions or signature has a
/// different fingerprint (up to the hash's collisions): the rendering it hashes loses nothing.
/// Metadata tokens are rendered by what they name — type and member names, full signatures,
/// the assembly a type reference resolves to — rather than by their row numbers, so a rebuild of
/// the same source that renumbers the image's tables keeps the fingerprint. The exception is
/// `calli`'s standalone signature, which renders as its blob's bytes: its type operands are
/// coded row numbers, so a renumbering can change such a body's fingerprint, and the gate then
/// refuses it until it is reviewed again.
type IlBodyFingerprint =
    private
    | IlBodyFingerprint of string

    /// The sixteen lowercase hex digits.
    member this.Hex : string =
        match this with
        | IlBodyFingerprint hex -> hex

    override this.ToString () : string = this.Hex

    /// Parse a fingerprint written out in full, as `Hex` renders it. Anything else is refused.
    static member OfHex (hex : string) : IlBodyFingerprint =
        let isLowerHex (c : char) : bool =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

        if hex.Length <> 16 || not (Seq.forall isLowerHex hex) then
            failwith $"%s{hex} is not an IL body fingerprint: expected sixteen lowercase hex digits"

        IlBodyFingerprint hex

[<RequireQualifiedAccess>]
module IlBodyFingerprint =

    // Everything below renders exactly: names where metadata gives them, row numbers where it
    // does not, never a display abbreviation. Two operands that differ must render differently,
    // or the gate would admit a body its row never reviewed.

    let private invariant (i : int) : string = i.ToString CultureInfo.InvariantCulture

    /// Every UTF-16 code unit as four hex digits, so that an unpaired surrogate survives the UTF-8
    /// encoding the hash is taken over.
    let private codeUnits (s : string) : string =
        s
        |> Seq.map (fun c -> (int c).ToString ("x4", CultureInfo.InvariantCulture))
        |> String.concat ""

    let private row (handle : System.Reflection.Metadata.EntityHandle) : string =
        $"0x%08X{System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle}"

    let private typeDefText (assembly : DumpedAssembly) (handle : System.Reflection.Metadata.TypeDefinitionHandle) =
        match assembly.TypeDefs.TryGetValue handle with
        | true, typeInfo -> IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo
        | false, _ -> $"TypeDef %s{row (System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit handle)}"

    let rec private typeRefText (assembly : DumpedAssembly) (typeRef : TypeRef) : string =
        let name =
            if String.IsNullOrEmpty typeRef.Namespace then
                typeRef.Name
            else
                $"%s{typeRef.Namespace}.%s{typeRef.Name}"

        match typeRef.ResolutionScope with
        | TypeRefResolutionScope.Assembly handle ->
            match assembly.AssemblyReferences.TryGetValue handle with
            | true, reference -> $"[%s{reference.Name.Name}]%s{name}"
            | false, _ ->
                $"[AssemblyRef %s{row (System.Reflection.Metadata.AssemblyReferenceHandle.op_Implicit handle)}]%s{name}"
        | TypeRefResolutionScope.TypeRef parent ->
            match assembly.TypeRefs.TryGetValue parent with
            | true, parent -> $"%s{typeRefText assembly parent}/%s{name}"
            | false, _ ->
                $"[TypeRef %s{row (System.Reflection.Metadata.TypeReferenceHandle.op_Implicit parent)}]/%s{name}"
        | TypeRefResolutionScope.ModuleDef _ -> $"[this module]%s{name}"
        | TypeRefResolutionScope.ModuleRef handle ->
            $"[ModuleRef %s{row (System.Reflection.Metadata.ModuleReferenceHandle.op_Implicit handle)}]%s{name}"

    /// Generic parameters render by position, which is what a signature records.
    let rec private typeText (assembly : DumpedAssembly) (typeDefn : TypeDefn) : string =
        let recurse = typeText assembly

        match typeDefn with
        | TypeDefn.PrimitiveType primitive -> $"%O{primitive}"
        | TypeDefn.Array (element, rank) -> $"%s{recurse element}[rank %s{invariant rank}]"
        | TypeDefn.Pinned inner -> $"pinned(%s{recurse inner})"
        | TypeDefn.Pointer inner -> $"%s{recurse inner}*"
        | TypeDefn.Byref inner -> $"%s{recurse inner}&"
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> $"%s{recurse element}[]"
        | TypeDefn.Modified modified ->
            let kind = if modified.IsRequired then "modreq" else "modopt"
            $"%s{recurse modified.Unmodified} %s{kind}(%s{recurse modified.Modifier})"
        | TypeDefn.FromReference (typeRef, _) -> typeRefText assembly typeRef
        | TypeDefn.FromDefinition (identity, _) ->
            if identity.AssemblyFullName = assembly.DefinitionFullName then
                typeDefText assembly identity.TypeDefinition.Get
            else
                let handle : System.Reflection.Metadata.EntityHandle =
                    System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit identity.TypeDefinition.Get

                $"[%s{identity.AssemblyFullName}]TypeDef %s{row handle}"
        | TypeDefn.GenericInstantiation (generic, args) ->
            let args = args |> Seq.map recurse |> String.concat ", "
            $"%s{recurse generic}<%s{args}>"
        | TypeDefn.FunctionPointer signature -> $"fnptr %s{signatureText assembly signature}"
        | TypeDefn.GenericTypeParameter index -> $"!%s{invariant index}"
        | TypeDefn.GenericMethodParameter index -> $"!!%s{invariant index}"
        | TypeDefn.Void -> "void"

    and private signatureText (assembly : DumpedAssembly) (signature : TypeMethodSignature<TypeDefn>) : string =
        let parameters =
            signature.ParameterTypes |> List.map (typeText assembly) |> String.concat ", "

        let returns =
            match signature.ReturnType with
            | MethodReturnType.Void -> "void"
            | MethodReturnType.Returns returns -> typeText assembly returns

        $"header 0x%02X{signature.Header.Get.RawValue} generics %s{invariant signature.GenericParameterCount} required %s{invariant signature.RequiredParameterCount} (%s{parameters}) : %s{returns}"

    let rec private tokenText (assembly : DumpedAssembly) (token : MetadataToken) : string =
        match token with
        | MetadataToken.TypeDefinition handle -> $"type %s{typeDefText assembly handle}"
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, typeRef -> $"type %s{typeRefText assembly typeRef}"
            | false, _ -> $"TypeRef %s{row (System.Reflection.Metadata.TypeReferenceHandle.op_Implicit handle)}"
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, spec -> $"type %s{typeText assembly spec.Signature}"
            | false, _ -> $"TypeSpec %s{row (System.Reflection.Metadata.TypeSpecificationHandle.op_Implicit handle)}"
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, field ->
                $"field %s{typeDefText assembly field.DeclaringType.Identity.TypeDefinition.Get}::%s{field.Name} : %s{typeText assembly field.Signature}"
            | false, _ -> $"FieldDef %s{row (System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit handle)}"
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, callee ->
                $"method %s{typeDefText assembly callee.RequiredDeclaringType.Definition.Get}::%s{callee.Name} %s{signatureText assembly callee.Signature}"
            | false, _ -> $"MethodDef %s{row (System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle)}"
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, reference ->
                let signature =
                    match reference.Signature with
                    | MemberSignature.Method signature -> $"method %s{signatureText assembly signature}"
                    | MemberSignature.Field fieldType -> $"field %s{typeText assembly fieldType}"

                $"memberref (%s{tokenText assembly reference.Parent})::%s{assembly.Strings reference.Name} %s{signature}"
            | false, _ -> $"MemberRef %s{row (System.Reflection.Metadata.MemberReferenceHandle.op_Implicit handle)}"
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                let args = spec.Signature |> Seq.map (typeText assembly) |> String.concat ", "
                $"(%s{tokenText assembly spec.Method})<%s{args}>"
            | false, _ ->
                $"MethodSpec %s{row (System.Reflection.Metadata.MethodSpecificationHandle.op_Implicit handle)}"
        | MetadataToken.StandaloneSignature handle ->
            let metadata : System.Reflection.Metadata.MetadataReader =
                System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

            let blob : byte[] =
                metadata.GetBlobBytes (metadata.GetStandaloneSignature handle).Signature

            $"standalone sig %s{Convert.ToHexString blob}"
        | other -> $"token %O{other}"

    let private renderOp (assembly : DumpedAssembly) (op : IlOp) : string =
        match op with
        | IlOp.Nullary op -> op.ToString ()
        // `UnaryConstIlOp.ToString` prints floating-point operands with `%f`, which keeps six
        // decimal places; the bit pattern is the operand itself.
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 f) -> $"Ldc_R4 0x%08X{BitConverter.SingleToInt32Bits f}"
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 f) -> $"Ldc_R8 0x%016X{BitConverter.DoubleToInt64Bits f}"
        | IlOp.UnaryConst op -> op.ToString ()
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata token) ->
            $"%O{op} %s{tokenText assembly token.Token}"
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope index) ->
            $"%O{op} DynamicScope[%s{invariant index}]"
        | IlOp.UnaryStringToken (op, StringOperand.FromMetadata token) ->
            $"%O{op} utf16 %s{codeUnits (assembly.Strings token.Token)}"
        | IlOp.UnaryStringToken (op, StringOperand.FromDynamicScope index) ->
            $"%O{op} DynamicScope[%s{invariant index}]"
        | IlOp.Switch targets -> "Switch " + (targets |> Seq.map invariant |> String.concat ",")

    let private renderRegion (assembly : DumpedAssembly) (region : ExceptionRegion) : string =
        let offsets (o : ExceptionOffset) : string =
            $"try %s{invariant o.TryOffset}+%s{invariant o.TryLength} handler %s{invariant o.HandlerOffset}+%s{invariant o.HandlerLength}"

        match region with
        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) ->
            $"catch %s{tokenText assembly token} %s{offsets o}"
        | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope index, o) ->
            $"catch DynamicScope[%s{invariant index}] %s{offsets o}"
        | ExceptionRegion.Filter (filterOffset, o) -> $"filter %s{invariant filterOffset} %s{offsets o}"
        | ExceptionRegion.Finally o -> $"finally %s{offsets o}"
        | ExceptionRegion.Fault o -> $"fault %s{offsets o}"

    /// The text the fingerprint hashes: `method`'s signature, locals, instructions (each at its
    /// offset) and exception regions, every token rendered by what it names. `None` when the
    /// method has no IL body.
    ///
    /// Exposed so that a mismatch can be diagnosed by comparing two renderings line by line.
    let canonicalText
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string option
        =
        match method.Body with
        | MethodBody.Il body ->
            let lines =
                [
                    yield $"signature static=%b{method.IsStatic} %s{signatureText assembly method.Signature}"
                    yield $"localsinit=%b{body.LocalsInit}"

                    match body.LocalVars with
                    | None -> ()
                    | Some locals ->
                        for local in locals do
                            yield $"local %s{typeText assembly local}"

                    for op, offset in body.Instructions do
                        yield $"%s{invariant offset} %s{renderOp assembly op}"

                    for region in body.ExceptionRegions do
                        yield renderRegion assembly region
                ]

            Some (String.concat "\n" lines)
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> None

    /// `method`'s IL body fingerprint, or `None` when it has no IL body.
    let ofMethod
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : IlBodyFingerprint option
        =
        canonicalText assembly method
        |> Option.map (fun text ->
            let hash = SHA256.HashData (Encoding.UTF8.GetBytes text)
            Convert.ToHexString(hash, 0, 8).ToLowerInvariant () |> IlBodyFingerprint.OfHex
        )
