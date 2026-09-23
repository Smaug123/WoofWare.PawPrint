namespace WoofWare.PawPrint

open System
open System.Globalization
open System.Security.Cryptography
open System.Text

/// The identity of a method's IL body: sixteen lowercase hex digits of the SHA-256 of
/// `IlBodyFingerprint.canonicalText`.
///
/// A body with different instructions, operands, locals, exception regions or signature has a
/// different fingerprint (up to the hash's collisions): what is hashed is an unambiguous
/// serialisation that loses nothing. Metadata tokens are serialised by what they name — type and
/// member names, full signatures, type kinds, the assembly a type reference resolves to — rather
/// than by their row numbers, so a rebuild of the same source that renumbers the image's tables
/// keeps the fingerprint.
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

    // The canonical text is an S-expression, so that it parses one way only: every composite is a
    // parenthesised list headed by a fixed tag, every name is `x` followed by its UTF-16 code
    // units in hex, and every number is `n` followed by its decimal digits. No name or number can
    // then contain a delimiter, and two operands that differ serialise differently; otherwise the
    // gate could admit a body its row never reviewed. Metadata that names nothing (a row the
    // image does not resolve) serialises by its row number.

    let private node (tag : string) (children : string list) : string =
        "(" + String.concat " " (tag :: children) + ")"

    let private number (i : int64) : string =
        "n" + i.ToString CultureInfo.InvariantCulture

    let private int32Atom (i : int) : string = number (int64 i)

    /// Hex rather than the characters themselves, so that an unpaired surrogate survives the UTF-8
    /// encoding the hash is taken over and no character can be read as a delimiter.
    let private name (s : string) : string =
        "x"
        + (s
           |> Seq.map (fun c -> (int c).ToString ("x4", CultureInfo.InvariantCulture))
           |> String.concat "")

    let private row (handle : System.Reflection.Metadata.EntityHandle) : string =
        node
            "row"
            [
                int32Atom (System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle)
            ]

    let rec private typeDefText
        (assembly : DumpedAssembly)
        (handle : System.Reflection.Metadata.TypeDefinitionHandle)
        : string
        =
        match assembly.TypeDefs.TryGetValue handle with
        | true, typeInfo ->
            let enclosing =
                if typeInfo.DeclaringType.IsNil then
                    node "toplevel" []
                else
                    typeDefText assembly typeInfo.DeclaringType

            node "typedef" [ enclosing ; name typeInfo.Namespace ; name typeInfo.Name ]
        | false, _ -> node "typedef" [ row (System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit handle) ]

    let rec private typeRefText (assembly : DumpedAssembly) (typeRef : TypeRef) : string =
        let scope =
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.Assembly handle ->
                match assembly.AssemblyReferences.TryGetValue handle with
                | true, reference -> node "assembly" [ name reference.Name.Name ]
                | false, _ ->
                    node "assembly" [ row (System.Reflection.Metadata.AssemblyReferenceHandle.op_Implicit handle) ]
            | TypeRefResolutionScope.TypeRef parent ->
                match assembly.TypeRefs.TryGetValue parent with
                | true, parent -> node "nested" [ typeRefText assembly parent ]
                | false, _ -> node "nested" [ row (System.Reflection.Metadata.TypeReferenceHandle.op_Implicit parent) ]
            | TypeRefResolutionScope.ModuleDef _ -> node "thismodule" []
            | TypeRefResolutionScope.ModuleRef handle ->
                node "moduleref" [ row (System.Reflection.Metadata.ModuleReferenceHandle.op_Implicit handle) ]

        node "typeref" [ scope ; name typeRef.Namespace ; name typeRef.Name ]

    /// Generic parameters serialise by position, which is what a signature records.
    let rec private typeText (assembly : DumpedAssembly) (typeDefn : TypeDefn) : string =
        let recurse = typeText assembly

        match typeDefn with
        | TypeDefn.PrimitiveType primitive -> node "primitive" [ $"%O{primitive}" ]
        | TypeDefn.Array (element, rank) -> node "array" [ recurse element ; int32Atom rank ]
        | TypeDefn.Pinned inner -> node "pinned" [ recurse inner ]
        | TypeDefn.Pointer inner -> node "pointer" [ recurse inner ]
        | TypeDefn.Byref inner -> node "byref" [ recurse inner ]
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> node "szarray" [ recurse element ]
        | TypeDefn.Modified modified ->
            let kind = if modified.IsRequired then "modreq" else "modopt"
            node kind [ recurse modified.Unmodified ; recurse modified.Modifier ]
        | TypeDefn.FromReference (typeRef, kind) -> node "named" [ $"%O{kind}" ; typeRefText assembly typeRef ]
        | TypeDefn.FromDefinition (identity, kind) ->
            let definition =
                if identity.AssemblyFullName = assembly.DefinitionFullName then
                    typeDefText assembly identity.TypeDefinition.Get
                else
                    node
                        "foreigndef"
                        [
                            name identity.AssemblyFullName
                            row (
                                System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit identity.TypeDefinition.Get
                            )
                        ]

            node "named" [ $"%O{kind}" ; definition ]
        | TypeDefn.GenericInstantiation (generic, args) ->
            node "instantiate" (recurse generic :: (args |> Seq.map recurse |> List.ofSeq))
        | TypeDefn.FunctionPointer signature -> node "fnptr" [ signatureText assembly signature ]
        | TypeDefn.GenericTypeParameter index -> node "typevar" [ int32Atom index ]
        | TypeDefn.GenericMethodParameter index -> node "methodvar" [ int32Atom index ]
        | TypeDefn.Void -> node "void" []

    and private signatureText (assembly : DumpedAssembly) (signature : TypeMethodSignature<TypeDefn>) : string =
        let returns =
            match signature.ReturnType with
            | MethodReturnType.Void -> node "void" []
            | MethodReturnType.Returns returns -> typeText assembly returns

        node
            "signature"
            [
                int32Atom (int signature.Header.Get.RawValue)
                int32Atom signature.GenericParameterCount
                int32Atom signature.RequiredParameterCount
                node "parameters" (signature.ParameterTypes |> List.map (typeText assembly))
                node "returns" [ returns ]
            ]

    let rec private tokenText (assembly : DumpedAssembly) (token : MetadataToken) : string =
        match token with
        | MetadataToken.TypeDefinition handle -> typeDefText assembly handle
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, typeRef -> typeRefText assembly typeRef
            | false, _ -> node "typeref" [ row (System.Reflection.Metadata.TypeReferenceHandle.op_Implicit handle) ]
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, spec -> node "typespec" [ typeText assembly spec.Signature ]
            | false, _ ->
                node "typespec" [ row (System.Reflection.Metadata.TypeSpecificationHandle.op_Implicit handle) ]
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, field ->
                node
                    "field"
                    [
                        typeDefText assembly field.DeclaringType.Identity.TypeDefinition.Get
                        name field.Name
                        typeText assembly field.Signature
                    ]
            | false, _ -> node "field" [ row (System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit handle) ]
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, callee ->
                node
                    "method"
                    [
                        typeDefText assembly callee.RequiredDeclaringType.Definition.Get
                        name callee.Name
                        signatureText assembly callee.Signature
                    ]
            | false, _ -> node "method" [ row (System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle) ]
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, reference ->
                let signature =
                    match reference.Signature with
                    | MemberSignature.Method signature -> signatureText assembly signature
                    | MemberSignature.Field fieldType -> node "fieldtype" [ typeText assembly fieldType ]

                node
                    "memberref"
                    [
                        tokenText assembly reference.Parent
                        name (assembly.Strings reference.Name)
                        signature
                    ]
            | false, _ -> node "memberref" [ row (System.Reflection.Metadata.MemberReferenceHandle.op_Implicit handle) ]
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                node
                    "methodspec"
                    (tokenText assembly spec.Method
                     :: (spec.Signature |> Seq.map (typeText assembly) |> List.ofSeq))
            | false, _ ->
                node
                    "methodspec"
                    [
                        row (System.Reflection.Metadata.MethodSpecificationHandle.op_Implicit handle)
                    ]
        | MetadataToken.StandaloneSignature handle ->
            // Decoded as `calli` decodes it, so each type it names serialises by name.
            let metadata : System.Reflection.Metadata.MetadataReader =
                System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

            let signature =
                (metadata.GetStandaloneSignature handle).DecodeMethodSignature (TypeDefn.typeProvider assembly.Name, ())
                |> TypeMethodSignature.make

            node "standalone" [ signatureText assembly signature ]
        | other -> node "token" [ node "row" [ int32Atom (MetadataToken.toInt other) ] ]

    let private renderOp (assembly : DumpedAssembly) (op : IlOp) : string =
        match op with
        | IlOp.Nullary op -> node "nullary" [ op.ToString () ]
        // `UnaryConstIlOp.ToString` is the case name, a space and the operand, but prints a
        // floating-point operand with `%f`, which keeps six decimal places; the bit pattern is the
        // operand itself.
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 f) ->
            node "const" [ "Ldc_R4" ; int32Atom (BitConverter.SingleToInt32Bits f) ]
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 f) ->
            node "const" [ "Ldc_R8" ; number (BitConverter.DoubleToInt64Bits f) ]
        | IlOp.UnaryConst op ->
            match (op.ToString ()).Split ' ' with
            | [| case ; operand |] -> node "const" [ case ; "n" + operand ]
            | _ -> failwith $"IlBodyFingerprint: %O{op} does not render as a case name and one operand"
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata token) ->
            node "token" [ $"%O{op}" ; tokenText assembly token.Token ]
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope index) ->
            node "token" [ $"%O{op}" ; node "dynamicscope" [ int32Atom index ] ]
        | IlOp.UnaryStringToken (op, StringOperand.FromMetadata token) ->
            node "string" [ $"%O{op}" ; name (assembly.Strings token.Token) ]
        | IlOp.UnaryStringToken (op, StringOperand.FromDynamicScope index) ->
            node "string" [ $"%O{op}" ; node "dynamicscope" [ int32Atom index ] ]
        | IlOp.Switch targets -> node "switch" (targets |> Seq.map int32Atom |> List.ofSeq)

    let private renderRegion (assembly : DumpedAssembly) (region : ExceptionRegion) : string =
        let offsets (o : ExceptionOffset) : string list =
            [
                int32Atom o.TryOffset
                int32Atom o.TryLength
                int32Atom o.HandlerOffset
                int32Atom o.HandlerLength
            ]

        match region with
        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) ->
            node "catch" (tokenText assembly token :: offsets o)
        | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope index, o) ->
            node "catch" (node "dynamicscope" [ int32Atom index ] :: offsets o)
        | ExceptionRegion.Filter (filterOffset, o) -> node "filter" (int32Atom filterOffset :: offsets o)
        | ExceptionRegion.Finally o -> node "finally" (offsets o)
        | ExceptionRegion.Fault o -> node "fault" (offsets o)

    /// The text the fingerprint hashes: `method`'s signature, locals, instructions (each at its
    /// offset) and exception regions, every token serialised by what it names. `None` when the
    /// method has no IL body.
    ///
    /// Exposed so that a mismatch can be diagnosed by comparing two renderings.
    let canonicalText
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string option
        =
        match method.Body with
        | MethodBody.Il body ->
            let locals =
                match body.LocalVars with
                | None -> node "nolocals" []
                | Some locals -> node "locals" (locals |> Seq.map (typeText assembly) |> List.ofSeq)

            node
                "body"
                [
                    node "static" [ $"%b{method.IsStatic}" ]
                    signatureText assembly method.Signature
                    node "localsinit" [ $"%b{body.LocalsInit}" ]
                    locals
                    node
                        "instructions"
                        (body.Instructions
                         |> List.map (fun (op, offset) -> node "at" [ int32Atom offset ; renderOp assembly op ]))
                    node "regions" (body.ExceptionRegions |> Seq.map (renderRegion assembly) |> List.ofSeq)
                ]
            |> Some
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
