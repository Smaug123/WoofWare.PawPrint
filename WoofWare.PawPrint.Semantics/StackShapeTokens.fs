namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// The stack effects of a body's token-bearing instructions, read from the metadata of the module
/// that owns the body. Every signature a token leads to is a blob in that same module, so nothing
/// here loads or resolves anything: a `MemberRef` into another assembly still carries its own
/// signature blob.
[<RequireQualifiedAccess>]
module StackShapeTokens =

    /// Whether the type is `void` under any custom modifiers: an init-only setter returns
    /// `void modreq(IsExternalInit)`, which the signature decoder reports as a modified type
    /// rather than as `MethodReturnType.Void`, and a `ret` from it pops nothing.
    let rec private isVoid (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.Void -> true
        | TypeDefn.Modified modified -> isVoid modified.Unmodified
        | TypeDefn.Pinned inner -> isVoid inner
        | _ -> false

    /// Whether a call through this signature leaves a value on the stack: not for a void return,
    /// under any custom modifiers.
    let returnsValue (returnType : MethodReturnType<TypeDefn>) : bool =
        match returnType with
        | MethodReturnType.Void -> false
        | MethodReturnType.Returns ty -> not (isVoid ty)

    /// What a call through this signature takes from the stack and leaves on it.
    let calleeShape (signature : TypeMethodSignature<TypeDefn>) : TokenShape =
        // An explicit `this` (ECMA-335 II.15.3) is already the first parameter type, so only an
        // implicit one adds a slot.
        let header = signature.Header.Get

        let arguments =
            signature.ParameterTypes.Length
            + (if header.IsInstance && not header.HasExplicitThis then
                   1
               else
                   0)

        TokenShape.Callee (arguments, returnsValue signature.ReturnType)

    /// The method signature a call token leads to, or `None` for a token that does not name one:
    /// a field, a type, or a row the table does not have. Each is invalid IL, which the
    /// instruction refuses if it executes, and no claim is made about it ahead of that. The
    /// signatures here were decoded when the assembly was read, so nothing is decoded now.
    let rec private methodSignature
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : TypeMethodSignature<TypeDefn> option
        =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, definition -> Some definition.Signature
            | false, _ -> None
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, reference ->
                match reference.Signature with
                | MemberSignature.Method signature -> Some signature
                | MemberSignature.Field _ -> None
            | false, _ -> None
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec -> methodSignature assembly spec.Method
            | false, _ -> None
        | _ -> None

    /// What a `calli` through a standalone signature takes from the stack and leaves on it, read
    /// from the blob's header, its parameter count and whether its return is `void` under any
    /// custom modifiers, and nothing else: the parameter types may use an encoding the type
    /// provider refuses, and a call that never executes must not be refused for that. `None` for
    /// a row the table lacks, a blob that is not a method signature, or one that ends early.
    let private calliShape (assembly : DumpedAssembly) (handle : StandaloneSignatureHandle) : TokenShape option =
        let reader = assembly.PeReader.GetMetadataReader ()

        // The metadata reader answers a row it does not have, or a blob that ends early, with an
        // exception; either is a token no call can execute through.
        try
            let mutable blob =
                reader.GetBlobReader (reader.GetStandaloneSignature(handle).Signature)

            let header = blob.ReadSignatureHeader ()

            if header.Kind <> SignatureKind.Method then
                None
            else
                if header.IsGeneric then
                    blob.ReadCompressedInteger () |> ignore

                let parameters = blob.ReadCompressedInteger ()

                let rec returnsValue () : bool =
                    match blob.ReadSignatureTypeCode () with
                    | SignatureTypeCode.OptionalModifier
                    | SignatureTypeCode.RequiredModifier ->
                        blob.ReadTypeHandle () |> ignore
                        returnsValue ()
                    | SignatureTypeCode.Void -> false
                    | _ -> true

                let returnsValue = returnsValue ()

                // An explicit `this` (ECMA-335 II.15.3) is already the first parameter type, so
                // only an implicit one adds a slot.
                let arguments =
                    parameters
                    + (if header.IsInstance && not header.HasExplicitThis then
                           1
                       else
                           0)

                Some (TokenShape.Callee (arguments, returnsValue))
        with
        | :? System.BadImageFormatException
        | :? System.ArgumentOutOfRangeException -> None

    /// The effect of one token-bearing instruction, or `None` for an opcode whose effect does not
    /// depend on its token or a token that does not name what the opcode needs.
    let ofMetadataToken
        (assembly : DumpedAssembly)
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        : TokenShape option
        =
        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Calli ->
            match token with
            | MetadataToken.StandaloneSignature handle -> calliShape assembly handle
            | _ -> methodSignature assembly token |> Option.map calleeShape
        | UnaryMetadataTokenIlOp.Newobj ->
            // The constructor's `this` is the object being made, not an argument on the stack.
            methodSignature assembly token
            |> Option.map (fun signature -> TokenShape.Callee (signature.ParameterTypes.Length, false))
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> None

    /// The token effects of the instructions at `offsets` in a body whose operands are all
    /// metadata tokens of `assembly`. Only what control can reach need be read
    /// (`StackShape.reachable`): CoreCLR's importer reads only what it imports, and a token on
    /// dead code may name what cannot be resolved. A body minted by `Reflection.Emit` has
    /// `DynamicScope` operands instead, which only something holding the scope can read.
    let ofBody
        (assembly : DumpedAssembly)
        (offsets : Set<int>)
        (body : MethodInstructions<'methodVars>)
        : Map<int, TokenShape>
        =
        body.Instructions
        |> List.choose (fun (instruction, offset) ->
            match instruction with
            | _ when not (offsets.Contains offset) -> None
            | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata sourced) ->
                ofMetadataToken assembly op sourced.Token
                |> Option.map (fun shape -> offset, shape)
            | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope index) ->
                failwith
                    $"stack shape: %O{op} at offset %d{offset} names DynamicScope entry %d{index}, which metadata alone cannot read"
            | IlOp.Nullary _
            | IlOp.UnaryConst _
            | IlOp.UnaryStringToken _
            | IlOp.Switch _ -> None
        )
        |> Map.ofList
