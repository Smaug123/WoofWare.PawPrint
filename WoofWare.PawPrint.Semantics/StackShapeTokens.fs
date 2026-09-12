namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// The generic arguments a token supplies for the generic parameters of what it names: a
/// `MemberRef` whose parent is an instantiated type supplies the type's, a `MethodSpec` supplies
/// the method's. Each argument is a type in the *caller's* context, so an argument that is itself
/// a generic parameter is the caller's own and is read through the caller's instantiation.
type GenericSubstitution =
    {
        TypeArguments : ImmutableArray<TypeDefn> option
        MethodArguments : ImmutableArray<TypeDefn> option
    }

    /// No arguments: a generic parameter is the caller's own.
    static member None : GenericSubstitution =
        {
            TypeArguments = None
            MethodArguments = None
        }

/// The shapes of the generic parameters of the method being analysed, from its instantiation:
/// what `!n` and `!!n` mean where no token binds them. CoreCLR compiles each value-type
/// instantiation of a generic method separately, so an argument of type `T` is a float32 in
/// `M<float>` and the analysis sees it as one.
type GenericBinding =
    {
        TypeParameter : int -> SlotShape
        MethodParameter : int -> SlotShape
    }

    /// Every parameter is `Other`: the analysis of a body with no instantiation to consult.
    static member AtDefinition : GenericBinding =
        {
            TypeParameter = fun _ -> SlotShape.Other
            MethodParameter = fun _ -> SlotShape.Other
        }

/// The stack effects of a body's token-bearing instructions, read from the metadata of the module
/// that owns the body. Every signature a token leads to is a blob in that same module, so nothing
/// here loads or resolves anything: a `MemberRef` into another assembly still carries its own
/// signature blob, and a `TypeRef` named as a primitive is judged by its name and the assembly
/// its resolution scope names.
[<RequireQualifiedAccess>]
module StackShapeTokens =

    let private isSingleOrDouble (ns : string) (name : string) : SlotShape option =
        if ns = "System" && name = "Single" then
            Some (SlotShape.Float FloatWidth.Single)
        elif ns = "System" && name = "Double" then
            Some (SlotShape.Float FloatWidth.Double)
        else
            None

    /// The assemblies a reference to `System.Single` or `System.Double` resolves into CoreLib's
    /// type from: CoreLib itself, and the facades that forward the primitives. A guest is free to
    /// declare its own `System.Single`, and to the JIT that is a struct; its references to it
    /// name the guest's assembly.
    let private frameworkAssemblies : Set<string> =
        Set.ofList [ "System.Private.CoreLib" ; "System.Runtime" ; "mscorlib" ; "netstandard" ]

    /// The shape a type reference names: a float only when named as the primitive and scoped to
    /// a framework assembly.
    let private shapeOfTypeRef (assembly : DumpedAssembly) (typeRef : TypeRef) : SlotShape =
        match isSingleOrDouble typeRef.Namespace typeRef.Name with
        | None -> SlotShape.Other
        | Some shape ->
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.Assembly handle ->
                match assembly.AssemblyReferences.TryGetValue handle with
                | true, reference when frameworkAssemblies.Contains reference.Name.Name -> shape
                | _ -> SlotShape.Other
            | TypeRefResolutionScope.ModuleDef _
            | TypeRefResolutionScope.ModuleRef _
            | TypeRefResolutionScope.TypeRef _ -> SlotShape.Other

    /// The shape of a value of this type, with a generic parameter's shape supplied by the
    /// caller: `typeParameter` for `!n` and `methodParameter` for `!!n`. A `TypeRef` named
    /// `System.Single` or `System.Double` is a float only when scoped to a framework assembly,
    /// and a `TypeDef` of that name only in CoreLib: a guest is free to declare its own
    /// `System.Single`, and to the JIT that is a struct.
    let rec shapeOfTypeDefnBinding
        (assembly : DumpedAssembly)
        (typeParameter : int -> SlotShape)
        (methodParameter : int -> SlotShape)
        (ty : TypeDefn)
        : SlotShape
        =
        match ty with
        | TypeDefn.PrimitiveType PrimitiveType.Single -> SlotShape.Float FloatWidth.Single
        | TypeDefn.PrimitiveType PrimitiveType.Double -> SlotShape.Float FloatWidth.Double
        | TypeDefn.PrimitiveType _ -> SlotShape.Other
        | TypeDefn.Modified modified ->
            shapeOfTypeDefnBinding assembly typeParameter methodParameter modified.Unmodified
        | TypeDefn.Pinned inner -> shapeOfTypeDefnBinding assembly typeParameter methodParameter inner
        | TypeDefn.GenericTypeParameter index -> typeParameter index
        | TypeDefn.GenericMethodParameter index -> methodParameter index
        | TypeDefn.FromReference (typeRef, _) -> shapeOfTypeRef assembly typeRef
        | TypeDefn.FromDefinition (identity, _) ->
            if
                identity.AssemblyFullName = assembly.DefinitionFullName
                && AssemblyDefinitionName.isNamed "System.Private.CoreLib" assembly.DefinitionFullName
            then
                match assembly.TypeDefs.TryGetValue identity.TypeDefinition.Get with
                | true, typeInfo ->
                    isSingleOrDouble typeInfo.Namespace typeInfo.Name
                    |> Option.defaultValue SlotShape.Other
                | false, _ -> SlotShape.Other
            else
                SlotShape.Other
        | TypeDefn.Array _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.GenericInstantiation _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> SlotShape.Other

    /// `shapeOfTypeDefnBinding` under the generic arguments a token supplies, and then the
    /// caller's own instantiation: a parameter the substitution does not bind is the caller's,
    /// and an argument is a type in the caller's context, so both are read through `binding`.
    let shapeOfTypeDefnWith
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (substitution : GenericSubstitution)
        (ty : TypeDefn)
        : SlotShape
        =
        let inCaller (argument : TypeDefn) : SlotShape =
            shapeOfTypeDefnBinding assembly binding.TypeParameter binding.MethodParameter argument

        let bind (arguments : ImmutableArray<TypeDefn> option) (fallback : int -> SlotShape) (index : int) : SlotShape =
            match arguments with
            | Some arguments when index >= 0 && index < arguments.Length -> inCaller arguments.[index]
            | _ -> fallback index

        shapeOfTypeDefnBinding
            assembly
            (bind substitution.TypeArguments binding.TypeParameter)
            (bind substitution.MethodArguments binding.MethodParameter)
            ty

    /// `shapeOfTypeDefnWith` for a type no token supplies arguments for: an argument, local or
    /// return of the method itself.
    let shapeOfTypeDefn (assembly : DumpedAssembly) (binding : GenericBinding) (ty : TypeDefn) : SlotShape =
        shapeOfTypeDefnWith assembly binding GenericSubstitution.None ty

    /// Whether the type is `void` under any custom modifiers: an init-only setter returns
    /// `void modreq(IsExternalInit)`, which the signature decoder reports as a modified type
    /// rather than as `MethodReturnType.Void`, and a `ret` from it pops nothing.
    let rec private isVoid (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.Void -> true
        | TypeDefn.Modified modified -> isVoid modified.Unmodified
        | TypeDefn.Pinned inner -> isVoid inner
        | _ -> false

    /// What a call through this signature leaves on the stack: nothing for a void return, under
    /// any custom modifiers, and otherwise the shape of the returned value.
    let returnShape
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (substitution : GenericSubstitution)
        (returnType : MethodReturnType<TypeDefn>)
        : SlotShape option
        =
        match returnType with
        | MethodReturnType.Void -> None
        | MethodReturnType.Returns ty ->
            if isVoid ty then
                None
            else
                Some (shapeOfTypeDefnWith assembly binding substitution ty)

    /// What a call through this signature takes from the stack and leaves on it, under the
    /// generic arguments the call site supplies.
    let calleeShape
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (substitution : GenericSubstitution)
        (signature : TypeMethodSignature<TypeDefn>)
        : TokenShape
        =
        // An explicit `this` (ECMA-335 II.15.3) is already the first parameter type, so only an
        // implicit one adds a slot.
        let header = signature.Header.Get

        let arguments =
            signature.ParameterTypes.Length
            + (if header.IsInstance && not header.HasExplicitThis then
                   1
               else
                   0)

        TokenShape.Callee (arguments, returnShape assembly binding substitution signature.ReturnType)

    /// The type arguments a member reference's parent supplies: those of an instantiated type,
    /// and none for a type definition or reference.
    let private parentTypeArguments
        (assembly : DumpedAssembly)
        (parent : MetadataToken)
        : ImmutableArray<TypeDefn> option
        =
        match parent with
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, spec ->
                match spec.Signature with
                | TypeDefn.GenericInstantiation (_, arguments) -> Some arguments
                | _ -> None
            | false, _ -> None
        | _ -> None

    /// The method signature a call token leads to and the generic arguments it supplies, or
    /// `None` for a token that does not name one: a field, a type, or a row the table does not
    /// have. Each is invalid IL, which the instruction refuses if it executes, and no claim is
    /// made about it ahead of that. The signatures here were decoded when the assembly was read,
    /// so nothing is decoded now.
    let rec private methodSignature
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : (TypeMethodSignature<TypeDefn> * GenericSubstitution) option
        =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, definition -> Some (definition.Signature, GenericSubstitution.None)
            | false, _ -> None
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, reference ->
                match reference.Signature with
                | MemberSignature.Method signature ->
                    Some (
                        signature,
                        { GenericSubstitution.None with
                            TypeArguments = parentTypeArguments assembly reference.Parent
                        }
                    )
                | MemberSignature.Field _ -> None
            | false, _ -> None
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                methodSignature assembly spec.Method
                |> Option.map (fun (signature, substitution) ->
                    signature,
                    { substitution with
                        MethodArguments = Some spec.Signature
                    }
                )
            | false, _ -> None
        | _ -> None

    /// The type a type-position token names, or `None` for a token that names no type.
    let private typeOfToken (assembly : DumpedAssembly) (token : MetadataToken) : TypeDefn option =
        match token with
        | MetadataToken.TypeDefinition handle ->
            match assembly.TypeDefs.TryGetValue handle with
            | true, typeInfo -> Some (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Unknown))
            | false, _ -> None
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, typeRef -> Some (TypeDefn.FromReference (typeRef, SignatureTypeKind.Unknown))
            | false, _ -> None
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, spec -> Some spec.Signature
            | false, _ -> None
        | _ -> None

    /// What a `calli` through this method-signature blob takes from the stack and leaves on it,
    /// read from the blob's header, its parameter count and its return type's leading code alone:
    /// a float's width, a generic parameter's binding, a type token's name and scope, `void`
    /// under any custom modifiers, and `Other` for anything else, without decoding a type. The
    /// parameter types may use an encoding the type provider refuses, and a call that never
    /// executes must not be refused for that. `None` for a blob that is not a method signature
    /// or that ends early.
    let calliShapeOfBlob
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (blob : BlobReader)
        : TokenShape option
        =
        let mutable blob = blob

        // The metadata reader answers a blob that ends early with an exception; that is a token
        // no call can execute through.
        try
            let header = blob.ReadSignatureHeader ()

            if header.Kind <> SignatureKind.Method then
                None
            else
                if header.IsGeneric then
                    blob.ReadCompressedInteger () |> ignore

                let parameters = blob.ReadCompressedInteger ()

                // The reader reports a type code it cannot read, a blob that ends early included,
                // as `Invalid` rather than by throwing.
                let rec returns () : Result<SlotShape option, unit> =
                    match blob.ReadSignatureTypeCode () with
                    | SignatureTypeCode.Invalid -> Error ()
                    | SignatureTypeCode.OptionalModifier
                    | SignatureTypeCode.RequiredModifier ->
                        blob.ReadTypeHandle () |> ignore
                        returns ()
                    | SignatureTypeCode.Void -> Ok None
                    | SignatureTypeCode.Single -> Ok (Some (SlotShape.Float FloatWidth.Single))
                    | SignatureTypeCode.Double -> Ok (Some (SlotShape.Float FloatWidth.Double))
                    | SignatureTypeCode.GenericTypeParameter ->
                        Ok (Some (binding.TypeParameter (blob.ReadCompressedInteger ())))
                    | SignatureTypeCode.GenericMethodParameter ->
                        Ok (Some (binding.MethodParameter (blob.ReadCompressedInteger ())))
                    | SignatureTypeCode.TypeHandle ->
                        // `valuetype System.Single` names the primitive by token rather than by
                        // code; the name and scope decide, as they do for any other type.
                        let handle : EntityHandle = blob.ReadTypeHandle ()

                        typeOfToken assembly (MetadataToken.ofInt (Ecma335.MetadataTokens.GetToken handle))
                        |> Option.map (shapeOfTypeDefn assembly binding)
                        |> Option.defaultValue SlotShape.Other
                        |> Some
                        |> Ok
                    | _ -> Ok (Some SlotShape.Other)

                match returns () with
                | Error () -> None
                | Ok returns ->

                // An explicit `this` (ECMA-335 II.15.3) is already the first parameter type, so
                // only an implicit one adds a slot.
                let arguments =
                    parameters
                    + (if header.IsInstance && not header.HasExplicitThis then
                           1
                       else
                           0)

                Some (TokenShape.Callee (arguments, returns))
        with
        | :? System.BadImageFormatException
        | :? System.ArgumentOutOfRangeException -> None

    /// `calliShapeOfBlob` for the standalone signature a `calli` token names; `None` also for a
    /// row the table lacks.
    let private calliShape
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (handle : StandaloneSignatureHandle)
        : TokenShape option
        =
        let reader = assembly.PeReader.GetMetadataReader ()

        try
            calliShapeOfBlob assembly binding (reader.GetBlobReader (reader.GetStandaloneSignature(handle).Signature))
        with
        | :? System.BadImageFormatException
        | :? System.ArgumentOutOfRangeException -> None

    /// The type of the field a token names and the generic arguments the token supplies, or
    /// `None` for a token that names no field.
    let private fieldType
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : (TypeDefn * GenericSubstitution) option
        =
        match token with
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, field -> Some (field.Signature, GenericSubstitution.None)
            | false, _ -> None
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, reference ->
                match reference.Signature with
                | MemberSignature.Field ty ->
                    Some (
                        ty,
                        { GenericSubstitution.None with
                            TypeArguments = parentTypeArguments assembly reference.Parent
                        }
                    )
                | MemberSignature.Method _ -> None
            | false, _ -> None
        | _ -> None

    /// The effect of one token-bearing instruction, or `None` for an opcode whose effect does not
    /// depend on its token or a token that does not name what the opcode needs.
    let ofMetadataToken
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        : TokenShape option
        =
        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Calli ->
            match token with
            | MetadataToken.StandaloneSignature handle -> calliShape assembly binding handle
            | _ ->
                methodSignature assembly token
                |> Option.map (fun (signature, substitution) -> calleeShape assembly binding substitution signature)
        | UnaryMetadataTokenIlOp.Newobj ->
            // The constructor's `this` is the object being made, not an argument on the stack.
            methodSignature assembly token
            |> Option.map (fun (signature, _) -> TokenShape.Callee (signature.ParameterTypes.Length, None))
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldsfld ->
            fieldType assembly token
            |> Option.map (fun (ty, substitution) ->
                TokenShape.Field (shapeOfTypeDefnWith assembly binding substitution ty)
            )
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Ldelem ->
            typeOfToken assembly token
            |> Option.map (fun ty -> TokenShape.Type (shapeOfTypeDefn assembly binding ty))
        | UnaryMetadataTokenIlOp.Jmp
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

    /// Whether the assembly asks for debuggable code: a `DebuggableAttribute` with
    /// `DisableOptimizations` under `Default` (what a compiler stamps on a Debug build), which has
    /// the VM compile every method of the assembly, dynamic methods it hosts included, with
    /// `CORJIT_FLAG_DEBUG_CODE`: no constant folding and no early block merging.
    let private optimisationsDisabled (assembly : DumpedAssembly) : bool =
        let assemblyDefinitionToken = 0x20000001

        let isDebuggableAttribute (ns : string) (name : string) : bool =
            ns = "System.Diagnostics" && name = "DebuggableAttribute"

        let disablesOptimizations (attribute : WoofWare.PawPrint.CustomAttribute) : bool =
            let isDebuggable =
                match attribute.Constructor with
                | MetadataToken.MemberReference handle ->
                    match assembly.Members.TryGetValue handle with
                    | true, reference ->
                        match reference.Parent with
                        | MetadataToken.TypeReference typeRef ->
                            match assembly.TypeRefs.TryGetValue typeRef with
                            | true, typeRef -> isDebuggableAttribute typeRef.Namespace typeRef.Name
                            | false, _ -> false
                        | _ -> false
                    | false, _ -> false
                | MetadataToken.MethodDef handle ->
                    // CoreLib declares the attribute itself, so its own stamp names a MethodDef.
                    let reader = assembly.PeReader.GetMetadataReader ()

                    match assembly.TypeDefs.TryGetValue (reader.GetMethodDefinition(handle).GetDeclaringType ()) with
                    | true, typeInfo -> isDebuggableAttribute typeInfo.Namespace typeInfo.Name
                    | false, _ -> false
                | _ -> false

            // ECMA-335 II.23.3: a prolog of 0x0001, the fixed arguments, then the count of named
            // arguments. The attribute has two constructors: `(DebuggingModes)`, an int32 whose
            // 0x001 bit is `Default` (JIT tracking) and 0x100 `DisableOptimizations`, and
            // `(bool isJITTrackingEnabled, bool isJITOptimizerDisabled)`. The VM honours the
            // disable bit only under the tracking bit (`Assembly::GetDebuggingCustomAttributes`).
            let blob = attribute.Value

            isDebuggable
            && blob.Length >= 2
            && blob.[0] = 1uy
            && blob.[1] = 0uy
            && (
                match blob.Length with
                | 8 -> (int blob.[2] &&& 0x01) <> 0 && (int blob.[3] &&& 0x01) <> 0
                | 6 -> blob.[2] <> 0uy && blob.[3] <> 0uy
                | _ -> false
            )

        match assembly.CustomAttributesByParentToken.TryGetValue assemblyDefinitionToken with
        | false, _ -> false
        | true, attributes ->
            attributes
            |> Seq.exists (fun tokenInt ->
                match MetadataToken.ofInt tokenInt with
                | MetadataToken.CustomAttribute handle ->
                    match assembly.Attributes.TryGetValue handle with
                    | true, attribute -> disablesOptimizations attribute
                    | false, _ -> false
                | _ -> false
            )

    /// How the JIT compiles a method of this assembly: with no optimisation if the assembly asks
    /// for debuggable code or the method is marked `NoOptimization`; fully optimised from the
    /// start if it is marked `AggressiveOptimization`; and at Tier-0 otherwise.
    let compilationModeOf (assembly : DumpedAssembly) (handle : MethodDefinitionHandle) : CompilationMode =
        let implementation =
            assembly.PeReader.GetMetadataReader().GetMethodDefinition(handle).ImplAttributes

        if
            optimisationsDisabled assembly
            || implementation.HasFlag System.Reflection.MethodImplAttributes.NoOptimization
        then
            CompilationMode.Unoptimised
        elif implementation.HasFlag System.Reflection.MethodImplAttributes.AggressiveOptimization then
            CompilationMode.FullyOptimised
        else
            CompilationMode.Tier0

    /// How the JIT compiles a dynamic method whose scope is this assembly's module: with no
    /// optimisation if the assembly asks for debuggable code, and fully optimised otherwise,
    /// since tiering never touches a dynamic method.
    let dynamicCompilationModeOf (assembly : DumpedAssembly) : CompilationMode =
        if optimisationsDisabled assembly then
            CompilationMode.Unoptimised
        else
            CompilationMode.FullyOptimised

    /// The token effects of the instructions at `offsets` in a body whose operands are all
    /// metadata tokens of `assembly`, under the instantiation `binding`. Only what control can
    /// reach need be read (`StackShape.reachable`): CoreCLR's importer reads only what it
    /// imports, and a token on dead code may name what cannot be resolved. A body minted by
    /// `Reflection.Emit` has `DynamicScope` operands instead, which only something holding the
    /// scope can read.
    let ofBody
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (offsets : Set<int>)
        (body : MethodInstructions<'methodVars>)
        : Map<int, TokenShape>
        =
        body.Instructions
        |> List.choose (fun (instruction, offset) ->
            match instruction with
            | _ when not (offsets.Contains offset) -> None
            | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata sourced) ->
                ofMetadataToken assembly binding op sourced.Token
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
