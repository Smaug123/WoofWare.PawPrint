namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// What CoreCLR executes when it calls a method it treats as a JIT intrinsic.
///
/// `[Intrinsic]` permits the JIT to replace a call with code of its own, and it usually does so
/// only as an optimisation. Under MinOpts it expands nothing it is not obliged to
/// (`impIntrinsic`, importercalls.cpp), and otherwise emits an ordinary call to the method's IL;
/// so for most intrinsics the IL is a complete implementation. There are two exceptions, and each
/// is a placeholder body that must not be run as it stands.
[<RequireQualifiedAccess>]
type IntrinsicBody =
    /// The method's own IL is its semantics.
    | OwnIl
    /// The IL calls the method itself, non-virtually. By CoreCLR's convention that call is a
    /// placeholder the JIT must expand ("The recursive non-virtual calls to Jit intrinsics are
    /// must-expand by convention", importercalls.cpp), so interpreting the IL recurses without end.
    | JitExpansion
    /// A CoreLib method whose IL cannot return, declared on one of the four classes for which
    /// CoreCLR's VM substitutes IL of its own (`getILIntrinsicImplementationFor*`,
    /// jitinterface.cpp): the body CoreLib ships throws `PlatformNotSupportedException` in place of
    /// the one the runtime supplies.
    | VmSubstitution
    /// The method has no IL: an InternalCall, a P/Invoke, a runtime-provided or an abstract body.
    | NoIl

[<RequireQualifiedAccess>]
module IntrinsicBody =

    let private corelib : string = "System.Private.CoreLib"

    /// The classes whose intrinsics CoreCLR's VM may give a body of its own
    /// (`getMethodInfoWorker`, jitinterface.cpp).
    let private vmSubstitutingClasses : Set<string * string> =
        Set.ofList
            [
                "System.Runtime.CompilerServices", "Unsafe"
                "System.Runtime.CompilerServices", "RuntimeHelpers"
                "System.Threading", "Interlocked"
                "System", "Activator"
            ]

    let private getMemberRefParentType (assembly : DumpedAssembly) (handle : MemberReferenceHandle) : TypeRef =
        match assembly.Members.[handle].Parent with
        | MetadataToken.TypeReference r -> assembly.TypeRefs.[r]
        | other -> failwith $"IntrinsicBody: an attribute constructor's parent %O{other} is not a TypeRef"

    /// Whether `method` is a JIT intrinsic: it carries `[Intrinsic]` itself, or its declaring type
    /// does, as the hardware-intrinsic classes do.
    ///
    /// Broader than CoreCLR, which honours a type-level `[Intrinsic]` only on the hardware-intrinsic
    /// classes (`fIsHardwareIntrinsic`, methodtablebuilder.cpp) and the attribute only in CoreLib.
    /// Over-including is harmless to a caller that runs `OwnIl` as it stands.
    let isIntrinsic (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        MethodInfo.isJITIntrinsic (getMemberRefParentType assembly) assembly.Methods definition
        || MethodInfo.hasIntrinsicAttribute (getMemberRefParentType assembly) assembly.Methods declaringType.Attributes

    /// The MethodDef a call token names, when that MethodDef is in `assembly`: directly, through a
    /// MethodSpec, or through a MemberRef whose parent is one of `assembly`'s own types or an
    /// instantiation of one, matched by name and signature.
    let rec private namedDefinition
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : MethodDefinitionHandle option
        =
        match token with
        | MetadataToken.MethodDef handle -> Some handle
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec -> namedDefinition assembly spec.Method
            | false, _ -> None
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | false, _ -> None
            | true, reference ->

            let parent =
                match reference.Parent with
                | MetadataToken.TypeDefinition handle -> Some handle
                | MetadataToken.TypeSpecification handle ->
                    match assembly.TypeSpecs.TryGetValue handle with
                    | true, spec ->
                        match spec.Signature with
                        | TypeDefn.GenericInstantiation (TypeDefn.FromDefinition (identity, _), _) when
                            identity.AssemblyFullName = assembly.ThisAssemblyDefinition.FullName
                            ->
                            Some identity.TypeDefinition.Get
                        | _ -> None
                    | false, _ -> None
                | _ -> None

            match parent, reference.Signature with
            | Some parent, MemberSignature.Method signature ->
                let candidates =
                    assembly.TypeDefs.[parent].Methods
                    |> List.filter (fun m -> m.Name = reference.PrettyName && m.Signature = signature)

                match candidates with
                | [ m ] -> m.TryMetadata |> Option.map _.Handle
                | _ -> None
            | _ -> None
        | _ -> None

    /// Whether `body` contains a `call` or `callvirt` naming `method` itself, where `method` is not
    /// virtual: the shape CoreCLR's importer tests with `gtIsRecursiveCall`.
    let private callsItself
        (assembly : DumpedAssembly)
        (method : MethodDefinitionHandle)
        (body : MethodInstructions<TypeDefn>)
        : bool
        =
        if assembly.Methods.[method].IsVirtual then
            false
        else
            body.Instructions
            |> List.exists (fun (op, _) ->
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata token)
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, MetadataOperand.FromMetadata token) ->
                    namedDefinition assembly token.Token = Some method
                | _ -> false
            )

    let private canReturn (body : MethodInstructions<TypeDefn>) : bool =
        StackShape.reachable body
        |> Set.exists (fun offset ->
            match body.Locations.[offset] with
            | IlOp.Nullary NullaryIlOp.Ret -> true
            | _ -> false
        )

    /// What CoreCLR executes for the intrinsic `method`, defined in `assembly`. Meaningful only for
    /// a method `isIntrinsic` accepts: for any other, CoreCLR runs the IL whatever it contains.
    let classify (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : IntrinsicBody =
        let definition = assembly.Methods.[method]

        match definition.Body with
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> IntrinsicBody.NoIl
        | MethodBody.Il body ->
            if callsItself assembly method body then
                IntrinsicBody.JitExpansion
            else

            let declaringType =
                assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

            if
                assembly.ThisAssemblyDefinition.Name.Name = corelib
                && vmSubstitutingClasses.Contains (declaringType.Namespace, declaringType.Name)
                && not (canReturn body)
            then
                IntrinsicBody.VmSubstitution
            else
                IntrinsicBody.OwnIl
