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

    /// The full name of the type a TypeRef names, in `TypeInfo.fullName`'s spelling.
    let rec private typeRefFullName (assembly : DumpedAssembly) (typeRef : TypeRef) : string =
        match typeRef.ResolutionScope with
        | TypeRefResolutionScope.TypeRef parent ->
            $"%s{typeRefFullName assembly assembly.TypeRefs.[parent]}+%s{typeRef.Name}"
        | TypeRefResolutionScope.Assembly _
        | TypeRefResolutionScope.ModuleDef _
        | TypeRefResolutionScope.ModuleRef _ ->
            if System.String.IsNullOrEmpty typeRef.Namespace then
                typeRef.Name
            else
                $"%s{typeRef.Namespace}.%s{typeRef.Name}"

    /// Whether a `call` or `callvirt` token can name `method` itself: the shape CoreCLR's importer
    /// tests with `gtIsRecursiveCall`.
    ///
    /// A parent named by definition (a TypeDef, or an instantiation of this assembly's own type)
    /// is compared exactly, method and signature alike. A parent named through a TypeRef is not
    /// resolvable from this image alone -- its scope may be this assembly, this module, or another
    /// assembly that forwards back here -- so it is compared by the type's full name, the method's
    /// name and its arity. That can mistake a same-named method on a same-named type elsewhere for
    /// a self-call, which errs towards refusing to run a body rather than towards recursing in it.
    let rec private namesItself
        (assembly : DumpedAssembly)
        (method : MethodDefinitionHandle)
        (token : MetadataToken)
        : bool
        =
        let definition = assembly.Methods.[method]
        let ownType = definition.RequiredDeclaringType.Definition.Get

        let ownTypeName =
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) assembly.TypeDefs.[ownType]

        let byTypeRef (typeRef : TypeRef) (reference : MemberReference<MetadataToken>) =
            match reference.Signature with
            | MemberSignature.Method signature ->
                typeRefFullName assembly typeRef = ownTypeName
                && reference.PrettyName = definition.Name
                && signature.ParameterTypes.Length = definition.Signature.ParameterTypes.Length
                && signature.GenericParameterCount = definition.Signature.GenericParameterCount
            | MemberSignature.Field _ -> false

        let byDefinition (reference : MemberReference<MetadataToken>) =
            match reference.Signature with
            | MemberSignature.Method signature ->
                reference.PrettyName = definition.Name && signature = definition.Signature
            | MemberSignature.Field _ -> false

        match token with
        | MetadataToken.MethodDef handle -> handle = method
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec -> namesItself assembly method spec.Method
            | false, _ -> false
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | false, _ -> false
            | true, reference ->

            let rec ofTypeDefn (ty : TypeDefn) : bool =
                match ty with
                | TypeDefn.GenericInstantiation (generic, _) -> ofTypeDefn generic
                | TypeDefn.FromDefinition (identity, _) ->
                    identity.AssemblyFullName = assembly.ThisAssemblyDefinition.FullName
                    && identity.TypeDefinition.Get = ownType
                    && byDefinition reference
                | TypeDefn.FromReference (typeRef, _) -> byTypeRef typeRef reference
                | _ -> false

            match reference.Parent with
            | MetadataToken.TypeDefinition parent -> parent = ownType && byDefinition reference
            | MetadataToken.TypeReference parent ->
                match assembly.TypeRefs.TryGetValue parent with
                | true, typeRef -> byTypeRef typeRef reference
                | false, _ -> false
            | MetadataToken.TypeSpecification parent ->
                match assembly.TypeSpecs.TryGetValue parent with
                | true, spec -> ofTypeDefn spec.Signature
                | false, _ -> false
            | _ -> false
        | _ -> false

    /// Whether `body` contains a `call` or `callvirt` that can name `method` itself, where `method`
    /// is not virtual.
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
                    namesItself assembly method token.Token
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
