namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// What a must-expand intrinsic asks CoreCLR's JIT for.
///
/// The JIT recognises the hardware-intrinsic classes by namespace (`lookupNamedIntrinsic`,
/// importercalls.cpp) and answers each of their placeholders from the CPU it compiles for
/// (`HWIntrinsicInfo::lookupId`, hwintrinsic.cpp). Outside those classes, a placeholder is an
/// operation the JIT emits code for itself.
[<RequireQualifiedAccess>]
type JitExpansion =
    /// `IsSupported` on a hardware-intrinsic class: whether the CPU has that instruction set.
    | IsSupportedQuery of IntrinsicClass
    /// `IsHardwareAccelerated` on a vector API: whether the CPU accelerates it.
    | IsHardwareAcceleratedQuery of IntrinsicClass
    /// Any other placeholder on a hardware-intrinsic class: the instruction itself.
    | HardwareInstruction of IntrinsicClass
    /// A placeholder the JIT implements in code of its own, such as a memory barrier.
    | Primitive of IntrinsicPrimitive
    /// A placeholder this classification does not know: an operation of a runtime other than the
    /// ones it was transcribed from, or a method outside CoreLib that calls itself.
    | Unrecognised

/// What CoreCLR's JIT emits at a must-expand call site: a call from a JIT-expansion method to
/// itself, which is where `gtIsRecursiveCall` (importercalls.cpp) applies. The rest of the
/// method's IL is its own and runs as written; for most placeholders the self-call is the whole
/// body, but some guard it (`Avx2.GatherVector128` validates its scale first).
[<RequireQualifiedAccess>]
type SelfCallExpansion =
    /// The call takes no arguments and yields this Boolean.
    | Constant of bool
    /// The call's arguments are discarded and `IntrinsicBody.platformNotSupportedHelper` is
    /// called in its place (`impUnsupportedNamedIntrinsic`, importercalls.cpp), which throws
    /// `PlatformNotSupportedException`. The helper's frame is in `new StackTrace(e)`, and out of
    /// the formatted trace because its class is `[StackTraceHidden]`.
    | ThrowPlatformNotSupported
    /// The CPU has the instruction, and the JIT emits it.
    | HardwareInstruction of IntrinsicClass
    /// The runtime's own operation, whose effect on a caller is `IntrinsicPrimitive.contract`.
    | Primitive of IntrinsicPrimitive
    /// A `JitExpansion.Unrecognised` placeholder: what the JIT emits is not known.
    | Unrecognised

/// What CoreCLR executes when it calls a method it treats as a JIT intrinsic.
///
/// `[Intrinsic]` permits the JIT to replace a call with code of its own, and it usually does so
/// only as an optimisation. Under MinOpts it expands nothing it is not obliged to
/// (`impIntrinsic`, importercalls.cpp), and otherwise emits an ordinary call to the method's IL;
/// so for most intrinsics the IL is a complete implementation. There are three exceptions: IL with
/// a call to itself that must not run as a call, a body the VM replaces outright, and a method the
/// JIT replaces at every call to it.
[<RequireQualifiedAccess>]
type IntrinsicBody =
    /// The method's own IL is its semantics.
    | OwnIl
    /// The IL calls the method itself, non-virtually. By CoreCLR's convention that call is a
    /// placeholder the JIT must expand ("The recursive non-virtual calls to Jit intrinsics are
    /// must-expand by convention", importercalls.cpp), so interpreting that call recurses without
    /// end. `IntrinsicBody.expandSelfCall` says what the call does instead.
    | JitExpansion of JitExpansion
    /// A CoreLib method whose IL cannot return, declared on one of the four classes for which
    /// CoreCLR's VM substitutes IL of its own (`getILIntrinsicImplementationFor*`,
    /// jitinterface.cpp): the body CoreLib ships throws `PlatformNotSupportedException` in place of
    /// the one the runtime supplies. `VmSubstitution` transcribes that IL where it is the same for
    /// every instantiation.
    | VmSubstitution
    /// A CoreLib method the JIT replaces at every call to it, whatever it is optimising for: one of
    /// the named intrinsics `impIntrinsic` (importercalls.cpp) expands before it tests whether to
    /// optimise. What replaces the call is not the method's IL, which throws
    /// `UnreachableException`. Only code the runtime generates calls these (IL stubs, the async
    /// resumption stub, runtime-async methods).
    | CallSiteExpansion
    /// A method of CoreLib's `System.StubHelpers.StructureMarshaler<T>`, whose body CoreCLR's VM
    /// replaces with a stub that performs the operation on T field by field when T is a value type
    /// that is neither blittable nor an enum
    /// (`StructMarshalStubs::TryGenerateStructMarshallingMethod`, dllimport.cpp). For any other T,
    /// the method's own IL runs.
    | StructMarshalling of StructMarshalOperation
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

    /// The methods `IntrinsicBody.CallSiteExpansion` names, by namespace, class and name, in every
    /// runtime PawPrint supports: .NET 10's `StubHelpers.AsyncCallContinuation` is .NET 11's
    /// `AsyncHelpers.AsyncCallContinuation`. Of the other named intrinsics `impIntrinsic` handles
    /// before its optimisation test, `RuntimeHelpers.IsRuntimeAsync` becomes whether the method
    /// calling it is runtime-async, as its IL answers for a method that is not, and the JIT
    /// declines to expand the rest (`AsyncHelpers.Await` and the like), so a call runs their IL.
    let private callSiteExpansions : Set<string * string * string> =
        Set.ofList
            [
                "System.StubHelpers", "StubHelpers", "GetStubContext"
                "System.StubHelpers", "StubHelpers", "NextCallReturnAddress"
                "System.StubHelpers", "StubHelpers", "AsyncCallContinuation"
                "System.Runtime.CompilerServices", "AsyncHelpers", "AsyncCallContinuation"
                "System.Runtime.CompilerServices", "AsyncHelpers", "AsyncSuspend"
                "System.Runtime.CompilerServices", "AsyncHelpers", "TailAwait"
                "System.Runtime.CompilerServices", "RuntimeHelpers", "SetNextCallGenericContext"
                "System.Runtime.CompilerServices", "RuntimeHelpers", "SetNextCallAsyncContinuation"
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
        StackFlow.reachable body
        |> Set.exists (fun offset ->
            match body.Locations.[offset] with
            | IlOp.Nullary NullaryIlOp.Ret -> true
            | _ -> false
        )

    /// The class `method` is declared on, walked out through the classes enclosing it.
    let private intrinsicClassOf (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : IntrinsicClass =
        let rec walk (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) (path : string list) : IntrinsicClass =
            if ty.IsNested then
                walk assembly.TypeDefs.[ty.DeclaringType] (ty.Name :: path)
            else
                {
                    Namespace = ty.Namespace
                    Path = ty.Name :: path
                }

        walk assembly.TypeDefs.[assembly.Methods.[method].RequiredDeclaringType.Definition.Get] []

    /// The namespaces whose placeholders the JIT answers from the CPU it compiles for
    /// (`lookupNamedIntrinsic`, importercalls.cpp). It checks only its own architecture's
    /// namespace, but every other architecture's classes carry IL that is not a placeholder.
    let private isHardwareIntrinsicNamespace (ns : string) : bool =
        ns = "System.Numerics"
        || ns = "System.Runtime.Intrinsics"
        || ns.StartsWith ("System.Runtime.Intrinsics.", System.StringComparison.Ordinal)

    /// The capability query `method` is by its name and signature, whatever its IL: a static,
    /// parameterless `bool` getter `IsSupported` or `IsHardwareAccelerated` on a CoreLib class in a
    /// hardware-intrinsic namespace that is not generic.
    let private queryOf (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : JitExpansion option =
        let definition = assembly.Methods.[method]
        let intrinsicClass = intrinsicClassOf assembly method

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        // On a generic class (`Vector128<T>.IsSupported`) the JIT answers about `T`, not about the
        // CPU.
        let isQuery =
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && isHardwareIntrinsicNamespace intrinsicClass.Namespace
            && definition.IsStatic
            && declaringType.Generics.IsEmpty
            && definition.Signature.ParameterTypes.IsEmpty
            && definition.Signature.GenericParameterCount = 0
            && definition.Signature.ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Boolean)

        match definition.Name with
        | "get_IsSupported" when isQuery -> Some (JitExpansion.IsSupportedQuery intrinsicClass)
        | "get_IsHardwareAccelerated" when isQuery -> Some (JitExpansion.IsHardwareAcceleratedQuery intrinsicClass)
        | _ -> None

    /// What the placeholder `method`, a JIT expansion in `assembly`, asks the JIT for.
    let private expansionOf (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : JitExpansion =
        let intrinsicClass = intrinsicClassOf assembly method

        if
            assembly.ThisAssemblyDefinition.Name.Name <> corelib
            || not (isHardwareIntrinsicNamespace intrinsicClass.Namespace)
        then
            match IntrinsicPrimitive.recognise assembly method with
            | Some primitive -> JitExpansion.Primitive primitive
            | None -> JitExpansion.Unrecognised
        else
            match queryOf assembly method with
            | Some query -> query
            | None -> JitExpansion.HardwareInstruction intrinsicClass

    /// What CoreCLR executes for the intrinsic `method`, defined in `assembly`. Meaningful only for
    /// a method `isIntrinsic` accepts: for any other, CoreCLR runs the IL whatever it contains.
    let classify (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : IntrinsicBody =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        if
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && not declaringType.IsNested
            && callSiteExpansions.Contains (declaringType.Namespace, declaringType.Name, definition.Name)
        then
            IntrinsicBody.CallSiteExpansion
        else

        // `getMethodInfoWorker` (jitinterface.cpp) offers every intrinsic of corelib.h's
        // `STRUCTURE_MARSHALER` to `TryGenerateStructMarshallingMethod`, which generates a body
        // for the three corelib.h binds as `CONVERT_TO_UNMANAGED_CORE`, `CONVERT_TO_MANAGED` and
        // `FREE_CORE`, and leaves any other to be compiled as usual.
        let structMarshalling =
            if
                assembly.ThisAssemblyDefinition.Name.Name = corelib
                && not declaringType.IsNested
                && declaringType.Namespace = "System.StubHelpers"
                && declaringType.Name = "StructureMarshaler`1"
            then
                match definition.Name with
                | "ConvertToUnmanagedCore" -> Some StructMarshalOperation.Marshal
                | "ConvertToManaged" -> Some StructMarshalOperation.Unmarshal
                | "FreeCore" -> Some StructMarshalOperation.Cleanup
                | _ -> None
            else
                None

        match structMarshalling with
        | Some operation -> IntrinsicBody.StructMarshalling operation
        | None ->

        match definition.Body with
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> IntrinsicBody.NoIl
        | MethodBody.Il body ->
            if callsItself assembly method body then
                IntrinsicBody.JitExpansion (expansionOf assembly method)
            else if
                assembly.ThisAssemblyDefinition.Name.Name = corelib
                && vmSubstitutingClasses.Contains (declaringType.Namespace, declaringType.Name)
                && not (canReturn body)
            then
                IntrinsicBody.VmSubstitution
            else
                IntrinsicBody.OwnIl

    /// The parameterless `Internal.Runtime.CompilerHelpers.ThrowHelpers` method of this name, which
    /// corelib.h binds to one of the JIT's throw helpers.
    let private throwHelper (corelib : DumpedAssembly) (name : string) : MethodDefinitionHandle =
        let found =
            corelib.TryGetTopLevelTypeDef "Internal.Runtime.CompilerHelpers" "ThrowHelpers"
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m ->
                m.Name = name
                && m.IsStatic
                && m.Signature.ParameterTypes.IsEmpty
                && m.Signature.GenericParameterCount = 0
            )
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> facts.Handle
        | found ->
            failwith
                $"IntrinsicBody: expected %s{corelib.DefinitionFullName} to define one ThrowHelpers.%s{name}(), found %d{found.Length}"

    /// The CoreLib method CoreCLR binds to `CORINFO_HELP_THROW_PLATFORM_NOT_SUPPORTED`
    /// (corelib.h): `Internal.Runtime.CompilerHelpers.ThrowHelpers.ThrowPlatformNotSupportedException()`.
    let platformNotSupportedHelper (corelib : DumpedAssembly) : MethodDefinitionHandle =
        throwHelper corelib "ThrowPlatformNotSupportedException"

    /// The CoreLib method CoreCLR binds to `CORINFO_HELP_THROW_ARGUMENTOUTOFRANGEEXCEPTION`
    /// (corelib.h): `Internal.Runtime.CompilerHelpers.ThrowHelpers.ThrowArgumentOutOfRangeException()`.
    /// The JIT calls it when a hardware instruction's immediate operand is out of range
    /// (`addRangeCheckIfNeeded` and `impUnsupportedNamedIntrinsic`, hwintrinsic.cpp).
    let argumentOutOfRangeHelper (corelib : DumpedAssembly) : MethodDefinitionHandle =
        throwHelper corelib "ThrowArgumentOutOfRangeException"

    /// What a call from a method whose body `classify` finds to be `expansion` to itself does, on
    /// a CPU `profile` describes. A call to that method from anywhere else runs its IL, which
    /// reaches that site.
    let expandSelfCall (profile : HardwareIntrinsicsProfile) (expansion : JitExpansion) : SelfCallExpansion =
        match expansion with
        | JitExpansion.IsSupportedQuery c -> SelfCallExpansion.Constant (profile.IsSupported.Contains c)
        | JitExpansion.IsHardwareAcceleratedQuery c ->
            SelfCallExpansion.Constant (profile.IsHardwareAccelerated.Contains c)
        | JitExpansion.HardwareInstruction c ->
            if profile.IsSupported.Contains c || profile.IsHardwareAccelerated.Contains c then
                SelfCallExpansion.HardwareInstruction c
            else
                SelfCallExpansion.ThrowPlatformNotSupported
        // Not answered from the profile: the profile is what the capability queries report, and
        // the JIT picks the instructions for these from the real CPU's baseline, which every CPU
        // has (with `DOTNET_EnableHWIntrinsic=0` on Arm64, `Math.ReciprocalEstimate` still
        // estimates in hardware).
        | JitExpansion.Primitive primitive -> SelfCallExpansion.Primitive primitive
        | JitExpansion.Unrecognised -> SelfCallExpansion.Unrecognised

    /// What every call to `method` returns on a CPU `profile` describes, where that is a constant:
    /// `method` is a capability query, CoreCLR's JIT compiles each call of it to the answer
    /// `profile` gives, and its IL returns that answer too. The IL either returns its call to
    /// itself, or is CoreLib's stub returning `false` for a class of another architecture than the
    /// one CoreLib was built for, which the JIT answers false at every call
    /// (`lookupNamedIntrinsic`, importercalls.cpp), even when it does not optimise. `None` for any
    /// other method, and for a stub whose class `profile` claims, as no CPU of CoreLib's
    /// architecture can.
    let constantResult
        (profile : HardwareIntrinsicsProfile)
        (assembly : DumpedAssembly)
        (method : MethodDefinitionHandle)
        : bool option
        =
        if not (isIntrinsic assembly method) then
            None
        else

        let definition = assembly.Methods.[method]

        match classify assembly method, definition.Body with
        | IntrinsicBody.JitExpansion expansion, MethodBody.Il body ->
            // `classify` found a call naming the method itself, and this body's only call is this one.
            match expandSelfCall profile expansion, body.Instructions with
            | SelfCallExpansion.Constant value,
              [ IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata _), _
                IlOp.Nullary NullaryIlOp.Ret, _ ] -> Some value
            | _ -> None
        | IntrinsicBody.OwnIl, MethodBody.Il body ->
            // The VM marks a stub intrinsic, so that the JIT answers a call of it, only by the
            // stub's own `[Intrinsic]`: it honours its class's only on the architecture CoreLib was
            // built for (`fIsHardwareIntrinsic`, methodtablebuilder.cpp). Without it a call of the
            // stub is an ordinary call, whose branch the JIT compiles unless it inlines the stub.
            match queryOf assembly method, body.Instructions with
            | Some query, [ IlOp.Nullary NullaryIlOp.LdcI4_0, _ ; IlOp.Nullary NullaryIlOp.Ret, _ ] when
                MethodInfo.isJITIntrinsic (getMemberRefParentType assembly) assembly.Methods definition
                && expandSelfCall profile query = SelfCallExpansion.Constant false
                ->
                Some false
            | _ -> None
        | _ -> None

    /// The IL CoreCLR's VM runs in place of `method`'s own, when its own is a placeholder that
    /// `VmSubstitution` transcribes. `None` means the method's own body is what runs, or else that
    /// no IL can: `classify` says which.
    ///
    /// A method whose own IL works is given its own IL here even when the VM substitutes it, so
    /// this is what runs, not what CoreCLR runs; `VmSubstitution.unsafeStub` answers the latter.
    let substitutedBody
        (assembly : DumpedAssembly)
        (method : MethodDefinitionHandle)
        : MethodInstructions<TypeDefn> option
        =
        if isIntrinsic assembly method then
            match classify assembly method with
            | IntrinsicBody.VmSubstitution -> VmSubstitution.unsafeStub assembly method
            | IntrinsicBody.JitExpansion _
            | IntrinsicBody.CallSiteExpansion
            | IntrinsicBody.StructMarshalling _
            | IntrinsicBody.OwnIl
            | IntrinsicBody.NoIl -> None
        else
            None
