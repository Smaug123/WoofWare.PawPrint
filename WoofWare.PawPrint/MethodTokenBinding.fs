namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// Why PawPrint cannot bind a token an instruction names. CoreCLR binds it, or fails to in a way
/// PawPrint does not raise into the guest; either way the instruction cannot run.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal UnboundToken =
    /// The TypeRef row `reference` of `assembly`, which the token names, does not bind.
    | TypeReference of assembly : DumpedAssembly * reference : TypeRef * failure : TypeBindingFailure
    /// A type the binding reaches does not bind.
    | Type of TypeBindingFailure
    /// The MemberRef row `reference` of `assembly`, which the token names, binds no member.
    | MemberReference of assembly : DumpedAssembly * reference : MemberReferenceHandle * failure : MemberBindingFailure
    /// A shape PawPrint does not implement binding.
    | Unmodelled of UnmodelledBinding

[<RequireQualifiedAccess>]
module internal UnboundToken =

    /// What PawPrint could not bind, and why, for whoever reads the failed run.
    let describe (unbound : UnboundToken) : string =
        match unbound with
        | UnboundToken.TypeReference (assembly, reference, failure) ->
            TypeResolution.describeReferenceFailure reference assembly failure
        | UnboundToken.Type failure -> TypeBindingFailure.describe failure
        | UnboundToken.MemberReference (assembly, reference, failure) ->
            MemberReferenceInstantiation.describeFailure assembly reference failure
        | UnboundToken.Unmodelled unmodelled -> unmodelled.Diagnostic

    /// A shape PawPrint does not implement binding, described by `diagnostic`.
    let unmodelled (diagnostic : string) : UnboundToken =
        UnboundToken.Unmodelled
            {
                Diagnostic = diagnostic
            }

/// What a method token names, bound against the executing frame's generic context as CoreCLR's
/// `CEEInfo::resolveToken` binds it for the instruction holding it.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal MethodTokenBinding =
    /// The method, at the instantiation the token names, and as its metadata declares it.
    | Method of
        concretized : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
        fromMetadata : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>
    /// The token names its method through a type CoreCLR refuses a token of
    /// (`MemberReferenceResolution.RefusedParent`), so the instruction raises
    /// `InvalidProgramException`.
    | RefusedParent of TypeOperandRefusal
    /// PawPrint cannot bind the token.
    | Unbound of UnboundToken

/// How an instruction binds a MethodDef row naming a method of a generic type, which says nothing of
/// the type's instantiation.
[<RequireQualifiedAccess>]
type internal GenericTypeMethodRow =
    /// CoreCLR binds it to the typical instantiation (`MemberLoader::GetMethodDescFromMethodDef`),
    /// which PawPrint does not model, so the row is unbound. For `call` and `ldftn`, which use the
    /// method the row names.
    | Typical
    /// At the executing frame's instantiation of the declaring type. For `ldvirtftn`, which then
    /// dispatches on its receiver: that agrees with CoreCLR where the receiver's instantiation is
    /// the frame's, as when a method of the type loads its own virtual (measured on .NET 10).
    | AtExecutingFrame

/// Binds the method token of a `call`, `ldftn`, `ldvirtftn` or `ldtoken` against the executing
/// frame's generic context. Each binding returns the state it leaves behind, having loaded
/// assemblies and registered concrete types on the way.
[<RequireQualifiedAccess>]
module internal MethodTokenBinding =

    /// `ty`, read in `assemblyFullName`, concretised against the executing frame's generic context.
    let concretizeInFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (assemblyFullName : string)
        (ty : TypeDefn)
        (state : IlMachineState)
        : IlMachineState * Result<ConcreteTypeHandle, TypeBindingFailure>
        =
        let frame = state.ThreadState.[thread].MethodState.ExecutingMethod

        let typeSystem, result =
            TypeSystemState.concretizeType
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                state.TypeSystem
                assemblyFullName
                frame.DeclaringTypeGenerics
                frame.Generics
                ty

        state.WithTypeSystem typeSystem, result

    /// `method`, which a MemberRef row names and whose declaring type's arguments the row's parent
    /// spells as `extractedTypeArgs`, instantiated for the executing frame: at `methodGenerics` when
    /// a MethodSpec wraps the row, and otherwise as the row names it alone.
    let instantiateReferencedMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (opName : string)
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (extractedTypeArgs : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle> option)
        (state : IlMachineState)
        : IlMachineState * MethodTokenBinding
        =
        match methodGenerics with
        | None when not method.Generics.IsEmpty ->
            // A *bare* MemberReference to a generic method names that method's typical
            // instantiation -- its own `!!i` stand unbound -- exactly as a bare MethodDef does, and
            // for the same reason: CoreCLR reaches `MemberLoader::GetDescFromMemberRef` with
            // `strictMetadataChecks` off for `ldtoken`, which permits the uninstantiated form. An
            // instantiation arrives as a MethodSpec *wrapping* this row, which is the only spelling
            // any compiler emits.
            //
            // Refused here rather than concretized: there are no method generics to substitute,
            // and concretization would refuse `!!0` as out of scope, which is CoreCLR's answer to
            // a signature, not to this token, and names neither.
            state,
            MethodTokenBinding.Unbound (
                UnboundToken.unmodelled
                    $"TODO: %s{opName} of a bare MemberReference to generic method %s{method.Name} names the typical instantiation, which needs open generic RuntimeMethodHandle support; got %O{method}"
            )
        | _ ->

        if extractedTypeArgs.IsEmpty && not method.DeclaringTypeGenerics.IsEmpty then
            // The parent named a generic type definition without an instantiation: `G`1::M()`
            // rather than `G`1<int>::M()`. A TypeReference parent carries no type arguments, which
            // means the *typical* instantiation when the type is generic. PawPrint cannot name
            // that, and the fall-through is silent and wrong rather than loud: with no arguments
            // extracted, `resolveTargetTypeGenerics` reaches for the *executing frame's*
            // declaring-type generics, so a caller that happens to be a generic type of the same
            // arity receives its own instantiation. Measured: `ldtoken List`1::get_Count()` from a
            // frame on `Gen<string>` produced a handle for `List<System.String>`.
            //
            // No compiler emits this row -- C# reaches a generic type's member through a TypeSpec
            // parent that spells the instantiation out -- so this guards hand-written IL.
            state,
            MethodTokenBinding.Unbound (
                UnboundToken.unmodelled
                    $"TODO: %s{opName} of a MemberReference whose parent names generic type definition %s{MethodOwner.describe method.Owner}::%s{method.Name} without an instantiation refers to the typical instantiation, which needs an open-generic declaring type; a TypeSpecification parent, which is what compilers emit, carries the instantiation instead"
            )
        else

        // `extractedTypeArgs` are the declaring type's arguments, read from the parent TypeSpec
        // already substituted against this frame. Handing them over explicitly is what keeps a
        // member of `G<List<T>>` resolving at `G<List<string>>` rather than at the frame's own
        // `G<string>`: concretization falls back to the frame's declaring-type generics when given
        // none.
        let state, concretized =
            match methodGenerics with
            | Some methodGenerics ->
                ExecutionConcretization.concretizeMethodForExecutionWithConcreteMethodGenerics
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    methodGenerics
                    (Some extractedTypeArgs)
                    state
            | None ->
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    None
                    (Some extractedTypeArgs)
                    state

        match concretized with
        | Ok (concretized, _declaringType) -> state, MethodTokenBinding.Method (concretized, method)
        | Error failure -> state, MethodTokenBinding.Unbound (UnboundToken.Type failure)

    /// The method MemberRef row `reference` of `assembly` names, used by an instruction as `use'`
    /// says, instantiated for the executing frame: at `methodGenerics` when a MethodSpec wraps the
    /// row.
    let private bindMemberReference
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (use' : MemberReferenceUse)
        (opName : string)
        (assembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle> option)
        (state : IlMachineState)
        : IlMachineState * MethodTokenBinding
        =
        let state, resolution =
            IlMachineState.resolveMember use' loggerFactory baseClassTypes thread assembly reference state

        match resolution with
        | Error failure ->
            state, MethodTokenBinding.Unbound (UnboundToken.MemberReference (assembly, reference, failure))
        | Ok (MemberReferenceResolution.RefusedParent refusal) -> state, MethodTokenBinding.RefusedParent refusal
        | Ok (MemberReferenceResolution.Resolved (_, Choice2Of2 _field, _)) ->
            // ECMA-335 II.22.25: the row's signature says whether it names a field or a method, and
            // this one says a field.
            state,
            MethodTokenBinding.Unbound (
                UnboundToken.unmodelled
                    $"%s{opName}: MemberReference %O{reference} of %s{assembly.Name.Name} names a field, not a method"
            )
        | Ok (MemberReferenceResolution.Resolved (_, Choice1Of2 method, extractedTypeArgs)) ->
            instantiateReferencedMethod
                loggerFactory
                baseClassTypes
                thread
                opName
                method
                extractedTypeArgs
                methodGenerics
                state

    /// The method MethodSpec row `handle` of `assembly` names, used by an instruction as `use'`
    /// says: a generic method at the instantiation the row spells, over either of the two parents
    /// a row may have.
    let bindMethodSpecification
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (use' : MemberReferenceUse)
        (opName : string)
        (assembly : DumpedAssembly)
        (handle : MethodSpecificationHandle)
        (state : IlMachineState)
        : IlMachineState * MethodTokenBinding
        =
        let spec = assembly.MethodSpecs.[handle]

        match spec.Method with
        | MetadataToken.MethodDef token ->
            match assembly.Methods.TryGetValue token with
            | false, _ ->
                state,
                MethodTokenBinding.Unbound (
                    UnboundToken.unmodelled
                        $"%s{opName}: MethodSpecification %O{handle} of %s{assembly.Name.Name} names MethodDef %O{token}, which the assembly does not define"
                )
            | true, method ->

            let method =
                method
                |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

            if not method.DeclaringTypeGenerics.IsEmpty then
                // The spec binds the *method's* generics; nothing here binds the declaring type's.
                // CoreCLR takes the owning MethodTable from the MethodDef row via
                // `MemberLoader::GetMethodDescFromMethodSpec` without applying the caller's class
                // instantiation, so the declaring type is the typical one. Concretizing with no type
                // arguments would fall back to the executing frame's `DeclaringTypeGenerics`, which
                // is not what the token means.
                //
                // No compiler emits this shape: a generic method on a generic type is referenced
                // through a MemberReference with a TypeSpec parent (measured), which carries the
                // instantiation explicitly. So this guards hand-written IL.
                state,
                MethodTokenBinding.Unbound (
                    UnboundToken.unmodelled
                        $"TODO: %s{opName} of a MethodSpecification over a MethodDef declared on generic type %s{MethodOwner.describe method.Owner} names the typical instantiation of the declaring type, which needs open generic RuntimeMethodHandle support; got %O{method}"
                )
            else

            let state, concretized =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    (Some spec.Signature)
                    None
                    state

            match concretized with
            | Ok (concretized, _declaringType) -> state, MethodTokenBinding.Method (concretized, method)
            | Error failure -> state, MethodTokenBinding.Unbound (UnboundToken.Type failure)
        | MetadataToken.MemberReference reference ->
            // The spec's arguments, concretized against the executing frame, so that resolving the
            // row can pick the right overload: its signature may name them by index.
            let rec concretizeArguments
                (state : IlMachineState)
                (acc : ConcreteTypeHandle list)
                (remaining : TypeDefn list)
                : IlMachineState * Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure>
                =
                match remaining with
                | [] -> state, Ok (List.rev acc |> ImmutableArray.CreateRange)
                | argument :: remaining ->
                    match
                        concretizeInFrame loggerFactory baseClassTypes thread assembly.DefinitionFullName argument state
                    with
                    | state, Error failure -> state, Error failure
                    | state, Ok concrete -> concretizeArguments state (concrete :: acc) remaining

            match concretizeArguments state [] (List.ofSeq spec.Signature) with
            | state, Error failure -> state, MethodTokenBinding.Unbound (UnboundToken.Type failure)
            | state, Ok methodGenerics ->
                bindMemberReference
                    loggerFactory
                    baseClassTypes
                    thread
                    use'
                    opName
                    assembly
                    reference
                    (Some methodGenerics)
                    state
        | other ->
            state,
            MethodTokenBinding.Unbound (
                UnboundToken.unmodelled $"Unrecognised MethodSpecification kind for %s{opName}: %O{other}"
            )

    /// The method `token` of `assembly` names, as a `call`, `ldftn` or `ldvirtftn` names it, binding
    /// a MethodDef row of a method on a generic type as `genericTypeRow` says.
    let bind
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (use' : MemberReferenceUse)
        (opName : string)
        (genericTypeRow : GenericTypeMethodRow)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        (state : IlMachineState)
        : IlMachineState * MethodTokenBinding
        =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | false, _ ->
                state,
                MethodTokenBinding.Unbound (
                    UnboundToken.unmodelled $"%s{opName}: MethodDef %O{handle} is not defined in %s{assembly.Name.Name}"
                )
            | true, method ->

            let method =
                method
                |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

            match genericTypeRow with
            | GenericTypeMethodRow.Typical when not method.DeclaringTypeGenerics.IsEmpty ->
                // Measured on .NET 10: a `call` of such a row from `Gen<string>` raises
                // `InvalidOperationException` from the callee's prestub ("not fully instantiated"),
                // and an `ldftn` makes its pointer. Concretizing with no type arguments would fall
                // back to the executing frame's instantiation instead.
                state,
                MethodTokenBinding.Unbound (
                    UnboundToken.unmodelled
                        $"TODO: %s{opName} of MethodDef %s{MethodOwner.describe method.Owner}::%s{method.Name}, declared on a generic type, names the typical instantiation, which CoreCLR binds and PawPrint does not model; compilers name such a method through a MemberReference whose TypeSpecification parent carries the instantiation"
                )
            | GenericTypeMethodRow.Typical
            | GenericTypeMethodRow.AtExecutingFrame ->

            let state, concretized =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    None
                    None
                    state

            match concretized with
            | Ok (concretized, _declaringType) -> state, MethodTokenBinding.Method (concretized, method)
            | Error failure -> state, MethodTokenBinding.Unbound (UnboundToken.Type failure)
        | MetadataToken.MemberReference reference ->
            bindMemberReference loggerFactory baseClassTypes thread use' opName assembly reference None state
        | MetadataToken.MethodSpecification handle ->
            bindMethodSpecification loggerFactory baseClassTypes thread use' opName assembly handle state
        | other ->
            state,
            MethodTokenBinding.Unbound (
                UnboundToken.unmodelled $"Unexpectedly asked to %s{opName} a non-method: %O{other}"
            )
