namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// Why CoreCLR's JIT refuses to compile a method over one of its instructions' tokens. The
/// compilation raises in the method's caller, before the method has run anything.
[<RequireQualifiedAccess>]
type internal CompilationFailure =
    /// A `call` or `ldftn` names an abstract method itself, rather than through a `constrained.`
    /// prefix that resolves it: `BadImageFormatException`, "Bad IL format." (`BFA_BAD_IL`).
    | NamesAbstractMethod
    /// The token names a type CoreCLR refuses as the instruction's operand (`TypeOperand.refusal`),
    /// or names its member through one: `InvalidProgramException`.
    | RefusesTypeOperand of TypeOperandRefusal

/// Which accessor of a multi-dimensional array type a `call` names.
[<RequireQualifiedAccess>]
type internal ArrayAccessorKind =
    | Get
    | Set
    | Address

/// An accessor of a multi-dimensional array type, which the runtime synthesises (ECMA-335
/// II.14.2): a MemberRef row whose parent is a TypeSpec of `TypeDefn.Array`.
[<NoEquality ; NoComparison>]
type internal ArrayAccessor =
    {
        Kind : ArrayAccessorKind
        Element : TypeDefn
        Rank : int
        /// The MemberRef row's signature, as the call site spells it.
        Signature : MemberSignature
    }

/// What a `call` or `ldftn` does, decided by binding its token, with the type its `constrained.`
/// prefix names, as CoreCLR's JIT binds them when it compiles the method holding the instruction.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal CallSiteBinding =
    /// The instruction calls this method, or loads a pointer to it.
    | Method of WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    /// The `call` names an accessor of a multi-dimensional array type.
    | ArrayAccessor of ArrayAccessor
    /// The JIT compiles a throw of this failure in place of the instruction: a `constrained.` call
    /// or load of a static virtual method that resolves to no implementation, in code CoreCLR
    /// compiles for the exact instantiation (`getCallInfo`, jitinterface.cpp).
    | RaisesWhenExecuted of DispatchFailure
    /// CoreCLR refuses to compile the method holding the instruction.
    | CompilationFails of CompilationFailure
    /// PawPrint cannot bind the instruction's token.
    | Unbound of UnboundToken

/// What the type token of a `constrained.` prefix names, bound as CoreCLR's JIT binds it when it
/// compiles the method holding the prefix.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal ConstrainedPrefixBinding =
    /// The type the prefix names.
    | Type of ConcreteTypeHandle
    /// CoreCLR refuses to compile the method holding the prefix.
    | CompilationFails of CompilationFailure
    /// PawPrint cannot bind the prefix's token.
    | Unbound of UnboundToken

/// Binds the tokens of `call`, `ldftn` and the `constrained.` prefix as CoreCLR's JIT does when it
/// compiles the method holding them, against the executing frame's generic context. Each binding
/// returns the state it leaves behind, having loaded assemblies and registered concrete types on
/// the way, and decides what the instruction does without running any of it.
[<RequireQualifiedAccess>]
module internal CallSiteBinding =

    /// The accessor of a multi-dimensional array type `token` of `assembly` names, if it names one:
    /// a MemberRef row whose parent TypeSpec is `TypeDefn.Array`. `None` for any other token.
    let private arrayAccessor
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : Result<ArrayAccessor, UnboundToken> option
        =
        match token with
        | MetadataToken.MemberReference reference ->
            match assembly.Members.TryGetValue reference with
            | false, _ -> None
            | true, memberRef ->

            match memberRef.Parent with
            | MetadataToken.TypeSpecification spec ->
                match assembly.TypeSpecs.TryGetValue spec with
                | true, typeSpec ->
                    match typeSpec.Signature with
                    | TypeDefn.Array (element, rank) ->
                        let accessor (kind : ArrayAccessorKind) : Result<ArrayAccessor, UnboundToken> =
                            Ok
                                {
                                    Kind = kind
                                    Element = element
                                    Rank = rank
                                    Signature = memberRef.Signature
                                }

                        match assembly.Strings memberRef.Name with
                        | "Get" -> accessor ArrayAccessorKind.Get
                        | "Set" -> accessor ArrayAccessorKind.Set
                        | "Address" -> accessor ArrayAccessorKind.Address
                        | other ->
                            UnboundToken.unmodelled
                                $"unexpected synthesized member %s{other} on multi-dim array (rank %d{rank}); expected Get/Set/Address/.ctor"
                            |> Error
                        |> Some
                    | _ -> None
                | false, _ -> None
            | _ -> None
        | _ -> None

    /// Which static virtual method a `constrained.` call or load of `fromMetadata`, bound as
    /// `concretized`, runs, where the prefix names `constrained`; and what the instruction does
    /// where it runs none.
    ///
    /// `opName` names the instruction, for diagnostics. `isLdftn` says it is an `ldftn`, where a
    /// failure in code CoreCLR shares between instantiations is not the JIT's to raise.
    let private constrainedStaticImplementation
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (opName : string)
        (isLdftn : bool)
        (constrained : ConcreteTypeHandle)
        (fromMetadata : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (concretized : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * CallSiteBinding
        =
        let typeSystem, resolvedAgainst =
            ConstrainedType.resolve
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                state.TypeSystem
                constrained
                fromMetadata

        let state = state.WithTypeSystem typeSystem

        let resolvedAgainst =
            match resolvedAgainst with
            | Error failure -> Error (UnboundToken.Type failure)
            | Ok (ConstrainedType.AsNamed named) -> Ok named
            | Ok (ConstrainedType.EnumUnderlying (_, underlying, RetargetedCall.Named)) -> Ok underlying
            | Ok (ConstrainedType.EnumUnderlying (_, _, RetargetedCall.ObjectGetHashCode)) ->
                UnboundToken.unmodelled
                    $"%s{opName}: expected a static interface method, got %s{MethodOwner.describe fromMetadata.Owner}::%s{fromMetadata.Name}, a class's method"
                |> Error
            | Ok (ConstrainedType.Undecided enum) ->
                let described =
                    AllConcreteTypes.describe state.TypeSystem._LoadedAssemblies state.TypeSystem.ConcreteTypes enum

                UnboundToken.unmodelled
                    $"TODO: %s{opName} of %s{MethodOwner.describe fromMetadata.Owner}::%s{fromMetadata.Name} on the enum %s{described}: CoreCLR resolves it against the enum's underlying primitive if the method holds Object.GetHashCode's slot number, but PawPrint places no slot for this method, or reads no single instance field `value__` off the enum"
                |> Error

        match resolvedAgainst with
        | Error unbound -> state, CallSiteBinding.Unbound unbound
        | Ok resolvedAgainst ->

        let typeSystem, implementation =
            StaticVirtualDispatch.resolveConstrainedStaticInterfaceMethod
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                opName
                resolvedAgainst
                fromMetadata
                concretized
                state.TypeSystem

        let state = state.WithTypeSystem typeSystem

        match implementation with
        | Error failure -> state, CallSiteBinding.Unbound (UnboundToken.Type failure)
        | Ok (ConstrainedStaticImplementation.Runs (implementation, _declaringType)) ->
            state, CallSiteBinding.Method implementation
        | Ok (ConstrainedStaticImplementation.Fails failure) ->
            let frame = state.ThreadState.[thread].MethodState.ExecutingMethod

            // Where CoreCLR raises an `ldftn`'s failure depends on whether it compiled this frame as
            // shared code. In exact code the JIT resolves the slot as it compiles, and `getCallInfo`
            // (jitinterface.cpp) plants a throw helper at the `ldftn` itself. In shared code the
            // pointer comes from a generic dictionary slot holding a stub that resolves and throws
            // when called (`CreateStubForStaticVirtualDispatch`, genericdict.cpp) -- unless the JIT
            // inlined this frame into exact code, where the `ldftn` throws. Measured: under default
            // tiering the delegate is made, and under `DOTNET_TieredCompilation=0` an inlined one
            // throws at the `ldftn`.
            if isLdftn && CodeSharing.isShared baseClassTypes state.TypeSystem frame then
                state,
                CallSiteBinding.Unbound (
                    UnboundToken.unmodelled
                        $"TODO: constrained. Ldftn of %s{fromMetadata.Name} resolved to %s{DispatchFailure.describe failure}, in %s{frame.Name}, which CoreCLR runs as shared generic code. Measured: real .NET then makes a delegate over a stub, and invoking it raises what that failure does, unless the JIT inlined the frame into exact code, where the ldftn throws; which one happens depends on the JIT"
                )
            else
                state, CallSiteBinding.RaisesWhenExecuted failure

    /// What a `call` or `ldftn` (`opName`, used as `use'`) of method token `token` does, where its
    /// `constrained.` prefix names `constrained`; `constrainedOpName` names the instruction under
    /// that prefix.
    let private bindMethodSite
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (use' : MemberReferenceUse)
        (opName : string)
        (constrainedOpName : string)
        (isLdftn : bool)
        (assembly : DumpedAssembly)
        (constrained : ConcreteTypeHandle option)
        (token : MetadataToken)
        (state : IlMachineState)
        : IlMachineState * CallSiteBinding
        =
        match MethodTokenBinding.bind loggerFactory baseClassTypes thread use' opName assembly token state with
        | state, MethodTokenBinding.Unbound unbound -> state, CallSiteBinding.Unbound unbound
        | state, MethodTokenBinding.RefusedParent refusal ->
            state, CallSiteBinding.CompilationFails (CompilationFailure.RefusesTypeOperand refusal)
        | state, MethodTokenBinding.Method (concretized, fromMetadata) ->

        match constrained with
        | Some constrained ->
            constrainedStaticImplementation
                loggerFactory
                baseClassTypes
                thread
                constrainedOpName
                isLdftn
                constrained
                fromMetadata
                concretized
                state
        | None ->

        match concretized.Body with
        | MethodBody.Abstract -> state, CallSiteBinding.CompilationFails CompilationFailure.NamesAbstractMethod
        | MethodBody.Il _
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _ -> state, CallSiteBinding.Method concretized

    /// What a `call` of metadata token `token` of `assembly` does, where its `constrained.` prefix
    /// names `constrained`.
    let bindCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (assembly : DumpedAssembly)
        (constrained : ConcreteTypeHandle option)
        (token : MetadataToken)
        (state : IlMachineState)
        : IlMachineState * CallSiteBinding
        =
        // An array accessor has no managed body for member resolution to find: it would look the
        // name up on `System.Array` and fail.
        match arrayAccessor assembly token with
        | Some (Ok accessor) -> state, CallSiteBinding.ArrayAccessor accessor
        | Some (Error unbound) -> state, CallSiteBinding.Unbound unbound
        | None ->
            bindMethodSite
                loggerFactory
                baseClassTypes
                thread
                (MemberReferenceUse.Other "call")
                "call"
                "constrained.call"
                false
                assembly
                constrained
                token
                state

    /// What an `ldftn` of metadata token `token` of `assembly` does, where its `constrained.` prefix
    /// names `constrained`.
    let bindLdftn
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (assembly : DumpedAssembly)
        (constrained : ConcreteTypeHandle option)
        (token : MetadataToken)
        (state : IlMachineState)
        : IlMachineState * CallSiteBinding
        =
        bindMethodSite
            loggerFactory
            baseClassTypes
            thread
            (MemberReferenceUse.Other (string<UnaryMetadataTokenIlOp> UnaryMetadataTokenIlOp.Ldftn))
            "Ldftn"
            "Ldftn"
            true
            assembly
            constrained
            token
            state

    /// What the `constrained.` prefix of metadata token `token` of `assembly` names.
    let bindConstrainedPrefix
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        (state : IlMachineState)
        : IlMachineState * ConstrainedPrefixBinding
        =
        let state, named =
            match token with
            | MetadataToken.TypeDefinition handle ->
                match IlMachineState.lookupTypeDefn loggerFactory baseClassTypes state assembly handle with
                | state, Ok ty -> state, Ok (ty, assembly)
                | state, Error failure -> state, Error (UnboundToken.Type failure)
            | MetadataToken.TypeReference handle ->
                match IlMachineState.lookupTypeRef loggerFactory baseClassTypes state assembly handle with
                | state, Ok named -> state, Ok named
                | state, Error failure ->
                    state, Error (UnboundToken.TypeReference (assembly, assembly.TypeRefs.[handle], failure))
            | MetadataToken.TypeSpecification spec -> state, Ok (assembly.TypeSpecs.[spec].Signature, assembly)
            | other -> state, Error (UnboundToken.unmodelled $"unexpected token %O{other} in Constrained")

        match named with
        | Error unbound -> state, ConstrainedPrefixBinding.Unbound unbound
        | Ok (ty, declaredIn) ->

        match
            MethodTokenBinding.concretizeInFrame
                loggerFactory
                baseClassTypes
                thread
                declaredIn.DefinitionFullName
                ty
                state
        with
        | state, Error failure -> state, ConstrainedPrefixBinding.Unbound (UnboundToken.Type failure)
        | state, Ok handle ->

        match TypeOperand.refusal baseClassTypes state.TypeSystem.ConcreteTypes handle with
        | Some refusal ->
            state, ConstrainedPrefixBinding.CompilationFails (CompilationFailure.RefusesTypeOperand refusal)
        | None -> state, ConstrainedPrefixBinding.Type handle
