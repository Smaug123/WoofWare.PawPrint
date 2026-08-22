namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// What a `MemberReference` names, resolved against the executing frame's generic context.
///
/// A `MemberReference` row does not say which of the two it is in its parent; the *signature* does,
/// and it is parsed eagerly (`MemberSignature.Field` / `MemberSignature.Method`). But which member,
/// and at which instantiation, needs resolution — so this is what resolution hands back.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal ResolvedMemberToken =
    | Method of
        concretized : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
        fromMetadata : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>
    | Field of declaringType : ConcreteTypeHandle * field : FieldDefinitionHandle

/// What an `ldtoken` operand names: the three things a `Runtime*Handle` can stand for.
///
/// `ldtoken` is the one opcode whose operand ranges over all three, which is why its operand is
/// handed to the ops as a raw token rather than narrowed the way `ldfld`'s and `castclass`'s are.
/// Parsing it into this once means each refusal is stated in exactly one place, and the push that
/// follows cannot ask a question the classification has not already answered.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal LdtokenTarget =
    | Type of RuntimeTypeHandleTarget
    | Method of WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    /// The declaring type is what makes this a *particular* field rather than a metadata row:
    /// `typeof(G<int>).GetField(f).FieldHandle` and `typeof(G<>).GetField(f).FieldHandle` share a
    /// `FieldDefinitionHandle` and are observably different handles. No assembly name rides along:
    /// the field-handle registry derives it from this target, which already pins it.
    | Field of declaringType : RuntimeTypeHandleTarget * field : FieldDefinitionHandle

[<RequireQualifiedAccess>]
module internal UnaryMetadataTokenOps =
    /// Resolve a `MemberReference` against the executing frame's generic context.
    ///
    /// Shared by `ldftn`/`ldvirtftn` and `ldtoken`, which is sound precisely because CoreCLR
    /// resolves `mdtMemberRef` the same way for both: `CEEInfo::resolveToken` reaches
    /// `GetTypeContext(pResolvedToken->tokenContext, &typeContext)` before
    /// `MemberLoader::GetDescFromMemberRef`, whatever the opcode (`vm/jitinterface.cpp`). The two
    /// opcodes part company only on what they will *accept*: `ldftn` refuses the field arm, and
    /// `ldtoken` does not.
    ///
    /// This sharing deliberately stops short of `MethodDef`, which CoreCLR resolves through
    /// `GetMethodDescFromMethodDef` — a call that takes no `SigTypeContext` at all, so a bare
    /// `MethodDef` names the typical instantiation rather than the frame's. Routing `ldtoken`'s
    /// `MethodDef` through the frame-substituting arm `ldftn` uses would produce a wrong handle
    /// rather than a failure.
    ///
    let private resolveMemberReferenceToken
        (ctx : UnaryMetadataIlOpContext)
        (handle : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * ResolvedMemberToken
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let state, _, resolved, extractedTypeArgs =
            IlMachineState.resolveMember loggerFactory baseClassTypes thread ctx.ActiveAssembly handle state

        match resolved with
        | Choice1Of2 method when not method.Generics.IsEmpty ->
            // A *bare* MemberReference to a generic method names that method's typical
            // instantiation -- its own `!!i` stand unbound -- exactly as a bare MethodDef does, and
            // for the same reason: CoreCLR reaches `MemberLoader::GetDescFromMemberRef` with
            // `strictMetadataChecks` off for `ldtoken`, which permits the uninstantiated form. An
            // instantiation arrives as a MethodSpec *wrapping* this row, which is the only spelling
            // any compiler emits, and which `resolveMethodSpecificationToken` handles.
            //
            // Refused rather than concretized: `concretizeMethodForExecution` has no method
            // generics to substitute and would fail deep inside with "Generic method parameter 0",
            // naming neither the token nor the shape.
            failwith
                $"TODO: ldtoken/ldftn of a bare MemberReference to generic method %s{method.Name} names the typical instantiation, which needs open generic RuntimeMethodHandle support; got %O{method}"
        | Choice1Of2 method ->
            // `extractedTypeArgs` are the parent TypeSpec's arguments, already substituted against
            // this frame. Handing them over explicitly is what keeps a member of `G<List<T>>`
            // resolving at `G<List<string>>` rather than at the frame's own `G<string>`:
            // `concretizeMethodForExecution` falls back to the frame's declaring-type generics when
            // given none.
            let state, concretized, _ =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    None
                    (Some extractedTypeArgs)
                    state

            state, ResolvedMemberToken.Method (concretized, method)
        | Choice2Of2 field ->
            // The same projection `UnaryMetadataFieldOps.resolveFieldToken` performs for
            // `ldfld`/`ldsfld`: `resolveMember` has already substituted the parent's instantiation
            // into `field.DeclaringType.Generics`, and this turns that into the declaring type's
            // handle. The field's own signature is deliberately untouched.
            let state, declaringTypeHandle, _ =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            state, ResolvedMemberToken.Field (declaringTypeHandle, field.Handle)

    /// Resolve a `MethodSpecification` — a generic method at a particular instantiation — against
    /// the executing frame's generic context, over either of the two parents a spec may have.
    ///
    /// Returns the concretized method alongside the pre-concretization one, which carries the
    /// metadata names worth logging.
    let private resolveMethodSpecificationToken
        (opName : string)
        (ctx : UnaryMetadataIlOpContext)
        (handle : MethodSpecificationHandle)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let spec = activeAssy.MethodSpecs.[handle]

        match spec.Method with
        | MetadataToken.MethodDef token ->
            let method =
                activeAssy.Methods.[token]
                |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

            let state, concretized, _ =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    (Some spec.Signature)
                    None
                    state

            state, concretized, method
        | MetadataToken.MemberReference ref ->
            // Concretize the spec's generic method args against the current frame's
            // generic context so `resolveMember` can pick the right overload — the
            // member signature may reference these method type parameters by index.
            let state, methodGenerics =
                ((state, []), spec.Signature)
                ||> Seq.fold (fun (state, acc) typeDefn ->
                    let state, concreteType =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            activeAssy.DefinitionFullName
                            currentMethod.DeclaringTypeGenerics
                            currentMethod.Generics
                            typeDefn

                    state, concreteType :: acc
                )

            let methodGenerics = List.rev methodGenerics |> ImmutableArray.CreateRange

            let state, _, method, extractedTypeArgs =
                IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy ref state

            match method with
            | Choice2Of2 _field ->
                // ECMA-335 II.22.29: a MethodSpec's Method column indexes MethodDef or MemberRef,
                // and a MemberRef reached this way must carry a method signature.
                failwith
                    $"%s{opName}: MethodSpecification %O{handle} names a MemberReference that resolves to a field, which is not a generic method"
            | Choice1Of2 method ->
                let state, concretized, _ =
                    ExecutionConcretization.concretizeMethodForExecutionWithConcreteMethodGenerics
                        loggerFactory
                        baseClassTypes
                        thread
                        method
                        methodGenerics
                        (Some extractedTypeArgs)
                        state

                state, concretized, method
        | k -> failwith $"Unrecognised MethodSpecification kind for %s{opName}: %O{k}"

    /// Resolve the method token of an `ldftn`/`ldvirtftn` to the concrete method its function
    /// pointer names, before any virtual dispatch. Returns the concretized method alongside the
    /// pre-concretization one, which carries the metadata names worth logging.
    ///
    /// `opName` names the opcode being executed, so that a token shape we cannot resolve produces
    /// a failure that says which instruction hit it.
    let private resolveMethodPointerTarget
        (opName : string)
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Resolution mirrors `UnaryMetadataCallOps.executeCall`: in-assembly methods arrive as
        // MethodDef (optionally wrapped in a MethodSpec for generic methods), cross-assembly
        // methods arrive as MemberReference (optionally MethodSpec-wrapped). MemberReference
        // resolution must thread the extracted declaring-type generics back to the concretization
        // step, otherwise generic types defined in another assembly would lose their instantiation
        // when projected onto the eval-stack function pointer. For the MethodSpec(MemberReference)
        // case we additionally bypass the default re-substitution path: the spec's method-generic
        // args are caller-relative and have already been resolved against the current frame, so
        // we hand the concrete handles directly to the concretization step rather than letting it
        // re-substitute them against the (target type's) generics.
        let state, concretizedMethod, method =
            match metadataToken with
            | MetadataToken.MethodDef handle ->
                let method =
                    activeAssy.Methods.[handle]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                let state, concretized, _ =
                    ExecutionConcretization.concretizeMethodForExecution
                        loggerFactory
                        baseClassTypes
                        thread
                        method
                        None
                        None
                        state

                state, concretized, method
            | MetadataToken.MemberReference h ->
                match resolveMemberReferenceToken ctx h state with
                | state, ResolvedMemberToken.Method (concretized, method) -> state, concretized, method
                | _, ResolvedMemberToken.Field _ -> failwith $"tried to %s{opName} a field"
            | MetadataToken.MethodSpecification h -> resolveMethodSpecificationToken opName ctx h state
            | t -> failwith $"Unexpectedly asked to %s{opName} a non-method: {t}"

        state, concretizedMethod, method

    /// Push a function pointer to `concretizedMethod` and advance past the instruction.
    let private pushFunctionPointer
        (ctx : UnaryMetadataIlOpContext)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        state
        |> IlMachineState.pushToEvalStack'
            (EvalStackValue.NativeInt (
                NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed concretizedMethod)
            ))
            ctx.Thread
        |> IlMachineState.advanceProgramCounter ctx.Thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeLdftn (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let logger = ctx.Logger
        let thread = ctx.Thread

        let state, concretizedMethod, method = resolveMethodPointerTarget "Ldftn" ctx state

        // `ldftn` is one of the three opcodes ECMA and the JIT importer allow `constrained.` to
        // precede, so it must consume the prefix. Consuming means both halves: *clearing* it, or a
        // later call-like instruction in this frame picks it up (frames start at
        // `PrefixState.empty`, so the damage is frame-scoped but real — see
        // `ConstrainedLdftnPrefixNotLeaked.cs`), and *honouring* it, because the prefix is what
        // directs resolution from an interface's abstract declaration to the constrained type's
        // implementation.
        //
        // Unlike `call`, this never reinstalls the prefix: `ldftn` runs no class initialiser and
        // resolution cannot suspend, so the instruction is atomic and never re-executes.
        let activeFrameId = state.ThreadState.[thread].ActiveMethodState

        let pendingConstrained, state =
            match state.ThreadState.[thread].MethodState.PendingPrefix.Constrained with
            | None -> None, state
            | Some _ as cur ->
                let cleared =
                    state
                    |> IlMachineState.mapFrame
                        thread
                        activeFrameId
                        (fun frame ->
                            { frame with
                                PendingPrefix =
                                    { frame.PendingPrefix with
                                        Constrained = None
                                    }
                            }
                        )

                cur, cleared

        let state, target =
            match pendingConstrained with
            | None -> state, concretizedMethod
            | Some constrainedTypeHandle ->
                let state, implementation, _declaringTypeHandle =
                    UnaryMetadataCallOps.resolveConstrainedStaticInterfaceMethod
                        "Ldftn"
                        ctx
                        constrainedTypeHandle
                        method
                        concretizedMethod
                        state

                state, implementation

        logger.LogDebug (
            "Pushed pointer to function {LdFtnAssembly}.{LdFtnType}.{LdFtnMethodName}",
            method.DeclaringAssemblyFullName,
            method.RequiredDeclaringType.Name,
            method.Name
        )

        pushFunctionPointer ctx target state

    /// ECMA-335 III.4.18. Pops an object reference and pushes a function pointer to the body that
    /// a `callvirt` of the same token on the same receiver would have run.
    ///
    /// The dispatch has to happen here rather than at the call: a delegate built from this pointer
    /// is invoked with `performInterfaceResolution = false` (`AbstractMachine.dispatchDelegateInvoke`),
    /// so whatever method the pointer names is the method that runs. That matches CoreCLR, which
    /// also binds the target eagerly — `Delegate.Equals` compares the stored `_methodPtr`, so two
    /// delegates over receivers of different runtime types must hold different pointers.
    let executeLdvirtftn (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        let state, callSiteMethod, method = resolveMethodPointerTarget "Ldvirtftn" ctx state

        // CoreCLR's importer falls through to its `ldftn` handling when the named method is
        // `static`, `final`, or not `virtual` (`case CEE_LDVIRTFTN`, importer.cpp): the receiver is
        // discarded via `gtUnusedValNode` — note, *without* a null check — and the named method's
        // pointer is pushed. We mirror that rather than ECMA-335's unconditional
        // NullReferenceException, because a spurious throw would change guest control flow against
        // the oracle, whereas the elided check is only ever elided where the receiver's identity
        // provably cannot affect the answer.
        //
        // No guest test reaches this branch. Every `ldvirtftn` Roslyn was observed to emit names a
        // method that is `virtual` and not `final` — even through a `sealed` receiver, and even for
        // a `sealed override`, where the token names the least-derived non-final declaration (see
        // `LdvirtftnVirtualDispatch.cs`) — and the test corpus is compiled from C#, so nothing
        // written there can select this path. Hand-written IL could, which is why the branch exists
        // rather than failing. `MethodInfo.DispatchesVirtually` is unit-tested against
        // `System.Reflection` in `TestDispatchesVirtually.fs`, so the branch *condition* is covered
        // even though the branch body is not.
        if not callSiteMethod.DispatchesVirtually then
            let _receiver, state = IlMachineState.popEvalStack thread state

            logger.LogDebug (
                "Pushed pointer to non-virtual function {LdVirtFtnAssembly}.{LdVirtFtnType}.{LdVirtFtnMethodName}",
                method.DeclaringAssemblyFullName,
                method.RequiredDeclaringType.Name,
                method.Name
            )

            pushFunctionPointer ctx callSiteMethod state
        else

        // `DispatchesVirtually` reads only the method's own `MethodAttributes.Final`, but CoreCLR's
        // `CORINFO_FLG_FINAL` is `IsMdFinal(attribs) || pMT->IsSealed()` (`jitinterface.cpp`,
        // `getMethodAttribsInternal`), so a `virtual` non-`final` method declared on a *sealed* type
        // takes CoreCLR's non-dispatching path — which does not null-check. Dispatching here would
        // pick the same body (nothing derives from a sealed type), so the only difference is that we
        // would throw NullReferenceException on a null receiver where CoreCLR does not.
        //
        // Refuse the shape rather than diverge silently. No C# compiler can produce it: a new
        // `virtual` member on a sealed type is CS0549, and Roslyn marks overrides in sealed types
        // `final`. Reaching this means hand-written IL, and the honest answer there is a loud stop
        // at the faulting instruction rather than a NullReferenceException the real runtime would
        // never have raised. `callvirt` needs no such guard: it null-checks unconditionally, so the
        // omission is unobservable there.
        let declaringTypeIsSealed =
            match state.LoadedAssembly callSiteMethod.DeclaringAssemblyFullName with
            | None ->
                failwith
                    $"Ldvirtftn: declaring assembly for %O{callSiteMethod} is not loaded: %O{callSiteMethod.DeclaringAssemblyFullName}"
            | Some declaringAssy ->
                declaringAssy.TypeDefs.[callSiteMethod.RequiredDeclaringType.Definition.Get].TypeAttributes.HasFlag
                    TypeAttributes.Sealed

        if declaringTypeIsSealed then
            failwith
                $"Ldvirtftn names %O{callSiteMethod}, which is virtual and not final but is declared on a sealed type. CoreCLR sets CORINFO_FLG_FINAL for a sealed declaring type, so it takes the non-dispatching path, which performs no null check; PawPrint would dispatch and would raise NullReferenceException on a null receiver. No C# compiler emits this shape, so this is hand-written IL; teach `MethodInfo.DispatchesVirtually` about sealed declaring types to support it."
        else

        // Peek before popping so that the raise leaves the evaluation stack as the faulting
        // instruction found it, matching `executeCallvirt`'s null check. (Nothing re-executes this
        // instruction, so this is consistency rather than a correctness requirement.)
        match state.ThreadState.[thread].MethodState.EvaluationStack |> EvalStack.Peek with
        | None -> failwith "Ldvirtftn: expected an object reference on the eval stack, but it was empty"
        | Some EvalStackValue.NullObjectRef ->
            // CoreCLR raises this from the dispatch helper (`ResolveVirtualFunctionPointer`,
            // jithelpers.cpp), so it is an ordinary catchable managed exception.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        // ECMA-335 III.4.18 types the popped operand `O`, and PawPrint spells every such receiver
        // — a boxed value type included — as an `ObjectRef`. Anything else is malformed IL rather
        // than a shape to reinterpret, so the arm below says so instead of letting `getTypeOfObj`
        // fail with a message that never mentions the instruction.
        | Some (EvalStackValue.ObjectRef _) ->
            let receiver, state = IlMachineState.popEvalStack thread state

            let state, receiverType =
                IlMachineStateExecution.getTypeOfObj loggerFactory baseClassTypes state receiver

            // Exactly `callvirt`'s dispatch: same resolver, same `walkBaseTypes = true`, same
            // method-generic context. `None` means no override exists, in which case the call
            // site's own method is the answer.
            let state, resolved =
                IlMachineStateExecution.tryResolveVirtualImplementation
                    loggerFactory
                    baseClassTypes
                    thread
                    callSiteMethod.Generics
                    callSiteMethod
                    receiverType
                    true
                    state

            let target = resolved |> Option.defaultValue callSiteMethod

            // Known limit. When the receiver is a boxed
            // value type and the slot resolves to a struct instance method, CoreCLR hands back the
            // *unboxing* entry point — `GetMethodDescOfVirtualizedCode` (method.cpp) passes
            // `pTargetMT->IsValueType()` as `forceBoxedEntryPoint` to
            // `FindOrCreateAssociatedMethodDesc` — whose address differs from `ldftn S::M`. A
            // `FunctionPointerTarget.Managed` names only a method, and its equality is nominal, so
            // the two collapse to one value here.
            //
            // Calling through the pointer is unaffected, which is why the boxed-receiver case in
            // `LdvirtftnVirtualDispatch.cs` passes: `callMethodWithCommitment` converts an
            // `ObjectRef` receiver to a byref into the box for a value-type callee, which is what
            // an unboxing stub does. Only pointer *identity* is lost, and observing that needs two
            // pointers to the same struct method obtained by different routes — which C# cannot
            // express, since it offers no way to take `ldftn` of a struct method against an object
            // receiver.
            //
            // This cannot be guarded the way the sealed-declaring-type case above is: that shape is
            // unreachable from C#, whereas this one is ordinary code (`ICounter c = someStruct;
            // Func<int> f = c.Count;`) that works correctly today, so refusing it would remove
            // working behaviour to protect an unobservable distinction. It is the same missing
            // entry-point flavour already parked against `ActivatorCreateInstanceStructCtor.cs`;
            // both consumers close together when `FunctionPointerTarget` can name one.
            logger.LogDebug (
                "Pushed pointer to virtual function {LdVirtFtnAssembly}.{LdVirtFtnType}.{LdVirtFtnMethodName}, dispatched from {LdVirtFtnCallSite}",
                target.DeclaringAssemblyFullName,
                target.RequiredDeclaringType.Name,
                target.Name,
                method.Name
            )

            pushFunctionPointer ctx target state
        | Some other -> failwith $"Ldvirtftn: expected an object reference receiver on the eval stack, got %O{other}"

    let executeLdtoken (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Pushing a `RuntimeTypeHandle` for a target that is already in hand. `handleTypeToken`
        // below reaches this after resolving a metadata token; a `DynamicScope` operand arrives
        // here directly, its entry being a `RuntimeTypeHandle` the guest already built.
        let pushHandleForTarget (target : RuntimeTypeHandleTarget) (state : IlMachineState) : IlMachineState =
            let ty = baseClassTypes.RuntimeTypeHandle
            let field = ty.Fields |> List.exactlyOne

            if field.Name <> "m_type" then
                failwith $"unexpected field name ${field.Name} for BCL type RuntimeTypeHandle"

            // For a scope operand this is a lookup that cannot miss, rather than an allocation: the
            // target was read back out of a `RuntimeType`'s `m_handle`, and only
            // `TypeHandleRegistry.getOrAllocate` ever plants one there, recording `target -> that
            // same address` as it does. So the guest gets the `RuntimeType` it already had, which is
            // what makes `ReferenceEquals(Type.GetTypeFromHandle(h), typeof(X))` hold.
            let alloc, state =
                IlMachineState.getOrAllocateType loggerFactory baseClassTypes target state

            let state, runtimeTypeHandleHandle =
                DumpedAssembly.typeInfoToTypeDefn'
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.RuntimeTypeHandle
                |> IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            let vt =
                // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L92
                let mTypeField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.RuntimeTypeHandle "m_type"

                FieldIdentity.cliField
                    runtimeTypeHandleHandle
                    mTypeField
                    (CliType.ObjectRef (Some alloc))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeType)
                |> List.singleton
                |> CliValueType.OfFields
                    baseClassTypes
                    state.ConcreteTypes
                    runtimeTypeHandleHandle
                    (DeclaredTypeFacts.ofTypeInfo
                        baseClassTypes
                        state._LoadedAssemblies
                        baseClassTypes.RuntimeTypeHandle)

            IlMachineState.pushToEvalStack (CliType.ValueType vt) thread state

        // `allowOpenGenericDefinition` is PawPrint's spelling of CoreCLR's `PermitUninstDefOrRef`,
        // which `CEEInfo::resolveToken` grants to `ldtoken` and to no other opcode
        // (`vm/jitinterface.cpp`, the `CORINFO_TOKENKIND_Ldtoken` ternary on the `LoadTypeDefOrRef`
        // call). Each arm's value has its own observer, because the arm a type reaches depends on
        // where it is declared: `typeof(List<>)` is a TypeReference and `typeof(Box<>)` a
        // TypeDefinition. Both live in `sourcesPure/LdtokenMemberTokens.cs`, and both are written
        // inside a *generic context* deliberately: with the token's placeholders unbound, clearing
        // the flag still reaches the same open-definition target through
        // `runtimeTypeHandleTargetForTypeToken`'s `containsUnboundGenericParameter` path, so the
        // flag only decides once they are bound.
        let targetForTypeToken
            (declaringAssembly : DumpedAssembly)
            (allowOpenGenericDefinition : bool)
            (typeDefn : TypeDefn)
            (state : IlMachineState)
            : IlMachineState * LdtokenTarget
            =
            let methodGenerics = currentMethod.Generics
            let typeGenerics = currentMethod.DeclaringTypeGenerics

            let state, target =
                IlMachineState.runtimeTypeHandleTargetForTypeToken
                    loggerFactory
                    baseClassTypes
                    declaringAssembly
                    allowOpenGenericDefinition
                    typeGenerics
                    methodGenerics
                    typeDefn
                    state

            state, LdtokenTarget.Type target

        // Classify first, push second. Every refusal below is stated once, and the pushes that
        // follow cannot ask a question this has not answered.
        let state, target: IlMachineState * LdtokenTarget =
            match ctx.LdtokenOperand with
            | ResolvedLdtokenOperand.FromScope target ->
                // Nothing to resolve and nothing to narrow. The entry *is* a `RuntimeTypeHandle`
                // the guest already holds, so unlike the metadata arms below there is no token to
                // look up, no generic context to substitute, and no closedness question: `ldtoken`
                // of an open definition, of a bare generic parameter and of `System.Void` are all
                // measured to run on real .NET, where the eleven consuming opcodes refuse all three.
                state, LdtokenTarget.Type target
            | ResolvedLdtokenOperand.FromMetadata (activeAssy, metadataToken) ->

            match metadataToken with
            | MetadataToken.FieldDefinition h ->
                let field = activeAssy.Fields.[h]

                if not field.DeclaringType.Generics.IsEmpty then
                    // A bare FieldDef names the *typical* instantiation: CoreCLR resolves
                    // `mdtFieldDef` through `MemberLoader::GetFieldDescFromFieldDef`, which takes no
                    // `SigTypeContext` (`vm/jitinterface.cpp`), so the surrounding frame's
                    // instantiation is not what this token means and substituting it would be wrong
                    // rather than merely unsupported. The open form needs a
                    // `RuntimeTypeHandleTarget.OpenGenericTypeDefinition` declaring type, which
                    // nothing yet builds here. (No C# compiler emits this shape; a MemberReference
                    // with a TypeSpec parent, handled below, is what a field of a generic type
                    // reaches through.)
                    failwith
                        $"TODO: ldtoken FieldDef on a generic declaring type names the typical instantiation, which needs an open-generic RuntimeFieldHandle; got %O{field}"

                let concretizationCtx : TypeConcretization.ConcretizationContext<_> =
                    {
                        ConcreteTypes = state.ConcreteTypes
                        LoadedAssemblies = state._LoadedAssemblies
                        BaseTypes = baseClassTypes
                    }

                let closedDeclaringHandle, concretizationCtx =
                    TypeConcretization.concretizeTypeDefinition concretizationCtx field.DeclaringType.Identity

                let state =
                    { state with
                        ConcreteTypes = concretizationCtx.ConcreteTypes
                        _LoadedAssemblies = concretizationCtx.LoadedAssemblies
                    }

                state, LdtokenTarget.Field (RuntimeTypeHandleTarget.Closed closedDeclaringHandle, h)
            | MetadataToken.MethodDef h ->
                let method =
                    activeAssy.Methods.[h]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                // As for FieldDef above: `mdtMethodDef` resolves through
                // `MemberLoader::GetMethodDescFromMethodDef`, which takes no `SigTypeContext`, so
                // both of these name the typical instantiation rather than the frame's.
                if not method.DeclaringTypeGenerics.IsEmpty then
                    failwith
                        $"TODO: ldtoken MethodDef for methods on generic declaring types names the typical instantiation, which needs open generic RuntimeMethodHandle support; got %O{method}"

                if not method.Generics.IsEmpty then
                    failwith
                        $"TODO: ldtoken MethodDef for generic methods names the typical instantiation, which needs open generic RuntimeMethodHandle support; got %O{method}"

                let state, concretizedMethod, _declaringTypeHandle =
                    ExecutionConcretization.concretizeMethodForExecution
                        loggerFactory
                        baseClassTypes
                        thread
                        method
                        None
                        None
                        state

                state, LdtokenTarget.Method concretizedMethod
            | MetadataToken.MemberReference h ->
                // The one token kind that may name either a method or a field, so it is resolved
                // and *then* branched on. CoreCLR resolves it against the enclosing frame's
                // `SigTypeContext`, which is what `resolveMemberReferenceToken` threads.
                match resolveMemberReferenceToken ctx h state with
                | state, ResolvedMemberToken.Method (concretized, _) -> state, LdtokenTarget.Method concretized
                | state, ResolvedMemberToken.Field (declaringType, fieldHandle) ->
                    state, LdtokenTarget.Field (RuntimeTypeHandleTarget.Closed declaringType, fieldHandle)
            | MetadataToken.MethodSpecification h ->
                let state, concretized, _ = resolveMethodSpecificationToken "Ldtoken" ctx h state
                state, LdtokenTarget.Method concretized
            | MetadataToken.TypeSpecification h ->
                // Use the raw TypeSpec signature directly, bypassing the lossy
                // resolveTypeFromDefn → TypeInfo → typeInfoToTypeDefn round-trip.
                // TypeInfo cannot represent array/pointer/byref wrappers, so the
                // round-trip would collapse e.g. typeof(X[]) to typeof(X).
                let sign = activeAssy.TypeSpecs.[h].Signature
                targetForTypeToken activeAssy false sign state
            | MetadataToken.TypeReference h ->
                let typeGenerics = currentMethod.DeclaringTypeGenerics

                let state, typeDefn, assy =
                    IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics h

                targetForTypeToken assy true typeDefn state
            | MetadataToken.TypeDefinition h ->
                let state, typeDefn =
                    IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h

                targetForTypeToken activeAssy true typeDefn state
            | _ ->
                // ECMA-335 III.4.17 admits exactly the kinds handled above.
                failwith $"Unexpected metadata token %O{metadataToken} in LdToken"

        let state =
            match target with
            | LdtokenTarget.Type target -> pushHandleForTarget target state
            | LdtokenTarget.Method concretizedMethod ->
                let runtimeMethodHandle, state =
                    IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedMethod state

                IlMachineState.pushToEvalStack runtimeMethodHandle thread state
            | LdtokenTarget.Field (declaringType, fieldHandle) ->
                let runtimeFieldHandle, state =
                    IlMachineState.getOrAllocateField loggerFactory baseClassTypes declaringType fieldHandle state

                IlMachineState.pushToEvalStack runtimeFieldHandle thread state

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
