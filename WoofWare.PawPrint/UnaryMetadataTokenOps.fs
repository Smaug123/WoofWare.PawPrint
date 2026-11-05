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
    /// A field reference that binds nothing, and why: binding it throws into the guest.
    | BindingFails of FieldReferenceFailure
    /// A reference whose member CoreCLR finds, but through a parent it refuses the instruction over
    /// (`MemberReferenceResolution.RefusedParent`). Never for `ldtoken`.
    | RefusedParent of TypeOperandRefusal

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
    /// What the executing instruction does with a MemberRef it names: `ldtoken` may name a member of
    /// a generic definition's typical instantiation, which every other instruction refuses.
    let private memberReferenceUse (ctx : UnaryMetadataIlOpContext) : MemberReferenceUse =
        match ctx.Op with
        | UnaryMetadataTokenIlOp.Ldtoken -> MemberReferenceUse.Ldtoken
        | op -> MemberReferenceUse.Other (string<UnaryMetadataTokenIlOp> op)

    /// Refuse a `MemberReference` whose parent named a *generic type definition* without supplying
    /// an instantiation -- `G`1::M()` rather than `G`1<int>::M()`.
    ///
    /// A `TypeReference` parent carries no type arguments, which is correct when the referenced
    /// type is non-generic and means the *typical* instantiation when it is not. PawPrint cannot
    /// name that: the handle registries hold `Closed` and `OpenGenericTypeDefinition` declaring
    /// types, and building the latter for a member is the open-generic handle work this does not
    /// do.
    ///
    /// Refused rather than left alone, because the fall-through is silent and wrong rather than
    /// loud. With no arguments extracted, `resolveTargetTypeGenerics` reaches for the *executing
    /// frame's* declaring-type generics, so a caller that happens to be a generic type of the same
    /// arity receives a handle for its own instantiation. Measured: `ldtoken List`1::get_Count()`
    /// from a frame on `Gen<string>` produced a handle for `List<System.String>`.
    ///
    /// No compiler emits this row -- C# reaches a generic type's member through a TypeSpec parent
    /// that spells the instantiation out -- so this guards hand-written IL.
    let private refuseTypicalDeclaringType
        (opName : string)
        (extractedTypeArgs : ImmutableArray<TypeDefn>)
        (declaringTypeGenerics : ImmutableArray<TypeDefn>)
        (describeMember : unit -> string)
        : unit
        =
        if extractedTypeArgs.IsEmpty && not declaringTypeGenerics.IsEmpty then
            failwith
                $"TODO: %s{opName} of a MemberReference whose parent names generic type definition %s{describeMember ()} without an instantiation refers to the typical instantiation, which needs an open-generic declaring type; a TypeSpecification parent, which is what compilers emit, carries the instantiation instead"

    /// Resolve a `MemberReference` an `ldtoken` names against the executing frame's generic context,
    /// as CoreCLR does: `CEEInfo::resolveToken` reaches
    /// `GetTypeContext(pResolvedToken->tokenContext, &typeContext)` before
    /// `MemberLoader::GetDescFromMemberRef`, whatever the opcode (`vm/jitinterface.cpp`). A method
    /// arm is instantiated as every other instruction's is (`MethodTokenBinding`).
    ///
    /// `MethodDef` is not resolved here: CoreCLR resolves it through `GetMethodDescFromMethodDef`,
    /// which takes no `SigTypeContext` at all, so a bare `MethodDef` names the typical
    /// instantiation rather than the frame's.
    let private resolveMemberReferenceToken
        (ctx : UnaryMetadataIlOpContext)
        (handle : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * ResolvedMemberToken
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let state, resolution =
            IlMachineState.resolveMember
                (memberReferenceUse ctx)
                loggerFactory
                baseClassTypes
                thread
                ctx.ActiveAssembly
                handle
                state

        let resolution =
            TokenBindingStop.memberReference ctx.ActiveAssembly handle resolution

        match resolution with
        | MemberReferenceResolution.RefusedParent refusal -> state, ResolvedMemberToken.RefusedParent refusal
        | MemberReferenceResolution.Resolved (_, resolved, extractedTypeArgs) ->

        match resolved with
        | Choice1Of2 method ->
            match
                MethodTokenBinding.instantiateReferencedMethod
                    loggerFactory
                    baseClassTypes
                    thread
                    "ldtoken"
                    method
                    extractedTypeArgs
                    None
                    state
            with
            | state, MethodTokenBinding.Method (concretized, method) ->
                state, ResolvedMemberToken.Method (concretized, method)
            | state, MethodTokenBinding.RefusedParent refusal -> state, ResolvedMemberToken.RefusedParent refusal
            | _, MethodTokenBinding.Unbound unbound -> failwith (UnboundToken.describe unbound)
        | Choice2Of2 (FieldReferenceBinding.Fails failure) -> state, ResolvedMemberToken.BindingFails failure
        | Choice2Of2 (FieldReferenceBinding.Bound field) ->
            refuseTypicalDeclaringType
                "ldtoken"
                extractedTypeArgs
                field.DeclaringType.Generics
                (fun () -> $"%s{field.DeclaringType.Name}::%s{field.Name}")

            // The same projection `UnaryMetadataFieldOps.resolveFieldToken` performs for
            // `ldfld`/`ldsfld`: `resolveMember` has already substituted the parent's instantiation
            // into `field.DeclaringType.Generics`, and this turns that into the declaring type's
            // handle. The field's own signature is deliberately untouched.
            let state, declaringTypeHandleResult =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            let declaringTypeHandle, _ = BindingStop.bound declaringTypeHandleResult

            state, ResolvedMemberToken.Field (declaringTypeHandle, field.Handle)

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
        // `ldftn` is one of the three opcodes ECMA and the JIT importer allow `constrained.` to
        // precede, so it must consume the prefix. Consuming means both halves: *clearing* it, or a
        // later call-like instruction in this frame picks it up (frames start at
        // `PrefixState.empty`, so the damage is frame-scoped but real -- see
        // `ConstrainedLdftnPrefixNotLeaked.cs`), and *honouring* it, because the prefix is what
        // directs resolution from an interface's abstract declaration to the constrained type's
        // implementation.
        //
        // Unlike `call`, this never reinstalls the prefix: `ldftn` runs no class initialiser and
        // resolution cannot suspend, so the instruction is atomic and never re-executes.
        let constrained, state = UnaryMetadataCallOps.takeConstrainedPrefix ctx.Thread state

        let state, binding =
            CallSiteBinding.bindLdftn
                ctx.LoggerFactory
                ctx.BaseClassTypes
                ctx.Thread
                ctx.ActiveAssembly
                constrained
                ctx.MetadataToken
                state

        match binding with
        | CallSiteBinding.Method target ->
            ctx.Logger.LogDebug (
                "Pushed pointer to function {LdFtnAssembly}.{LdFtnType}.{LdFtnMethodName}",
                target.DeclaringAssemblyFullName,
                target.RequiredDeclaringType.Name,
                target.Name
            )

            pushFunctionPointer ctx target state
        | CallSiteBinding.ArrayAccessor accessor ->
            failwith $"BUG: ldftn bound to the multi-dim array accessor %O{accessor.Kind}, which only call names"
        | CallSiteBinding.RaisesWhenExecuted failure ->
            IlMachineStateExecution.raiseDispatchFailure
                ctx.LoggerFactory
                ctx.BaseClassTypes
                (IlMachineStateExecution.DispatchOrigin.Instruction IlMachineStateExecution.DispatchSite.ConstrainedType)
                failure
                ctx.Thread
                state
        | CallSiteBinding.CompilationFails failure -> UnaryMetadataCallOps.raiseCompilationFailure ctx failure state
        | CallSiteBinding.Unbound unbound -> failwith (UnboundToken.describe unbound)

    /// ECMA-335 III.4.18. Pops an object reference and pushes a function pointer to the body that
    /// a `callvirt` of the same token on the same receiver would have run.
    ///
    /// The dispatch has to happen here rather than at the call: a closed delegate built from this
    /// pointer is invoked with `CallDispatch.Direct`
    /// (`AbstractMachine.dispatchDelegateInvoke`), so whatever method the pointer names is the
    /// method that runs. That matches CoreCLR, which
    /// also binds the target eagerly — `Delegate.Equals` compares the stored `_methodPtr`, so two
    /// delegates over receivers of different runtime types must hold different pointers.
    let executeLdvirtftn (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        match
            MethodTokenBinding.bind
                loggerFactory
                baseClassTypes
                thread
                (memberReferenceUse ctx)
                "Ldvirtftn"
                GenericTypeMethodRow.AtExecutingFrame
                ctx.ActiveAssembly
                ctx.MetadataToken
                state
        with
        | _, MethodTokenBinding.Unbound unbound -> failwith (UnboundToken.describe unbound)
        | state, MethodTokenBinding.RefusedParent refusal -> UnaryMetadataIlOpContext.raiseTypeRefusal ctx refusal state
        | state, MethodTokenBinding.Method (callSiteMethod, method) ->

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
                (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.NullReference)
                []
                thread
                state
        // ECMA-335 III.4.18 types the popped operand `O`, and PawPrint spells every such receiver
        // — a boxed value type included — as an `ObjectRef`. Anything else is malformed IL rather
        // than a shape to reinterpret, and the arm after this one says so.
        | Some (EvalStackValue.ObjectRef receiver) ->
            let receiverType = ManagedHeap.getObjectConcreteType receiver state.ManagedHeap

            // Exactly `callvirt`'s dispatch: same resolver, same `walkBaseTypes = true`, same
            // method-generic context. `NotOverridden` means no override exists, in which case the
            // call site's own method is the answer.
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

            let target =
                match resolved with
                | IlMachineStateExecution.ResolvedVirtualCall.Runs implementation -> Ok implementation
                | IlMachineStateExecution.ResolvedVirtualCall.NotOverridden -> Ok callSiteMethod
                | IlMachineStateExecution.ResolvedVirtualCall.Fails failure -> Error failure

            match target with
            | Error failure ->
                // Measured: real .NET raises as it resolves the slot, so no delegate is made.
                IlMachineStateExecution.raiseDispatchFailure
                    loggerFactory
                    baseClassTypes
                    (IlMachineStateExecution.DispatchOrigin.Instruction IlMachineStateExecution.DispatchSite.Receiver)
                    failure
                    thread
                    state
            | Ok target ->

            let _, state = IlMachineState.popEvalStack thread state

            // Known limit. When the receiver is a boxed
            // value type and the slot resolves to a struct instance method, CoreCLR hands back the
            // *unboxing* entry point — `GetMethodDescOfVirtualizedCode` (method.cpp) passes
            // `pTargetMT->IsValueType()` as `forceBoxedEntryPoint` to
            // `FindOrCreateAssociatedMethodDesc` — whose address differs from `ldftn S::M`.
            // `FunctionPointerTarget.UnboxingStub` could name it, but this pushes
            // `FunctionPointerTarget.Managed`, so the two collapse to one value here.
            //
            // Calling through the pointer is unaffected, which is why the boxed-receiver case in
            // `LdvirtftnVirtualDispatch.cs` passes: `callMethodWithCommitment` converts an
            // `ObjectRef` receiver to a byref into the box for a value-type callee, which is what
            // an unboxing stub does. Only pointer *identity* is lost. `RuntimeMethodHandle.GetFunctionPointer`
            // does answer the stub for a struct's virtual method, but C# can hold this pointer only
            // inside a delegate, whose `_methodPtr` is private, so comparing the two takes private
            // reflection.
            //
            // This cannot be guarded the way the sealed-declaring-type case above is: that shape is
            // unreachable from C#, whereas this one is ordinary code (`ICounter c = someStruct;
            // Func<int> f = c.Count;`) that works correctly today, so refusing it would remove
            // working behaviour to protect an unobservable distinction. Nor can this simply push
            // the stub: its main consumer is the delegate constructor, which takes only `Managed`
            // today, and which on CoreCLR (`COMDelegate::DelegateConstruct`) stores the stub for
            // *every* closed delegate over a value-type method, `ldftn`-built ones included.
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
                LoadedTypeInfo.typeInfoToTypeDefn'
                    baseClassTypes
                    state.TypeSystem._LoadedAssemblies
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
                    (AllConcreteTypes.getRequiredNonGenericHandle
                        state.TypeSystem.ConcreteTypes
                        baseClassTypes.RuntimeType)
                |> List.singleton
                |> CliValueType.OfFields
                    baseClassTypes
                    state.TypeSystem.ConcreteTypes
                    runtimeTypeHandleHandle
                    (DeclaredTypeFacts.ofTypeInfo
                        baseClassTypes
                        state.TypeSystem._LoadedAssemblies
                        baseClassTypes.RuntimeTypeHandle)

            IlMachineState.pushToEvalStack (CliType.ValueType vt) thread state

        // `allowOpenGenericDefinition` is PawPrint's spelling of CoreCLR's `PermitUninstDefOrRef`,
        // which `CEEInfo::resolveToken` grants to `ldtoken` and to no other opcode
        // (`vm/jitinterface.cpp`, the `CORINFO_TOKENKIND_Ldtoken` ternary on the `LoadTypeDefOrRef`
        // call).
        //
        // The flag only decides anything once the token's placeholder arguments are *bound* by the
        // enclosing frame: unbound, `runtimeTypeHandleTargetForTypeToken` reaches the same
        // open-definition target through its `containsUnboundGenericParameter` path either way.
        // `sourcesPure/TypeOpenGenericDefinitionInGenericContext.cs` is the guest that binds them,
        // and it observes the `TypeSpecification` and `TypeDefinition` arms.
        //
        // `sourcesPure/TypeOpenGenericDefinitionCrossAssembly.cs` is its cross-assembly twin and
        // observes the `TypeReference` arm. That arm needed the twin because which arm a type
        // reaches depends on where it is declared: `typeof(List<>)` is a TypeReference and
        // `typeof(Box<>)` a TypeDefinition, so one guest cannot stand in for the other.
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

        let operand = ctx.LdtokenOperand

        // Classify first, push second. Every refusal below is stated once, and the pushes that
        // follow cannot ask a question this has not answered.
        // `Error` is a field MemberRef that binds nothing, and why.
        let state, (target : Result<LdtokenTarget, FieldReferenceFailure>) =
            match operand with
            | ResolvedLdtokenOperand.FromScope target ->
                // Nothing to resolve and nothing to narrow. The entry *is* a `RuntimeTypeHandle`
                // the guest already holds, so unlike the metadata arms below there is no token to
                // look up, no generic context to substitute, and no closedness question: `ldtoken`
                // of an open definition, of a bare generic parameter and of `System.Void` are all
                // measured to run on real .NET, where the eleven consuming opcodes refuse all three.
                state, Ok (LdtokenTarget.Type target)
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
                        ConcreteTypes = state.TypeSystem.ConcreteTypes
                        LoadedAssemblies = state.TypeSystem._LoadedAssemblies
                        BaseTypes = baseClassTypes
                    }

                let closedDeclaringHandle, concretizationCtx =
                    TypeConcretization.concretizeTypeDefinition
                        (TypeResolution.directoryLoader loggerFactory state.DotnetRuntimeDirs)
                        concretizationCtx
                        field.DeclaringType.Identity

                let state =
                    { state with
                        TypeSystem =
                            { state.TypeSystem with
                                ConcreteTypes = concretizationCtx.ConcreteTypes
                                _LoadedAssemblies = concretizationCtx.LoadedAssemblies
                            }
                    }

                state,
                Ok (LdtokenTarget.Field (RuntimeTypeHandleTarget.Closed (BindingStop.bound closedDeclaringHandle), h))
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

                let state, concretizedMethodResult =
                    ExecutionConcretization.concretizeMethodForExecution
                        loggerFactory
                        baseClassTypes
                        thread
                        method
                        None
                        None
                        state

                let concretizedMethod, _declaringTypeHandle =
                    BindingStop.bound concretizedMethodResult

                state, Ok (LdtokenTarget.Method concretizedMethod)
            | MetadataToken.MemberReference h ->
                // The one token kind that may name either a method or a field, so it is resolved
                // and *then* branched on. CoreCLR resolves it against the enclosing frame's
                // `SigTypeContext`, which is what `resolveMemberReferenceToken` threads.
                match resolveMemberReferenceToken ctx h state with
                | state, ResolvedMemberToken.Method (concretized, _) -> state, Ok (LdtokenTarget.Method concretized)
                | state, ResolvedMemberToken.Field (declaringType, fieldHandle) ->
                    state, Ok (LdtokenTarget.Field (RuntimeTypeHandleTarget.Closed declaringType, fieldHandle))
                | state, ResolvedMemberToken.BindingFails failure -> state, Error failure
                | _, ResolvedMemberToken.RefusedParent refusal ->
                    failwith
                        $"BUG: ldtoken's MemberReference was refused for naming %O{refusal}, but resolveMember refuses no reference for ldtoken"
            | MetadataToken.MethodSpecification h ->
                match
                    MethodTokenBinding.bindMethodSpecification
                        loggerFactory
                        baseClassTypes
                        thread
                        (memberReferenceUse ctx)
                        "Ldtoken"
                        activeAssy
                        h
                        state
                with
                | state, MethodTokenBinding.Method (concretized, _) -> state, Ok (LdtokenTarget.Method concretized)
                | _, MethodTokenBinding.Unbound unbound -> failwith (UnboundToken.describe unbound)
                | _, MethodTokenBinding.RefusedParent refusal ->
                    failwith
                        $"BUG: ldtoken's MethodSpecification was refused for naming %O{refusal}, but resolveMember refuses no reference for ldtoken"
            | MetadataToken.TypeSpecification h ->
                // Use the raw TypeSpec signature directly, bypassing the lossy
                // resolveTypeFromDefn → TypeInfo → typeInfoToTypeDefn round-trip.
                // TypeInfo cannot represent array/pointer/byref wrappers, so the
                // round-trip would collapse e.g. typeof(X[]) to typeof(X).
                let sign = activeAssy.TypeSpecs.[h].Signature
                targetForTypeToken activeAssy false sign state |> Tuple.rmap Ok
            | MetadataToken.TypeReference h ->
                let state, typeDefnResult =
                    IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy h

                let typeDefn, assy =
                    TokenBindingStop.typeToken activeAssy (MetadataToken.TypeReference h) typeDefnResult

                targetForTypeToken assy true typeDefn state |> Tuple.rmap Ok
            | MetadataToken.TypeDefinition h ->
                let state, typeDefn =
                    IlMachineState.lookupTypeDefn loggerFactory baseClassTypes state activeAssy h

                let typeDefn =
                    TokenBindingStop.typeToken activeAssy (MetadataToken.TypeDefinition h) typeDefn

                targetForTypeToken activeAssy true typeDefn state |> Tuple.rmap Ok
            | _ ->
                // ECMA-335 III.4.17 admits exactly the kinds handled above.
                failwith $"Unexpected metadata token %O{metadataToken} in LdToken"

        match target with
        | Error failure -> UnaryMetadataFieldOps.throwBindingFailure ctx failure state
        | Ok target ->

        // `typeof(X).IsValueType` is folded to its answer here, at the `ldtoken`, as the JIT folds
        // it; see `TypeofIntrinsicFold`. Only a metadata token names a closed type the two calls
        // after it can be read against: a `DynamicScope` operand's neighbours name scope entries.
        let state, folded =
            match target, operand with
            | LdtokenTarget.Type (RuntimeTypeHandleTarget.Closed handle),
              ResolvedLdtokenOperand.FromMetadata (activeAssy, _) ->
                TypeofIntrinsicFold.tryFoldIsValueType
                    loggerFactory
                    baseClassTypes
                    thread
                    activeAssy
                    currentMethod
                    handle
                    state
            | _ -> state, false

        if folded then
            state, WhatWeDid.Executed
        else

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
