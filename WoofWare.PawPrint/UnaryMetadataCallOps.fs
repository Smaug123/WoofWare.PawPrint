namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataCallOps =
    /// Per-dimension bounds check for a multi-dim array access. Indices smaller than
    /// the running flat-offset upper bound can still target the wrong cell — e.g.
    /// `arr[1, 5]` on a `[3, 4]` array has flat offset 9 (< total length 12) but lies
    /// in the row-1 region rather than producing IndexOutOfRangeException.
    let private indicesOutOfRange (lengths : ImmutableArray<int>) (indices : int[]) : bool =
        let mutable bad = false

        for k = 0 to indices.Length - 1 do
            if indices.[k] < 0 || indices.[k] >= lengths.[k] then
                bad <- true

        bad

    /// Row-major flatten: ECMA layout for `arr[i_0, ..., i_{n-1}]` with lengths
    /// `[L_0, ..., L_{n-1}]` is `((((i_0)*L_1)+i_1)*L_2 + i_2)*...*L_{n-1} + i_{n-1}`.
    let private rowMajorOffset (lengths : ImmutableArray<int>) (indices : int[]) : int =
        // The iterative form starting from `flat = 0` reproduces the ECMA formula because
        // the first multiplication (by L_0) is on a zero accumulator.
        let mutable flat = 0

        for k = 0 to indices.Length - 1 do
            flat <- flat * lengths.[k] + indices.[k]

        flat

    /// Pop `rank` Int32 indices off the eval stack — the topmost popped value is the
    /// rightmost index (dimension `rank-1`) — followed by the array reference. Returns
    /// the indices in dimension order along with the array address (`None` if the
    /// receiver was null; callers should raise `NullReferenceException`).
    let private popMultiDimIndicesAndArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (rank : int)
        (state : IlMachineState)
        : int[] * ManagedHeapAddress option * IlMachineState
        =
        let indices = Array.zeroCreate<int> rank
        let mutable s = state

        for i = rank - 1 downto 0 do
            let v, s' = IlMachineState.popEvalStack thread s

            match v with
            | EvalStackValue.Int32 (Int32Source.Verbatim n) ->
                indices.[i] <- n
                s <- s'
            | other ->
                failwith $"unexpectedly popped non-Int32 value %O{other} as multi-dim array index at dimension %d{i}"

        let arrEval, s = IlMachineState.popEvalStack thread s
        let arrAddr = IlMachineState.evalStackValueToObjectRef baseClassTypes s arrEval
        indices, arrAddr, s

    /// Implements `call instance void T[<rank>]::Set(int32, ..., int32, T)` — the
    /// runtime-synthesized element-store operation for a multi-dimensional array of
    /// element type `elementType`. Pops the value (top of stack), then `rank` Int32
    /// indices (top-of-stack is the rightmost), then the array reference. Per-dimension
    /// bounds violations raise `IndexOutOfRangeException`; on success the coerced value
    /// is written into the row-major backing store at the computed flat offset.
    let private executeMultiDimArraySet
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (elementType : TypeDefn)
        (rank : int)
        (signature : MemberSignature)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Rank-1 ELEMENT_TYPE_ARRAY morphs to SZARRAY at runtime per ECMA-335 II.14.2;
        // the symmetric morphing in `executeMultiDimArrayNewobj` isn't implemented either,
        // and C# never emits this form.
        if rank < 2 then
            failwith
                $"TODO: multi-dim array Set on rank-%d{rank} ELEMENT_TYPE_ARRAY; rank-1 should morph to SZARRAY per CoreCLR semantics"

        let methodSig =
            match signature with
            | MemberSignature.Method m -> m
            | MemberSignature.Field _ ->
                failwith $"BUG: multi-dim array Set for rank %d{rank} had a field signature; expected method signature"

        // Zero-lower-bound form: `rank` Int32 indices followed by the element value.
        // ECMA-335 II.14.2 also defines a 2*rank Int32 form for non-zero lower bounds,
        // which is not yet supported (C# never emits it).
        if methodSig.ParameterTypes.Length <> rank + 1 then
            failwith
                $"TODO: multi-dim array Set for rank %d{rank} had %d{methodSig.ParameterTypes.Length} parameters; only the zero-lower-bound form (%d{rank} Int32 indices + 1 value) is implemented"

        let value, state = IlMachineState.popEvalStack thread state

        let indices, arrAddrOpt, state =
            popMultiDimIndicesAndArray baseClassTypes thread rank state

        match arrAddrOpt with
        | None ->
            // Don't advance PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->

        let arrObj =
            match ManagedHeap.tryGetArrayShape arrAddr state.ManagedHeap with
            | Some v -> v
            | None -> failwith $"multi-dim array Set: array allocation not found at %O{arrAddr}"

        if arrObj.Lengths.Length <> rank then
            failwith
                $"multi-dim array Set: rank %d{rank} from metadata does not match the allocated array's rank %d{arrObj.Lengths.Length} at %O{arrAddr}"

        if indicesOutOfRange arrObj.Lengths indices then
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.IndexOutOfRangeException
                thread
                state
        else

        let flat = rowMajorOffset arrObj.Lengths indices

        let typeGenerics = currentMethod.DeclaringTypeGenerics
        let methodGenerics = currentMethod.Generics

        let state, zeroOfType, _elementHandle =
            IlMachineState.cliTypeZeroOf
                loggerFactory
                baseClassTypes
                activeAssy
                elementType
                typeGenerics
                methodGenerics
                state

        // ECMA-335 III.4.x runtime-assignment-compatibility gate. A covariant
        // view (e.g. `object[,]` aliasing `string[,]`) must reject stores whose
        // value's runtime type is not assignable to the array's stored element
        // type, raising ArrayTypeMismatchException. Null and value-typed-element
        // arrays pass through unchanged.
        match
            IlMachineStateExecution.checkArrayStoreVariance loggerFactory baseClassTypes thread arrAddr value state
        with
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Raised state -> state, WhatWeDid.Executed
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Allowed state ->

        let coerced = EvalStackValue.toCliTypeCoerced zeroOfType value

        let state =
            IlMachineState.setArrayValue arrAddr coerced flat state
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    /// Implements `call instance T T[<rank>]::Get(int32, ..., int32)` — the
    /// runtime-synthesized element-load operation for a multi-dimensional array of
    /// element type `elementType`. Pops `rank` Int32 indices (top-of-stack is the
    /// rightmost) and the array reference, then pushes the loaded element. Per-dimension
    /// bounds violations raise `IndexOutOfRangeException`.
    let private executeMultiDimArrayGet
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (rank : int)
        (signature : MemberSignature)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        if rank < 2 then
            failwith
                $"TODO: multi-dim array Get on rank-%d{rank} ELEMENT_TYPE_ARRAY; rank-1 should morph to SZARRAY per CoreCLR semantics"

        let methodSig =
            match signature with
            | MemberSignature.Method m -> m
            | MemberSignature.Field _ ->
                failwith $"BUG: multi-dim array Get for rank %d{rank} had a field signature; expected method signature"

        if methodSig.ParameterTypes.Length <> rank then
            failwith
                $"TODO: multi-dim array Get for rank %d{rank} had %d{methodSig.ParameterTypes.Length} parameters; only the zero-lower-bound form (%d{rank} Int32 indices) is implemented"

        let indices, arrAddrOpt, state =
            popMultiDimIndicesAndArray baseClassTypes thread rank state

        match arrAddrOpt with
        | None ->
            // Don't advance PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->

        let arrObj =
            match ManagedHeap.tryGetArrayShape arrAddr state.ManagedHeap with
            | Some v -> v
            | None -> failwith $"multi-dim array Get: array allocation not found at %O{arrAddr}"

        if arrObj.Lengths.Length <> rank then
            failwith
                $"multi-dim array Get: rank %d{rank} from metadata does not match the allocated array's rank %d{arrObj.Lengths.Length} at %O{arrAddr}"

        if indicesOutOfRange arrObj.Lengths indices then
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.IndexOutOfRangeException
                thread
                state
        else

        let flat = rowMajorOffset arrObj.Lengths indices

        let state =
            state
            |> IlMachineState.pushToEvalStack (IlMachineState.getArrayValue arrAddr flat state) thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    /// Implements `call instance T& T[<rank>]::Address(int32, ..., int32)` — the
    /// runtime-synthesized element-address operation for a multi-dimensional array of
    /// element type `elementType`. Pops `rank` Int32 indices (top-of-stack is the
    /// rightmost) and the array reference, then pushes a managed byref to the slot at
    /// the row-major flat offset.
    ///
    /// ECMA-335 III.4.10: without the `readonly.` prefix, the metadata-derived element
    /// type must exactly equal the array's stored element type (no assignment-compat
    /// fallback); otherwise `ArrayTypeMismatchException`. With `readonly.`, the result
    /// is a controlled-mutability byref and the check is suppressed — matching the
    /// szarray `ldelema` precedent in `UnaryMetadataArrayOps.executeLdelema`.
    let private executeMultiDimArrayAddress
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (elementType : TypeDefn)
        (rank : int)
        (signature : MemberSignature)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        if rank < 2 then
            failwith
                $"TODO: multi-dim array Address on rank-%d{rank} ELEMENT_TYPE_ARRAY; rank-1 should morph to SZARRAY per CoreCLR semantics"

        let methodSig =
            match signature with
            | MemberSignature.Method m -> m
            | MemberSignature.Field _ ->
                failwith
                    $"BUG: multi-dim array Address for rank %d{rank} had a field signature; expected method signature"

        if methodSig.ParameterTypes.Length <> rank then
            failwith
                $"TODO: multi-dim array Address for rank %d{rank} had %d{methodSig.ParameterTypes.Length} parameters; only the zero-lower-bound form (%d{rank} Int32 indices) is implemented"

        // ECMA-335 III.2.2: capture-and-clear the `readonly.` prefix. The prefix's scope
        // is exactly this instruction, so we always clear it; we capture into `wasReadonly`
        // so the element-type check below can branch on it.
        let activeFrameId = state.ThreadState.[thread].ActiveMethodState
        let wasReadonly = state.ThreadState.[thread].MethodState.PendingPrefix.Readonly

        let state =
            if wasReadonly then
                state
                |> IlMachineState.mapFrame
                    thread
                    activeFrameId
                    (fun frame ->
                        { frame with
                            PendingPrefix =
                                { frame.PendingPrefix with
                                    Readonly = false
                                }
                        }
                    )
            else
                state

        let indices, arrAddrOpt, state =
            popMultiDimIndicesAndArray baseClassTypes thread rank state

        match arrAddrOpt with
        | None ->
            // Don't advance PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->

        let arrObj =
            match ManagedHeap.tryGetArrayShape arrAddr state.ManagedHeap with
            | Some v -> v
            | None -> failwith $"multi-dim array Address: array allocation not found at %O{arrAddr}"

        if arrObj.Lengths.Length <> rank then
            failwith
                $"multi-dim array Address: rank %d{rank} from metadata does not match the allocated array's rank %d{arrObj.Lengths.Length} at %O{arrAddr}"

        if indicesOutOfRange arrObj.Lengths indices then
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.IndexOutOfRangeException
                thread
                state
        else

        let flat = rowMajorOffset arrObj.Lengths indices

        let buildResult (state : IlMachineState) : IlMachineState * WhatWeDid =
            let result =
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrAddr, flat), [])
                |> EvalStackValue.ManagedPointer

            let state =
                IlMachineState.pushToEvalStack' result thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        if wasReadonly then
            // The readonly. prefix produces a controlled-mutability byref and suppresses
            // the array-element-type check.
            buildResult state
        else

        let typeGenerics = currentMethod.DeclaringTypeGenerics
        let methodGenerics = currentMethod.Generics

        let state, _zeroOfType, tokenElementHandle =
            IlMachineState.cliTypeZeroOf
                loggerFactory
                baseClassTypes
                activeAssy
                elementType
                typeGenerics
                methodGenerics
                state

        let arrayElementHandle =
            match arrObj.ConcreteType with
            | ConcreteTypeHandle.Array (h, _) -> h
            | other ->
                failwith $"BUG: multi-dim array Address: array at %O{arrAddr} has non-Array ConcreteType %O{other}"

        if tokenElementHandle <> arrayElementHandle then
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.ArrayTypeMismatchException
                thread
                state
        else
            buildResult state

    /// Detect `call` targeting a runtime-synthesized member on a multi-dim array
    /// (ECMA-335 II.14.2): a MemberReference whose parent TypeSpec is `TypeDefn.Array`.
    /// `newobj` for the synthesized ctor is handled in `executeNewobj`; this entry
    /// point covers the `Set`/`Get`/`Address` members invoked via plain `call`. Returns
    /// `None` for ordinary method calls so they take the normal resolution path.
    let private tryGetMultiDimArrayCall
        (activeAssy : DumpedAssembly)
        (metadataToken : MetadataToken)
        : (string * TypeDefn * int * MemberSignature) option
        =
        match metadataToken with
        | MetadataToken.MemberReference mrHandle ->
            match activeAssy.Members.TryGetValue mrHandle with
            | true, memberRef ->
                match memberRef.Parent with
                | MetadataToken.TypeSpecification specHandle ->
                    match activeAssy.TypeSpecs.TryGetValue specHandle with
                    | true, ts ->
                        match ts.Signature with
                        | TypeDefn.Array (elt, rank) ->
                            let name = activeAssy.Strings memberRef.Name
                            Some (name, elt, rank, memberRef.Signature)
                        | _ -> None
                    | false, _ -> None
                | _ -> None
            | false, _ -> None
        | _ -> None

    /// Resolve a `constrained.`-prefixed reference to a static abstract interface member down to
    /// the implementation the constrained type supplies, returning it alongside its declaring
    /// type's handle.
    ///
    /// Shared by `constrained. call` and `constrained. ldftn`, which pick their target the same
    /// way: CoreCLR routes both through `getCallInfo` with the constrained token, and the switch
    /// there is `pConstrainedResolvedToken != NULL && pMD->IsInterface() && pMD->IsStatic()`
    /// (`jitinterface.cpp`, `getCallInfo`). That test is computed before anything branches on
    /// `CORINFO_CALLINFO_LDFTN`, so the *method chosen* cannot differ between the two opcodes;
    /// what differs afterwards is only what the caller does with it.
    ///
    /// `opName` names the prefixed instruction (`constrained.call` / `Ldftn`), so a failure says
    /// which one hit it rather than always blaming `call`.
    ///
    /// The instance-receiver forms of the prefix (`CORINFO_DEREF_THIS` / `CORINFO_BOX_THIS`) are
    /// not implemented: Roslyn emits `constrained.` before `ldftn` only for static
    /// abstract interface members, and before `call`/`callvirt` the instance cases are handled by
    /// `executeCallvirt`'s own transformation. Anything else fails loudly here rather than being
    /// guessed at.
    let resolveConstrainedStaticInterfaceMethod
        (opName : string)
        (ctx : UnaryMetadataIlOpContext)
        (constrainedTypeHandle : ConcreteTypeHandle)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          ConcreteTypeHandle
        =
        let methodDeclAssy =
            state._LoadedAssemblies.ByDefinitionName methodToCall.DeclaringAssemblyFullName

        let methodDeclType =
            methodDeclAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        if not methodToCall.IsStatic || not methodDeclType.IsInterface then
            failwith
                $"%s{opName}: expected a static interface method, got %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name}"

        match constrainedTypeHandle with
        | ConcreteTypeHandle.Concrete _ ->
            // Registration is checked eagerly, and separately from rendering: an unregistered
            // handle would otherwise surface as a confusing resolution failure below rather than
            // as the bookkeeping error it is.
            if (AllConcreteTypes.lookup constrainedTypeHandle state.ConcreteTypes).IsNone then
                failwith $"%s{opName}: constrained type handle %O{constrainedTypeHandle} is not registered"
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"%s{opName}: static interface dispatch for non-concrete constrained type %O{constrainedTypeHandle} is not implemented"

        let state, implementation =
            IlMachineStateExecution.tryResolveVirtualImplementation
                ctx.LoggerFactory
                ctx.BaseClassTypes
                ctx.Thread
                concretizedMethod.Generics
                concretizedMethod
                constrainedTypeHandle
                true
                state

        match implementation with
        | None ->
            let constrained =
                AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes constrainedTypeHandle

            failwith $"%s{opName}: could not find static implementation of %s{methodToCall.Name} on %s{constrained}"
        | Some implementation when not implementation.IsStatic ->
            failwith
                $"%s{opName}: resolved non-static implementation %s{MethodOwner.describe implementation.Owner}::%s{implementation.Name}"
        | Some implementation ->
            let declaringTypeHandle =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    implementation.RequiredDeclaringType.Identity
                    implementation.DeclaringTypeGenerics
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{opName}: resolved implementation declaring type %s{MethodOwner.describe implementation.Owner} is not registered"
                )

            state, implementation, declaringTypeHandle

    /// Refuse a `call`/`callvirt` whose arguments violate ECMA-335 III.3.19: each argument must be
    /// assignable to its declared parameter type. Returns `state` unchanged when every argument
    /// PawPrint is able to check satisfies that, and throws otherwise.
    ///
    /// Real .NET does not check this, so refusing declines a program the CLR would have run. That
    /// is the intended trade. CoreCLR shares one instance `FieldDesc` across compatible generic
    /// instantiations (`methodtable.h:1964`) whereas PawPrint keys field storage on the exact
    /// instantiation, so an argument of the wrong instantiation can yield a wrong answer rather
    /// than a late failure, and the callee is the wrong place to discover it.
    ///
    /// Only `ObjectRef` values passed to reference-typed parameters are checked. Anything else is
    /// left unchecked rather than presumed correct.
    ///
    /// `checkReceiver` is set for a non-virtual `call`, where the token fixes the callee outright,
    /// so a receiver of the wrong instantiation reaches a body that reads fields through the
    /// declared one. It is not set for `callvirt`, which resolves the callee *from* the receiver,
    /// making the receiver's type a dispatch question rather than an argument-passing one.
    ///
    /// The `SprintfBasic` F# case guards against over-refusal: it makes the same
    /// `newobj PrintfFormat`5` / `call` taking the `PrintfFormat`4` base, with a consistent
    /// instantiation, and must keep passing.
    let private refuseUnverifiableArguments
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : string)
        (checkReceiver : bool)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let parameters = concretizedMethod.Signature.ParameterTypes
        let arity = List.length parameters

        // `tryIsValueType` answers `None` for a structural handle, which by design has no TypeDef to
        // ask. An array is a reference type all the same, so leaving it to that lookup would skip
        // every array-typed parameter. Byrefs, pointers and function pointers arrive as
        // `ManagedPointer` or `NativeInt` rather than `ObjectRef`, so they never reach the test.
        let isReferenceType (state : IlMachineState) (declared : ConcreteTypeHandle) : bool =
            match declared with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> false
            | ConcreteTypeHandle.Concrete _ ->
                AllConcreteTypes.tryIsValueType baseClassTypes state._LoadedAssemblies state.ConcreteTypes declared = Some
                    false

        let refuse
            (state : IlMachineState)
            (what : string)
            (actual : ConcreteTypeHandle)
            (declared : ConcreteTypeHandle)
            : unit
            =
            let describe = AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes

            failwith
                $"Unverifiable %s{op} of %s{concretizedMethod.Name}: %s{what} has type %s{describe actual}, which is not assignable to the declared type %s{describe declared}. ECMA-335 III.3.19 requires each argument to be assignable to its declared parameter type; real .NET does not check this and would run the call, so this is a defect in the guest image rather than in the interpreter. PawPrint refuses because it keys field storage on the exact generic instantiation, so continuing risks a wrong answer rather than merely a late failure."

        // The receiver sits directly beneath the arguments.
        let state =
            if not checkReceiver || concretizedMethod.IsStatic then
                state
            else

            let declaring =
                concretizedMethod.TryDeclaringType
                |> Option.bind (fun ct ->
                    AllConcreteTypes.findExistingConcreteType state.ConcreteTypes ct.Identity ct.Generics
                )

            match
                declaring,
                state.ThreadState.[thread].MethodState.EvaluationStack
                |> EvalStack.PeekNthFromTop arity
            with
            | Some declaring, Some (EvalStackValue.ObjectRef addr) when isReferenceType state declaring ->
                let actual = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

                let state, assignable =
                    IlMachineStateExecution.isAssignableFrom loggerFactory baseClassTypes actual declaring state

                if not assignable then
                    refuse state "the receiver" actual declaring

                state
            | _ -> state

        // Arguments sit above the receiver, last argument on top: declared parameter `index` is
        // `arity - 1 - index` slots down. The receiver of an instance method is at `arity`, and is
        // not checked: `callvirt` resolves the callee from it, so a receiver of the wrong type is a
        // dispatch question rather than an argument-passing one.
        ((state, 0), parameters)
        ||> List.fold (fun (state, index) declared ->
            let argument =
                state.ThreadState.[thread].MethodState.EvaluationStack
                |> EvalStack.PeekNthFromTop (arity - 1 - index)

            // `ObjectRef` is the only shape with a metadata type to compare against the
            // declaration: `getTypeOfObj` is partial over `NativeInt`, `ManagedPointer` and
            // `UserDefinedValueType`, so asking about those would turn a diagnostic into a crash.
            //
            // Value-typed parameters are excluded because an object reference arriving at one is
            // usually PawPrint's representation rather than a guest defect: a `RuntimeTypeHandle`
            // argument reaches `Type.GetTypeFromHandle` as an `ObjectRef` to the
            // `System.RuntimeType` it wraps, and `RuntimeFieldHandle` reaches
            // `RuntimeHelpers.CreateSpan` as a `RuntimeFieldInfoStub`. Assignability between
            // reference types is the question ECMA's rule asks; boxing and handle representation
            // are a different matter.
            //
            // The reference-type question is asked second because answering it can walk the
            // assembly and TypeDef tables, and this runs on every interpreted call. Whole check
            // measured by `WoofWare.PawPrint.Performance` at +3.6% on the reference-argument-heavy
            // guest (783.4ms vs 756.4ms) and within noise on the stack-heavy one, which passes only
            // ints and so reaches neither that lookup nor the assignability test.
            match argument with
            | Some (EvalStackValue.ObjectRef addr) when isReferenceType state declared ->
                let actual = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

                let state, assignable =
                    IlMachineStateExecution.isAssignableFrom loggerFactory baseClassTypes actual declared state

                if not assignable then
                    refuse state $"argument %d{index}" actual declared

                state, index + 1
            | _ -> state, index + 1
        )
        |> fst

    /// Push a frame for the method `concretizedMethod`, having already resolved it however this
    /// opcode's operand demanded. Shared by the metadata and `DynamicScope` paths of `executeCall`
    /// so that the two cannot drift on how the callee is entered.
    ///
    /// No class-initialisation check here: `callMethod` arms it on the callee's frame and the
    /// dispatch loop runs it as that frame's prologue, which is where the CLR puts it. This call
    /// therefore always commits, and the opcode never re-executes, so nothing needs to reinstall
    /// the `constrained.` prefix.
    let private enterCallee
        (ctx : UnaryMetadataIlOpContext)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let state =
            refuseUnverifiableArguments
                ctx.LoggerFactory
                ctx.BaseClassTypes
                "call"
                true
                concretizedMethod
                ctx.Thread
                state

        let threadState = state.ThreadState.[ctx.Thread]

        let state, commitment =
            IlMachineStateExecution.callMethodWithCommitment
                ctx.LoggerFactory
                ctx.BaseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                true
                IlMachineStateExecution.CallSiteTransition.StaysCooperative
                concretizedMethod.Generics
                concretizedMethod
                ctx.Thread
                threadState
                None
                ReturnValueDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state

        match commitment with
        | IlMachineStateExecution.CallCommitment.Aborted fatal -> state, WhatWeDid.Aborted fatal
        | IlMachineStateExecution.CallCommitment.Committed
        | IlMachineStateExecution.CallCommitment.Raised -> state, WhatWeDid.Executed

    let executeCall (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        // Split on the operand before anything else: `ctx.ActiveAssembly` and `ctx.MetadataToken`
        // are partial, and a scope operand has neither, so binding them eagerly would fail for a
        // dynamic method's `call` even though nothing below would have used them.
        match ctx.Operand with
        | ResolvedMetadataOperand.ScopeMethod handle ->
            // Everything the metadata path does between here and `enterCallee` is token resolution:
            // multi-dim array Get/Set synthesis, MethodSpec/MemberRef lookup, and concretising the
            // result against the caller's generic context. None of it applies. A `DynamicMethod` is
            // never generic and never an instance method, and the operand is already the callee's
            // identity, so the only step left is turning that identity into a frame's worth of
            // method — which is the same step `CreateDelegate`'s invocation path takes, latching
            // `initLocals` and the callee's `catch` clause types on first execution.
            //
            // No `constrained.` handling either, and no arm to refuse one: a dynamic method's body
            // cannot contain `constrained.` at all, because that prefix's own operand is a scope
            // entry which `IlDecoding.scopeOperandKind` refuses, so the decoder rejects such a body
            // when the method is minted. Nothing can be pending here.
            let state, concretizedMethod =
                DynamicMethodExecution.concretize
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    "call naming a DynamicScope entry"
                    handle
                    state

            match concretizedMethod with
            | Ok concretizedMethod -> enterCallee ctx concretizedMethod state
            | Error (exceptionType, why) ->
                // The callee could not be compiled. Real .NET raises this from the *caller's* call
                // site, because that is where the callee's first JIT happens, so the caller's own
                // handlers see it — which is what raising here rather than pushing a frame gives.
                // Don't advance the PC: exception dispatch needs the faulting instruction's offset.
                ctx.Logger.LogWarning ("call refused a DynamicMethod callee: {Reason}", why)

                IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    exceptionType
                    (DynamicScopeOperand.clrMessageFor ctx.BaseClassTypes exceptionType)
                    ctx.Thread
                    state
        | ResolvedMetadataOperand.FromMetadata _
        | ResolvedMetadataOperand.ScopeType _
        | ResolvedMetadataOperand.ScopeField _
        | ResolvedMetadataOperand.ScopeTypeTarget _ ->

        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Multi-dimensional array Get/Set are runtime-synthesized (ECMA-335 II.14.2): the
        // metadata token is a MemberReference whose parent is a TypeSpec of TypeDefn.Array.
        // There's no managed body to resolve, so route to the inline element-access path
        // before invoking ordinary member resolution (which would look up the name on
        // System.Array and fail).
        match tryGetMultiDimArrayCall activeAssy metadataToken with
        | Some (name, elt, rank, sig0) ->
            match name with
            | "Set" -> executeMultiDimArraySet ctx state elt rank sig0
            | "Get" -> executeMultiDimArrayGet ctx state rank sig0
            | "Address" -> executeMultiDimArrayAddress ctx state elt rank sig0
            | other ->
                failwith
                    $"unexpected synthesized member %s{other} on multi-dim array (rank %d{rank}); expected Get/Set/Address/.ctor"
        | None ->

        // For MethodSpec(MemberReference) the spec's method-generic args are caller-relative
        // and have already been substituted against the current frame to drive overload
        // resolution; we surface them as `preConcretizedMethodGenerics` so the concretization
        // step below uses them directly rather than re-substituting against the (target type's)
        // generics, which would be the wrong context.
        let state, methodToCall, methodGenerics, typeArgsFromMetadata, preConcretizedMethodGenerics =
            match metadataToken with
            | MetadataToken.MethodSpecification h ->
                let spec = activeAssy.MethodSpecs.[h]

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

                match spec.Method with
                | MetadataToken.MethodDef token ->
                    let method =
                        activeAssy.Methods.[token]
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    state, method, Some spec.Signature, None, None
                | MetadataToken.MemberReference ref ->
                    let state, _, method, extractedTypeArgs =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy ref state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Call a field"
                    | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, Some methodGenerics
                | k -> failwith $"Unrecognised kind: %O{k}"
            | MetadataToken.MemberReference h ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy h state

                match method with
                | Choice2Of2 _field -> failwith "tried to Call a field"
                | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, None

            | MetadataToken.MethodDef defn ->
                match activeAssy.Methods.TryGetValue defn with
                | true, method ->
                    let method = method |> MethodInfo.mapTypeGenerics (fun _ -> failwith "not generic")
                    state, method, None, None, None
                | false, _ -> failwith $"could not find method in {activeAssy.Name}"
            | k -> failwith $"Unrecognised kind: %O{k}"

        // Capture the pending `constrained.` prefix up front and clear it from the current frame,
        // so a stale prefix cannot leak to a later call in the same frame. Nothing reinstalls it:
        // the class-initialisation check runs as the callee's prologue, so this instruction
        // commits exactly once.
        let activeFrameId = state.ThreadState.[thread].ActiveMethodState

        let pendingConstrained, state =
            let cur = state.ThreadState.[thread].MethodState.PendingPrefix.Constrained

            match cur with
            | None -> None, state
            | Some _ ->
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

        let state, concretizedMethod, declaringTypeHandle =
            match preConcretizedMethodGenerics with
            | Some concrete ->
                ExecutionConcretization.concretizeMethodForExecutionWithConcreteMethodGenerics
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    concrete
                    typeArgsFromMetadata
                    state
            | None ->
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    methodGenerics
                    typeArgsFromMetadata
                    state

        let state, concretizedMethod, declaringTypeHandle =
            match pendingConstrained with
            | None -> state, concretizedMethod, declaringTypeHandle
            | Some constrainedTypeHandle ->
                resolveConstrainedStaticInterfaceMethod
                    "constrained.call"
                    ctx
                    constrainedTypeHandle
                    methodToCall
                    concretizedMethod
                    state

        enterCallee ctx concretizedMethod state

    let executeCallvirt (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread


        // TODO: this is presumably super incomplete
        // For MethodSpec(MemberReference) the spec's method-generic args are caller-relative
        // and already concretized against the current frame; we surface them as
        // `preConcretizedMethodGenerics` so the concretization step uses them directly rather
        // than re-substituting against the target type's generics, which would be the wrong
        // context whenever `spec.Signature` references the caller's class or method generics.
        let state, methodToCall, methodGenerics, typeArgsFromMetadata, preConcretizedMethodGenerics =
            match metadataToken with
            | MetadataToken.MethodSpecification h ->
                let spec = activeAssy.MethodSpecs.[h]

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

                match spec.Method with
                | MetadataToken.MethodDef token ->
                    let method =
                        activeAssy.Methods.[token]
                        |> MethodInfo.mapTypeGenerics (fun (p, _) -> spec.Signature.[p.SequenceNumber])

                    state, method, Some spec.Signature, None, None
                | MetadataToken.MemberReference ref ->
                    let state, _, method, extractedTypeArgs =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy ref state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Callvirt a field"
                    | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, Some methodGenerics
                | k -> failwith $"Unrecognised kind: %O{k}"
            | MetadataToken.MemberReference h ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy h state

                match method with
                | Choice2Of2 _field -> failwith "tried to Callvirt a field"
                | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, None

            | MetadataToken.MethodDef defn ->
                match activeAssy.Methods.TryGetValue defn with
                | true, method ->
                    let method = method |> MethodInfo.mapTypeGenerics (fun _ -> failwith "not generic")
                    state, method, None, None, None
                | false, _ -> failwith $"could not find method in {activeAssy.Name}"
            | k -> failwith $"Unrecognised kind: %O{k}"

        let state, concretizedMethod, declaringTypeHandle =
            match preConcretizedMethodGenerics with
            | Some concrete ->
                ExecutionConcretization.concretizeMethodForExecutionWithConcreteMethodGenerics
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    concrete
                    typeArgsFromMetadata
                    state
            | None ->
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    methodGenerics
                    typeArgsFromMetadata
                    state

        // Capture the pending `constrained.` prefix up front and clear it from the current
        // frame. This ensures that if the callee's class initializer throws an exception
        // that lands in a catch handler within the same method, a later unrelated callvirt
        // in that handler won't inherit a stale prefix.
        let activeFrameId = state.ThreadState.[thread].ActiveMethodState

        let pendingConstrained, state =
            let cur = state.ThreadState.[thread].MethodState.PendingPrefix.Constrained

            match cur with
            | None -> None, state
            | Some _ ->
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

        // No class-initialisation check here. It could not be right at this point even in
        // principle: it would have to name the type at the call site, and the callee is not
        // resolved until the `constrained.` transformation and virtual dispatch below have run.
        // Measured on .NET 10, `callvirt IFace::M` resolving to `Impl.M` runs `Impl`'s
        // initialiser and never `IFace`'s. `callMethod` arms the check on the callee's frame
        // once that resolution has happened, and the dispatch loop runs it as the prologue.

        // Apply a pending `constrained.` prefix (ECMA III.2.1). The prefix transforms the
        // receiver on the stack so the rest of the callvirt logic is unchanged: for a
        // reference-type T the byref is dereferenced, for a value-type T with a method
        // inherited from Object/ValueType/Enum the byref is dereferenced and boxed.
        //
        // The receiver lives beneath the N method arguments. Temporarily lift the args
        // off so the transformation always sees the receiver on top of the stack, then
        // push the args back in their original order.
        let state, concretizedMethod, performInterfaceResolution =
            match pendingConstrained with
            | None -> state, concretizedMethod, true
            | Some tHandle ->

            let nArgs = MethodInfo.arity methodToCall

            let state, argsBottomToTop =
                let rec loop (state : IlMachineState) (acc : EvalStackValue list) (remaining : int) =
                    if remaining = 0 then
                        state, acc
                    else
                        let v, state = IlMachineState.popEvalStack thread state
                        loop state (v :: acc) (remaining - 1)

                loop state [] nArgs

            // ECMA III.2.1 case 1: dereference the managed pointer receiver and push the
            // dereferenced value. Shared by the reference-type and array paths.
            //
            // After the dereference the existing callvirt logic takes over, including
            // virtual dispatch against the receiver's runtime type.
            let applyCase1 (state : IlMachineState) : IlMachineState =
                let ptr, state = IlMachineState.popEvalStack thread state

                match ptr with
                | EvalStackValue.ManagedPointer src ->
                    let deref = IlMachineState.readManagedByref baseClassTypes state src
                    IlMachineState.pushToEvalStack deref thread state
                | other ->
                    failwith $"constrained.callvirt: expected ManagedPointer receiver on the eval stack, got %O{other}"

            let transformed, concretizedMethod, performInterfaceResolution =
                match tHandle with
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    // Arrays are reference types: take ECMA case 1 without consulting the
                    // concrete-type mapping (which doesn't store structural wrappers).
                    applyCase1 state, concretizedMethod, true
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ ->
                    failwith
                        $"constrained.callvirt: unexpected handle kind %O{tHandle}; pointers, byrefs and fnptrs cannot be generic type arguments"
                | ConcreteTypeHandle.Concrete _ ->

                let tConcrete, tDefn =
                    AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes tHandle
                    |> Option.get

                let tAssy = state._LoadedAssemblies.ByDefinitionName tConcrete.AssemblyFullName

                let tIsValueType =
                    DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies tDefn

                if not tIsValueType then
                    // Reference-type T: dereference the byref to the underlying ObjectRef.
                    applyCase1 state, concretizedMethod, true
                else
                    // Value-type T. If T has its own implementation of the method, invoke it
                    // non-virtually with the managed pointer still serving as `this` (ECMA
                    // case 2). Otherwise, if the method belongs to Object/ValueType/Enum, box
                    // and let ordinary virtual dispatch handle the boxed receiver (case 3).
                    let methodDeclAssyName = methodToCall.DeclaringAssemblyFullName
                    let methodDeclTypeName = methodToCall.RequiredDeclaringType.Name
                    let methodDeclNamespace = methodToCall.RequiredDeclaringType.Namespace

                    let isBaseMethodType =
                        methodDeclAssyName = baseClassTypes.Corelib.DefinitionFullName
                        && methodDeclNamespace = "System"
                        && (methodDeclTypeName = "Object"
                            || methodDeclTypeName = "ValueType"
                            || methodDeclTypeName = "Enum")

                    let state, directImplementation =
                        IlMachineStateExecution.tryResolveVirtualImplementation
                            loggerFactory
                            baseClassTypes
                            thread
                            concretizedMethod.Generics
                            concretizedMethod
                            tHandle
                            false
                            state

                    match directImplementation with
                    | Some directImplementation ->
                        match state.ThreadState.[thread].MethodState.EvaluationStack |> EvalStack.Peek with
                        | Some (EvalStackValue.ManagedPointer _) -> state, directImplementation, false
                        | Some other ->
                            failwith
                                $"constrained.callvirt case 2: expected ManagedPointer receiver on the eval stack, got %O{other}"
                        | None -> failwith "constrained.callvirt case 2: expected a receiver on the eval stack"
                    | None when isBaseMethodType ->
                        let ptr, state = IlMachineState.popEvalStack thread state

                        let src =
                            match ptr with
                            | EvalStackValue.ManagedPointer src -> src
                            | other ->
                                failwith
                                    $"constrained.callvirt (box case): expected ManagedPointer receiver on the eval stack, got %O{other}"

                        let derefCli = IlMachineState.readManagedByref baseClassTypes state src
                        let derefEval = EvalStackValue.ofCliType derefCli

                        // Share the Box opcode's construction strategy: reuse an existing
                        // CliValueType when the dereferenced value already carries one,
                        // otherwise rebuild from T's instance fields (primitive-like values
                        // like enums and IntPtr arrive flattened).
                        let cvt, state =
                            match derefEval with
                            | EvalStackValue.UserDefinedValueType cvt -> cvt, state
                            | _ ->
                                let instanceFields =
                                    tDefn.Fields
                                    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                                let state, fieldValues =
                                    ((state, []), instanceFields)
                                    ||> List.fold (fun (state, acc) field ->
                                        let state, fieldZero, fieldTypeHandle =
                                            IlMachineState.cliTypeZeroOf
                                                loggerFactory
                                                baseClassTypes
                                                tAssy
                                                field.Signature
                                                tConcrete.Generics
                                                ImmutableArray.Empty
                                                state

                                        let coerced = EvalStackValue.toCliTypeCoerced fieldZero derefEval

                                        let cliField : CliField =
                                            {
                                                Id = FieldId.metadata tHandle field.Handle field.Name
                                                Name = field.Name
                                                Contents = coerced
                                                Offset = field.Offset
                                                Type = fieldTypeHandle
                                                MarshallingDescriptor = field.MarshallingDescriptor
                                            }

                                        state, cliField :: acc
                                    )

                                List.rev fieldValues
                                // Unreachable for an inline array (it is never primitive-like, so
                                // it arrives as `UserDefinedValueType` and takes the branch above),
                                // but routed through the shared expansion all the same.
                                |> InlineArrayStorage.expand
                                    (fun () -> $"%s{tDefn.Namespace}.%s{tDefn.Name}")
                                    tDefn.Layout
                                    (InlineArrayStorage.effectiveLength
                                        (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies tDefn)
                                        tDefn.InlineArrayLength)
                                |> CliValueType.OfFields
                                    baseClassTypes
                                    state.ConcreteTypes
                                    tHandle
                                    (DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies tDefn),
                                state

                        let addr, state = IlMachineState.allocateManagedObject tHandle cvt state

                        IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef addr) thread state,
                        concretizedMethod,
                        true
                    | None ->
                        failwith
                            $"constrained.callvirt case 2: non-base method %s{methodToCall.Name} had no direct value-type implementation for type %s{tConcrete.Namespace}.%s{tConcrete.Name}"

            // Restore the method arguments on top of the transformed receiver. argsBottomToTop
            // has the bottom-most arg at the head; pushing left-to-right returns each arg to
            // its original slot (with the top-most arg landing on top).
            let state =
                (transformed, argsBottomToTop)
                ||> List.fold (fun state arg -> IlMachineState.pushToEvalStack' arg thread state)

            state, concretizedMethod, performInterfaceResolution

        // Callvirt always performs a null check on the receiver, even for non-virtual methods.
        if
            not concretizedMethod.IsStatic
            && (
                match
                    state.ThreadState.[thread].MethodState.EvaluationStack
                    |> EvalStack.PeekNthFromTop (MethodInfo.arity concretizedMethod)
                with
                | Some EvalStackValue.NullObjectRef -> true
                | _ -> false
            )
        then
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        else

        let state =
            refuseUnverifiableArguments loggerFactory baseClassTypes "callvirt" false concretizedMethod thread state

        let threadState = state.ThreadState.[thread]

        let state, commitment =
            IlMachineStateExecution.callMethodWithCommitment
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                performInterfaceResolution
                false
                true
                IlMachineStateExecution.CallSiteTransition.StaysCooperative
                concretizedMethod.Generics
                concretizedMethod
                thread
                threadState
                None
                ReturnValueDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state

        match commitment with
        | IlMachineStateExecution.CallCommitment.Aborted fatal -> state, WhatWeDid.Aborted fatal
        | IlMachineStateExecution.CallCommitment.Committed
        | IlMachineStateExecution.CallCommitment.Raised -> state, WhatWeDid.Executed

    /// The first instruction at or after `offset` that is not itself a prefix — PawPrint's
    /// counterpart to CoreCLR's `impGetNonPrefixOpcode` (`importer.cpp`), which skips exactly
    /// `unaligned.`, `volatile.`, `tail.`, `constrained.` and `readonly.`.
    ///
    /// `None` when the scan runs off the end of the body, or the method has no IL at all.
    let rec private nextNonPrefixOp
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (offset : int)
        : IlOp option
        =
        match method.Body with
        | MethodBody.Il instructions ->
            match Map.tryFind offset instructions.Locations with
            | None -> None
            | Some op ->
                match op with
                | IlOp.Nullary NullaryIlOp.Volatile
                | IlOp.Nullary NullaryIlOp.Tail
                | IlOp.Nullary NullaryIlOp.Readonly
                | IlOp.UnaryConst (UnaryConstIlOp.Unaligned _)
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, _) ->
                    nextNonPrefixOp method (offset + IlOp.NumberOfBytes op)
                | _ -> Some op
        | _ -> None

    let executeConstrained (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA III.2.1: record the constrained type and advance PC; the next instruction
        // (guaranteed by ECMA to be callvirt) consumes the prefix and branches on the
        // three cases (reference type / value type with direct impl / value type falling
        // through to a method on Object/ValueType/Enum).
        let state, ty, assy =
            match metadataToken with
            | MetadataToken.TypeDefinition h ->
                let state, ty = IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h
                state, ty, activeAssy
            | MetadataToken.TypeReference ref ->
                IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy ref
            | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
            | _ -> failwith $"unexpected token {metadataToken} in Constrained"

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.DefinitionFullName
                currentMethod.DeclaringTypeGenerics
                currentMethod.Generics
                ty

        let activeFrameId = state.ThreadState.[thread].ActiveMethodState

        let state =
            state
            |> IlMachineState.mapFrame
                thread
                activeFrameId
                (fun frame ->
                    { frame with
                        PendingPrefix =
                            { frame.PendingPrefix with
                                Constrained = Some typeHandle
                            }
                    }
                )
            |> IlMachineState.advanceProgramCounter thread

        // The prefix we just armed is consumed by exactly one instruction, and only
        // `call`/`callvirt`/`ldftn` know how to consume it (ECMA III.2.1; `importer.cpp`,
        // `case CEE_CONSTRAINED`, rejects anything else with
        // `BADCODE("constrained. has to be followed by callvirt, call or ldftn")`). Anything
        // else here means the prefix would sit armed on this frame and be silently applied to
        // some later call.
        //
        // "Followed by" is the next *non-prefix* opcode, not the next opcode: the importer asks
        // `impGetNonPrefixOpcode`, so `constrained. tail. callvirt` is legal and must not be
        // rejected here. The PC has already advanced, so the scan starts at the successor.
        //
        // A scan that finds nothing is a `constrained.` at the very end of a body, which is
        // malformed for the same reason.
        let currentMethodInfo = state.ThreadState.[thread].MethodState.ExecutingMethod
        let successorOffset = state.ThreadState.[thread].MethodState.IlOpIndex

        match nextNonPrefixOp currentMethodInfo successorOffset with
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, _))
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _))
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, _)) -> ()
        | Some other ->
            failwith
                $"constrained. must be followed by call, callvirt or ldftn (ignoring intervening prefixes), but %O{currentMethodInfo} has %O{other} at IL offset %i{successorOffset}"
        | None ->
            failwith
                $"constrained. must be followed by call, callvirt or ldftn, but %O{currentMethodInfo} has no further instruction after IL offset %i{successorOffset}"

        state |> Tuple.withRight WhatWeDid.Executed

    /// How much two types must agree for PawPrint's callee-driven `calli` to reproduce what the
    /// real runtime does. This starts from the CLI evaluation-stack representation (ECMA-335
    /// III.1.1) — signedness and sub-`int32` width do not survive the load onto the stack, so a
    /// call site and its target may disagree about those and still describe the same call — but
    /// it is *not* simply that model, because a `calli` also crosses a method boundary where the
    /// ABI footprint matters.
    ///
    /// Hence `Float32` and `Float64` are distinguished even though both are `F` on the stack:
    /// reading a `float32` return slot as `float64` yields garbage on CoreCLR, not the target's
    /// value, so permitting that pun would make PawPrint silently return the plausible answer
    /// where the real runtime returns nonsense. The integer widths and signedness are
    /// *not* split, because there the two runtimes do agree. Both halves of that
    /// were measured rather than reasoned about: a bitmask probe over five puns
    /// (`short`/`byte`/`uint`/`float` returns and a signedness-punned parameter) on osx-arm64
    /// gave CoreCLR 23 and PawPrint 31 — differing on the float bit alone. Splitting the
    /// integer cases would reject calls that work today.
    ///
    /// `None` from `calliStackKind` means "not classified" — a non-primitive type, or a generic
    /// parameter left unsubstituted in a raw signature. Those are not compared, so this
    /// classifier is a source of *refusals*, never of permission: agreeing here does not assert
    /// the call is well-typed, only that it is not one of the mismatches we can detect cheaply.
    [<RequireQualifiedAccess>]
    type private CalliStackKind =
        | Int32
        | Int64
        | Float32
        | Float64
        | NativeInt
        | ObjectRef

    /// The type a `calli` call site says comes back, or `None` if it says nothing does.
    ///
    /// A call-site signature is decoded, not concretised, so its return column still mirrors the
    /// blob: a `modopt(CallConvCdecl) void` return sits in `MethodReturnType.Returns`, and reading
    /// that case as "returns a value" would reject a legitimately void target.
    let private callSiteReturnType (signature : TypeMethodSignature<TypeDefn>) : TypeDefn option =
        match signature.ReturnType with
        | MethodReturnType.Void -> None
        | MethodReturnType.Returns retTy ->
            match TypeDefn.stripCustomModifiers retTy with
            | TypeDefn.Void -> None
            | stripped -> Some stripped

    let private calliStackKind (t : TypeDefn) : CalliStackKind option =
        match TypeDefn.stripCustomModifiers t with
        | TypeDefn.PrimitiveType p ->
            match p with
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32 -> Some CalliStackKind.Int32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64 -> Some CalliStackKind.Int64
            // Separated, unlike the integer widths above: see the type's doc comment.
            | PrimitiveType.Single -> Some CalliStackKind.Float32
            | PrimitiveType.Double -> Some CalliStackKind.Float64
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> Some CalliStackKind.NativeInt
            | PrimitiveType.String
            | PrimitiveType.Object -> Some CalliStackKind.ObjectRef
            // A TypedReference is not an ordinary stack value; don't pretend to classify it.
            | PrimitiveType.TypedReference -> None
        | _ -> None

    /// Both kinds are known and they differ, i.e. this is a mismatch we can prove.
    let private calliKindsConflict (a : TypeDefn) (b : TypeDefn) : bool =
        match calliStackKind a, calliStackKind b with
        | Some ka, Some kb -> ka <> kb
        | _ -> false

    /// `calli` through `FunctionPointerTarget.RuntimeAllocator`: the JIT's `newobj`
    /// allocation helper. Managed signature `MethodTable* -> object`.
    ///
    /// Two QCalls hand it out, and both are invoked the same way — by a `calli` in a managed
    /// cache object: `RuntimeTypeHandle_GetActivationInfo` for
    /// `RuntimeType.ActivatorCache.CreateUninitializedObject`, and
    /// `ReflectionInvocation_GetBoxInfo` for `RuntimeType.BoxCache.Box`.
    ///
    /// This is a synchronous runtime primitive, not a managed call: no frame is pushed, so it
    /// never suspends, and the peek-don't-pop retry dance the managed path needs does not
    /// apply here.
    ///
    /// Per CoreCLR (`reflectioninvocation.cpp`, "This method will not run the type's static
    /// cctor"), allocation does *not* initialise the type. That is observable: a struct with a
    /// static constructor and no instance constructor is activated through this helper plus a
    /// no-op ctor stub, so its `.cctor` never runs at all. Hence no `loadClass` here.
    let private executeAllocatorCalli
        (ctx : UnaryMetadataIlOpContext)
        (callSiteSignature : TypeMethodSignature<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let operation = "calli (runtime newobj allocator)"
        let thread = ctx.Thread

        // The allocator's signature is known exactly, so check the call site against all of
        // it rather than merely counting slots as the managed path has to.
        let callSiteHeader = callSiteSignature.Header.Get

        if callSiteHeader.IsInstance then
            failwith $"%s{operation}: call site declares an instance signature, but the allocation helper is static"

        match callSiteSignature.ParameterTypes with
        | [ paramTy ] ->
            match TypeDefn.stripCustomModifiers paramTy with
            | TypeDefn.Pointer _
            | TypeDefn.PrimitiveType PrimitiveType.IntPtr
            | TypeDefn.PrimitiveType PrimitiveType.UIntPtr -> ()
            | other ->
                failwith
                    $"%s{operation}: call site declares its parameter as %O{other}, but the allocation helper takes a MethodTable*"
        | other ->
            failwith
                $"%s{operation}: call site declares %d{other.Length} parameters, but the allocation helper takes exactly one (the MethodTable*)"

        match callSiteReturnType callSiteSignature with
        | None ->
            failwith $"%s{operation}: call site returns void, but the allocation helper returns an object reference"
        | Some retTy ->
            match calliStackKind retTy with
            | Some CalliStackKind.ObjectRef -> ()
            | _ ->
                failwith
                    $"%s{operation}: call site declares a return type of %O{retTy}, but the allocation helper returns an object reference"

        // The pointer sits above its argument, so it comes off first.
        let _fnPtr, state = IlMachineState.popEvalStack thread state
        let methodTableArg, state = IlMachineState.popEvalStack thread state

        let typeHandle = EvalStackValue.requireMethodTable operation methodTableArg

        // Neither producer of this pointer can legitimately pair it with a Nullable MethodTable,
        // by two different routes: `GetActivationInfo` hands back a *null* allocator for
        // `Nullable<T>` and `ActivatorCache` substitutes its own `ReturnNull` stub, while
        // `GetBoxInfo` substitutes the underlying `T`'s MethodTable before returning. That
        // matters beyond tidiness: `Box` never boxes `Nullable<T>` as itself, and the unbox
        // reader relies on it, so allocating one here would put a heap object on the heap that no
        // reader is prepared for.
        match AllConcreteTypes.lookup typeHandle state.ConcreteTypes with
        | Some ct when InternalTypeKind.kind ctx.BaseClassTypes ct = InternalTypeKind.Nullable ->
            failwith
                $"%s{operation}: invoked with a Nullable<T> MethodTable (%O{typeHandle}); neither RuntimeTypeHandle_GetActivationInfo (which returns a null allocator, so ActivatorCache substitutes its own null-returning stub) nor ReflectionInvocation_GetBoxInfo (which substitutes the underlying T's MethodTable) can produce this pairing, so this pointer should never have been called"
        | _ -> ()

        let addr, state =
            IlMachineState.allocateUninitialisedInstance ctx.LoggerFactory ctx.BaseClassTypes typeHandle state

        state
        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    /// `calli` (ECMA-335 III.3.20). The function pointer sits on top of the eval stack,
    /// above the arguments; the metadata token is a StandaloneSignature describing the
    /// *call site*, not the callee.
    ///
    /// Design note. We drive the actual invocation from the `MethodInfo` carried by the
    /// function-pointer value (`NativeIntSource.FunctionPointer`), exactly as the delegate
    /// dispatch path does in `AbstractMachine.dispatchDelegateInvoke` — `callMethod` pops
    /// arguments according to the callee's own signature, so that is the single source of
    /// truth for argument handling. The call-site signature is used only to *validate*
    /// that the two agree on how many eval-stack slots this call consumes. Without that
    /// check, a mismatch (whether from a bug in our own `ldftn`/function-pointer
    /// representation, or from genuinely divergent IL) would silently pop the wrong number
    /// of values and corrupt the frame — a failure that surfaces arbitrarily far from its
    /// cause.
    ///
    /// Known divergence. ECMA-335 defines `calli`'s marshalling by the call-site signature,
    /// so a guest may legally pun a function pointer to a signature whose *types* differ from
    /// the target's (C# permits `(delegate*&lt;int, long&gt;)p` where `p` is
    /// `delegate*&lt;int, int&gt;`, and CoreCLR runs it). Driving invocation from the callee
    /// cannot reproduce that: the result would be pushed as the target's `Int32` and the
    /// caller's `int64` store would then have no legal coercion. Doing it properly means
    /// coercing arguments and the result to the call-site types, which requires carrying the
    /// call-site signature onto the frame and applying it in `returnStackFrame`. Until then we
    /// detect the mismatch here and fail at the faulting instruction, rather than letting the
    /// call proceed and die later inside `toCliTypeCoerced` with a message that never mentions
    /// `calli`. See docs/divergences.md.
    let executeCalli (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let thread = ctx.Thread

        let callSiteSignature =
            match ctx.MetadataToken with
            | MetadataToken.StandaloneSignature handle ->
                let metadataReader = activeAssy.PeReader.GetMetadataReader ()

                (metadataReader.GetStandaloneSignature handle)
                    .DecodeMethodSignature (TypeDefn.typeProvider activeAssy.Name, ())
                |> TypeMethodSignature.make
            | k -> failwith $"calli: expected a StandaloneSignature metadata token describing the call site, got %O{k}"

        // Peek rather than pop: this read only inspects the pointer for validation. It stays
        // on the stack (above the arguments) until the call is actually made, and is popped
        // exactly once there.
        let fnPtr = IlMachineState.peekEvalStack thread state

        // A function pointer is recognised by its `FunctionPointer` provenance; anything
        // that is semantically zero is a null pointer. Matching `FunctionPointer` first is
        // not required for correctness — `NativeIntSource.isZero` answers `false` for
        // it, because a function pointer is never null — but it keeps the two arms readable
        // as "is it a pointer to something" then "is it null".
        let target =
            match fnPtr with
            | None -> failwith "calli: eval stack was empty; expected a function pointer on top"
            | Some (EvalStackValue.NativeInt (NativeIntSource.FunctionPointer target)) -> Some target
            | Some (EvalStackValue.NativeInt src) when NativeIntSource.isZero src ->
                // Every spelling of a null function pointer lands here, not just
                // `Verbatim 0L`: `ldnull; conv.i` yields
                // `ManagedPointer ManagedPointerSource.Null`, which is zero throughout
                // PawPrint. Reusing the existing predicate keeps this arm honest as new
                // `NativeIntSource` cases appear.
                None
            | Some other ->
                // Anything else is either a genuinely bogus value or a pointer provenance
                // our `NativeIntSource` model can't yet render as a callable target; either
                // way, calling through it would be a guess.
                failwith $"calli: expected a function pointer on top of the eval stack, got %O{other}"

        match target with
        | None ->
            // The CLI does not specify this case. ECMA-335 III.3.20's "Exceptions" lists only
            // `System.SecurityException`, and its "Correctness" requires `ftn` to hold a method
            // address, so a null one is not correct CIL. PawPrint chooses a deterministic
            // catchable exception over emulating CoreCLR's segfault; docs/divergences.md has the
            // argument. Don't advance the PC; exception dispatch needs the faulting instruction's
            // offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some FunctionPointerTarget.RuntimeAllocator -> executeAllocatorCalli ctx callSiteSignature state
        | Some (FunctionPointerTarget.Dynamic handle) ->
            // The same boundary `FunctionPointerTarget.requireManaged` enforces on the delegate
            // path, stated separately because `calli` does not go through it. Reachable in
            // principle — `Marshal.GetFunctionPointerForDelegate` over a delegate bound to a
            // dynamic method would hand the guest one of these — and it must fail here rather
            // than be silently mistaken for some other target.
            failwith
                $"calli: the function pointer names %O{handle}; PawPrint can mint and bind a Reflection.Emit method but cannot yet execute one"
        | Some (FunctionPointerTarget.Managed methodToCall) ->

        // Slots this call consumes: the callee's declared parameters, plus `this` when the
        // callee is an instance method. Arity comes from the signature, not the Param table:
        // see `MetadataMethodFacts.Parameters` for why `Parameters.Length` understates arity for
        // methods whose parameters carry no metadata.
        let calleeSlots =
            MethodInfo.arity methodToCall + (if methodToCall.IsStatic then 0 else 1)

        // Slots the call site pushed: its declared parameters, plus `this` when the
        // signature carries an *implicit* receiver. Under EXPLICITTHIS (ECMA-335 II.15.3)
        // HASTHIS is also set, but the receiver is already the first entry in
        // ParameterTypes, so counting it again would reject legal instance stubs.
        //
        // Note the two sides need not agree on *which* of them supplies `this`: CoreCLR
        // takes the address of an instance method (e.g. a constructor, via
        // `GetMultiCallableAddrOfCode`) and calls it through a call site whose signature is
        // static with the receiver as an explicit leading argument. Only the total matters.
        let callSiteHeader = callSiteSignature.Header.Get

        let callSiteHasImplicitThis =
            callSiteHeader.IsInstance
            && not (callSiteHeader.Attributes.HasFlag SignatureAttributes.ExplicitThis)

        let callSiteSlots =
            callSiteSignature.ParameterTypes.Length
            + (if callSiteHasImplicitThis then 1 else 0)

        if calleeSlots <> callSiteSlots then
            failwith
                $"calli: call-site signature consumes %d{callSiteSlots} eval-stack slots but target %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} consumes %d{calleeSlots}; refusing to execute a call that would corrupt the frame"

        // Whether a result is left on the caller's stack is decided by the *callee*
        // signature, because that is what `callMethod` and the frame return use. If the
        // call site disagrees about void-ness, the caller's stack ends up one slot short
        // (calling a void target through a value-returning signature, so the following
        // load underflows) or one slot long (the reverse, leaving junk behind). Neither is
        // recoverable, and both would surface far from here.
        let callSiteReturnsValue = (callSiteReturnType callSiteSignature).IsSome

        // The callee's signature has been concretised, which is where a `void` under custom
        // modifiers has already become `Void`; the call site's has not, which is why it needs
        // `callSiteReturnType` rather than a match on the DU.
        let calleeReturnsValue =
            match methodToCall.Signature.ReturnType with
            | MethodReturnType.Void -> false
            | MethodReturnType.Returns _ -> true

        if callSiteReturnsValue <> calleeReturnsValue then
            let describe (b : bool) = if b then "a value" else "void"

            failwith
                $"calli: call-site signature returns %s{describe callSiteReturnsValue} but target %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} returns %s{describe calleeReturnsValue}; refusing to execute a call that would leave the caller's eval stack the wrong depth"

        // The two signatures agree on shape; now check they agree on *representation*, to the
        // extent we can prove it. See the "Known divergence" note on this function: we invoke
        // the target directly, so a call site that puns the types would be silently ignored
        // here and would fail later in `toCliTypeCoerced` with no mention of `calli`.
        //
        // Compare against the target's *raw* signature, which is in the same `TypeDefn` form as
        // the decoded call site (`Signature` is concretized to `ConcreteTypeHandle`s and so is
        // not comparable). For a generic target the raw signature still holds type parameters;
        // `calliStackKind` declines to classify those, so they are skipped rather than
        // spuriously rejected.
        // Only a declared method has a raw signature. A synthesised target has none — there is no
        // metadata form of a method the runtime supplies — so there is nothing to disagree with
        // and the comparison is simply skipped. That is sound because this check is a
        // source of *refusals* and never of permission: declining to run it forfeits an error we
        // might have caught, not a guarantee we were relying on. The slot-count and return-shape
        // checks above, which are what guard frame integrity, apply to every target.
        match methodToCall.TryMetadata with
        | None -> ()
        | Some facts ->
            let calleeRaw = facts.RawSignature

            // Positions only line up when both sides agree on who supplies `this`. When they do
            // not (the EXPLICITTHIS / receiver-as-explicit-argument case described above) the
            // lists are offset relative to each other and cannot be compared element-wise; the
            // slot-count check above still guards frame integrity there.
            let receiverConventionsAgree = callSiteHasImplicitThis = not methodToCall.IsStatic

            if
                receiverConventionsAgree
                && callSiteSignature.ParameterTypes.Length = calleeRaw.ParameterTypes.Length
            then
                List.iteri2
                    (fun i (callSiteTy : TypeDefn) (calleeTy : TypeDefn) ->
                        if calliKindsConflict callSiteTy calleeTy then
                            failwith
                                $"calli: call-site signature declares parameter %d{i} as %O{callSiteTy} but target %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} declares it as %O{calleeTy}; these occupy different evaluation-stack representations, and PawPrint does not yet marshal calli arguments through the call-site signature"
                    )
                    callSiteSignature.ParameterTypes
                    calleeRaw.ParameterTypes

            match callSiteSignature.ReturnType, calleeRaw.ReturnType with
            | MethodReturnType.Returns callSiteRet, MethodReturnType.Returns calleeRet when
                calliKindsConflict callSiteRet calleeRet
                ->
                failwith
                    $"calli: call-site signature declares a return type of %O{callSiteRet} but target %s{MethodOwner.describe methodToCall.Owner}::%s{methodToCall.Name} returns %O{calleeRet}; these occupy different evaluation-stack representations, and PawPrint does not yet marshal the calli result through the call-site signature"
            | _ -> ()

        let declaringTypeHandle =
            AllConcreteTypes.findExistingConcreteType
                state.ConcreteTypes
                methodToCall.RequiredDeclaringType.Identity
                methodToCall.DeclaringTypeGenerics
            |> Option.defaultWith (fun () ->
                failwith
                    $"calli: declaring type %s{MethodOwner.describe methodToCall.Owner} of the target method is not registered in AllConcreteTypes"
            )

        // `calli` is the only instruction whose call site describes its own transition, and so the
        // only one that can make the *legal* entry into a `[UnmanagedCallersOnly]` method: a
        // `delegate* unmanaged<...>` StandaloneSignature leaves cooperative mode, where every
        // managed route -- including a `calli` through `delegate*<...>` over the very same address
        // -- does not.
        // CoreCLR names a modifier in the module that owns the signature
        // (`GetNameOfTypeRefOrDef(pModule, ...)`), which for a StandaloneSignature is the assembly
        // being interpreted. A TypeDef token is scoped to that module, so a `FromDefinition`
        // modifier reached from here is always one of its own rows.
        let resolveModifierName (identity : ResolvedTypeIdentity) : (string * string) option =
            match activeAssy.TypeDefs.TryGetValue identity.TypeDefinition.Get with
            | true, typeDef -> Some (typeDef.Namespace, typeDef.Name)
            | false, _ -> None

        let callSiteTransition =
            IlMachineStateExecution.CallSiteTransition.ofCallSiteSignature resolveModifierName callSiteSignature

        // No class-initialisation check here: `callMethodWithCommitment` arms it on the callee's
        // frame, and it is the callee's prologue that runs it. That includes the per-kind
        // question of whether a *synthesised* method's declaring type is initialised at all —
        // `SynthesisedMethod.initialisesDeclaringType`, which a struct-marshal stub answers no,
        // as `sourcesPure/MarshalStructureToPtrStaticCtorDormant.cs` pins.

        // The pointer sits above the arguments, and arguments are popped from the top of the
        // stack, so it has to come off before we call in.
        let fnPtrValue, state = IlMachineState.popEvalStack thread state
        let threadState = state.ThreadState.[thread]

        let state, commitment =
            IlMachineStateExecution.callMethodWithCommitment
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                true
                callSiteTransition
                methodToCall.Generics
                methodToCall
                thread
                threadState
                None
                ReturnValueDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state

        // This `calli` commits exactly once, so the function pointer popped above is simply gone
        // and nothing has to put it back: class initialisation happens in the callee's own frame,
        // after this instruction is finished with, so there is no retry to prepare for.
        match commitment with
        | IlMachineStateExecution.CallCommitment.Aborted fatal -> state, WhatWeDid.Aborted fatal
        | IlMachineStateExecution.CallCommitment.Committed
        | IlMachineStateExecution.CallCommitment.Raised -> state, WhatWeDid.Executed
