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
    /// The iterative form starting from `flat = 0` reproduces this because the first
    /// multiplication (by L_0) is on a zero accumulator.
    let private rowMajorOffset (lengths : ImmutableArray<int>) (indices : int[]) : int =
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
            match state.ManagedHeap.Arrays.TryGetValue arrAddr with
            | true, v -> v
            | false, _ -> failwith $"multi-dim array Set: array allocation not found at %O{arrAddr}"

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

        let typeGenerics = currentMethod.DeclaringType.Generics
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
            match state.ManagedHeap.Arrays.TryGetValue arrAddr with
            | true, v -> v
            | false, _ -> failwith $"multi-dim array Get: array allocation not found at %O{arrAddr}"

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
            |> IlMachineState.pushToEvalStack arrObj.Elements.[flat] thread
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
            match state.ManagedHeap.Arrays.TryGetValue arrAddr with
            | true, v -> v
            | false, _ -> failwith $"multi-dim array Address: array allocation not found at %O{arrAddr}"

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

        let typeGenerics = currentMethod.DeclaringType.Generics
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

    let executeCall (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
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
                                activeAssy.Name
                                currentMethod.DeclaringType.Generics
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
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            methodGenerics
                            ref
                            state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Call a field"
                    | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, Some methodGenerics
                | k -> failwith $"Unrecognised kind: %O{k}"
            | MetadataToken.MemberReference h ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember
                        loggerFactory
                        baseClassTypes
                        thread
                        activeAssy
                        ImmutableArray.Empty
                        h
                        state

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

        // Capture the pending `constrained.` prefix up front and clear it from the current
        // frame before attempting class init. This avoids leaking a stale prefix to later
        // calls in the same frame if class initialisation throws into a local handler; if
        // class init suspends this call, we re-install the prefix for re-entry.
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

        let reinstallConstrained (state : IlMachineState) : IlMachineState =
            match pendingConstrained with
            | None -> state
            | Some h ->
                state
                |> IlMachineState.mapFrame
                    thread
                    activeFrameId
                    (fun frame ->
                        { frame with
                            PendingPrefix =
                                { frame.PendingPrefix with
                                    Constrained = Some h
                                }
                        }
                    )

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
                let methodDeclAssy = state._LoadedAssemblies.[methodToCall.DeclaringType.Assembly]

                let methodDeclType =
                    methodDeclAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

                if not methodToCall.IsStatic || not methodDeclType.IsInterface then
                    failwith
                        $"constrained.call: expected a static interface method call, got %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}::%s{methodToCall.Name}"

                let constrainedConcrete =
                    match constrainedTypeHandle with
                    | ConcreteTypeHandle.Concrete _ ->
                        AllConcreteTypes.lookup constrainedTypeHandle state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"constrained.call: constrained type handle %O{constrainedTypeHandle} is not registered"
                        )
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ ->
                        failwith
                            $"constrained.call: static interface dispatch for non-concrete constrained type %O{constrainedTypeHandle} is not implemented"

                let state, implementation =
                    IlMachineStateExecution.tryResolveVirtualImplementation
                        loggerFactory
                        baseClassTypes
                        thread
                        concretizedMethod.Generics
                        concretizedMethod
                        constrainedTypeHandle
                        true
                        state

                match implementation with
                | None ->
                    failwith
                        $"constrained.call: could not find static implementation of %s{methodToCall.Name} on %s{constrainedConcrete.Namespace}.%s{constrainedConcrete.Name}"
                | Some implementation when not implementation.IsStatic ->
                    failwith
                        $"constrained.call: resolved non-static implementation %s{implementation.DeclaringType.Namespace}.%s{implementation.DeclaringType.Name}::%s{implementation.Name}"
                | Some implementation ->
                    let declaringTypeHandle =
                        AllConcreteTypes.findExistingConcreteType
                            state.ConcreteTypes
                            implementation.DeclaringType.Identity
                            implementation.DeclaringType.Generics
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"constrained.call: resolved implementation declaring type %s{implementation.DeclaringType.Namespace}.%s{implementation.DeclaringType.Name} is not registered"
                        )

                    state, implementation, declaringTypeHandle

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | NothingToDo state ->
            let threadState = state.ThreadState.[thread]

            IlMachineStateExecution.callMethod
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                true
                concretizedMethod.Generics
                concretizedMethod
                thread
                threadState
                None
                ConstructedObjectDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state,
            WhatWeDid.Executed
        | FirstLoadThis state -> reinstallConstrained state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) ->
            // Park this thread on the other thread's in-progress cctor. The PC has not been
            // advanced, so when the scheduler wakes us we re-execute this call opcode; restore
            // any pending `constrained.` prefix that we cleared above so the retry sees it.
            reinstallConstrained state, WhatWeDid.BlockedOnClassInit blockedBy

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
                                activeAssy.Name
                                currentMethod.DeclaringType.Generics
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
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            methodGenerics
                            ref
                            state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Callvirt a field"
                    | Choice1Of2 method -> state, method, None, Some extractedTypeArgs, Some methodGenerics
                | k -> failwith $"Unrecognised kind: %O{k}"
            | MetadataToken.MemberReference h ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember
                        loggerFactory
                        baseClassTypes
                        thread
                        activeAssy
                        ImmutableArray.Empty
                        h
                        state

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
        // frame before attempting class init. This ensures that if the class initializer
        // throws an exception that lands in a catch handler within the same method, a
        // later unrelated callvirt in that handler won't inherit a stale prefix. If the
        // class hasn't been initialized yet we re-install the prefix on this frame so that
        // re-entry (after the cctor completes) sees it again.
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

        let reinstallConstrained (state : IlMachineState) : IlMachineState =
            match pendingConstrained with
            | None -> state
            | Some h ->
                state
                |> IlMachineState.mapFrame
                    thread
                    activeFrameId
                    (fun frame ->
                        { frame with
                            PendingPrefix =
                                { frame.PendingPrefix with
                                    Constrained = Some h
                                }
                        }
                    )

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | FirstLoadThis state ->
            // The cctor frame has been pushed; the original callvirt will re-execute. We
            // re-install the prefix on the original frame so the re-entry sees it.
            reinstallConstrained state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) ->
            // Another thread owns the cctor lock; park this one. The PC has not been advanced,
            // so on wake we re-execute the callvirt; re-install the cleared prefix for the retry.
            reinstallConstrained state, WhatWeDid.BlockedOnClassInit blockedBy
        | NothingToDo state ->

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

                let tConcrete = AllConcreteTypes.lookup tHandle state.ConcreteTypes |> Option.get

                let tAssy = state._LoadedAssemblies.[tConcrete.Assembly]
                let tDefn = tAssy.TypeDefs.[tConcrete.Definition.Get]

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
                    let methodDeclAssyName = methodToCall.DeclaringType.Assembly
                    let methodDeclTypeName = methodToCall.DeclaringType.Name
                    let methodDeclNamespace = methodToCall.DeclaringType.Namespace

                    let isBaseMethodType =
                        methodDeclAssyName.FullName = baseClassTypes.Corelib.Name.FullName
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
                                    tDefn.Layout
                                    (CharSetMetadata.ofTypeAttributes tDefn.TypeAttributes),
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

        let threadState = state.ThreadState.[thread]

        IlMachineStateExecution.callMethod
            loggerFactory
            baseClassTypes
            None
            ConstructionState.NotConstructing
            performInterfaceResolution
            false
            true
            concretizedMethod.Generics
            concretizedMethod
            thread
            threadState
            None
            ConstructedObjectDisposition.PushToCaller
            false // wrapExceptionInTargetInvocation
            state,
        WhatWeDid.Executed

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
                IlMachineState.lookupTypeRef
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    ref
            | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
            | _ -> failwith $"unexpected token {metadataToken} in Constrained"

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let activeFrameId = state.ThreadState.[thread].ActiveMethodState

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
    /// cause. We deliberately do not use the call-site signature's parameter *types* to
    /// coerce arguments: `callMethod` already coerces from the callee signature, and a
    /// second source of truth would be worse than none.
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
                |> TypeMethodSignature.make (
                    function
                    | TypeDefn.Void -> MethodReturnType.Void
                    | retType -> MethodReturnType.Returns retType
                )
            | k -> failwith $"calli: expected a StandaloneSignature metadata token describing the call site, got %O{k}"

        // Peek rather than pop: `loadClass` below may suspend this instruction for class
        // initialisation, in which case the PC is not advanced and the whole `calli` is
        // re-executed later. Popping here would lose the function pointer on that retry.
        let fnPtr = IlMachineState.peekEvalStack thread state

        // A function pointer is recognised by its `FunctionPointer` provenance; anything
        // that is semantically zero is a null pointer. The `FunctionPointer` case must be
        // matched first — `NativeIntSource.isZero` has no answer for it (and says so).
        let methodToCall =
            match fnPtr with
            | None -> failwith "calli: eval stack was empty; expected a function pointer on top"
            | Some (EvalStackValue.NativeInt (NativeIntSource.FunctionPointer mi)) -> Some mi
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

        match methodToCall with
        | None ->
            // ECMA-335 III.3.20: calli throws NullReferenceException if the function
            // pointer is null. Don't advance the PC; exception dispatch needs the
            // faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some methodToCall ->

        // Slots this call consumes: the callee's declared parameters, plus `this` when the
        // callee is an instance method. Arity comes from the signature, not the Param table:
        // see `MethodInfo.Parameters` for why `Parameters.Length` understates arity for
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
                $"calli: call-site signature consumes %d{callSiteSlots} eval-stack slots but target %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}::%s{methodToCall.Name} consumes %d{calleeSlots}; refusing to execute a call that would corrupt the frame"

        // Whether a result is left on the caller's stack is decided by the *callee*
        // signature, because that is what `callMethod` and the frame return use. If the
        // call site disagrees about void-ness, the caller's stack ends up one slot short
        // (calling a void target through a value-returning signature, so the following
        // load underflows) or one slot long (the reverse, leaving junk behind). Neither is
        // recoverable, and both would surface far from here.
        let returnsValue (ret : MethodReturnType<'a>) : bool =
            match ret with
            | MethodReturnType.Void -> false
            | MethodReturnType.Returns _ -> true

        let callSiteReturnsValue = returnsValue callSiteSignature.ReturnType
        let calleeReturnsValue = returnsValue methodToCall.Signature.ReturnType

        if callSiteReturnsValue <> calleeReturnsValue then
            let describe (b : bool) = if b then "a value" else "void"

            failwith
                $"calli: call-site signature returns %s{describe callSiteReturnsValue} but target %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}::%s{methodToCall.Name} returns %s{describe calleeReturnsValue}; refusing to execute a call that would leave the caller's eval stack the wrong depth"

        let declaringTypeHandle =
            AllConcreteTypes.findExistingConcreteType
                state.ConcreteTypes
                methodToCall.DeclaringType.Identity
                methodToCall.DeclaringType.Generics
            |> Option.defaultWith (fun () ->
                failwith
                    $"calli: declaring type %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name} of the target method is not registered in AllConcreteTypes"
            )

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | NothingToDo state ->
            // Only now is it safe to consume the function pointer.
            let _, state = IlMachineState.popEvalStack thread state
            let threadState = state.ThreadState.[thread]

            IlMachineStateExecution.callMethod
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                true
                methodToCall.Generics
                methodToCall
                thread
                threadState
                None
                ConstructedObjectDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state,
            WhatWeDid.Executed
        | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) ->
            // Park this thread on the other thread's in-progress cctor. The PC has not been
            // advanced and we have not popped the function pointer, so re-executing this
            // `calli` when the scheduler wakes us sees exactly the stack we started with.
            state, WhatWeDid.BlockedOnClassInit blockedBy
