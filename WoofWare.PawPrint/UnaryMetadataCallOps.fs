namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataCallOps =
    /// Detect a `Get`, `Set`, or `Address` call on a runtime-synthesized multi-dim array
    /// method. C# emits these as `MemberReference` whose parent is a `TypeSpecification`
    /// of `TypeDefn.Array(elt, rank)`. The methods correspond to no IL body and cannot be
    /// resolved through `resolveMember`, so we recognise them up front and dispatch
    /// directly against the heap allocation.
    ///
    /// Returns `Some (elt, rank, methodName)` for a recognised call. The element
    /// `TypeDefn` is needed by `Address` so it can perform the same exact-equality
    /// element-type check that `ldelema` does on szarrays (ECMA-335 III.4.10).
    let private tryRecognizeMultiDimArrayMethod
        (activeAssy : DumpedAssembly)
        (metadataToken : MetadataToken)
        : (TypeDefn * int * string) option
        =
        match metadataToken with
        | MetadataToken.MemberReference mrHandle ->
            let memberRef = activeAssy.Members.[mrHandle]

            match memberRef.Parent with
            | MetadataToken.TypeSpecification specHandle ->
                match activeAssy.TypeSpecs.[specHandle].Signature with
                | TypeDefn.Array (elt, rank) ->
                    match memberRef.PrettyName with
                    | "Get"
                    | "Set"
                    | "Address" -> Some (elt, rank, memberRef.PrettyName)
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    /// Pop `rank` Int32 indices from the eval stack. The C# compiler pushes indices
    /// left-to-right (i_0, i_1, ..., i_{rank-1}), so i_{rank-1} is on top and is
    /// popped first; we fill the result array from the back so callers see indices
    /// in dimension order.
    let private popMultiDimIndices (rank : int) (thread : ThreadId) (state : IlMachineState) : int[] * IlMachineState =
        let indices = Array.zeroCreate<int> rank
        let mutable state = state

        for i = rank - 1 downto 0 do
            let popped, newState = IlMachineState.popEvalStack thread state
            state <- newState

            match popped with
            | EvalStackValue.Int32 v -> indices.[i] <- v
            | other ->
                failwith
                    $"Multi-dim array Get/Set/Address: expected Int32 index on eval stack at dimension %d{i}, got %O{other}"

        indices, state

    /// Convert per-dimension indices into a flat row-major index, validating bounds.
    /// Returns `Error ()` if any index is out of bounds; the caller must raise
    /// IndexOutOfRangeException. The row-major formula for `n` dims with sizes
    /// `d_0, d_1, ..., d_{n-1}` is: `flat = ((((i_0)*d_1 + i_1)*d_2 + i_2)*...)*d_{n-1} + i_{n-1}`.
    let private flatIndexRowMajor (lengths : ImmutableArray<int>) (indices : int[]) : Result<int, unit> =
        if indices.Length <> lengths.Length then
            failwith $"flatIndexRowMajor: rank mismatch (indices=%d{indices.Length}, lengths=%d{lengths.Length})"

        let mutable inBounds = true

        for i = 0 to indices.Length - 1 do
            if indices.[i] < 0 || indices.[i] >= lengths.[i] then
                inBounds <- false

        if not inBounds then
            Result.Error ()
        else
            let mutable acc = 0

            for i = 0 to indices.Length - 1 do
                acc <- acc * lengths.[i] + indices.[i]

            Result.Ok acc

    let private executeMultiDimArrayGet
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (rank : int)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let indices, state = popMultiDimIndices rank thread state
        let arrRef, state = IlMachineState.popEvalStack thread state

        match IlMachineState.evalStackValueToObjectRef state arrRef with
        | None ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->
            let arrAlloc = state.ManagedHeap.Arrays.[arrAddr]

            match flatIndexRowMajor arrAlloc.Lengths indices with
            | Result.Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.IndexOutOfRangeException
                    thread
                    state
            | Result.Ok flatIdx ->
                let value = IlMachineState.getArrayValue arrAddr flatIdx state

                let state =
                    state
                    |> IlMachineState.pushToEvalStack value thread
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed

    let private executeMultiDimArraySet
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (rank : int)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        // Stack from bottom to top for `arr.Set(i_0, ..., i_{rank-1}, value)`:
        //   [arr, i_0, i_1, ..., i_{rank-1}, value]
        // Pop value first, then indices, then receiver.
        let value, state = IlMachineState.popEvalStack thread state
        let indices, state = popMultiDimIndices rank thread state
        let arrRef, state = IlMachineState.popEvalStack thread state

        match IlMachineState.evalStackValueToObjectRef state arrRef with
        | None ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->
            let arrAlloc = state.ManagedHeap.Arrays.[arrAddr]

            let elementHandle =
                match arrAlloc.ConcreteType with
                | ConcreteTypeHandle.Array (elt, _) -> elt
                | other -> failwith $"executeMultiDimArraySet: expected Array allocation, got %O{other}"

            // ECMA-335 III.4.16 (the multi-dim Set counterpart of stelem.ref):
            // for reference-typed arrays, the runtime element type must be
            // assignable from the value's runtime type, otherwise
            // ArrayTypeMismatchException. Without this check a covariant cast
            // (`object[,] o = (object[,])new string[1,1]; o[0,0] = new object();`)
            // would corrupt the heap by storing a non-string into a string[,].
            // For value-type arrays the IL verifier already enforces an exact
            // match, so we only need the dynamic check on object references; for
            // null the assignment is unconditionally legal. The szarray
            // Stelem_ref in NullaryIlOp.fs has the same logical requirement
            // (TODO at NullaryIlOp.fs:497) and is still missing it, but that's
            // a separate gap.
            //
            // CoreCLR raises this *before* the bounds check, so a Set that is
            // simultaneously type-incompatible and out-of-range observes
            // ArrayTypeMismatchException, not IndexOutOfRangeException.
            let state, isAssignable =
                match value with
                | EvalStackValue.NullObjectRef -> state, true
                | EvalStackValue.ObjectRef valueAddr ->
                    let valueRuntimeType = ManagedHeap.getObjectConcreteType valueAddr state.ManagedHeap

                    IlMachineStateExecution.isAssignableFrom
                        loggerFactory
                        baseClassTypes
                        valueRuntimeType
                        elementHandle
                        state
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _
                | EvalStackValue.NativeInt _
                | EvalStackValue.ManagedPointer _
                | EvalStackValue.UserDefinedValueType _ -> state, true

            if not isAssignable then
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.ArrayTypeMismatchException
                    thread
                    state
            else

            match flatIndexRowMajor arrAlloc.Lengths indices with
            | Result.Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.IndexOutOfRangeException
                    thread
                    state
            | Result.Ok flatIdx ->
                let zero, state =
                    IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

                let coerced = EvalStackValue.toCliTypeCoerced zero value

                let state =
                    IlMachineState.setArrayValue arrAddr coerced flatIdx state
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed

    let private executeMultiDimArrayAddress
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (elt : TypeDefn)
        (rank : int)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA-335 III.2.2: Roslyn emits `readonly.` before the multi-dim
        // `Address` call when the language constructs a `ref readonly` element
        // (e.g. `ref readonly object x = ref o[0, 0]` over `object[,] o`).
        // The prefix's scope is exactly the next `Address` call, so consume
        // it here regardless of outcome and skip the element-type check below
        // when it was set, matching the szarray ldelema treatment in
        // UnaryMetadataArrayOps.executeLdelema.
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

        let indices, state = popMultiDimIndices rank thread state
        let arrRef, state = IlMachineState.popEvalStack thread state

        match IlMachineState.evalStackValueToObjectRef state arrRef with
        | None ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | Some arrAddr ->
            let arrAlloc = state.ManagedHeap.Arrays.[arrAddr]

            let runtimeElementHandle =
                match arrAlloc.ConcreteType with
                | ConcreteTypeHandle.Array (eltHandle, _) -> eltHandle
                | other -> failwith $"executeMultiDimArrayAddress: expected Array allocation, got %O{other}"

            let state, typeMismatch =
                if wasReadonly then
                    // The `readonly.` prefix suppresses the element-type check entirely;
                    // we don't even need to resolve the metadata token's element type.
                    state, false
                else
                    // ECMA-335 III.4.10 (the multi-dim Address counterpart of ldelema):
                    // the metadata-declared element type must exactly equal the array's
                    // runtime element type, otherwise ArrayTypeMismatchException. Without
                    // this check a covariant cast (e.g. `object[,] o =
                    // (object[,])new string[1,1];`) could yield a writable byref that the
                    // caller could use to install a non-string into the underlying string
                    // array.
                    let state, expectedElementHandle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            activeAssy.Name
                            currentMethod.DeclaringType.Generics
                            currentMethod.Generics
                            elt

                    state, expectedElementHandle <> runtimeElementHandle

            if typeMismatch then
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.ArrayTypeMismatchException
                    thread
                    state
            else

            match flatIndexRowMajor arrAlloc.Lengths indices with
            | Result.Error () ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.IndexOutOfRangeException
                    thread
                    state
            | Result.Ok flatIdx ->
                let result =
                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrAddr, flatIdx), [])
                    |> EvalStackValue.ManagedPointer

                let state =
                    IlMachineState.pushToEvalStack' result thread state
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed

    /// Try to dispatch a recognised multi-dim array `Get`/`Set`/`Address` call. Returns
    /// `Some` if the call was handled, `None` otherwise (the caller falls through to
    /// the normal call resolution path).
    let private tryDispatchMultiDimArrayCall
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        : (IlMachineState * WhatWeDid) option
        =
        match tryRecognizeMultiDimArrayMethod ctx.ActiveAssembly ctx.MetadataToken with
        | None -> None
        | Some (_elt, rank, "Get") -> Some (executeMultiDimArrayGet ctx state rank)
        | Some (_elt, rank, "Set") -> Some (executeMultiDimArraySet ctx state rank)
        | Some (elt, rank, "Address") -> Some (executeMultiDimArrayAddress ctx state elt rank)
        | Some (_, _, name) ->
            failwith
                $"tryDispatchMultiDimArrayCall: unexpected method name %s{name} (only Get/Set/Address are runtime-synthesised on multi-dim arrays)"

    let executeCall (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Multi-dim array Get/Set/Address are runtime-synthesized methods with no IL body
        // to resolve. Dispatch them directly before the normal resolution path which would
        // fail to find any matching member on `System.Array`.
        match tryDispatchMultiDimArrayCall ctx state with
        | Some result -> result
        | None ->

        let state, methodToCall, methodGenerics, typeArgsFromMetadata =
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

                    state, method, Some spec.Signature, None
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
                    | Choice1Of2 method -> state, method, Some spec.Signature, Some extractedTypeArgs
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
                | Choice1Of2 method -> state, method, None, Some extractedTypeArgs

            | MetadataToken.MethodDef defn ->
                match activeAssy.Methods.TryGetValue defn with
                | true, method ->
                    let method = method |> MethodInfo.mapTypeGenerics (fun _ -> failwith "not generic")
                    state, method, None, None
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
                let methodDeclAssy =
                    state._LoadedAssemblies.[methodToCall.DeclaringType.Assembly.FullName]

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
                    | ConcreteTypeHandle.Pointer _ ->
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
                None
                false
                false
                true
                concretizedMethod.Generics
                concretizedMethod
                thread
                threadState
                None
                false
                state,
            WhatWeDid.Executed
        | FirstLoadThis state -> reinstallConstrained state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException

    let executeCallvirt (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // Multi-dim array Get/Set/Address are runtime-synthesized; see executeCall above.
        // C# emits `call` rather than `callvirt` for these, but the IL is permitted to use
        // either form, so dispatch from both opcodes.
        match tryDispatchMultiDimArrayCall ctx state with
        | Some result -> result
        | None ->

        // TODO: this is presumably super incomplete
        let state, methodToCall, methodGenerics, typeArgsFromMetadata =
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

                    state, method, Some spec.Signature, None
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
                    | Choice1Of2 method -> state, method, Some spec.Signature, Some extractedTypeArgs
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
                | Choice1Of2 method -> state, method, None, Some extractedTypeArgs

            | MetadataToken.MethodDef defn ->
                match activeAssy.Methods.TryGetValue defn with
                | true, method ->
                    let method = method |> MethodInfo.mapTypeGenerics (fun _ -> failwith "not generic")
                    state, method, None, None
                | false, _ -> failwith $"could not find method in {activeAssy.Name}"
            | k -> failwith $"Unrecognised kind: %O{k}"

        let state, concretizedMethod, declaringTypeHandle =
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

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | FirstLoadThis state ->
            // The cctor frame has been pushed; the original callvirt will re-execute. We
            // re-install the prefix on the original frame so the re-entry sees it.
            let state =
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

            state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
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

            let nArgs = methodToCall.Parameters.Length

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
                    let deref = IlMachineState.readManagedByref state src
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
                | ConcreteTypeHandle.Pointer _ ->
                    failwith
                        $"constrained.callvirt: unexpected handle kind %O{tHandle}; pointers and byrefs cannot be generic type arguments"
                | ConcreteTypeHandle.Concrete _ ->

                let tConcrete = AllConcreteTypes.lookup tHandle state.ConcreteTypes |> Option.get

                let tAssy = state._LoadedAssemblies.[tConcrete.Assembly.FullName]
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

                        let derefCli = IlMachineState.readManagedByref state src
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
                                            }

                                        state, cliField :: acc
                                    )

                                List.rev fieldValues
                                |> CliValueType.OfFields baseClassTypes state.ConcreteTypes tHandle tDefn.Layout,
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
                    |> EvalStack.PeekNthFromTop concretizedMethod.Parameters.Length
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
            None
            performInterfaceResolution
            false
            true
            concretizedMethod.Generics
            concretizedMethod
            thread
            threadState
            None
            false
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
