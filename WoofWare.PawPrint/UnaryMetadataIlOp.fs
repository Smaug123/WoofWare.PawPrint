namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryMetadataTokenIlOp)
        (sourcedMetadataToken : SourcedMetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        let logger = loggerFactory.CreateLogger (op.ToString ())

        let activeAssy =
            state.LoadedAssembly sourcedMetadataToken.SourceAssembly
            |> Option.defaultWith (fun () ->
                let available = state._LoadedAssemblies.Keys |> String.concat " ; "

                failwith
                    $"Metadata token source assembly %O{sourcedMetadataToken.SourceAssembly} is not loaded; available assemblies: {available}"
            )

        let metadataToken = sourcedMetadataToken.Token
        let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

        let heapValueByref (addr : ManagedHeapAddress) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        match op with
        | Call ->
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
                            |> MethodInfo.mapTypeGenerics (fun (par, _) ->
                                TypeDefn.GenericTypeParameter par.SequenceNumber
                            )

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
                            false
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

        | Callvirt ->

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
                        failwith
                            $"constrained.callvirt: expected ManagedPointer receiver on the eval stack, got %O{other}"

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
                                        |> List.filter (fun field ->
                                            not (field.Attributes.HasFlag FieldAttributes.Static)
                                        )

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

        | Castclass ->
            let actualObj, state = IlMachineState.popEvalStack thread state

            let state, targetType, _targetAssy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    ImmutableArray.Empty
                    metadataToken

            let state, targetConcreteType =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    targetType

            match actualObj with
            | EvalStackValue.NullObjectRef ->
                // Per ECMA-335 III.4.3: null ref is always valid for castclass on reference types.
                let state =
                    state
                    |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed
            | EvalStackValue.ObjectRef addr ->
                let objConcreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

                let state, isAssignable =
                    IlMachineState.isConcreteTypeAssignableTo
                        loggerFactory
                        baseClassTypes
                        state
                        objConcreteType
                        targetConcreteType

                if isAssignable then
                    let state =
                        state
                        |> IlMachineState.pushToEvalStack' actualObj thread
                        |> IlMachineState.advanceProgramCounter thread

                    state, WhatWeDid.Executed
                else
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.InvalidCastException
                        thread
                        state
            | other -> failwith $"Castclass: unexpected eval stack value {other}"
        | Newobj ->
            let state, ctor, typeArgsFromMetadata =
                match metadataToken with
                | MethodDef md ->
                    let method = activeAssy.Methods.[md]

                    state, MethodInfo.mapTypeGenerics (fun _ -> failwith "non-generic method") method, None
                | MemberReference mr ->
                    let state, _, method, extractedTypeArgs =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match method with
                    | Choice1Of2 mr -> state, mr, Some extractedTypeArgs
                    | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            let state, concretizedCtor, declaringTypeHandle =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    ctor
                    None
                    typeArgsFromMetadata
                    state

            let state, init =
                IlMachineStateExecution.ensureTypeInitialised
                    loggerFactory
                    baseClassTypes
                    thread
                    declaringTypeHandle
                    state

            match init with
            | WhatWeDid.BlockedOnClassInit state -> failwith "TODO: another thread is running the initialiser"
            | WhatWeDid.SuspendedForClassInit -> state, WhatWeDid.SuspendedForClassInit
            | WhatWeDid.ThrowingTypeInitializationException -> state, WhatWeDid.ThrowingTypeInitializationException
            | WhatWeDid.Executed ->

            let ctorAssembly = state.LoadedAssembly ctor.DeclaringType.Assembly |> Option.get
            let ctorType = ctorAssembly.TypeDefs.[ctor.DeclaringType.Definition.Get]

            do
                logger.LogDebug (
                    "Creating object of type {ConstructorAssembly}.{ConstructorType}",
                    ctorAssembly.Name.Name,
                    ctorType.Name
                )

            let state, allFields =
                IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state declaringTypeHandle

            let fields =
                CliValueType.OfFields baseClassTypes state.ConcreteTypes declaringTypeHandle ctorType.Layout allFields

            // Note: this is a bit unorthodox for value types, which *aren't* heap-allocated.
            // We'll perform their construction on the heap, though, to keep the interface
            // of Newobj uniform.
            // On completion of the constructor, we'll copy the value back off the heap,
            // and put it on the eval stack directly.
            let allocatedAddr, state =
                let ty =
                    AllConcreteTypes.findExistingConcreteType
                        state.ConcreteTypes
                        concretizedCtor.DeclaringType.Identity
                        concretizedCtor.DeclaringType.Generics
                    |> Option.get

                IlMachineState.allocateManagedObject ty fields state

            let state =
                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ctorType then
                    state
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.ManagedPointer (heapValueByref allocatedAddr))
                        thread
                else
                    state
                    |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

            let threadState = state.ThreadState.[thread]

            IlMachineStateExecution.callMethod
                loggerFactory
                baseClassTypes
                None
                (Some allocatedAddr)
                false
                false
                true
                concretizedCtor.Generics
                concretizedCtor
                thread
                threadState
                None
                false
                state,
            WhatWeDid.Executed
        | Newarr ->
            let currentState = state.ThreadState.[thread]
            let popped, methodState = MethodState.popFromStack currentState.MethodState

            let currentState =
                ThreadState.setFrame currentState.ActiveMethodState methodState currentState

            let len =
                match popped with
                | EvalStackValue.Int32 v -> v
                | popped -> failwith $"unexpectedly popped value %O{popped} to serve as array len"

            let typeGenerics = currentMethod.DeclaringType.Generics

            let state, elementType, assy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    metadataToken

            let state, zeroOfType, concreteTypeHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    assy
                    elementType
                    typeGenerics
                    methodState.Generics
                    state

            let arrayType = ConcreteTypeHandle.OneDimArrayZero concreteTypeHandle

            let alloc, state =
                IlMachineState.allocateArray arrayType (fun () -> zeroOfType) len state

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add thread currentState
                }
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Box ->
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
                | _ -> failwith $"unexpected token {metadataToken} in Box"

            let state, typeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    ty

            let toBox, state = state |> IlMachineState.popEvalStack thread

            let targetType =
                AllConcreteTypes.lookup typeHandle state.ConcreteTypes |> Option.get

            let defn =
                state._LoadedAssemblies.[targetType.Assembly.FullName].TypeDefs.[targetType.Definition.Get]

            let isNullable =
                targetType.Namespace = "System"
                && targetType.Name = "Nullable`1"
                && targetType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

            let toPush, state =
                if isNullable then
                    // Nullable<T> boxing: null when !HasValue, box underlying T when HasValue.
                    match toBox with
                    | EvalStackValue.UserDefinedValueType cvt ->
                        let hasValueField =
                            IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "hasValue"

                        let hasValue = CliValueType.DereferenceFieldById hasValueField cvt

                        match hasValue with
                        | CliType.Bool 0uy ->
                            // Nullable with HasValue=false boxes to null.
                            EvalStackValue.NullObjectRef, state
                        | CliType.Bool _ ->
                            // Nullable with HasValue=true: box the underlying value as T.
                            let underlyingTypeHandle = targetType.Generics.[0]

                            let valueField =
                                IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "value"

                            let value = CliValueType.DereferenceFieldById valueField cvt

                            let cvt, state =
                                match value with
                                | CliType.ValueType existingCvt ->
                                    // Multi-field struct: use the stored CliValueType directly.
                                    existingCvt, state
                                | _ ->
                                    // Primitive or single-field: reconstruct from type definition.
                                    let underlyingConcreteType =
                                        AllConcreteTypes.lookup underlyingTypeHandle state.ConcreteTypes |> Option.get

                                    let underlyingDefn =
                                        state._LoadedAssemblies.[underlyingConcreteType.Assembly.FullName].TypeDefs
                                            .[underlyingConcreteType.Definition.Get]

                                    let underlyingInstanceFields =
                                        underlyingDefn.Fields
                                        |> List.filter (fun field ->
                                            not (field.Attributes.HasFlag FieldAttributes.Static)
                                        )

                                    let underlyingAssembly =
                                        state._LoadedAssemblies.[underlyingConcreteType.Assembly.FullName]

                                    let valueAsEval = EvalStackValue.ofCliType value

                                    let state, fieldValues =
                                        ((state, []), underlyingInstanceFields)
                                        ||> List.fold (fun (state, acc) field ->
                                            let state, fieldZero, fieldTypeHandle =
                                                IlMachineState.cliTypeZeroOf
                                                    loggerFactory
                                                    baseClassTypes
                                                    underlyingAssembly
                                                    field.Signature
                                                    underlyingConcreteType.Generics
                                                    ImmutableArray.Empty
                                                    state

                                            let coerced = EvalStackValue.toCliTypeCoerced fieldZero valueAsEval

                                            let cliField : CliField =
                                                {
                                                    Id = FieldId.metadata underlyingTypeHandle field.Handle field.Name
                                                    Name = field.Name
                                                    Contents = coerced
                                                    Offset = field.Offset
                                                    Type = fieldTypeHandle
                                                }

                                            state, cliField :: acc
                                        )

                                    List.rev fieldValues
                                    |> CliValueType.OfFields
                                        baseClassTypes
                                        state.ConcreteTypes
                                        underlyingTypeHandle
                                        underlyingDefn.Layout,
                                    state

                            let addr, state =
                                IlMachineState.allocateManagedObject underlyingTypeHandle cvt state

                            EvalStackValue.ObjectRef addr, state
                        | other -> failwith $"Box Nullable: expected Bool for hasValue field, got %O{other}"
                    | _ -> failwith $"Box Nullable: expected UserDefinedValueType on eval stack, got %O{toBox}"
                elif DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn then
                    // Boxing a value type: wrap it in a heap object and push an ObjectRef
                    let cvt, state =
                        match toBox with
                        | EvalStackValue.UserDefinedValueType cvt ->
                            // Already have the CliValueType with the right field structure
                            cvt, state
                        | _ ->
                            // Primitive value on the eval stack (Int32, Int64, Float, etc.)
                            // Construct a CliValueType from the type definition's instance fields
                            let targetAssembly = state._LoadedAssemblies.[targetType.Assembly.FullName]

                            let instanceFields =
                                defn.Fields
                                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                            let state, fieldValues =
                                ((state, []), instanceFields)
                                ||> List.fold (fun (state, acc) field ->
                                    let state, fieldZero, fieldTypeHandle =
                                        IlMachineState.cliTypeZeroOf
                                            loggerFactory
                                            baseClassTypes
                                            targetAssembly
                                            field.Signature
                                            targetType.Generics
                                            ImmutableArray.Empty
                                            state

                                    let coerced = EvalStackValue.toCliTypeCoerced fieldZero toBox

                                    let cliField : CliField =
                                        {
                                            Id = FieldId.metadata typeHandle field.Handle field.Name
                                            Name = field.Name
                                            Contents = coerced
                                            Offset = field.Offset
                                            Type = fieldTypeHandle
                                        }

                                    state, cliField :: acc
                                )

                            let cvt =
                                List.rev fieldValues
                                |> CliValueType.OfFields baseClassTypes state.ConcreteTypes typeHandle defn.Layout

                            cvt, state

                    let addr, state = IlMachineState.allocateManagedObject typeHandle cvt state

                    EvalStackValue.ObjectRef addr, state
                else
                    // Reference type: box is a no-op, value passes through unchanged
                    toBox, state

            state
            |> IlMachineState.pushToEvalStack' toPush thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldelema ->
            let index, state = IlMachineState.popEvalStack thread state
            let arr, state = IlMachineState.popEvalStack thread state

            let index =
                match index with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith $"TODO: {index}"

            let arrAddr =
                match IlMachineState.evalStackValueToObjectRef state arr with
                | Some addr -> addr
                | None -> failwith "TODO: throw NRE"

            // TODO: throw ArrayTypeMismatchException if incorrect types

            let arr = state.ManagedHeap.Arrays.[arrAddr]

            if index < 0 || index >= arr.Length then
                failwith "TODO: throw IndexOutOfRangeException"

            let result =
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrAddr, index), [])
                |> EvalStackValue.ManagedPointer

            let state =
                IlMachineState.pushToEvalStack' result thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Isinst ->
            let actualObj, state = IlMachineState.popEvalStack thread state

            let state, targetType, _targetAssy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    ImmutableArray.Empty
                    metadataToken

            let state, targetConcreteType =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    targetType

            let isinstCheck
                (state : IlMachineState)
                (objConcreteType : ConcreteTypeHandle)
                (successValue : EvalStackValue)
                : IlMachineState * EvalStackValue
                =
                let state, result =
                    IlMachineState.isConcreteTypeAssignableTo
                        loggerFactory
                        baseClassTypes
                        state
                        objConcreteType
                        targetConcreteType

                if result then
                    state, successValue
                else
                    state, EvalStackValue.NullObjectRef

            let state, returnObj =
                match actualObj with
                | EvalStackValue.NullObjectRef ->
                    // null IsInstance check always succeeds and results in a null reference
                    state, EvalStackValue.NullObjectRef
                | EvalStackValue.ObjectRef addr ->
                    let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                    isinstCheck state concreteType actualObj
                | EvalStackValue.ManagedPointer src ->
                    match IlMachineState.readManagedByref state src with
                    | CliType.ObjectRef None -> state, EvalStackValue.NullObjectRef
                    | CliType.ObjectRef (Some addr) ->
                        let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                        isinstCheck state concreteType (EvalStackValue.ObjectRef addr)
                    | other -> failwith $"TODO: Isinst on managed pointer to non-object-ref {other}"
                | esv -> failwith $"TODO: Isinst on {esv}"

            let state =
                state
                |> IlMachineState.pushToEvalStack' returnObj thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Stfld ->
            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    let field =
                        activeAssy.Fields.[f]
                        |> FieldInfo.mapTypeGenerics (fun _ -> failwith "no generics allowed in FieldDefinition")

                    state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, field, _ =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            do
                logger.LogTrace (
                    "Storing in object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    field.DeclaringType.Name,
                    field.Name,
                    field.Signature
                )

            if field.Attributes.HasFlag FieldAttributes.Static then
                failwith
                    $"stfld cannot store static field %O{field.DeclaringType.Assembly.Name}.%s{field.DeclaringType.Namespace}.%s{field.DeclaringType.Name}::%s{field.Name}; use stsfld. This indicates invalid IL or a misresolved field token."

            let valueToStore, state = IlMachineState.popEvalStack thread state
            let currentObj, state = IlMachineState.popEvalStack thread state

            let state, declaringTypeHandle, typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

            let state, zero, concreteTypeHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    activeAssy
                    field.Signature
                    typeGenerics
                    ImmutableArray.Empty // field can't have its own generics
                    state

            let valueToStore = EvalStackValue.toCliTypeCoerced zero valueToStore

            match currentObj with
            | EvalStackValue.NullObjectRef ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | _ ->

            let state =
                match currentObj with
                | EvalStackValue.Int32 _ -> failwith "unexpectedly setting field on an int"
                | EvalStackValue.Int64 _ -> failwith "unexpectedly setting field on an int64"
                | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                    failwith
                        $"TODO: stfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}; synthetic MethodTableAuxiliaryData cache writes are not modelled"
                | EvalStackValue.NativeInt _ -> failwith "unexpectedly setting field on a nativeint"
                | EvalStackValue.Float _ -> failwith "unexpectedly setting field on a float"
                | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | EvalStackValue.ObjectRef addr ->
                    match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                    | false, _ -> failwith $"todo: array {addr}"
                    | true, v ->
                        let v = AllocatedNonArrayObject.SetFieldById fieldId valueToStore v

                        let heap =
                            { state.ManagedHeap with
                                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add addr v
                            }

                        { state with
                            ManagedHeap = heap
                        }
                | EvalStackValue.ManagedPointer src ->
                    IlMachineState.writeManagedByref
                        state
                        (ManagedPointerSource.appendProjection (ByrefProjection.Field fieldId) src)
                        valueToStore
                | EvalStackValue.UserDefinedValueType _ -> failwith "todo"

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Stsfld ->
            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match activeAssy.Fields.TryGetValue fieldHandle with
                    | false, _ -> failwith "TODO: Stsfld - throw MissingFieldException"
                    | true, field ->
                        let field =
                            field
                            |> FieldInfo.mapTypeGenerics (fun _ -> failwith "no generics allowed in FieldDefinition")

                        state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, method, _ =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match method with
                    | Choice1Of2 methodInfo ->
                        failwith $"unexpectedly asked to store to a non-field method: {methodInfo.Name}"
                    | Choice2Of2 fieldInfo -> state, fieldInfo
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            do
                let declaring = activeAssy.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogTrace (
                    "Storing in static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let state, declaringTypeHandle, typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
            | NothingToDo state ->

            let popped, state = IlMachineState.popEvalStack thread state

            let state, zero, concreteTypeHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    activeAssy
                    field.Signature
                    typeGenerics
                    ImmutableArray.Empty // field can't have its own generics
                    state

            let toStore = EvalStackValue.toCliTypeCoerced zero popped

            let state =
                IlMachineState.setStatic
                    declaringTypeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    toStore
                    state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        | Ldfld ->
            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    let field =
                        activeAssy.Fields.[f]
                        |> FieldInfo.mapTypeGenerics (fun _ -> failwith "no generics allowed on FieldDefinition")

                    state, field
                | MetadataToken.MemberReference mr ->
                    let state, assyName, field, _ =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            do
                let declaring = activeAssy.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogTrace (
                    "Loading object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            if field.Attributes.HasFlag FieldAttributes.Static then
                failwith
                    $"ldfld cannot load static field %O{field.DeclaringType.Assembly.Name}.%s{field.DeclaringType.Namespace}.%s{field.DeclaringType.Name}::%s{field.Name}; use ldsfld. This indicates invalid IL or a misresolved field token."

            let currentObj, state = IlMachineState.popEvalStack thread state

            let state, declaringTypeHandle, typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

            match currentObj with
            | EvalStackValue.NullObjectRef ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | _ ->

            let state =
                match currentObj with
                | EvalStackValue.Int32 i -> failwith "todo: int32"
                | EvalStackValue.Int64 int64 -> failwith "todo: int64"
                | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed methodTableFor))
                | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr methodTableFor) ->
                    match
                        MethodTableProjection.tryProjectField loggerFactory baseClassTypes field methodTableFor state
                    with
                    | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                    | None ->
                        failwith
                            $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTablePtr %O{methodTableFor}"
                | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                    match
                        MethodTableProjection.tryProjectAuxiliaryDataField baseClassTypes field methodTableFor state
                    with
                    | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                    | None ->
                        failwith
                            $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}"
                | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)) ->
                    failwith
                        $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through open generic RuntimeTypeHandleTarget %O{identity}"
                | EvalStackValue.NativeInt nativeIntSource -> failwith $"todo: nativeint {nativeIntSource}"
                | EvalStackValue.Float f -> failwith "todo: float"
                | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | EvalStackValue.ObjectRef managedHeapAddress ->
                    match RawArrayDataProjection.tryProjectField baseClassTypes field managedHeapAddress state with
                    | Some value -> IlMachineState.pushToEvalStack value thread state
                    | None ->
                        match state.ManagedHeap.NonArrayObjects.TryGetValue managedHeapAddress with
                        | false, _ -> failwith $"todo: array {managedHeapAddress}"
                        | true, v ->
                            IlMachineState.pushToEvalStack
                                (AllocatedNonArrayObject.DereferenceFieldById fieldId v)
                                thread
                                state
                | EvalStackValue.ManagedPointer src ->
                    let currentValue =
                        IlMachineState.readManagedByrefField baseClassTypes state src fieldId

                    IlMachineState.pushToEvalStack currentValue thread state
                | EvalStackValue.UserDefinedValueType vt ->
                    let result = vt |> CliValueType.DereferenceFieldById fieldId

                    IlMachineState.pushToEvalStack result thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldflda ->
            let ptr, state = IlMachineState.popEvalStack thread state

            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    let field =
                        activeAssy.Fields.[f]
                        |> FieldInfo.mapTypeGenerics (fun _ -> failwith "no generics allowed on FieldDefinition")

                    state, field
                | MetadataToken.MemberReference mr ->
                    let state, assyName, field, _ =
                        // TODO: generics
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            let state, declaringTypeHandle, _typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

            match ptr with
            | NullObjectRef ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | _ ->

            let result =
                match ptr with
                | Int32 _
                | Int64 _
                | Float _ -> failwith "expected pointer type"
                | NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                    failwith
                        $"TODO: ldflda {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}; synthetic MethodTableAuxiliaryData field addresses are not modelled"
                | NativeInt nativeIntSource ->
                    failwith
                        $"TODO: ldflda {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through native pointer %O{nativeIntSource}"
                | ManagedPointer src -> ManagedPointerSource.appendProjection (ByrefProjection.Field fieldId) src
                | NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
                | ObjectRef addr ->
                    match RuntimeFieldProjection.tryProjectFieldAddress baseClassTypes field addr state with
                    | Some ptr -> ptr
                    | None -> ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, fieldId), [])
                | UserDefinedValueType evalStackValueUserType -> failwith "todo"
                |> EvalStackValue.ManagedPointer

            state
            |> IlMachineState.pushToEvalStack' result thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldsfld ->
            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match activeAssy.Fields.TryGetValue fieldHandle with
                    | false, _ -> failwith "TODO: Ldsfld - throw MissingFieldException"
                    | true, field ->
                        let field =
                            field
                            |> FieldInfo.mapTypeGenerics (fun _ -> failwith "generics not allowed in FieldDefinition")

                        state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, field, _ =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            activeAssy
                            ImmutableArray.Empty
                            mr
                            state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            do
                let declaring =
                    state.LoadedAssembly field.DeclaringType.Assembly
                    |> Option.get
                    |> fun a -> a.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogTrace (
                    "Loading from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let state, declaringTypeHandle, typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
            | NothingToDo state ->

            let fieldValue, state =
                match
                    IlMachineState.getStatic
                        declaringTypeHandle
                        (ComparableFieldDefinitionHandle.Make field.Handle)
                        state
                with
                | None ->
                    let state, newVal, concreteTypeHandle =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            activeAssy
                            field.Signature
                            typeGenerics
                            ImmutableArray.Empty // field can't have its own generics
                            state

                    newVal,
                    IlMachineState.setStatic
                        declaringTypeHandle
                        (ComparableFieldDefinitionHandle.Make field.Handle)
                        newVal
                        state
                | Some v -> v, state

            do
                let declaring =
                    state
                        .LoadedAssembly(field.DeclaringType.Assembly)
                        .Value.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogTrace (
                    "Loaded from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType}), value {LoadedValue}",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature,
                    fieldValue
                )

            let state =
                IlMachineState.pushToEvalStack fieldValue thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        | Unbox_Any ->
            // ECMA-335 III.4.33
            let actualObj, state = IlMachineState.popEvalStack thread state

            let state, targetType, _targetAssy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    ImmutableArray.Empty
                    metadataToken

            let state, targetConcreteTypeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    targetType

            let targetConcreteType =
                AllConcreteTypes.lookup targetConcreteTypeHandle state.ConcreteTypes
                |> Option.get

            let targetDefn =
                state._LoadedAssemblies.[targetConcreteType.Assembly.FullName].TypeDefs
                    .[targetConcreteType.Definition.Get]

            let isNullable =
                targetConcreteType.Namespace = "System"
                && targetConcreteType.Name = "Nullable`1"
                && targetConcreteType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName

            let isValueType =
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn

            if isNullable then
                failwith "TODO: Unbox_Any for Nullable<T> unimplemented"
            elif not isValueType then
                // Reference-type target: behave exactly like castclass.
                // TODO: factor the shared castclass/unbox.any reference-type logic into a helper.
                match actualObj with
                | EvalStackValue.NullObjectRef ->
                    state
                    |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                | EvalStackValue.ObjectRef addr ->
                    let objConcreteType =
                        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                        | true, v -> v.ConcreteType
                        | false, _ ->
                            match state.ManagedHeap.Arrays.TryGetValue addr with
                            | true, _v -> failwith "TODO: Unbox_Any on array objects (reference-type target)"
                            | false, _ -> failwith $"Unbox_Any: could not find managed object with address {addr}"

                    let state, isAssignable =
                        IlMachineState.isConcreteTypeAssignableTo
                            loggerFactory
                            baseClassTypes
                            state
                            objConcreteType
                            targetConcreteTypeHandle

                    if isAssignable then
                        state
                        |> IlMachineState.pushToEvalStack' actualObj thread
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    else
                        IlMachineStateExecution.raiseRuntimeException
                            loggerFactory
                            baseClassTypes
                            baseClassTypes.InvalidCastException
                            thread
                            state
                | other -> failwith $"Unbox_Any (reference-type target): unexpected eval stack value {other}"
            else
                // Value-type target, non-Nullable.
                match actualObj with
                | EvalStackValue.NullObjectRef ->
                    IlMachineStateExecution.raiseRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.NullReferenceException
                        thread
                        state
                | EvalStackValue.ObjectRef addr ->
                    let boxedOpt =
                        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                        | true, v -> Some v
                        | false, _ ->
                            match state.ManagedHeap.Arrays.TryGetValue addr with
                            | true, _ ->
                                // Array object with non-array value-type target: wrong type, per CLR this
                                // is an InvalidCastException, not an interpreter abort.
                                None
                            | false, _ -> failwith $"Unbox_Any: could not find managed object with address {addr}"

                    match boxedOpt with
                    | None ->
                        IlMachineStateExecution.raiseRuntimeException
                            loggerFactory
                            baseClassTypes
                            baseClassTypes.InvalidCastException
                            thread
                            state
                    | Some boxed ->

                    // Exact-type match per ECMA-335 III.4.33, not assignability.
                    // TODO: relax to underlying-type equivalence so a boxed enum can be unboxed to its
                    // underlying integral type (spec's "same type-verifier type"). Needs a generic-method
                    // test to exercise; not in scope for this PR.
                    if boxed.ConcreteType = targetConcreteTypeHandle then
                        // Push the boxed value back onto the eval stack. The push path
                        // (EvalStackValue.ofCliType) handles primitive-like value types
                        // (IntPtr, RuntimeTypeHandle, enums, ...) via the flatten invariant,
                        // and leaves genuine user-defined value types as UserDefinedValueType.
                        //
                        // For primitive targets (Int32, Float64, etc.) the Box path stored
                        // the value in a single-field struct (e.g. { value__ = Int32 42 }).
                        // The zero of such types is a non-ValueType CliType, so we detect
                        // them here and extract the inner field before pushing.
                        let toPush, state =
                            if boxed.Contents.PrimitiveLikeKind.IsSome then
                                // Primitive-like (incl. enum): ofCliType will flatten on push.
                                CliType.ValueType boxed.Contents, state
                            else
                                let targetZero, state =
                                    IlMachineState.cliTypeZeroOfHandle state baseClassTypes targetConcreteTypeHandle

                                match targetZero with
                                | CliType.ValueType _ ->
                                    // Genuine user-defined value type: keep wrapped.
                                    CliType.ValueType boxed.Contents, state
                                | _ ->
                                    // Primitive target: Box stored the value in a single instance
                                    // field at offset 0 whose Size matches the primitive. Read it
                                    // back by offset/size — the Box path guarantees the shape, so
                                    // this is a nominal dereference, not a structural guess.
                                    let size = (CliType.SizeOf targetZero).Size

                                    CliValueType.DereferenceFieldAt 0 size boxed.Contents, state

                        state
                        |> IlMachineState.pushToEvalStack toPush thread
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    else
                        IlMachineStateExecution.raiseRuntimeException
                            loggerFactory
                            baseClassTypes
                            baseClassTypes.InvalidCastException
                            thread
                            state
                | other -> failwith $"Unbox_Any (value-type target): unexpected eval stack value {other}"
        | Stelem ->
            let declaringTypeGenerics = currentMethod.DeclaringType.Generics

            let state, assy, elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state,
                    activeAssy,
                    activeAssy.TypeDefs.[defn]
                    |> TypeInfo.mapGeneric (fun (p, _) -> TypeDefn.GenericTypeParameter p.SequenceNumber)
                | MetadataToken.TypeSpecification spec ->
                    let state, assy, ty =
                        IlMachineState.resolveTypeFromSpecConcrete
                            loggerFactory
                            baseClassTypes
                            spec
                            activeAssy
                            declaringTypeGenerics
                            currentMethod.Generics
                            state

                    state, assy, ty
                | x -> failwith $"TODO: Stelem element type resolution unimplemented for {x}"

            let contents, state = IlMachineState.popEvalStack thread state
            let index, state = IlMachineState.popEvalStack thread state

            let index =
                match index with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith $"Expected int32 index in Stelem, but got: {index}"

            let arr, state = IlMachineState.popEvalStack thread state

            let arr =
                match IlMachineState.evalStackValueToObjectRef state arr with
                | Some addr -> addr
                | None -> failwith "expected heap allocation for array, got null"

            let elementType =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies elementType

            let state, zeroOfType, concreteTypeHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    assy
                    elementType
                    declaringTypeGenerics
                    ImmutableArray.Empty
                    state

            let contents = EvalStackValue.toCliTypeCoerced zeroOfType contents

            IlMachineState.setArrayValue arr contents index state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldelem ->
            let declaringTypeGenerics = currentMethod.DeclaringType.Generics

            let state, assy, elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state,
                    activeAssy,
                    activeAssy.TypeDefs.[defn]
                    |> TypeInfo.mapGeneric (fun (p, _) -> TypeDefn.GenericTypeParameter p.SequenceNumber)
                | MetadataToken.TypeSpecification spec ->
                    let state, assy, ty =
                        IlMachineState.resolveTypeFromSpecConcrete
                            loggerFactory
                            baseClassTypes
                            spec
                            activeAssy
                            declaringTypeGenerics
                            currentMethod.Generics
                            state

                    state, assy, ty
                | x -> failwith $"TODO: Ldelem element type resolution unimplemented for {x}"

            let index, state = IlMachineState.popEvalStack thread state

            let index =
                match index with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith $"Expected int32 index in Stelem, but got: {index}"

            let arr, state = IlMachineState.popEvalStack thread state

            let arr =
                match IlMachineState.evalStackValueToObjectRef state arr with
                | Some addr -> addr
                | None -> failwith "expected heap allocation for array, got null"

            let toPush =
                match state.ManagedHeap.Arrays.TryGetValue arr with
                | false, _ -> failwith $"unexpectedly failed to find array allocation {arr} in Ldelem"
                | true, v ->
                    if 0 <= index && index < v.Elements.Length then
                        v.Elements.[index]
                    else
                        failwith "TODO: raise an out of bounds"

            IlMachineState.pushToEvalStack toPush thread state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Initobj ->
            let popped, state = IlMachineState.popEvalStack thread state
            let declaringTypeGenerics = currentMethod.DeclaringType.Generics

            let state, targetType, assy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    declaringTypeGenerics
                    metadataToken

            let state, zeroOfType, concreteTypeHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    assy
                    targetType
                    declaringTypeGenerics
                    ImmutableArray.Empty
                    state

            let state =
                match popped with
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.NativeInt _
                | EvalStackValue.Float _ -> failwith "unexpectedly not an address"
                | EvalStackValue.NullObjectRef
                | EvalStackValue.ObjectRef _ -> failwith "TODO: Initobj requires a managed pointer"
                | EvalStackValue.ManagedPointer src -> IlMachineState.writeManagedByref state src zeroOfType
                | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldsflda ->

            // TODO: check whether we should throw FieldAccessException

            let field =
                match metadataToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match activeAssy.Fields.TryGetValue fieldHandle with
                    | false, _ -> failwith "TODO: Ldsflda - throw MissingFieldException"
                    | true, field ->
                        field
                        |> FieldInfo.mapTypeGenerics (fun _ -> failwith "generics not allowed on FieldDefinition")
                | t -> failwith $"Unexpectedly asked to load a non-field: {t}"

            let state, declaringTypeHandle, typeGenerics =
                ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
            | NothingToDo state ->

            match
                IlMachineState.peByteRangeForFieldRva loggerFactory baseClassTypes activeAssy field typeGenerics state
            with
            | state, Some peByteRange ->
                let state, ptr =
                    IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed
            | state, None ->
                // TODO: if field type is unmanaged, push an unmanaged pointer
                let fieldHandle = ComparableFieldDefinitionHandle.Make field.Handle

                let state =
                    match IlMachineState.getStatic declaringTypeHandle fieldHandle state with
                    | Some _ -> state
                    | None ->
                        // Field is not yet initialised
                        let state, zero, _concreteTypeHandle =
                            IlMachineState.cliTypeZeroOf
                                loggerFactory
                                baseClassTypes
                                activeAssy
                                field.Signature
                                typeGenerics
                                ImmutableArray.Empty // field can't have its own generics
                                state

                        IlMachineState.setStatic declaringTypeHandle fieldHandle zero state

                let ptr =
                    ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringTypeHandle, fieldHandle), [])

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed

        | Ldftn ->
            let method, methodGenerics =
                match metadataToken with
                | MetadataToken.MethodDef handle ->
                    let method =
                        activeAssy.Methods.[handle]
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    method, None
                | MetadataToken.MethodSpecification h ->
                    let spec = activeAssy.MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token ->
                        let method =
                            activeAssy.Methods.[token]
                            |> MethodInfo.mapTypeGenerics (fun (par, _) ->
                                TypeDefn.GenericTypeParameter par.SequenceNumber
                            )

                        method, Some spec.Signature
                    | k -> failwith $"Unrecognised MethodSpecification kind: %O{k}"
                | t -> failwith $"Unexpectedly asked to Ldftn a non-method: {t}"

            let state, concretizedMethod, _declaringTypeHandle =
                ExecutionConcretization.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    method
                    methodGenerics
                    None
                    state

            logger.LogDebug (
                "Pushed pointer to function {LdFtnAssembly}.{LdFtnType}.{LdFtnMethodName}",
                method.DeclaringType.Assembly.Name,
                method.DeclaringType.Name,
                method.Name
            )

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.FunctionPointer concretizedMethod))
                thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Stobj ->
            let state, ty, assy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    metadataToken

            let state, typeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    ty

            let targetZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

            let valueToStore, state = IlMachineState.popEvalStack thread state
            let addr, state = IlMachineState.popEvalStack thread state

            let writeAt (src : ManagedPointerSource) : IlMachineState =
                let coerced = EvalStackValue.toCliTypeCoerced targetZero valueToStore

                match src with
                | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte _, _) ->
                    IlMachineState.writeManagedByrefBytes state src coerced
                | ManagedPointerSource.Byref _ -> IlMachineState.writeManagedByref state src coerced
                | ManagedPointerSource.Null -> failwith "unreachable: null Stobj target handled above"

            match addr with
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) ->
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.NullReferenceException
                    thread
                    state
            | EvalStackValue.ManagedPointer src
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
                writeAt src
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Stobj through native pointer %O{nativeIntSource} is not implemented"
            | EvalStackValue.ObjectRef _ ->
                failwith "Stobj on an object reference is invalid; expected a managed pointer"
            | EvalStackValue.Int32 _
            | EvalStackValue.Int64 _
            | EvalStackValue.Float _
            | EvalStackValue.UserDefinedValueType _ -> failwith $"Stobj target was not an address: %O{addr}"
        | Constrained ->
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
        | Ldtoken ->
            // Helper function to handle type tokens and create RuntimeTypeHandle
            let handleTypeToken
                (declaringAssembly : DumpedAssembly)
                (allowOpenGenericDefinition : bool)
                (typeDefn : TypeDefn)
                (state : IlMachineState)
                : IlMachineState
                =
                let ty = baseClassTypes.RuntimeTypeHandle
                let field = ty.Fields |> List.exactlyOne

                if field.Name <> "m_type" then
                    failwith $"unexpected field name ${field.Name} for BCL type RuntimeTypeHandle"

                let methodGenerics = currentMethod.Generics
                let typeGenerics = currentMethod.DeclaringType.Generics

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
                        baseClassTypes.Corelib.Name
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
                    |> CliValueType.OfFields baseClassTypes state.ConcreteTypes runtimeTypeHandleHandle Layout.Default

                IlMachineState.pushToEvalStack (CliType.ValueType vt) thread state

            let state =
                match metadataToken with
                | MetadataToken.FieldDefinition h ->
                    // TODO: how do we know what concrete type this is a field on?
                    let runtimeFieldHandle, state =
                        IlMachineState.getOrAllocateField loggerFactory baseClassTypes activeAssy.Name h state

                    IlMachineState.pushToEvalStack runtimeFieldHandle thread state
                | MetadataToken.MethodDef h ->
                    let method =
                        activeAssy.Methods.[h]
                        |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                    if not method.DeclaringType.Generics.IsEmpty then
                        failwith
                            $"TODO: ldtoken MethodDef for methods on generic declaring types requires open generic RuntimeMethodHandle support; got %O{method}"

                    if not method.Generics.IsEmpty then
                        failwith
                            $"TODO: ldtoken MethodDef for generic methods requires open generic RuntimeMethodHandle support; got %O{method}"

                    let state, concretizedMethod, _declaringTypeHandle =
                        ExecutionConcretization.concretizeMethodForExecution
                            loggerFactory
                            baseClassTypes
                            thread
                            method
                            None
                            None
                            state

                    let runtimeMethodHandle, state =
                        IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretizedMethod state

                    IlMachineState.pushToEvalStack runtimeMethodHandle thread state
                | MetadataToken.TypeSpecification h ->
                    // Use the raw TypeSpec signature directly, bypassing the lossy
                    // resolveTypeFromDefn → TypeInfo → typeInfoToTypeDefn round-trip.
                    // TypeInfo cannot represent array/pointer/byref wrappers, so the
                    // round-trip would collapse e.g. typeof(X[]) to typeof(X).
                    let sign = activeAssy.TypeSpecs.[h].Signature
                    handleTypeToken activeAssy false sign state
                | MetadataToken.TypeReference h ->
                    let typeGenerics = currentMethod.DeclaringType.Generics

                    let state, typeDefn, assy =
                        IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics h

                    handleTypeToken assy true typeDefn state
                | MetadataToken.TypeDefinition h ->
                    let state, typeDefn =
                        IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h

                    handleTypeToken activeAssy true typeDefn state
                | _ -> failwith $"Unexpected metadata token %O{metadataToken} in LdToken"

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Cpobj -> failwith "TODO: Cpobj unimplemented"
        | Ldobj ->
            let state, ty, assy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    metadataToken

            let state, typeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    ty

            let addr, state = state |> IlMachineState.popEvalStack thread

            let obj =
                match addr with
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NullReferenceException"
                | EvalStackValue.ObjectRef _ ->
                    failwith "Ldobj on an object reference is invalid; expected a managed pointer"
                | EvalStackValue.ManagedPointer ptr -> IlMachineState.readManagedByref state ptr
                | EvalStackValue.Float _
                | EvalStackValue.Int64 _
                | EvalStackValue.Int32 _ -> failwith "refusing to interpret constant as address"
                | _ -> failwith "TODO"

            let targetType =
                AllConcreteTypes.lookup typeHandle state.ConcreteTypes |> Option.get

            let defn =
                state._LoadedAssemblies.[targetType.Assembly.FullName].TypeDefs.[targetType.Definition.Get]

            let toPush, state =
                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn then
                    let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

                    EvalStackValue.ofCliType obj |> EvalStackValue.toCliTypeCoerced zero, state
                else
                    // III.4.13: reference types are just copied as pointers.
                    // We should have received a pointer, so let's just pass it back.
                    obj, state

            state
            |> IlMachineState.pushToEvalStack toPush thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Sizeof ->
            let state, ty, assy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    currentMethod.DeclaringType.Generics
                    metadataToken

            let state, typeHandle =
                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringType.Generics
                    currentMethod.Generics
                    ty

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

            let size = CliType.sizeOf zero

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Calli -> failwith "TODO: Calli unimplemented"
        | Unbox -> failwith "TODO: Unbox unimplemented"
        | Ldvirtftn -> failwith "TODO: Ldvirtftn unimplemented"
        | Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | Refanyval -> failwith "TODO: Refanyval unimplemented"
        | Jmp -> failwith "TODO: Jmp unimplemented"
