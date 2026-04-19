namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineStateExecution =
    let getTypeOfObj
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (esv : EvalStackValue)
        : IlMachineState * ConcreteTypeHandle
        =
        match esv with
        | EvalStackValue.Int32 _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int32
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.Int64 _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int64
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
        | EvalStackValue.Float _ ->
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Double
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
        | EvalStackValue.ManagedPointer _ -> failwith "cannot get type of managed pointer target"
        | EvalStackValue.ObjectRef addr ->
            let o = ManagedHeap.get addr state.ManagedHeap
            state, o.ConcreteType
        | EvalStackValue.NullObjectRef -> failwith "TODO: throw NullReferenceException"
        | EvalStackValue.UserDefinedValueType tuples -> failwith "todo"

    let isAssignableFrom
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (objToCast : ConcreteTypeHandle)
        (possibleTargetType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state objToCast possibleTargetType

    let callMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (wasInitialising : ConcreteTypeHandle option)
        (wasConstructing : ManagedHeapAddress option)
        (performInterfaceResolution : bool)
        (wasClassConstructor : bool)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (thread : ThreadId)
        (threadState : ThreadState)
        (callSiteIlOpIndexOverride : int option)
        (dispatchAsExceptionOnReturn : bool)
        (state : IlMachineState)
        : IlMachineState
        =
        let logger = loggerFactory.CreateLogger "CallMethod"
        let activeAssy = state.ActiveAssembly thread

        // Check for intrinsics first
        let isIntrinsic =
            MethodInfo.isJITIntrinsic
                (fun handle ->
                    match activeAssy.Members.[handle].Parent with
                    | MetadataToken.TypeReference r -> activeAssy.TypeRefs.[r]
                    | x -> failwith $"{x}"
                )
                activeAssy.Methods
                methodToCall

        match
            if isIntrinsic then
                Intrinsics.call loggerFactory baseClassTypes methodToCall thread state
            else
                None
        with
        | Some result -> result
        | None ->

        // Get zero values for all parameters
        let state, argZeroObjects =
            ((state, []), methodToCall.Signature.ParameterTypes)
            ||> List.fold (fun (state, zeros) tyHandle ->
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tyHandle
                state, zero :: zeros
            )

        let argZeroObjects = List.rev argZeroObjects

        let activeMethodState = threadState.MethodState

        let state, methodToCall =
            match methodToCall.Instructions, performInterfaceResolution, methodToCall.IsStatic with
            | None, true, false ->
                logger.LogDebug (
                    "Identifying target of virtual call for {TypeName}.{MethodName}",
                    methodToCall.DeclaringType.Name,
                    methodToCall.Name
                )
                // This might be an interface implementation, or implemented by native code.
                // If native code, we'll deal with that when we actually start implementing.

                // Since we're not static, there's a `this` on the eval stack.
                // It comes *below* all the arguments.
                let callingObj =
                    match
                        activeMethodState.EvaluationStack
                        |> EvalStack.PeekNthFromTop methodToCall.Parameters.Length
                    with
                    | None -> failwith "unexpectedly no `this` on the eval stack of instance method"
                    | Some this -> this

                let state, callingObjTyHandle =
                    getTypeOfObj loggerFactory baseClassTypes state callingObj

                let callingObjTy =
                    let ty =
                        AllConcreteTypes.lookup callingObjTyHandle state.ConcreteTypes |> Option.get

                    state.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get]

                let declaringAssy = state.LoadedAssembly(methodToCall.DeclaringType.Assembly).Value

                let methodDeclaringType =
                    declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

                let interfaceExplicitNamedMethod =
                    if methodDeclaringType.IsInterface then
                        Some
                            $"{TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) methodDeclaringType}.{methodToCall.Name}"
                    else
                        None

                // Does type `callingObjTy` implement this method? If so, this is probably a JIT intrinsic or
                // is supplied by the runtime.
                let selfImplementation, state =
                    (state, callingObjTy.Methods)
                    ||> List.mapFold (fun state meth ->
                        if
                            meth.Signature.GenericParameterCount
                            <> methodToCall.Signature.GenericParameterCount
                            || meth.Signature.RequiredParameterCount
                               <> methodToCall.Signature.RequiredParameterCount
                        then
                            None, state
                        else if

                            meth.Name <> methodToCall.Name && Some meth.Name <> interfaceExplicitNamedMethod
                        then
                            None, state
                        else

                        // TODO: check if methodToCall's declaringtype is an interface; if so, check the possible prefixed name first

                        let state, retType =
                            meth.Signature.ReturnType
                            |> IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                meth.DeclaringType.Assembly
                                methodToCall.DeclaringType.Generics
                                methodToCall.Generics

                        let paramTypes, state =
                            (state, meth.Signature.ParameterTypes)
                            ||> Seq.mapFold (fun state ty ->
                                ty
                                |> IlMachineState.concretizeType
                                    loggerFactory
                                    baseClassTypes
                                    state
                                    meth.DeclaringType.Assembly
                                    methodToCall.DeclaringType.Generics
                                    methodToCall.Generics
                                |> fun (a, b) -> b, a
                            )

                        let paramTypes = List.ofSeq paramTypes

                        let state, retAssignable =
                            isAssignableFrom
                                loggerFactory
                                baseClassTypes
                                retType
                                methodToCall.Signature.ReturnType
                                state

                        if retAssignable && paramTypes = methodToCall.Signature.ParameterTypes then
                            Some (meth, Some meth.Name = interfaceExplicitNamedMethod), state
                        else
                            None, state
                    )

                let selfImplementation =
                    selfImplementation
                    |> List.choose id
                    |> List.sortBy (fun (_, isInterface) -> if isInterface then -1 else 0)

                match selfImplementation with
                | (impl, true) :: l when (l |> List.forall (fun (_, b) -> not b)) ->
                    logger.LogDebug "Found concrete implementation from an interface"

                    let typeGenerics =
                        AllConcreteTypes.lookup callingObjTyHandle state.ConcreteTypes
                        |> Option.get
                        |> _.Generics

                    let state, meth, _ =
                        IlMachineState.concretizeMethodWithAllGenerics
                            loggerFactory
                            baseClassTypes
                            typeGenerics
                            impl
                            methodGenerics
                            state

                    state, meth
                | [ impl, false ] ->
                    logger.LogDebug "Found concrete implementation"
                    // Yes, callingObjTy implements the method directly. No need to look up interfaces.
                    let typeGenerics =
                        AllConcreteTypes.lookup callingObjTyHandle state.ConcreteTypes
                        |> Option.get
                        |> _.Generics

                    let state, meth, _ =
                        IlMachineState.concretizeMethodWithAllGenerics
                            loggerFactory
                            baseClassTypes
                            typeGenerics
                            impl
                            methodGenerics
                            state

                    state, meth
                | _ :: _ ->
                    selfImplementation
                    |> List.map (fun (m, _) -> m.Name)
                    |> String.concat ", "
                    |> failwithf "multiple options: %s"
                | [] ->

                logger.LogDebug "No concrete implementation found; scanning interfaces"

                // If not, what interfaces does it implement, and do any of those implement the method?
                let possibleInterfaceMethods, state =
                    (state, callingObjTy.ImplementedInterfaces)
                    ||> Seq.mapFold (fun state impl ->
                        let assy = state.LoadedAssembly impl.RelativeToAssembly |> Option.get

                        let state, defn =
                            match impl.InterfaceHandle with
                            | MetadataToken.TypeDefinition defn ->
                                let state, defn = IlMachineState.lookupTypeDefn baseClassTypes state assy defn

                                let state, _, defn =
                                    // TODO: generics
                                    IlMachineState.resolveTypeFromDefn
                                        loggerFactory
                                        baseClassTypes
                                        defn
                                        ImmutableArray.Empty
                                        ImmutableArray.Empty
                                        assy
                                        state

                                state, defn
                            | MetadataToken.TypeReference ty ->
                                let state, defn, assy =
                                    IlMachineState.lookupTypeRef loggerFactory baseClassTypes state assy Seq.empty ty

                                state, failwith "TODO"
                            | MetadataToken.TypeSpecification spec ->
                                // TODO: generics
                                let state, assy, defn =
                                    IlMachineState.resolveTypeFromSpec
                                        loggerFactory
                                        baseClassTypes
                                        spec
                                        assy
                                        ImmutableArray.Empty
                                        ImmutableArray.Empty
                                        state

                                state, defn
                            | handle -> failwith $"unexpected: {handle}"

                        logger.LogDebug (
                            "Interface {InterfaceName} (generics: {InterfaceGenerics})",
                            defn.Name,
                            defn.Generics
                        )

                        let s, state =
                            defn.Methods
                            |> Seq.filter (fun mi -> mi.Name = methodToCall.Name
                            // TODO: also the rest of the signature
                            )
                            |> Seq.mapFold
                                (fun state meth ->
                                    // TODO: generics
                                    let state, mi, _ =
                                        IlMachineState.concretizeMethodForExecution
                                            loggerFactory
                                            baseClassTypes
                                            thread
                                            meth
                                            None
                                            (if defn.Generics.IsEmpty then None else Some defn.Generics)
                                            state

                                    mi, state
                                )
                                state

                        s, state
                    )

                let possibleInterfaceMethods = possibleInterfaceMethods |> Seq.concat |> Seq.toList

                match possibleInterfaceMethods with
                | [] ->
                    logger.LogDebug "No interface implementation found either"
                    state, methodToCall
                | [ meth ] ->
                    logger.LogDebug (
                        "Exactly one interface implementation found {DeclaringTypeNamespace}.{DeclaringTypeName}.{MethodName} ({MethodGenerics})",
                        meth.DeclaringType.Namespace,
                        meth.DeclaringType.Name,
                        meth.Name,
                        meth.Generics
                    )

                    state, meth
                | _ -> failwith "TODO: handle overloads"
            | _, _, true
            | _, false, _
            | Some _, _, _ -> state, methodToCall

        // Helper to pop and coerce a single argument
        let popAndCoerceArg zeroType methodState =
            let value, newState = MethodState.popFromStack methodState
            EvalStackValue.toCliTypeCoerced zeroType value, newState

        let thisArgCoercionTarget
            (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            : CliType
            =
            let declaringAssembly =
                state.LoadedAssembly (methodToCall.DeclaringType.Assembly) |> Option.get

            let declaringType =
                declaringAssembly.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies declaringType then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
            else
                CliType.ObjectRef None

        // Collect arguments based on calling convention
        let args, afterPop =
            if methodToCall.IsStatic then
                // Static method: pop args in reverse order
                let args = ImmutableArray.CreateBuilder methodToCall.Parameters.Length
                let mutable currentState = activeMethodState

                for i = methodToCall.Parameters.Length - 1 downto 0 do
                    let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                    args.Add arg
                    currentState <- newState

                args.Reverse ()
                args.ToImmutable (), currentState
            else
                // Instance method: handle `this` pointer
                let argCount = methodToCall.Parameters.Length
                let args = ImmutableArray.CreateBuilder (argCount + 1)
                let mutable currentState = activeMethodState
                let thisArgTarget = thisArgCoercionTarget methodToCall

                match wasConstructing with
                | Some _ ->
                    // Constructor: `this` is on top of stack, by our own odd little calling convention
                    // where Newobj puts the object pointer on top
                    let thisArg, newState = popAndCoerceArg thisArgTarget currentState

                    currentState <- newState

                    // Pop remaining args in reverse
                    for i = argCount - 1 downto 0 do
                        let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                        args.Add arg
                        currentState <- newState

                    args.Add thisArg
                    args.Reverse ()
                    args.ToImmutable (), currentState
                | None ->
                    // Regular instance method: args then `this`
                    for i = argCount - 1 downto 0 do
                        let arg, newState = popAndCoerceArg argZeroObjects.[i] currentState
                        args.Add arg
                        currentState <- newState

                    let thisArg, newState =
                        let rawThis, newState = MethodState.popFromStack currentState

                        let coerced =
                            match thisArgTarget, rawThis with
                            | CliType.RuntimePointer _, EvalStackValue.ObjectRef addr ->
                                // Boxed value type receiver: implicit unbox to managed pointer
                                // into the heap object's value data.
                                CliType.RuntimePointer (
                                    CliRuntimePointer.Managed (
                                        ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
                                    )
                                )
                            | _ -> EvalStackValue.toCliTypeCoerced thisArgTarget rawThis

                        coerced, newState

                    args.Add thisArg
                    currentState <- newState

                    args.Reverse ()
                    args.ToImmutable (), currentState

        // Helper to create new frame with assembly loading
        let rec createNewFrame state =
            let returnInfo =
                Some
                    {
                        JumpTo = threadState.ActiveMethodState
                        WasInitialisingType = wasInitialising
                        WasConstructingObj = wasConstructing
                        CallSiteIlOpIndex = callSiteIlOpIndexOverride |> Option.defaultValue afterPop.IlOpIndex
                        DispatchAsExceptionOnReturn = dispatchAsExceptionOnReturn
                    }

            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    (state.ActiveAssembly thread)
                    methodToCall
                    methodGenerics
                    args
                    returnInfo
            with
            | Ok frame -> state, frame
            | Error toLoad ->
                let state' =
                    (state, toLoad)
                    ||> List.fold (fun s (asmRef : WoofWare.PawPrint.AssemblyReference) ->
                        let s, _, _ =
                            IlMachineState.loadAssembly
                                loggerFactory
                                (state.LoadedAssembly methodToCall.DeclaringType.Assembly |> Option.get)
                                (fst asmRef.Handle)
                                s

                        s
                    )

                createNewFrame state'

        let state, newFrame = createNewFrame state

        let oldFrame =
            if wasClassConstructor || not advanceProgramCounterOfCaller then
                afterPop
            else
                afterPop |> MethodState.advanceProgramCounter

        let threadState =
            ThreadState.setFrame threadState.ActiveMethodState oldFrame threadState

        let calleeFrameId, threadState = ThreadState.appendFrame newFrame threadState
        let newThreadState = ThreadState.setActiveFrame calleeFrameId threadState

        { state with
            ThreadState = state.ThreadState |> Map.add thread newThreadState
        }

    let rec loadClass
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : ConcreteTypeHandle)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        let logger = loggerFactory.CreateLogger "LoadClass"

        match TypeInitTable.tryGet ty state.TypeInitTable with
        | Some TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.Failed (tieAddr, tieType)) ->
            // The .cctor previously threw. Per ECMA-335, subsequent access should throw
            // TypeInitializationException. We rethrow the *same* cached instance to match
            // CLR identity semantics (ReferenceEquals across repeated accesses).
            match
                ExceptionDispatching.throwExceptionObject
                    loggerFactory
                    baseClassTypes
                    state
                    currentThread
                    tieAddr
                    tieType
            with
            | ExceptionDispatchResult.HandlerFound state -> StateLoadResult.ThrowingTypeInitializationException state
            | ExceptionDispatchResult.ExceptionUnhandled _ ->
                failwith $"Unhandled TypeInitializationException during class loading for type with cached TIE"
        | Some (TypeInitState.InProgress tid) when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | Some (TypeInitState.InProgress _) ->
            // This is usually signalled by WhatWeDid.Blocked
            failwith
                "TODO: cross-thread class init synchronization unimplemented - this thread has to wait for the other thread to finish initialisation"
        | None ->
            // We have work to do!

            // Look up the concrete type from the handle
            let concreteType =
                match AllConcreteTypes.lookup ty state.ConcreteTypes with
                | Some ct -> ct
                | None -> failwith $"ConcreteTypeHandle {ty} not found in ConcreteTypes mapping"

            let state, origAssyName =
                state.WithThreadSwitchedToAssembly concreteType.Assembly currentThread

            let sourceAssembly = state.LoadedAssembly concreteType.Assembly |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue concreteType.Definition.Get with
                | false, _ ->
                    failwith
                        $"Failed to find type definition {concreteType.Definition.Get} in {concreteType.Assembly.FullName}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // The CLR does not eagerly run base type initializers before the current type's .cctor.
            // Base types get initialized later when their own constructors or static members are touched.
            // TODO: also need to initialise any prerequisites that the CLI genuinely requires here;
            // if so, do them *before* WithTypeBeginInit, otherwise a suspended prerequisite causes
            // retries to see "in-progress" and skip this type's own .cctor.
            let state = state.WithTypeBeginInit currentThread ty

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && method.Parameters.IsEmpty)

            match cctor with
            | Some cctorMethod ->
                // Call the class constructor! Note that we *don't* use `callMethodInActiveAssembly`, because that
                // performs class loading, but we're already in the middle of loading this class.
                // TODO: factor out the common bit.
                let currentThreadState = state.ThreadState.[currentThread]

                // Convert the method's type generics from TypeDefn to ConcreteTypeHandle
                let cctorMethodWithTypeGenerics =
                    cctorMethod
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> concreteType.Generics.[par.SequenceNumber])

                // Convert method generics (should be empty for cctor)
                let cctorMethodWithMethodGenerics =
                    cctorMethodWithTypeGenerics
                    |> MethodInfo.mapMethodGenerics (fun _ -> failwith "cctor cannot be generic")

                // Convert method signature from TypeDefn to ConcreteTypeHandle using concretization
                let state, convertedSignature =
                    cctorMethodWithMethodGenerics.Signature
                    |> TypeMethodSignature.map
                        state
                        (fun state typeDefn ->
                            IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                concreteType.Assembly
                                concreteType.Generics
                                // no method generics for cctor
                                ImmutableArray.Empty
                                typeDefn
                        )

                // Convert method instructions (local variables)
                let state, convertedInstructions =
                    match cctorMethodWithMethodGenerics.Instructions with
                    | None -> state, None
                    | Some methodInstr ->
                        let state, convertedLocalVars =
                            match methodInstr.LocalVars with
                            | None -> state, None
                            | Some localVars ->
                                // Concretize each local variable type
                                let state, convertedVars =
                                    ((state, []), localVars)
                                    ||> Seq.fold (fun (state, acc) typeDefn ->
                                        let state, handle =
                                            IlMachineState.concretizeType
                                                loggerFactory
                                                baseClassTypes
                                                state
                                                concreteType.Assembly
                                                concreteType.Generics
                                                ImmutableArray.Empty // no method generics for cctor
                                                typeDefn

                                        state, handle :: acc
                                    )
                                    |> Tuple.rmap ImmutableArray.CreateRange

                                state, Some convertedVars

                        state, Some (MethodInstructions.setLocalVars convertedLocalVars methodInstr)

                let fullyConvertedMethod =
                    MethodInfo.setMethodVars convertedInstructions convertedSignature cctorMethodWithMethodGenerics

                callMethod
                    loggerFactory
                    baseClassTypes
                    (Some ty)
                    None
                    true
                    true
                    false
                    // constructor is surely not generic
                    ImmutableArray.Empty
                    fullyConvertedMethod
                    currentThread
                    currentThreadState
                    None
                    false
                    state
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state = state.WithTypeEndInit currentThread ty

                // Restore original assembly context if needed
                state.WithThreadSwitchedToAssembly origAssyName currentThread
                |> fst
                |> NothingToDo

    let ensureTypeInitialised
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match TypeInitTable.tryGet ty state.TypeInitTable with
        | None ->
            match loadClass loggerFactory baseClassTypes ty thread state with
            | NothingToDo state -> state, WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Some TypeInitState.Initialized -> state, WhatWeDid.Executed
        | Some (TypeInitState.Failed (tieAddr, tieType)) ->
            // The .cctor for this type threw. Per ECMA-335, subsequent access should throw
            // TypeInitializationException. Rethrow the cached instance for CLR identity semantics.
            match
                ExceptionDispatching.throwExceptionObject loggerFactory baseClassTypes state thread tieAddr tieType
            with
            | ExceptionDispatchResult.HandlerFound state -> state, WhatWeDid.ThrowingTypeInitializationException
            | ExceptionDispatchResult.ExceptionUnhandled _ ->
                failwith
                    "Unhandled TypeInitializationException during ensureTypeInitialised; should have been caught by a handler"
        | Some (TypeInitState.InProgress threadId) ->
            if threadId = thread then
                // II.10.5.3.2: avoid the deadlock by simply proceeding.
                state, WhatWeDid.Executed
            else
                state, WhatWeDid.BlockedOnClassInit threadId

    /// It may be useful to *not* advance the program counter of the caller, e.g. if you're using `callMethodInActiveAssembly`
    /// as a convenient way to move to a different method body rather than to genuinely perform a call.
    /// (Delegates do this, for example: we get a call to invoke the delegate, and then we implement the delegate as
    /// another call to its function pointer.)
    let callMethodInActiveAssembly
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (performInterfaceResolution : bool)
        (advanceProgramCounterOfCaller : bool)
        (methodGenerics : TypeDefn ImmutableArray option)
        (methodToCall : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
        (weAreConstructingObj : ManagedHeapAddress option)
        (typeArgsFromMetadata : TypeDefn ImmutableArray option)
        (dispatchAsExceptionOnReturn : bool)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let threadState = state.ThreadState.[thread]

        let state, concretizedMethod, declaringTypeHandle =
            IlMachineState.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                methodToCall
                methodGenerics
                typeArgsFromMetadata
                state

        let state, typeInit =
            ensureTypeInitialised loggerFactory baseClassTypes thread declaringTypeHandle state

        match typeInit with
        | WhatWeDid.Executed ->
            callMethod
                loggerFactory
                baseClassTypes
                None
                weAreConstructingObj
                performInterfaceResolution
                false
                advanceProgramCounterOfCaller
                concretizedMethod.Generics
                concretizedMethod
                thread
                threadState
                None
                dispatchAsExceptionOnReturn
                state,
            WhatWeDid.Executed
        | _ -> state, typeInit

    /// Allocate a runtime-synthesised exception, push its default constructor frame, and
    /// return to the dispatch loop.  When the ctor completes (Ret), returnStackFrame will
    /// signal DispatchException so the Ret handler can dispatch the exception.
    ///
    /// This mirrors the CLR's EEException::CreateThrowable which allocates, calls the
    /// default ctor, then overwrites HResult.
    /// See: https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
    let raiseManagedException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        // 0. Concretize the exception type and ensure it is initialized before allocating.
        //    This mirrors the Newobj pattern: type init must precede allocation so that if
        //    the .cctor needs to run (or has previously failed), we return early without
        //    leaking an allocation or corrupting the eval stack.
        let stk : SignatureTypeKind =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies exceptionTypeInfo

        let state, exnTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                exceptionTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (exceptionTypeInfo.Identity, stk))

        let state, typeInit =
            ensureTypeInitialised loggerFactory baseClassTypes currentThread exnTypeHandle state

        match typeInit with
        | WhatWeDid.Executed ->

            // 1. Allocate the zero-initialised exception with _HResult pre-set.
            let addr, _exnHandle, state =
                ExceptionDispatching.allocateRuntimeException loggerFactory baseClassTypes exceptionTypeInfo state

            // 2. Find the parameterless .ctor on the exception type.
            let assy = state._LoadedAssemblies.[exceptionTypeInfo.Assembly.FullName]
            let typeDef = assy.TypeDefs.[exceptionTypeInfo.Identity.TypeDefinition.Get]

            if not typeDef.Generics.IsEmpty then
                failwith
                    $"raiseManagedException: expected non-generic exception type, but %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %i{typeDef.Generics.Length} generic parameter(s)"

            let ctor =
                typeDef.Methods
                |> List.tryFind (fun method ->
                    method.Name = ".ctor" && not method.IsStatic && method.Parameters.IsEmpty
                )
                |> Option.defaultWith (fun () ->
                    failwith
                        $"raiseManagedException: no parameterless .ctor found on %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name}"
                )
                // The type has no generic parameters (guarded above), so any GenericParamFromMetadata
                // in the ctor's type-generic positions is unreachable. Map them to TypeDefn to satisfy
                // callMethodInActiveAssembly's signature.
                |> MethodInfo.mapTypeGenerics (fun _ ->
                    failwith "raiseManagedException: exception type was unexpectedly generic"
                )

            // 3. Push the allocated object ref as `this` for the ctor.
            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) currentThread state

            // 4. Call the ctor, marking the return state so that returnStackFrame dispatches
            //    the exception instead of pushing the object onto the caller's eval stack.
            //    Do NOT advance the caller's PC: when the ctor returns and exception dispatch
            //    begins, handler lookup and the stack-trace frame must see the faulting
            //    instruction's PC, not the next instruction.  (Same class of bug as call-site
            //    vs resumed-PC for cross-frame unwinding, which CallSiteIlOpIndex solves.)
            let state, _ =
                state.WithThreadSwitchedToAssembly exceptionTypeInfo.Assembly currentThread

            callMethodInActiveAssembly
                loggerFactory
                baseClassTypes
                currentThread
                false // no interface resolution
                false // do NOT advance caller PC — dispatch needs the faulting instruction's offset
                None // no method generics
                ctor
                (Some addr) // weAreConstructingObj
                None // no type args from metadata
                true // dispatchAsExceptionOnReturn
                state
        | other -> state, other
