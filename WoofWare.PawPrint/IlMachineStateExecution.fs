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
        | EvalStackValue.ManagedPointer src ->
            match src with
            | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) -> failwith "todo"
            | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) -> failwith "todo"
            | ManagedPointerSource.Heap addr ->
                let o = ManagedHeap.get addr state.ManagedHeap
                state, o.ConcreteType
            | ManagedPointerSource.ArrayIndex (arr, index) -> failwith "todo"
            | ManagedPointerSource.Null -> failwith "todo"
            | ManagedPointerSource.Field (managedPointerSource, fieldName) -> failwith "todo"
        | EvalStackValue.ObjectRef addr ->
            let o = ManagedHeap.get addr state.ManagedHeap
            state, o.ConcreteType
        | EvalStackValue.UserDefinedValueType tuples -> failwith "todo"

    let isAssignableFrom
        (objToCast : ConcreteTypeHandle)
        (possibleTargetType : ConcreteTypeHandle)
        (state : IlMachineState)
        : bool
        =
        if objToCast = possibleTargetType then
            true
        else

        let objToCast' = AllConcreteTypes.lookup objToCast state.ConcreteTypes |> Option.get

        let possibleTargetType' =
            AllConcreteTypes.lookup possibleTargetType state.ConcreteTypes |> Option.get

        // TODO: null can be assigned to any reference type; might not be relevant here?

        match possibleTargetType with
        | ConcreteObj state.ConcreteTypes -> true
        | ConcreteValueType state.ConcreteTypes when failwith "check if objToCast inherits ValueType" -> true
        | _ ->
            // Claude describes the algorithm here:
            // https://claude.ai/chat/f15e23f6-a27b-4655-9e69-e4d445dd1249
            failwith
                $"TODO: check inheritance chain and interfaces: is {objToCast'} assignable from {possibleTargetType'}?"

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
                Intrinsics.call baseClassTypes methodToCall thread state
            else
                None
        with
        | Some result -> result
        | None ->

        if methodToCall.Name = "GetValue" then
            printfn ""

        // Get zero values for all parameters
        let state, argZeroObjects =
            ((state, []), methodToCall.Signature.ParameterTypes)
            ||> List.fold (fun (state, zeros) tyHandle ->
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes tyHandle
                state, zero :: zeros
            )

        let argZeroObjects = List.rev argZeroObjects

        let activeMethodState = threadState.MethodStates.[threadState.ActiveMethodState]

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

                        if
                            isAssignableFrom retType methodToCall.Signature.ReturnType state
                            && paramTypes = methodToCall.Signature.ParameterTypes
                        then
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

                match wasConstructing with
                | Some _ ->
                    // Constructor: `this` is on top of stack, by our own odd little calling convention
                    // where Newobj puts the object pointer on top
                    let thisArg, newState =
                        popAndCoerceArg
                            (CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null))
                            currentState

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
                        popAndCoerceArg
                            (CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null))
                            currentState

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

        let newThreadState =
            { threadState with
                MethodStates = threadState.MethodStates.Add(newFrame).SetItem (threadState.ActiveMethodState, oldFrame)
                ActiveMethodState = threadState.MethodStates.Length
            }

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

            // First mark as in-progress to detect cycles
            let state = state.WithTypeBeginInit currentThread ty

            // Check if the type has a base type that needs initialization
            let firstDoBaseClass =
                match typeDef.BaseType with
                | Some baseTypeInfo ->
                    // Determine if base type is in the same or different assembly
                    match baseTypeInfo with
                    | BaseTypeInfo.ForeignAssemblyType _ -> failwith "TODO"
                    //logger.LogDebug (
                    //    "Resolved base type of {TypeDefNamespace}.{TypeDefName} to foreign assembly {ForeignAssemblyName}",
                    //    typeDef.Namespace,
                    //    typeDef.Name,
                    //    baseAssemblyName.Name
                    //)

                    //match loadClass loggerFactory baseTypeHandle baseAssemblyName currentThread state with
                    //| FirstLoadThis state -> Error state
                    //| NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeDef typeDefinitionHandle ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to this assembly, typedef",
                            typeDef.Namespace,
                            typeDef.Name
                        )

                        let baseTypeDefn =
                            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeDef

                        // Concretize the base type
                        let state, baseTypeHandle =
                            IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                sourceAssembly.Name
                                concreteType.Generics
                                // TODO: surely we have generics in scope here?
                                ImmutableArray.Empty
                                baseTypeDefn

                        // Recursively load the base class
                        match loadClass loggerFactory baseClassTypes baseTypeHandle currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeRef typeReferenceHandle ->
                        let state, assy, targetType =
                            // TypeRef won't have any generics; it would be a TypeSpec if it did
                            IlMachineState.resolveType
                                loggerFactory
                                typeReferenceHandle
                                ImmutableArray.Empty
                                (state.ActiveAssembly currentThread)
                                state

                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to a typeref in assembly {ResolvedAssemblyName}, {BaseTypeNamespace}.{BaseTypeName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            assy.Name.Name,
                            targetType.Namespace,
                            targetType.Name
                        )

                        // Create a TypeDefn from the resolved TypeRef
                        let baseTypeDefn =
                            targetType
                            |> DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies

                        // Concretize the base type
                        let state, baseTypeHandle =
                            IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                sourceAssembly.Name
                                concreteType.Generics
                                // TODO: surely we have generics in scope here?
                                ImmutableArray.Empty
                                baseTypeDefn

                        // Recursively load the base class
                        match loadClass loggerFactory baseClassTypes baseTypeHandle currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | BaseTypeInfo.TypeSpec typeSpecificationHandle ->
                        failwith "TODO: TypeSpec base type loading unimplemented"
                | None -> Ok state // No base type (or it's System.Object)

            match firstDoBaseClass with
            | Error state -> FirstLoadThis state
            | Ok state ->

            // TODO: also need to initialise all interfaces implemented by the type

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
        | Some TypeInitState.Initialized -> state, WhatWeDid.Executed
        | Some (InProgress threadId) ->
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
                state,
            WhatWeDid.Executed
        | _ -> state, typeInit
