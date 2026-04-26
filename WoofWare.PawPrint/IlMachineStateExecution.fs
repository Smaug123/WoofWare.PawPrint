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
            let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
            state, concreteType
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

    let tryResolveVirtualImplementation
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (state : IlMachineState)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        let logger = loggerFactory.CreateLogger "CallMethod"

        logger.LogDebug (
            "Identifying target of virtual call for {TypeName}.{MethodName}",
            methodToCall.DeclaringType.Name,
            methodToCall.Name
        )

        let declaringAssy = state.LoadedAssembly(methodToCall.DeclaringType.Assembly).Value

        let methodDeclaringType =
            declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

        let interfaceExplicitNamedMethod =
            if methodDeclaringType.IsInterface then
                Some
                    $"{TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) methodDeclaringType}.{methodToCall.Name}"
            else
                None

        let signatureMatchesTarget
            (candidateAssembly : AssemblyName)
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (candidateSignature : TypeMethodSignature<TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            if
                candidateSignature.GenericParameterCount
                <> methodToCall.Signature.GenericParameterCount
                || candidateSignature.RequiredParameterCount
                   <> methodToCall.Signature.RequiredParameterCount
            then
                state, false
            else

            let state, candidateSignature =
                candidateSignature
                |> TypeMethodSignature.map
                    state
                    (fun state ty ->
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            candidateAssembly
                            candidateTypeGenerics
                            methodToCall.Generics
                            ty
                    )

            let state, retAssignable =
                match candidateSignature.ReturnType, methodToCall.Signature.ReturnType with
                | MethodReturnType.Void, MethodReturnType.Void -> state, true
                | MethodReturnType.Returns retType, MethodReturnType.Returns targetType ->
                    isAssignableFrom loggerFactory baseClassTypes retType targetType state
                | MethodReturnType.Void, MethodReturnType.Returns _
                | MethodReturnType.Returns _, MethodReturnType.Void -> state, false

            state,
            retAssignable
            && candidateSignature.ParameterTypes = methodToCall.Signature.ParameterTypes

        let methodReferenceMatchesTarget
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (meth : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            if meth.Name <> methodToCall.Name then
                state, false
            else
                signatureMatchesTarget meth.DeclaringType.Assembly candidateTypeGenerics meth.Signature state

        let methodMatches
            (candidateTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (allowImplicitInterfaceImplementation : bool)
            (meth : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : (WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * bool) option *
              IlMachineState
            =
            if
                meth.Signature.GenericParameterCount
                <> methodToCall.Signature.GenericParameterCount
                || meth.Signature.RequiredParameterCount
                   <> methodToCall.Signature.RequiredParameterCount
            then
                None, state
            elif
                meth.Name <> methodToCall.Name
                && (not allowImplicitInterfaceImplementation
                    || Some meth.Name <> interfaceExplicitNamedMethod)
            then
                None, state
            elif
                not allowImplicitInterfaceImplementation
                && (not (meth.MethodAttributes.HasFlag MethodAttributes.Virtual)
                    || (meth.MethodAttributes.HasFlag MethodAttributes.NewSlot
                        && meth.Handle <> methodToCall.Handle))
            then
                None, state
            else

            let state, matches =
                signatureMatchesTarget meth.DeclaringType.Assembly candidateTypeGenerics meth.Signature state

            if matches then
                Some (meth, Some meth.Name = interfaceExplicitNamedMethod), state
            else
                None, state

        let concretizeTypeArgs
            (declaringAssembly : AssemblyName)
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (args : TypeDefn ImmutableArray)
            (state : IlMachineState)
            : IlMachineState * ImmutableArray<ConcreteTypeHandle>
            =
            ((state, ImmutableArray.CreateBuilder<ConcreteTypeHandle> ()), args)
            ||> Seq.fold (fun (state, acc) ty ->
                let state, handle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        declaringAssembly
                        contextTypeGenerics
                        methodGenerics
                        ty

                acc.Add handle
                state, acc
            )
            |> Tuple.rmap (fun builder -> builder.ToImmutable ())

        let concreteTypeHandlesToTypeDefns
            (state : IlMachineState)
            (handles : ImmutableArray<ConcreteTypeHandle>)
            : ImmutableArray<TypeDefn>
            =
            handles
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let resolveMethodReference
            (contextTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
            (relativeAssembly : DumpedAssembly)
            (token : MetadataToken)
            (state : IlMachineState)
            : IlMachineState *
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn> *
              TypeDefn ImmutableArray option
            =
            match token with
            | MetadataToken.MethodDef h ->
                let method =
                    relativeAssembly.Methods.[h]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                state, method, None
            | MetadataToken.MemberReference h ->
                let contextTypeGenerics = concreteTypeHandlesToTypeDefns state contextTypeGenerics
                let contextMethodGenerics = concreteTypeHandlesToTypeDefns state methodGenerics

                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMemberWithGenerics
                        loggerFactory
                        baseClassTypes
                        thread
                        relativeAssembly
                        contextTypeGenerics
                        contextMethodGenerics
                        methodGenerics
                        h
                        state

                match method with
                | Choice1Of2 method -> state, method, Some extractedTypeArgs
                | Choice2Of2 _field -> failwith "MethodImpl referenced a field where a method was expected"
            | other ->
                // ECMA-335 permits MethodSpec here for generic method implementations; resolve it when
                // MethodImpl dispatch reaches such metadata.
                failwith $"MethodImpl referenced unexpected metadata token %O{other}"

        let methodImplDeclarationCouldMatch (relativeAssembly : DumpedAssembly) (token : MetadataToken) : bool =
            match token with
            | MetadataToken.MethodDef h ->
                let method = relativeAssembly.Methods.[h]

                method.Name = methodToCall.Name
                && method.Signature.GenericParameterCount = methodToCall.Signature.GenericParameterCount
                && method.Signature.RequiredParameterCount = methodToCall.Signature.RequiredParameterCount
            | MetadataToken.MemberReference h ->
                let memberRef = relativeAssembly.Members.[h]

                match memberRef.Signature with
                | MemberSignature.Method signature ->
                    memberRef.PrettyName = methodToCall.Name
                    && signature.GenericParameterCount = methodToCall.Signature.GenericParameterCount
                    && signature.RequiredParameterCount = methodToCall.Signature.RequiredParameterCount
                | MemberSignature.Field _ -> false
            | _ -> false

        let concretizeImplementation
            (implementationTypeHandle : ConcreteTypeHandle)
            (implementation : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            (state : IlMachineState)
            : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            =
            let typeGenerics =
                AllConcreteTypes.lookup implementationTypeHandle state.ConcreteTypes
                |> Option.get
                |> _.Generics

            let state, meth, _ =
                ExecutionConcretization.concretizeMethodWithAllGenerics
                    loggerFactory
                    baseClassTypes
                    typeGenerics
                    implementation
                    methodGenerics
                    state

            state, meth

        let findClassImplementation (state : IlMachineState) : IlMachineState * _ option =
            // Resolution precedence: explicit MethodImpl entries, then method name/signature
            // matches on the current type, then the base type walk when enabled.
            let rec walkBase (state : IlMachineState) (currentTypeHandle : ConcreteTypeHandle) =
                if not walkBaseTypes then
                    state, None
                else
                    match currentTypeHandle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _ -> state, None
                    | ConcreteTypeHandle.Concrete _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        let state, baseType =
                            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state currentTypeHandle

                        match baseType with
                        | None -> state, None
                        | Some baseType -> walk state baseType

            and walk (state : IlMachineState) (currentTypeHandle : ConcreteTypeHandle) =
                match IlMachineState.tryGetConcreteTypeInfo state currentTypeHandle with
                | None -> walkBase state currentTypeHandle
                | Some (currentTy, currentTypeInfo) ->
                    let currentAssy = state._LoadedAssemblies.[currentTy.Identity.AssemblyFullName]

                    let state, matchingMethodImplBodies =
                        ((state, []), currentTypeInfo.MethodImpls.Values)
                        ||> Seq.fold (fun (state, acc) impl ->
                            if not (methodImplDeclarationCouldMatch currentAssy impl.Declaration) then
                                state, acc
                            else
                                let state, declaration, declarationTypeArgs =
                                    resolveMethodReference currentTy.Generics currentAssy impl.Declaration state

                                let state, declarationTypeGenerics =
                                    match declarationTypeArgs with
                                    | Some typeArgs ->
                                        concretizeTypeArgs
                                            declaration.DeclaringType.Assembly
                                            currentTy.Generics
                                            typeArgs
                                            state
                                    | None when declaration.DeclaringType.Generics.IsEmpty ->
                                        state, ImmutableArray.Empty
                                    | None when declaration.DeclaringType.Identity = currentTy.Identity ->
                                        state, currentTy.Generics
                                    | None ->
                                        failwith
                                            $"MethodImpl declaration for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} referenced generic MethodDef %s{declaration.Name} without concrete type arguments"

                                let matches, state =
                                    let state, matches =
                                        methodReferenceMatchesTarget declarationTypeGenerics declaration state

                                    matches, state

                                if not matches then
                                    state, acc
                                else
                                    match impl.Body with
                                    | MetadataToken.MethodDef body -> state, currentAssy.Methods.[body] :: acc
                                    | other ->
                                        failwith
                                            $"MethodImpl body for %s{currentTypeInfo.Namespace}.%s{currentTypeInfo.Name} was not a MethodDef: %O{other}"
                        )

                    match matchingMethodImplBodies with
                    | [ impl ] -> state, Some (currentTypeHandle, impl, "Found concrete implementation from MethodImpl")
                    | _ :: _ ->
                        matchingMethodImplBodies
                        |> List.map (fun m -> m.Name)
                        |> String.concat ", "
                        |> failwithf
                            "multiple MethodImpl bodies matched this virtual slot; overload/interface disambiguation is not implemented: %s"
                    | [] ->
                        let implementation, state =
                            (state, currentTypeInfo.Methods)
                            ||> List.mapFold (fun state meth ->
                                methodMatches currentTy.Generics methodDeclaringType.IsInterface meth state
                            )

                        let implementation =
                            implementation
                            |> List.choose id
                            |> List.sortBy (fun (_, isInterface) -> if isInterface then -1 else 0)

                        match implementation with
                        | (impl, true) :: l when (l |> List.forall (fun (_, b) -> not b)) ->
                            state, Some (currentTypeHandle, impl, "Found concrete implementation from an interface")
                        | [ impl, false ] -> state, Some (currentTypeHandle, impl, "Found concrete implementation")
                        | _ :: _ ->
                            implementation
                            |> List.map (fun (m, _) -> m.Name)
                            |> String.concat ", "
                            |> failwithf "multiple options: %s"
                        | [] -> walkBase state currentTypeHandle

            walk state dispatchTypeHandle

        let state, classImplementation = findClassImplementation state

        match classImplementation with
        | Some (implementationTypeHandle, impl, logMessage) ->
            logger.LogDebug logMessage
            let state, impl = concretizeImplementation implementationTypeHandle impl state
            state, Some impl
        | None when not walkBaseTypes -> state, None
        | None ->

        let rec findInterfaceScanTypeInfo
            (state : IlMachineState)
            (currentTypeHandle : ConcreteTypeHandle)
            : IlMachineState * TypeInfo<GenericParamFromMetadata, TypeDefn> option
            =
            match IlMachineState.tryGetConcreteTypeInfo state currentTypeHandle with
            | Some (_, typeInfo) -> state, Some typeInfo
            | None ->
                match currentTypeHandle with
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _ -> state, None
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ when walkBaseTypes ->
                    let state, baseType =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state currentTypeHandle

                    match baseType with
                    | None -> state, None
                    | Some baseType -> findInterfaceScanTypeInfo state baseType
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> state, None

        let state, callingObjTy =
            match findInterfaceScanTypeInfo state dispatchTypeHandle with
            | state, None -> failwith $"No metadata dispatch type available for virtual receiver %O{dispatchTypeHandle}"
            | state, Some typeInfo -> state, typeInfo

        logger.LogDebug "No concrete implementation found; scanning interfaces"

        let possibleInterfaceMethods, state =
            (state, callingObjTy.ImplementedInterfaces)
            ||> Seq.mapFold (fun state impl ->
                let assy = state.LoadedAssembly impl.RelativeToAssembly |> Option.get

                let state, defn =
                    match impl.InterfaceHandle with
                    | MetadataToken.TypeDefinition defn ->
                        let state, defn = IlMachineState.lookupTypeDefn baseClassTypes state assy defn

                        let state, _, defn =
                            IlMachineState.resolveTypeFromDefn
                                loggerFactory
                                baseClassTypes
                                defn
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                assy
                                state

                        state, defn
                    | MetadataToken.TypeReference _ -> failwith "TODO: interface dispatch through TypeReference"
                    | MetadataToken.TypeSpecification spec ->
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

                logger.LogDebug ("Interface {InterfaceName} (generics: {InterfaceGenerics})", defn.Name, defn.Generics)

                let s, state =
                    defn.Methods
                    |> Seq.filter (fun mi -> mi.Name = methodToCall.Name)
                    |> Seq.mapFold
                        (fun state meth ->
                            let state, mi, _ =
                                ExecutionConcretization.concretizeMethodForExecution
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
            state, None
        | [ meth ] ->
            logger.LogDebug (
                "Exactly one interface implementation found {DeclaringTypeNamespace}.{DeclaringTypeName}.{MethodName} ({MethodGenerics})",
                meth.DeclaringType.Namespace,
                meth.DeclaringType.Name,
                meth.Name,
                meth.Generics
            )

            state, Some meth
        | _ -> failwith "TODO: handle overloads"

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

        let shouldPerformVirtualResolution =
            performInterfaceResolution
            && not methodToCall.IsStatic
            && methodToCall.MethodAttributes.HasFlag MethodAttributes.Virtual
            && not (methodToCall.MethodAttributes.HasFlag MethodAttributes.Final)

        let state, methodToCall =
            if shouldPerformVirtualResolution then
                let callingObj =
                    match
                        activeMethodState.EvaluationStack
                        |> EvalStack.PeekNthFromTop methodToCall.Parameters.Length
                    with
                    | None -> failwith "unexpectedly no `this` on the eval stack of instance method"
                    | Some this -> this

                let state, callingObjTyHandle =
                    getTypeOfObj loggerFactory baseClassTypes state callingObj

                let state, resolved =
                    tryResolveVirtualImplementation
                        loggerFactory
                        baseClassTypes
                        thread
                        methodGenerics
                        methodToCall
                        callingObjTyHandle
                        true
                        state

                state, resolved |> Option.defaultValue methodToCall
            else
                state, methodToCall

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
                // Call the class constructor! We concretize manually and call `callMethod` directly,
                // because we're already in the middle of loading this class.
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

    /// Synthesise an exception from inside the runtime itself (the host emulating the CLR),
    /// as opposed to a `throw` opcode executed by guest IL. Allocates the exception without
    /// running the exception type's .cctor, pushes its default instance constructor frame,
    /// and returns to the dispatch loop. When the ctor completes (Ret), returnStackFrame
    /// will signal DispatchException so the Ret handler can dispatch the exception.
    ///
    /// Use this for opcode-manufactured exceptions like `NullReferenceException` from a null
    /// dereference or `InvalidCastException` from a failed `castclass`. Do NOT use it for
    /// dispatching exceptions that the guest itself constructs and throws via `newobj` + `throw`
    /// — those go through `ExceptionDispatching.throwExceptionObject` and the cctor will already
    /// have run during the guest's `newobj`.
    ///
    /// All current call sites pass a non-generic BCL exception type from `BaseClassTypes`. The
    /// cctor-skip is safe for those (their cctors are trivial or empty); it would not be safe
    /// for an arbitrary guest-defined exception type, which is why this entry point is
    /// reserved for runtime use.
    ///
    /// This is a runtime boundary, not guest `newobj` semantics. It mirrors the CLR's
    /// EEException::CreateThrowable path: allocate the object directly, call the default
    /// instance ctor, then overwrite HResult.
    /// See: https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
    let raiseRuntimeException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        // 1. Allocate the zero-initialised exception with _HResult pre-set.  This deliberately
        //    bypasses ensureTypeInitialised: opcode-manufactured exceptions are produced by the
        //    runtime rather than by guest `newobj` class-initialisation semantics.
        let addr, _exnHandle, state =
            ExceptionDispatching.allocateRuntimeException loggerFactory baseClassTypes exceptionTypeInfo state

        // 2. Find the parameterless .ctor on the exception type.
        let assy = state._LoadedAssemblies.[exceptionTypeInfo.Assembly.FullName]
        let typeDef = assy.TypeDefs.[exceptionTypeInfo.Identity.TypeDefinition.Get]

        if not typeDef.Generics.IsEmpty then
            failwith
                $"raiseRuntimeException: expected non-generic exception type, but %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name} has %i{typeDef.Generics.Length} generic parameter(s)"

        let ctor =
            typeDef.Methods
            |> List.tryFind (fun method -> method.Name = ".ctor" && not method.IsStatic && method.Parameters.IsEmpty)
            |> Option.defaultWith (fun () ->
                failwith
                    $"raiseRuntimeException: no parameterless .ctor found on %s{exceptionTypeInfo.Namespace}.%s{exceptionTypeInfo.Name}"
            )
            // The type has no generic parameters (guarded above), so any GenericParamFromMetadata
            // in the ctor's type-generic positions is unreachable. Map them to TypeDefn to satisfy
            // concretizeMethodForExecution's signature.
            |> MethodInfo.mapTypeGenerics (fun _ ->
                failwith<TypeDefn> "raiseRuntimeException: exception type was unexpectedly generic"
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

        let state, concretizedCtor, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                currentThread
                ctor
                None
                None
                state

        let threadState = state.ThreadState.[currentThread]

        callMethod
            loggerFactory
            baseClassTypes
            None
            (Some addr) // weAreConstructingObj
            false // no interface resolution
            false // wasClassConstructor
            false // do NOT advance caller PC — dispatch needs the faulting instruction's offset
            concretizedCtor.Generics
            concretizedCtor
            currentThread
            threadState
            None
            true // dispatchAsExceptionOnReturn
            state,
        WhatWeDid.Executed
