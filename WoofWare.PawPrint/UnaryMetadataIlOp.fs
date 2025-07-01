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
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        let activeAssy = state.ActiveAssembly thread

        match op with
        | Call ->
            let state, methodToCall, methodGenerics, typeArgsFromMetadata =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    let spec = activeAssy.MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token ->
                        let method =
                            activeAssy.Methods.[token]
                            |> MethodInfo.mapTypeGenerics (fun i _ -> spec.Signature.[i])

                        state, method, Some spec.Signature, None
                    | MetadataToken.MemberReference ref ->
                        let state, _, method, extractedTypeArgs =
                            IlMachineState.resolveMember
                                loggerFactory
                                baseClassTypes
                                thread
                                (state.ActiveAssembly thread)
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
                            (state.ActiveAssembly thread)
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

            let state, concretizedMethod, declaringTypeHandle =
                IlMachineState.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    methodGenerics
                    typeArgsFromMetadata
                    state

            match IlMachineState.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | NothingToDo state ->
                state.WithThreadSwitchedToAssembly methodToCall.DeclaringType.Assembly thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly
                    loggerFactory
                    baseClassTypes
                    thread
                    true
                    methodGenerics
                    methodToCall
                    None
                    typeArgsFromMetadata
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit

        | Callvirt ->

            // TODO: this is presumably super incomplete
            let state, methodToCall, methodGenerics, typeArgsFromMetadata =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    let spec = activeAssy.MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token ->
                        let method =
                            activeAssy.Methods.[token]
                            |> MethodInfo.mapTypeGenerics (fun i _ -> spec.Signature.[i])

                        state, method, Some spec.Signature, None
                    | MetadataToken.MemberReference ref ->
                        let state, _, method, extractedTypeArgs =
                            IlMachineState.resolveMember
                                loggerFactory
                                baseClassTypes
                                thread
                                (state.ActiveAssembly thread)
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
                            (state.ActiveAssembly thread)
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

            // TODO: this is pretty inefficient, we're concretising here and then immediately after in callMethodInActiveAssembly
            let state, concretizedMethod, declaringTypeHandle =
                IlMachineState.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    methodToCall
                    methodGenerics
                    typeArgsFromMetadata
                    state

            match IlMachineState.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            state.WithThreadSwitchedToAssembly methodToCall.DeclaringType.Assembly thread
            |> fst
            |> IlMachineState.callMethodInActiveAssembly
                loggerFactory
                baseClassTypes
                thread
                true
                methodGenerics
                methodToCall
                None
                typeArgsFromMetadata

        | Castclass -> failwith "TODO: Castclass unimplemented"
        | Newobj ->
            let logger = loggerFactory.CreateLogger "Newobj"

            let state, assy, ctor, typeArgsFromMetadata =
                match metadataToken with
                | MethodDef md ->
                    let method = activeAssy.Methods.[md]

                    state,
                    activeAssy.Name,
                    MethodInfo.mapTypeGenerics (fun _ -> failwith "non-generic method") method,
                    None
                | MemberReference mr ->
                    let state, name, method, extractedTypeArgs =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            (state.ActiveAssembly thread)
                            mr
                            state

                    match method with
                    | Choice1Of2 mr -> state, name, mr, Some extractedTypeArgs
                    | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            let state, concretizedCtor, declaringTypeHandle =
                IlMachineState.concretizeMethodForExecution
                    loggerFactory
                    baseClassTypes
                    thread
                    ctor
                    None
                    typeArgsFromMetadata
                    state

            let state, init =
                IlMachineState.ensureTypeInitialised loggerFactory baseClassTypes thread declaringTypeHandle state

            match init with
            | WhatWeDid.BlockedOnClassInit state -> failwith "TODO: another thread is running the initialiser"
            | WhatWeDid.SuspendedForClassInit -> state, SuspendedForClassInit
            | WhatWeDid.Executed ->

            let ctorAssembly = state.LoadedAssembly ctor.DeclaringType.Assembly |> Option.get
            let ctorType = ctorAssembly.TypeDefs.[ctor.DeclaringType.Definition.Get]

            do
                logger.LogDebug (
                    "Creating object of type {ConstructorAssembly}.{ConstructorType}",
                    ctorAssembly.Name.Name,
                    ctorType.Name
                )

            let typeGenerics =
                concretizedCtor.DeclaringType.Generics |> ImmutableArray.CreateRange

            let state, fieldZeros =
                ((state, []), ctorType.Fields)
                ||> List.fold (fun (state, zeros) field ->
                    // TODO: generics
                    let state, zero =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            ctorAssembly
                            field.Signature
                            typeGenerics
                            ImmutableArray.Empty
                            state

                    state, (field.Name, zero) :: zeros
                )

            let fields = List.rev fieldZeros

            // Note: this is a bit unorthodox for value types, which *aren't* heap-allocated.
            // We'll perform their construction on the heap, though, to keep the interface
            // of Newobj uniform.
            // On completion of the constructor, we'll copy the value back off the heap,
            // and put it on the eval stack directly.
            let allocatedAddr, state =
                IlMachineState.allocateManagedObject ctorType fields state

            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (ManagedPointerSource.Heap allocatedAddr))
                    thread

            let state, whatWeDid =
                state.WithThreadSwitchedToAssembly assy thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly
                    loggerFactory
                    baseClassTypes
                    thread
                    true
                    None
                    ctor
                    (Some allocatedAddr)
                    typeArgsFromMetadata

            match whatWeDid with
            | SuspendedForClassInit -> failwith "unexpectedly suspended while initialising constructor"
            | BlockedOnClassInit threadBlockingUs ->
                failwith "TODO: Newobj blocked on class init synchronization unimplemented"
            | Executed -> ()

            state, WhatWeDid.Executed
        | Newarr ->
            let currentState = state.ThreadState.[thread]
            let popped, newMethodState = MethodState.popFromStack currentState.MethodState

            let currentState =
                { currentState with
                    MethodStates = currentState.MethodStates.SetItem (currentState.ActiveMethodState, newMethodState)
                }

            let len =
                match popped with
                | EvalStackValue.Int32 v -> v
                | popped -> failwith $"unexpectedly popped value %O{popped} to serve as array len"

            let typeGenerics =
                newMethodState.ExecutingMethod.DeclaringType.Generics
                |> ImmutableArray.CreateRange

            let state, elementType, assy =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    let assy = state.LoadedAssembly currentState.ActiveAssembly |> Option.get
                    let defn = assy.TypeDefs.[defn]

                    let baseType =
                        defn.BaseType
                        |> DumpedAssembly.resolveBaseType baseClassTypes state._LoadedAssemblies defn.Assembly

                    let signatureTypeKind =
                        match baseType with
                        | ResolvedBaseType.Enum
                        | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                        | ResolvedBaseType.Object -> SignatureTypeKind.Class
                        | ResolvedBaseType.Delegate -> failwith "TODO: delegate"

                    let result =
                        TypeDefn.FromDefinition (
                            ComparableTypeDefinitionHandle.Make defn.TypeDefHandle,
                            defn.Assembly.FullName,
                            signatureTypeKind
                        )

                    state, result, assy
                | MetadataToken.TypeSpecification spec ->
                    let assy = state.LoadedAssembly currentState.ActiveAssembly |> Option.get
                    state, assy.TypeSpecs.[spec].Signature, assy
                | MetadataToken.TypeReference ref ->
                    let ref = state.ActiveAssembly(thread).TypeRefs.[ref]

                    // Convert ConcreteTypeHandles back to TypeDefn for metadata operations
                    let typeGenerics =
                        newMethodState.ExecutingMethod.DeclaringType.Generics
                        |> Seq.map (fun handle ->
                            Concretization.concreteHandleToTypeDefn
                                baseClassTypes
                                handle
                                state.ConcreteTypes
                                state._LoadedAssemblies
                        )
                        |> ImmutableArray.CreateRange

                    let state, assy, resolved =
                        IlMachineState.resolveTypeFromRef
                            loggerFactory
                            (state.ActiveAssembly thread)
                            ref
                            typeGenerics
                            state

                    let baseType =
                        resolved.BaseType
                        |> DumpedAssembly.resolveBaseType baseClassTypes state._LoadedAssemblies assy.Name

                    let signatureTypeKind =
                        match baseType with
                        | ResolvedBaseType.Enum
                        | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                        | ResolvedBaseType.Object -> SignatureTypeKind.Class
                        | ResolvedBaseType.Delegate -> failwith "TODO: delegate"

                    let result =
                        TypeDefn.FromDefinition (
                            ComparableTypeDefinitionHandle.Make resolved.TypeDefHandle,
                            assy.Name.FullName,
                            signatureTypeKind
                        )

                    state, result, assy
                | x -> failwith $"TODO: Newarr element type resolution unimplemented for {x}"

            let state, zeroOfType =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    assy
                    elementType
                    typeGenerics
                    newMethodState.Generics
                    state

            let alloc, state = IlMachineState.allocateArray (fun () -> zeroOfType) len state

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add thread currentState
                }
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Box -> failwith "TODO: Box unimplemented"
        | Ldelema ->
            let index, state = IlMachineState.popEvalStack thread state
            let arr, state = IlMachineState.popEvalStack thread state

            let index =
                match index with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith $"TODO: {index}"

            let arrAddr =
                match arr with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                | _ -> failwith $"Invalid array: %O{arr}"

            // TODO: throw ArrayTypeMismatchException if incorrect types

            let arr = state.ManagedHeap.Arrays.[arrAddr]

            if index < 0 || index >= arr.Length then
                failwith "TODO: throw IndexOutOfRangeException"

            let result =
                ManagedPointerSource.ArrayIndex (arrAddr, index)
                |> EvalStackValue.ManagedPointer

            let state =
                IlMachineState.pushToEvalStack' result thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Isinst ->
            let actualObj, state = IlMachineState.popEvalStack thread state

            let targetType : TypeDefn =
                match metadataToken with
                | MetadataToken.TypeDefinition td ->
                    let activeAssy = state.ActiveAssembly thread
                    let ty = activeAssy.TypeDefs.[td]

                    let baseTy =
                        DumpedAssembly.resolveBaseType
                            baseClassTypes
                            state._LoadedAssemblies
                            activeAssy.Name
                            ty.BaseType

                    let sigType =
                        match baseTy with
                        | ResolvedBaseType.Enum
                        | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                        | ResolvedBaseType.Object -> SignatureTypeKind.Class
                        | ResolvedBaseType.Delegate -> failwith "todo"

                    TypeDefn.FromDefinition (ComparableTypeDefinitionHandle.Make td, activeAssy.Name.FullName, sigType)
                | MetadataToken.TypeSpecification handle -> state.ActiveAssembly(thread).TypeSpecs.[handle].Signature
                | m -> failwith $"unexpected metadata token {m} in IsInst"

            let returnObj =
                match actualObj with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
                    // null IsInstance check always succeeds and results in a null reference
                    EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.ManagedPointer (ManagedPointerSource.LocalVariable _) -> failwith "TODO"
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr) ->
                    match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                    | true, v ->
                        { new TypeInfoEval<_> with
                            member _.Eval typeInfo = failwith "TODO"
                        }
                        |> v.Type.Apply
                    | false, _ ->

                    match state.ManagedHeap.Arrays.TryGetValue addr with
                    | true, v -> failwith "TODO"
                    | false, _ -> failwith $"could not find managed object with address {addr}"
                | esv -> failwith $"TODO: {esv}"

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
                        |> FieldInfo.mapTypeGenerics (fun _ _ -> failwith "no generics allowed in FieldDefinition")

                    state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, field, _ =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy mr state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            do
                let logger = loggerFactory.CreateLogger "Stfld"

                logger.LogInformation (
                    "Storing in object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    field.Name,
                    field.Name,
                    field.Signature
                )

            let valueToStore, state = IlMachineState.popEvalStack thread state

            let state, declaringTypeHandle, typeGenerics =
                IlMachineState.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            let state, zero =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    (state.ActiveAssembly thread)
                    field.Signature
                    typeGenerics
                    ImmutableArray.Empty // field can't have its own generics
                    state

            let valueToStore = EvalStackValue.toCliTypeCoerced zero valueToStore

            let currentObj, state = IlMachineState.popEvalStack thread state

            if field.Attributes.HasFlag FieldAttributes.Static then
                let state =
                    IlMachineState.setStatic declaringTypeHandle field.Name valueToStore state

                state, WhatWeDid.Executed
            else

            let state =
                match currentObj with
                | EvalStackValue.Int32 _ -> failwith "unexpectedly setting field on an int"
                | EvalStackValue.Int64 _ -> failwith "unexpectedly setting field on an int64"
                | EvalStackValue.NativeInt _ -> failwith "unexpectedly setting field on a nativeint"
                | EvalStackValue.Float _ -> failwith "unexpectedly setting field on a float"
                | EvalStackValue.ManagedPointer source ->
                    match source with
                    | ManagedPointerSource.Null -> failwith "TODO: raise NullReferenceException"
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        state
                        |> IlMachineState.setLocalVariable sourceThread methodFrame whichVar valueToStore
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) -> failwith "todo"
                    | ManagedPointerSource.ArrayIndex (arr, index) ->
                        state |> IlMachineState.setArrayValue arr valueToStore index
                    | ManagedPointerSource.Heap addr ->
                        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                        | false, _ -> failwith $"todo: array {addr}"
                        | true, v ->
                            let v =
                                { v with
                                    Fields = v.Fields |> Map.add field.Name valueToStore
                                }

                            let heap =
                                { state.ManagedHeap with
                                    NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add addr v
                                }

                            { state with
                                ManagedHeap = heap
                            }
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
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
                            |> FieldInfo.mapTypeGenerics (fun _ _ -> failwith "no generics allowed in FieldDefinition")

                        state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, method, _ =
                        IlMachineState.resolveMember
                            loggerFactory
                            baseClassTypes
                            thread
                            (state.ActiveAssembly thread)
                            mr
                            state

                    match method with
                    | Choice1Of2 methodInfo ->
                        failwith $"unexpectedly asked to store to a non-field method: {methodInfo.Name}"
                    | Choice2Of2 fieldInfo -> state, fieldInfo
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            do
                let logger = loggerFactory.CreateLogger "Stsfld"

                let declaring =
                    state.ActiveAssembly(thread).TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogInformation (
                    "Storing in static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let state, declaringTypeHandle, typeGenerics =
                IlMachineState.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineState.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let popped, state = IlMachineState.popEvalStack thread state

            let state, zero =
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
                IlMachineState.setStatic declaringTypeHandle field.Name toStore state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        | Ldfld ->
            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    let field =
                        activeAssy.Fields.[f]
                        |> FieldInfo.mapTypeGenerics (fun _ _ -> failwith "no generics allowed on FieldDefinition")

                    state, field
                | MetadataToken.MemberReference mr ->
                    let state, assyName, field, _ =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy mr state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            do
                let logger = loggerFactory.CreateLogger "Ldfld"
                let declaring = activeAssy.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogInformation (
                    "Loading object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let currentObj, state = IlMachineState.popEvalStack thread state

            let state, declaringTypeHandle, typeGenerics =
                IlMachineState.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            if field.Attributes.HasFlag FieldAttributes.Static then
                let declaringTypeHandle, state =
                    IlMachineState.concretizeFieldDeclaringType loggerFactory baseClassTypes field.DeclaringType state

                let state, staticField =
                    match IlMachineState.getStatic declaringTypeHandle field.Name state with
                    | Some v -> state, v
                    | None ->
                        let state, zero =
                            IlMachineState.cliTypeZeroOf
                                loggerFactory
                                baseClassTypes
                                (state.LoadedAssembly(field.DeclaringType.Assembly).Value)
                                field.Signature
                                typeGenerics
                                ImmutableArray.Empty // field can't have its own generics
                                state

                        let state = IlMachineState.setStatic declaringTypeHandle field.Name zero state
                        state, zero

                let state = state |> IlMachineState.pushToEvalStack staticField thread
                state, WhatWeDid.Executed
            else

            let state =
                match currentObj with
                | EvalStackValue.Int32 i -> failwith "todo: int32"
                | EvalStackValue.Int64 int64 -> failwith "todo: int64"
                | EvalStackValue.NativeInt nativeIntSource -> failwith $"todo: nativeint {nativeIntSource}"
                | EvalStackValue.Float f -> failwith "todo: float"
                | EvalStackValue.ManagedPointer managedPointerSource ->
                    match managedPointerSource with
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        let currentValue =
                            state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables
                                .[int<uint16> whichVar]

                        IlMachineState.pushToEvalStack currentValue thread state
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                        let currentValue =
                            state.ThreadState.[sourceThread].MethodStates.[methodFrame].Arguments.[int<uint16> whichVar]

                        IlMachineState.pushToEvalStack currentValue thread state
                    | ManagedPointerSource.Heap managedHeapAddress ->
                        match state.ManagedHeap.NonArrayObjects.TryGetValue managedHeapAddress with
                        | false, _ -> failwith $"todo: array {managedHeapAddress}"
                        | true, v -> IlMachineState.pushToEvalStack v.Fields.[field.Name] thread state
                    | ManagedPointerSource.ArrayIndex (arr, index) ->
                        let currentValue = state |> IlMachineState.getArrayValue arr index
                        IlMachineState.pushToEvalStack currentValue thread state
                    | ManagedPointerSource.Null -> failwith "TODO: raise NullReferenceException"
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith $"todo: {managedHeapAddress}"
                | EvalStackValue.UserDefinedValueType _ as udvt -> IlMachineState.pushToEvalStack' udvt thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldflda -> failwith "TODO: Ldflda unimplemented"
        | Ldsfld ->
            let logger = loggerFactory.CreateLogger "Ldsfld"

            let state, field =
                match metadataToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match activeAssy.Fields.TryGetValue fieldHandle with
                    | false, _ -> failwith "TODO: Ldsfld - throw MissingFieldException"
                    | true, field ->
                        let field =
                            field
                            |> FieldInfo.mapTypeGenerics (fun _ _ -> failwith "generics not allowed in FieldDefinition")

                        state, field
                | MetadataToken.MemberReference mr ->
                    let state, _, field, _ =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy mr state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            do
                let declaring =
                    state.LoadedAssembly field.DeclaringType.Assembly
                    |> Option.get
                    |> fun a -> a.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogInformation (
                    "Loading from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    field.DeclaringType.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let state, declaringTypeHandle, typeGenerics =
                IlMachineState.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineState.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let fieldValue, state =
                match IlMachineState.getStatic declaringTypeHandle field.Name state with
                | None ->
                    let state, newVal =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            activeAssy
                            field.Signature
                            typeGenerics
                            ImmutableArray.Empty // field can't have its own generics
                            state

                    newVal, IlMachineState.setStatic declaringTypeHandle field.Name newVal state
                | Some v -> v, state

            do
                let logger = loggerFactory.CreateLogger "Ldsfld"

                let declaring =
                    state
                        .LoadedAssembly(field.DeclaringType.Assembly)
                        .Value.TypeDefs.[field.DeclaringType.Definition.Get]

                logger.LogInformation (
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

        | Unbox_Any -> failwith "TODO: Unbox_Any unimplemented"
        | Stelem ->
            let assy =
                state.LoadedAssembly state.ThreadState.[thread].ActiveAssembly |> Option.get

            let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

            let declaringTypeGenerics =
                currentMethod.DeclaringType.Generics |> ImmutableArray.CreateRange

            let typeGenerics =
                currentMethod.DeclaringType.Generics
                |> ImmutableArray.CreateRange

            let state, assy, elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state,
                    assy,
                    assy.TypeDefs.[defn]
                    |> TypeInfo.mapGeneric (fun _ p -> TypeDefn.GenericTypeParameter p.SequenceNumber)
                | MetadataToken.TypeSpecification spec ->
                    let state, assy, ty =
                        IlMachineState.resolveTypeFromSpecConcrete
                            loggerFactory
                            baseClassTypes
                            spec
                            assy
                            typeGenerics
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
                match arr with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
                | EvalStackValue.ObjectRef addr -> addr
                | _ -> failwith $"expected heap allocation for array, got {arr}"

            let elementType =
                TypeInfo.toTypeDefn
                    baseClassTypes
                    _.Name
                    (fun x y -> x.TypeDefs.[y])
                    (fun x y -> x.TypeRefs.[y] |> failwithf "%+A")
                    (elementType
                     |> TypeInfo.mapGeneric (fun i _ ->
                         {
                             Name = "<unknown>"
                             SequenceNumber = i
                         }
                     ))

            let state, zeroOfType =
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
            let assy =
                state.LoadedAssembly state.ThreadState.[thread].ActiveAssembly |> Option.get

            let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod

            // Convert ConcreteTypeHandles back to TypeDefn for metadata operations
            let metadataMethodGenerics =
                currentMethod.Generics
                |> Seq.mapi (fun i _ -> TypeDefn.GenericMethodParameter i)
                |> ImmutableArray.CreateRange

            let declaringTypeGenerics =
                currentMethod.DeclaringType.Generics |> ImmutableArray.CreateRange

            let metadataTypeGenerics =
                currentMethod.DeclaringType.Generics
                |> Seq.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
                |> ImmutableArray.CreateRange

            let state, assy, elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state,
                    assy,
                    assy.TypeDefs.[defn]
                    |> TypeInfo.mapGeneric (fun _ p -> TypeDefn.GenericTypeParameter p.SequenceNumber)
                | MetadataToken.TypeSpecification spec ->
                    let state, assy, ty =
                        IlMachineState.resolveTypeFromSpec
                            loggerFactory
                            baseClassTypes
                            spec
                            assy
                            metadataTypeGenerics
                            metadataMethodGenerics
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
                match arr with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)
                | EvalStackValue.ObjectRef addr -> addr
                | _ -> failwith $"expected heap allocation for array, got {arr}"

            let toPush =
                match state.ManagedHeap.Arrays.TryGetValue arr with
                | false, _ -> failwith $"unexpectedly failed to find array allocation {arr} in Ldelem"
                | true, v ->
                    if 0 <= index && index < v.Elements.Length then
                        v.Elements.[index]
                    else
                        failwith "TODO: raise an out of bounds"

            failwith $"TODO: Ldelem {index} {arr} resulted in {toPush}"
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Initobj -> failwith "TODO: Initobj unimplemented"
        | Ldsflda ->

            // TODO: check whether we should throw FieldAccessException

            let field =
                match metadataToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match activeAssy.Fields.TryGetValue fieldHandle with
                    | false, _ -> failwith "TODO: Ldsflda - throw MissingFieldException"
                    | true, field ->
                        field
                        |> FieldInfo.mapTypeGenerics (fun _ _ -> failwith "generics not allowed on FieldDefinition")
                | t -> failwith $"Unexpectedly asked to load a non-field: {t}"

            let state, declaringTypeHandle, typeGenerics =
                IlMachineState.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

            match IlMachineState.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            if TypeDefn.isManaged field.Signature then
                match IlMachineState.getStatic declaringTypeHandle field.Name state with
                | Some v ->
                    IlMachineState.pushToEvalStack v thread state
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                | None ->
                    // Field is not yet initialised
                    let state, zero =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            activeAssy
                            field.Signature
                            typeGenerics
                            ImmutableArray.Empty // field can't have its own generics
                            state

                    IlMachineState.setStatic declaringTypeHandle field.Name zero state
                    |> IlMachineState.pushToEvalStack (CliType.ObjectRef None) thread
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
            else
                failwith "TODO: Ldsflda - push unmanaged pointer"

        | Ldftn ->
            let logger = loggerFactory.CreateLogger "Ldftn"

            let (method : MethodInfo<TypeDefn, WoofWare.PawPrint.GenericParameter, TypeDefn>), methodGenerics =
                match metadataToken with
                | MetadataToken.MethodDef handle ->
                    let method =
                        activeAssy.Methods.[handle]
                        |> MethodInfo.mapTypeGenerics (fun i _ -> TypeDefn.GenericTypeParameter i)

                    method, None
                | MetadataToken.MethodSpecification h ->
                    let spec = activeAssy.MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token ->
                        let method =
                            activeAssy.Methods.[token]
                            |> MethodInfo.mapTypeGenerics (fun i _ -> TypeDefn.GenericTypeParameter i)

                        method, Some spec.Signature
                    | k -> failwith $"Unrecognised MethodSpecification kind: %O{k}"
                | t -> failwith $"Unexpectedly asked to Ldftn a non-method: {t}"

            let state, concretizedMethod, _declaringTypeHandle =
                IlMachineState.concretizeMethodForExecution
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
        | Stobj -> failwith "TODO: Stobj unimplemented"
        | Constrained -> failwith "TODO: Constrained unimplemented"
        | Ldtoken ->
            let state =
                match metadataToken with
                | MetadataToken.FieldDefinition h ->
                    let ty = baseClassTypes.RuntimeFieldHandle
                    // this is a struct; it contains one field, an IRuntimeFieldInfo
                    // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1097
                    let field = ty.Fields |> List.exactlyOne

                    if field.Name <> "m_ptr" then
                        failwith $"unexpected field name ${field.Name} for BCL type RuntimeFieldHandle"

                    failwith ""
                | MetadataToken.MethodDef h ->
                    let ty = baseClassTypes.RuntimeMethodHandle
                    let field = ty.Fields |> List.exactlyOne
                    failwith ""
                | MetadataToken.TypeDefinition h ->
                    let ty = baseClassTypes.RuntimeTypeHandle
                    let field = ty.Fields |> List.exactlyOne

                    if field.Name <> "m_type" then
                        failwith $"unexpected field name ${field.Name} for BCL type RuntimeTypeHandle"

                    let currentMethod = state.ThreadState.[thread].MethodState

                    let methodGenerics = currentMethod.Generics

                    let typeGenerics = currentMethod.ExecutingMethod.DeclaringType.Generics

                    if not (methodGenerics.IsEmpty && typeGenerics.IsEmpty) then
                        failwith "TODO: generics"

                    let handle =
                        match IlMachineState.canonicaliseTypeDef (state.ActiveAssembly(thread).Name) h [] [] state with
                        | Error e -> failwith $"TODO: somehow need to load {e.FullName} first"
                        | Ok h -> h

                    let (_, alloc), state = IlMachineState.getOrAllocateType baseClassTypes handle state

                    IlMachineState.pushToEvalStack (CliType.ValueType [ CliType.ObjectRef (Some alloc) ]) thread state
                | _ -> failwith $"Unexpected metadata token %O{metadataToken} in LdToken"

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | Cpobj -> failwith "TODO: Cpobj unimplemented"
        | Ldobj -> failwith "TODO: Ldobj unimplemented"
        | Sizeof -> failwith "TODO: Sizeof unimplemented"
        | Calli -> failwith "TODO: Calli unimplemented"
        | Unbox -> failwith "TODO: Unbox unimplemented"
        | Ldvirtftn -> failwith "TODO: Ldvirtftn unimplemented"
        | Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | Refanyval -> failwith "TODO: Refanyval unimplemented"
        | Jmp -> failwith "TODO: Jmp unimplemented"
