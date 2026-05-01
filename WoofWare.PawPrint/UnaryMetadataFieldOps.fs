namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataFieldOps =
    let executeStfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

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
                IlMachineState.writeManagedByrefWithBase
                    baseClassTypes
                    state
                    (ManagedPointerSource.appendProjection (ByrefProjection.Field fieldId) src)
                    valueToStore
            | EvalStackValue.UserDefinedValueType _ -> failwith "todo"

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeStsfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

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

    let executeLdfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

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
                match MethodTableProjection.tryProjectField loggerFactory baseClassTypes field methodTableFor state with
                | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                | None ->
                    failwith
                        $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTablePtr %O{methodTableFor}"
            | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                match MethodTableProjection.tryProjectAuxiliaryDataField baseClassTypes field methodTableFor state with
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

    let executeLdflda (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread

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

    let executeLdsfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

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
                IlMachineState.getStatic declaringTypeHandle (ComparableFieldDefinitionHandle.Make field.Handle) state
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
                state.LoadedAssembly(field.DeclaringType.Assembly).Value.TypeDefs.[field.DeclaringType.Definition.Get]

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

    let executeLdsflda (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread


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
