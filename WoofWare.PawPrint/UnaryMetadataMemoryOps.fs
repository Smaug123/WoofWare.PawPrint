namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataMemoryOps =
    let executeInitobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let popped, state = IlMachineThreadState.popEvalStack thread state
        let declaringTypeGenerics = currentMethod.DeclaringType.Generics

        let state, targetType, assy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                declaringTypeGenerics
                metadataToken

        let state, zeroOfType, concreteTypeHandle =
            IlMachineTypeResolution.cliTypeZeroOf
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
            | EvalStackValue.ManagedPointer src ->
                IlMachineManagedByref.writeManagedByrefWithBase baseClassTypes state src zeroOfType
            | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"

        state
        |> IlMachineThreadState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeStobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, ty, assy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                currentMethod.DeclaringType.Generics
                metadataToken

        let state, typeHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let targetZero, state =
            IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes typeHandle

        let valueToStore, state = IlMachineThreadState.popEvalStack thread state
        let addr, state = IlMachineThreadState.popEvalStack thread state

        let writeAt (src : ManagedPointerSource) : IlMachineState =
            let coerced = EvalStackValue.toCliTypeCoerced targetZero valueToStore

            match src with
            | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte _, _) ->
                IlMachineManagedByref.writeManagedByrefBytes state src coerced
            | ManagedPointerSource.Byref _ ->
                IlMachineManagedByref.writeManagedByrefWithBase baseClassTypes state src coerced
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
            |> IlMachineThreadState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | EvalStackValue.NativeInt nativeIntSource ->
            failwith $"TODO: Stobj through native pointer %O{nativeIntSource} is not implemented"
        | EvalStackValue.ObjectRef _ -> failwith "Stobj on an object reference is invalid; expected a managed pointer"
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.Float _
        | EvalStackValue.UserDefinedValueType _ -> failwith $"Stobj target was not an address: %O{addr}"

    let executeLdobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, ty, assy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                currentMethod.DeclaringType.Generics
                metadataToken

        let state, typeHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let addr, state = state |> IlMachineThreadState.popEvalStack thread

        let obj =
            match addr with
            | EvalStackValue.NullObjectRef -> failwith "TODO: throw NullReferenceException"
            | EvalStackValue.ObjectRef _ ->
                failwith "Ldobj on an object reference is invalid; expected a managed pointer"
            | EvalStackValue.ManagedPointer ptr -> IlMachineManagedByref.readManagedByref state ptr
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
                let zero, state =
                    IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes typeHandle

                EvalStackValue.ofCliType obj |> EvalStackValue.toCliTypeCoerced zero, state
            else
                // III.4.13: reference types are just copied as pointers.
                // We should have received a pointer, so let's just pass it back.
                obj, state

        state
        |> IlMachineThreadState.pushToEvalStack toPush thread
        |> IlMachineThreadState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeSizeof (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, ty, assy =
            IlMachineRuntimeMetadata.resolveTypeMetadataToken
                loggerFactory
                baseClassTypes
                state
                activeAssy
                currentMethod.DeclaringType.Generics
                metadataToken

        let state, typeHandle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                assy.Name
                currentMethod.DeclaringType.Generics
                currentMethod.Generics
                ty

        let zero, state =
            IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes typeHandle

        let size = CliType.sizeOf zero

        state
        |> IlMachineThreadState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) thread
        |> IlMachineThreadState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
