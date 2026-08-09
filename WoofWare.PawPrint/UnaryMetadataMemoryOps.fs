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
                currentMethod.Generics
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
                IlMachineState.writeManagedByrefWithBase baseClassTypes state src zeroOfType
            | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeStobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

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
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte _, _)
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte _, _) ->
                IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state src coerced
            | ManagedPointerSource.Byref _ -> IlMachineState.writeManagedByrefWithBase baseClassTypes state src coerced
            | ManagedPointerSource.Null -> failwith "unreachable: null Stobj target handled above"
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"Stobj: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"

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
            | EvalStackValue.ManagedPointer ptr -> IlMachineState.readManagedByref baseClassTypes state ptr
            | EvalStackValue.Float _
            | EvalStackValue.Int64 _
            | EvalStackValue.Int32 _ -> failwith "refusing to interpret constant as address"
            | _ -> failwith "TODO"

        // The type token need not denote a nominal type: `ldobj !!T` with `T = int[]` — which
        // is what `Dictionary<TKey, TValue[]>.TryGetValue` emits on a hit — concretizes to a
        // structural array handle, which by design has no row in `AllConcreteTypes` and no
        // TypeDef to interrogate. Decide the copy's shape from the handle before touching any
        // metadata.
        let isValueType : bool =
            match typeHandle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                // Arrays are reference types, so III.4.13 reduces to `ldind.ref` below.
                false
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // Triggered by a `ldobj` whose type token is a byref, pointer or
                // function-pointer typespec. No C#/F# compiler emits that (a pointer
                // dereference is `ldind.i`), and the runtime's reflection stack rejects all
                // three as type arguments, so `ldobj !!T` cannot produce one either; only
                // hand-written IL naming such a typespec reaches here. Refusing loudly beats
                // guessing at a coercion no test can exercise.
                failwith
                    $"TODO: Ldobj with a byref/pointer/function-pointer type token (%O{typeHandle}) is not implemented"
            | ConcreteTypeHandle.Concrete _ ->

            match AllConcreteTypes.lookup typeHandle state.ConcreteTypes with
            | None -> failwith $"Ldobj: concrete type handle %O{typeHandle} has no row in AllConcreteTypes"
            | Some targetType ->

            state._LoadedAssemblies.[targetType.Assembly].TypeDefs.[targetType.Definition.Get]
            |> DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies

        let toPush, state =
            if isValueType then
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

    let executeSizeof (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

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
