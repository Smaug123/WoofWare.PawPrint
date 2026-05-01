namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataArrayOps =
    let executeNewarr (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

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

    let executeLdelema (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let thread = ctx.Thread

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

    let executeStelem (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

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

    let executeLdelem (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

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
