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
            | EvalStackValue.Int32 (Int32Source.Verbatim v) -> v
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

    /// Resolves the array element-type token shared by Ldelem and Stelem (ECMA-335 III.4.9
    /// "Ldelem", III.4.20 "Stelem"): both instructions' `type` operand is a metadata token
    /// that must be a TypeDef, TypeRef, or TypeSpec naming the array's element type, and the
    /// resolution rules for each token kind don't depend on which of the two instructions is
    /// doing the resolving.
    let private resolveElementTypeToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (activeAssy : DumpedAssembly)
        (declaringTypeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (metadataToken : MetadataToken)
        (opName : string)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match metadataToken with
        | MetadataToken.TypeDefinition defn ->
            state,
            activeAssy,
            activeAssy.TypeDefs.[defn]
            |> TypeInfo.mapGeneric (fun (p, _) -> TypeDefn.GenericTypeParameter p.SequenceNumber)
        | MetadataToken.TypeSpecification spec ->
            IlMachineState.resolveTypeFromSpecConcrete
                loggerFactory
                baseClassTypes
                spec
                activeAssy
                declaringTypeGenerics
                methodGenerics
                state
        | MetadataToken.TypeReference refHandle ->
            // A bare TypeRef in an ldelem/stelem token is a closed type name: a TypeRef row
            // is only ever a (namespace, name, resolution scope) triple, with nowhere to
            // record generic arguments, so a parametric element type always reaches these
            // instructions through a TypeSpec instead (e.g. `ldelem !!T` / `stelem !!T`
            // encode a TypeSpec wrapping a GenericTypeParameter). The generic-args array
            // here is therefore always empty -- this falls out of the TypeReference token
            // kind itself, not of which instruction is consuming it.
            IlMachineTypeResolution.resolveType loggerFactory refHandle ImmutableArray.Empty activeAssy state
        | x -> failwith $"TODO: {opName} element type resolution unimplemented for {x}"

    let executeLdelema (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let typeGenerics = currentMethod.DeclaringType.Generics
        let methodGenerics = currentMethod.Generics

        // ECMA-335 III.2.2: capture and consume the `readonly.` prefix that may have
        // been set by the immediately-preceding NullaryIlOp.Readonly. The prefix's
        // declared scope is the next ldelema, so we clear it regardless of outcome.
        // Its observable runtime effect is to suppress the array-element-type check
        // below; we capture the flag before clearing so the check can branch on it.
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

        let index, state = IlMachineState.popEvalStack thread state
        let arr, state = IlMachineState.popEvalStack thread state

        let index =
            match index with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
            | _ -> failwith $"TODO: {index}"

        let arrAddr =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state arr with
            | Some addr -> addr
            | None -> failwith "TODO: throw NRE"

        let arrAlloc = state.ManagedHeap.Arrays.[arrAddr]

        if index < 0 || index >= arrAlloc.Length then
            failwith "TODO: throw IndexOutOfRangeException"

        let arrayElementHandle =
            match arrAlloc.ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero element -> element
            | other -> failwith $"executeLdelema: array allocation has non-szarray type %O{other}"

        let buildResult (state : IlMachineState) : IlMachineState * WhatWeDid =
            let result =
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrAddr, index), [])
                |> EvalStackValue.ManagedPointer

            let state =
                IlMachineState.pushToEvalStack' result thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        if wasReadonly then
            // The readonly. prefix suppresses the array-element-type check, so we can skip
            // resolving the metadata token entirely.
            buildResult state
        else
            // ECMA-335 III.4.10: the array's runtime element type must exactly equal the
            // metadata token; otherwise ArrayTypeMismatchException. For a value-type token
            // this is a tautology under valid IL (the verifier rejects mismatches), but the
            // check is cheap and the readonly. prefix is the only legal way to bypass it.
            // The exact-equality semantics — not assignment-compatibility — match CoreCLR
            // (interpexec.cpp INTOP_LDELEMA_REF, where the JIT-resolved expectedMT is
            // compared with `arr->GetArrayElementTypeHandle()`).
            let state, elementType, elementAssy =
                IlMachineState.resolveTypeMetadataToken
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy
                    typeGenerics
                    metadataToken

            let state, _zeroOfType, tokenElementHandle =
                IlMachineState.cliTypeZeroOf
                    loggerFactory
                    baseClassTypes
                    elementAssy
                    elementType
                    typeGenerics
                    methodGenerics
                    state

            if tokenElementHandle <> arrayElementHandle then
                // Don't advance the PC: exception dispatch needs the faulting instruction's
                // offset for handler search and stack-trace construction.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    baseClassTypes.ArrayTypeMismatchException
                    thread
                    state
            else
                buildResult state

    let executeStelem (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let declaringTypeGenerics = currentMethod.DeclaringType.Generics

        let state, assy, elementType =
            resolveElementTypeToken
                loggerFactory
                baseClassTypes
                activeAssy
                declaringTypeGenerics
                currentMethod.Generics
                metadataToken
                "Stelem"
                state

        let contents, state = IlMachineState.popEvalStack thread state
        let index, state = IlMachineState.popEvalStack thread state

        let index =
            match index with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
            | _ -> failwith $"Expected int32 index in Stelem, but got: {index}"

        let arr, state = IlMachineState.popEvalStack thread state

        let arr =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state arr with
            | Some addr -> addr
            | None -> failwith "expected heap allocation for array, got null"

        // ECMA-335 III.4.x: bounds check fires before the array-store variance
        // check, matching CoreCLR ordering (e.g. `Store<object>(new string[0], 0, new object())`
        // must raise IndexOutOfRangeException, not ArrayTypeMismatchException).
        let arrAlloc =
            match state.ManagedHeap.Arrays.TryGetValue arr with
            | true, v -> v
            | false, _ -> failwith $"executeStelem: array allocation not found at %O{arr}"

        if index < 0 || index >= arrAlloc.Length then
            // Don't advance PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.IndexOutOfRangeException
                thread
                state
        else

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

        // ECMA-335 III.4.x runtime-assignment-compatibility gate (see
        // IlMachineStateExecution.checkArrayStoreVariance).
        match
            IlMachineStateExecution.checkArrayStoreVariance loggerFactory baseClassTypes thread arr contents state
        with
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Raised state -> state, WhatWeDid.Executed
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Allowed state ->

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
            resolveElementTypeToken
                loggerFactory
                baseClassTypes
                activeAssy
                declaringTypeGenerics
                currentMethod.Generics
                metadataToken
                "Ldelem"
                state

        let index, state = IlMachineState.popEvalStack thread state

        let index =
            match index with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
            | _ -> failwith $"Expected int32 index in Stelem, but got: {index}"

        let arr, state = IlMachineState.popEvalStack thread state

        let arr =
            match IlMachineState.evalStackValueToObjectRef baseClassTypes state arr with
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
