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
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let popped, state = IlMachineState.popEvalStack thread state
        let declaringTypeGenerics = currentMethod.DeclaringTypeGenerics

        let state, concreteTypeHandle =
            match ctx.Operand with
            | ResolvedMetadataOperand.ScopeType handle -> state, handle
            | ResolvedMetadataOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetType, assy =
                    IlMachineState.resolveTypeMetadataToken
                        loggerFactory
                        baseClassTypes
                        state
                        activeAssy
                        declaringTypeGenerics
                        metadataToken

                // `cliTypeZeroOf` split into its two halves (IlMachineTypeResolution.fs:586-607)
                // so that the zero below is computed the same way for both universes.
                let state = state.WithLoadedAssembly assy

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    declaringTypeGenerics
                    currentMethod.Generics
                    targetType

        let zeroOfType, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes concreteTypeHandle

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
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, typeHandle =
            match ctx.Operand with
            | ResolvedMetadataOperand.ScopeType handle -> state, handle
            | ResolvedMetadataOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, ty, assy =
                    IlMachineState.resolveTypeMetadataToken
                        loggerFactory
                        baseClassTypes
                        state
                        activeAssy
                        currentMethod.DeclaringTypeGenerics
                        metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringTypeGenerics
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
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L) ->
            // `Verbatim 0L` is the unmanaged spelling of null — `*(S*)null = v` is
            // `ldc.i4.0; conv.u; ...; stobj S` — and is also what a null managed pointer
            // normalises to on a `conv` round-trip. Without it a null store faults the
            // interpreter instead of the guest. `executeLdobj` guards the same four spellings.
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
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, typeHandle =
            match ctx.Operand with
            | ResolvedMetadataOperand.ScopeType handle -> state, handle
            | ResolvedMetadataOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, ty, assy =
                    IlMachineState.resolveTypeMetadataToken
                        loggerFactory
                        baseClassTypes
                        state
                        activeAssy
                        currentMethod.DeclaringTypeGenerics
                        metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        let addr, state = state |> IlMachineState.popEvalStack thread

        match addr with
        | EvalStackValue.NullObjectRef
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L) ->
            // ECMA-335 III.4.13: `ldobj` through a null address throws
            // `NullReferenceException`, which the guest may catch. `readManagedByref` cannot
            // raise it — it returns a `CliType` and has a dozen callers — so the check belongs
            // here, at the one site that can dispatch.
            //
            // All four spellings are reachable. `Verbatim 0L` is what an unmanaged null
            // pointer becomes (`*(S*)null` is `ldc.i4.0; conv.u; ldobj S`), and it is also
            // what a null managed pointer normalises to on any `conv` round-trip — see the
            // "Conv.U8 of a null managed pointer normalises to verbatim zero" case in
            // `TestBinaryArithmetic.fs`. A non-zero `Verbatim` is a genuine unimplemented
            // path, not a null, and still fails below.
            //
            // Deliberately no `advanceProgramCounter`: dispatch reads the faulting
            // instruction's PC to decide which handler regions are active and to build the
            // stack trace.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                baseClassTypes.NullReferenceException
                thread
                state
        | _ ->

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

            match
                AllConcreteTypes.tryIsValueType baseClassTypes state._LoadedAssemblies state.ConcreteTypes typeHandle
            with
            | Some isValueType -> isValueType
            | None -> failwith $"Ldobj: concrete type handle %O{typeHandle} has no row in AllConcreteTypes"

        // `ldobj T` loads a `T`, and the address it loads from need not be the address *of* a `T`:
        // `(Narrow*)&wide` names the first byte of an eight-byte slot, and `ldobj Narrow` reads
        // four of them. So a value-type load takes its width from the token via
        // `readManagedByrefAs`, rather than accepting whatever the pointer's own type view or root
        // happens to be and coercing that. A reference-typed token has nothing to narrow — the
        // load is a pointer copy — so it keeps the pointer-shaped read.
        let readThrough (ptr : ManagedPointerSource) (state : IlMachineState) : CliType * IlMachineState =
            if isValueType then
                let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

                let loaded = IlMachineState.readManagedByrefAs baseClassTypes state zero ptr

                EvalStackValue.ofCliType loaded |> EvalStackValue.toCliTypeCoerced zero, state
            else
                // III.4.13: reference types are just copied as pointers.
                // We should have received a pointer, so let's just pass it back.
                IlMachineState.readManagedByref baseClassTypes state ptr, state

        let toPush, state =
            match addr with
            | EvalStackValue.ObjectRef _ ->
                failwith "Ldobj on an object reference is invalid; expected a managed pointer"
            | EvalStackValue.ManagedPointer ptr
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> readThrough ptr state
            | EvalStackValue.Float _
            | EvalStackValue.Int64 _
            | EvalStackValue.Int32 _ -> failwith "refusing to interpret constant as address"
            | EvalStackValue.NullObjectRef -> failwith "unreachable: null Ldobj address handled above"
            | EvalStackValue.NativeInt nativeIntSource ->
                failwith $"TODO: Ldobj through native pointer %O{nativeIntSource} is not implemented"
            | EvalStackValue.UserDefinedValueType _ -> failwith $"Ldobj address was not an address: %O{addr}"

        state
        |> IlMachineState.pushToEvalStack toPush thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeSizeof (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, typeHandle =
            match ctx.Operand with
            | ResolvedMetadataOperand.ScopeType handle -> state, handle
            | ResolvedMetadataOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, ty, assy =
                    IlMachineState.resolveTypeMetadataToken
                        loggerFactory
                        baseClassTypes
                        state
                        activeAssy
                        currentMethod.DeclaringTypeGenerics
                        metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.Name
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

        let size = CliType.sizeOf zero

        state
        |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
