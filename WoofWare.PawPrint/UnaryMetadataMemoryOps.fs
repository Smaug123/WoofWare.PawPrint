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

        let declaringTypeGenerics = currentMethod.DeclaringTypeGenerics

        let state, concreteTypeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetTypeResult =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                let targetType, assy =
                    TokenBindingStop.typeToken activeAssy metadataToken targetTypeResult

                // `cliTypeZeroOf` split into its two halves (IlMachineTypeResolution.fs:586-607)
                // so that the zero below is computed the same way for both universes.
                let state = state.WithLoadedAssembly assy

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    declaringTypeGenerics
                    currentMethod.Generics
                    targetType

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx concreteTypeHandle state with
        | Some raised -> raised
        | None ->

        let popped, state = IlMachineState.popEvalStack thread state

        let zeroOfType, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes concreteTypeHandle

        match popped with
        | popped when EvalStackValue.isNullAddress popped ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.NullReference)
                []
                thread
                state
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref addressed) ->
            IlMachineState.writeManagedByrefWithBase baseClassTypes state addressed zeroOfType
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
            failwith
                $"initobj: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null
        | EvalStackValue.NullObjectRef -> failwith "unreachable: a null Initobj target is handled above"
        | EvalStackValue.NativeInt nativeIntSource ->
            failwith $"TODO: Initobj through native pointer %O{nativeIntSource} is not implemented"
        | EvalStackValue.ObjectRef _ -> failwith "Initobj on an object reference is invalid; expected a managed pointer"
        | EvalStackValue.Int32 _
        | EvalStackValue.Int64 _
        | EvalStackValue.Float _
        | EvalStackValue.UserDefinedValueType _ -> failwith $"Initobj target was not an address: %O{popped}"

    let executeStobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, typeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, tyResult =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                let ty, assy = TokenBindingStop.typeToken activeAssy metadataToken tyResult

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx typeHandle state with
        | Some raised -> raised
        | None ->

        let targetZero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

        let valueToStore, state = IlMachineState.popEvalStack thread state
        let addr, state = IlMachineState.popEvalStack thread state

        let writeAt (src : ManagedPointerSource) : IlMachineState =
            let coerced = EvalStackValue.toCliTypeCoerced targetZero valueToStore

            match src with
            | ManagedPointerSource.Byref addressed ->
                match addressed.Root with
                | ByrefRoot.StackMemoryByte _
                | ByrefRoot.NativeMemoryByte _ ->
                    IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state addressed coerced
                | _ -> IlMachineState.writeManagedByrefWithBase baseClassTypes state addressed coerced
            | ManagedPointerSource.Null -> failwith "unreachable: null Stobj target handled above"
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"Stobj: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"

        match addr with
        | addr when EvalStackValue.isNullAddress addr ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.NullReference)
                []
                thread
                state
        | EvalStackValue.ManagedPointer src ->
            writeAt src
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) ->
            writeAt (ManagedPointerSource.Byref src)
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | EvalStackValue.NullObjectRef -> failwith "unreachable: a null Stobj target is handled above"
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
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, tyResult =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                let ty, assy = TokenBindingStop.typeToken activeAssy metadataToken tyResult

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx typeHandle state with
        | Some raised -> raised
        | None ->

        let addr, state = state |> IlMachineState.popEvalStack thread

        match addr with
        | addr when EvalStackValue.isNullAddress addr ->
            // ECMA-335 III.4.13: `ldobj` through a null address throws
            // `NullReferenceException`, which the guest may catch. `readManagedByref` cannot
            // raise it — it returns a `CliType` and has a dozen callers — so the check belongs
            // here, at the one site that can dispatch. A non-zero `Verbatim` is an unimplemented
            // path, not a null, and still fails below.
            //
            // No `advanceProgramCounter`: dispatch reads the faulting
            // instruction's PC to decide which handler regions are active and to build the
            // stack trace.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.NullReference)
                []
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
            | ConcreteTypeHandle.Byref _ ->
                failwith $"unreachable: Ldobj's byref type operand %O{typeHandle} is refused above"
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // Triggered by a `ldobj` whose type token is a pointer or function-pointer
                // typespec. No C#/F# compiler emits that (a pointer dereference is `ldind.i`),
                // and the runtime's reflection stack rejects both as type arguments, so
                // `ldobj !!T` cannot produce one either; only hand-written IL naming such a
                // typespec reaches here. Refusing loudly beats guessing at a coercion no test
                // can exercise.
                failwith $"TODO: Ldobj with a pointer/function-pointer type token (%O{typeHandle}) is not implemented"
            | ConcreteTypeHandle.Concrete _ ->

            match
                AllConcreteTypes.tryIsValueType
                    baseClassTypes
                    state.TypeSystem._LoadedAssemblies
                    state.TypeSystem.ConcreteTypes
                    typeHandle
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

                let loaded =
                    IlMachineState.readManagedByrefAs
                        baseClassTypes
                        state
                        zero
                        (ManagedPointerSource.requireAddressed ptr)

                EvalStackValue.ofCliType loaded |> EvalStackValue.toCliTypeCoerced zero, state
            else
                // III.4.13: reference types are just copied as pointers.
                // We should have received a pointer, so let's just pass it back.
                IlMachineState.readManagedByref baseClassTypes state (ManagedPointerSource.requireAddressed ptr), state

        let toPush, state =
            match addr with
            | EvalStackValue.ObjectRef _ ->
                failwith "Ldobj on an object reference is invalid; expected a managed pointer"
            | EvalStackValue.ManagedPointer ptr -> readThrough ptr state
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
                readThrough (ManagedPointerSource.Byref ptr) state
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
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, tyResult =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                let ty, assy = TokenBindingStop.typeToken activeAssy metadataToken tyResult

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx typeHandle state with
        | Some raised -> raised
        | None ->

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

        let size = CliType.sizeOf zero

        state
        |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
