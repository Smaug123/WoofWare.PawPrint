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
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let currentState = state.ThreadState.[thread]
        let popped, methodState = MethodState.popFromStack currentState.MethodState

        // Commit the pop straight into `state` rather than merging a pre-pop `ThreadState`
        // entry back in at the end: there are two exits from here — the raise below and the
        // allocation — and both have to return a state in which the operand really has been
        // popped.
        let state =
            { state with
                ThreadState =
                    state.ThreadState
                    |> Map.add thread (ThreadState.setFrame currentState.ActiveMethodState methodState currentState)
            }

        let len =
            match popped with
            | EvalStackValue.Int32 (Int32Source.Verbatim v) -> v
            | popped ->
                failwith
                    $"unexpectedly popped value %O{popped} to serve as array len for Newarr. ECMA-335 III.4.13 also permits a native int here — Roslyn emits `conv.ovf.i; newarr` for `new T[someLong]` — which PawPrint does not yet decode."

        let typeGenerics = currentMethod.DeclaringTypeGenerics

        let state, concreteTypeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, elementType, assy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                // `cliTypeZeroOf` split into its two halves (IlMachineTypeResolution.fs:586-607)
                // so that the zero below is computed the same way for both universes.
                let state = state.WithLoadedAssembly assy

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    typeGenerics
                    methodState.Generics
                    elementType

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx concreteTypeHandle state with
        | Some raised -> raised
        | None ->

        let zeroOfType, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes concreteTypeHandle

        // A byref-like element type is refused before anything is allocated: `Span<int>` and
        // `TypedReference` are stack-only, so CoreCLR fails to load `Span<int>[]` at all and never
        // reaches the allocation. Measured on real .NET, against a control (`sizeof Span<int>` is
        // perfectly legal and answers 16, so this is a fact about `newarr`, not about the type).
        //
        // Universe-independent: the same IL is illegal whether its token came from metadata or
        // from a `DynamicScope`. It is only *reachable* from the latter, because no compiler
        // emits it.
        let state, elementIsByRefLike =
            match
                AllConcreteTypes.tryTypeInfo
                    state.TypeSystem._LoadedAssemblies
                    state.TypeSystem.ConcreteTypes
                    concreteTypeHandle
            with
            | None -> state, false
            | Some (_, elementDefn) ->
                state, LoadedTypeInfo.isByRefLike baseClassTypes state.TypeSystem._LoadedAssemblies elementDefn

        if elementIsByRefLike then
            // The exception *type* is reproduced; its message and `TypeName` are not. CoreCLR's
            // message is "Could not create array type 'System.Span`1[System.Int32][]' from assembly
            // 'System.Private.CoreLib, Version=…, PublicKeyToken=…' because the element type is
            // ByRef-like", and `TypeName` is that same mangled array-type name (both measured).
            // Reproducing them means a CLR-mangled type-name formatter *and* the ability to set
            // `_className`, which the runtime-synthesised-exception channel cannot do — it calls a
            // parameterless constructor and then writes `_message` and `_HResult` only.
            //
            // Setting the message alone would be worse than leaving both: `.Message` would name the
            // type while `.TypeName` stayed null, which is the disagreement `Intrinsics.fs` declines
            // to create for `ArgumentException._paramName` (lines 1908-1910, 3325-3326), and which
            // `ScopeEntryLookup.PastEnd` records for the same reason.
            //
            // Don't advance the PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionResolution (
                    ResolutionFault.TokenBinding,
                    baseClassTypes.TypeLoadException
                ))
                []
                thread
                state
        else

        // The length is checked *after* the element type has been resolved, as CoreCLR does:
        // the array MethodTable is in hand before `AllocateSzArray` is reached, so a type that
        // cannot be loaded is reported as such even when the length would independently have
        // thrown.
        match SzArrayAllocation.checkLength len with
        | Some err ->
            let fault, message = SzArrayAllocation.faultFor err

            // Don't advance the PC: exception dispatch needs the faulting instruction's
            // offset for handler search and stack-trace construction.
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionFault fault)
                (message |> Option.toList |> List.map RuntimeExceptionField.Message)
                thread
                state
        | None ->

        let arrayType = ConcreteTypeHandle.OneDimArrayZero concreteTypeHandle

        let alloc, state =
            IlMachineState.allocateArray arrayType (fun () -> zeroOfType) len state

        let state =
            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    /// The element type an `ldelem` or `stelem` names (ECMA-335 III.4.9, III.4.20), which must be
    /// a TypeDef, TypeRef or TypeSpec, resolved as `newarr` and `ldelema` resolve theirs.
    let private elementTypeOperand
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        let state, elementType, assy =
            IlMachineState.resolveTypeMetadataToken
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                ctx.ActiveAssembly
                ctx.MetadataToken

        let state = state.WithLoadedAssembly assy

        IlMachineState.concretizeType
            ctx.LoggerFactory
            ctx.BaseClassTypes
            state
            assy.DefinitionFullName
            ctx.CurrentMethod.DeclaringTypeGenerics
            ctx.CurrentMethod.Generics
            elementType

    let executeLdelema (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let typeGenerics = currentMethod.DeclaringTypeGenerics
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

        // Resolved whether or not the prefix is present: the JIT resolves the token as it compiles
        // the instruction, and refuses it there, either way (measured).
        let state, tokenElementHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, elementType, elementAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                // `cliTypeZeroOf` split into its two halves (IlMachineTypeResolution.fs:586-607).
                let state = state.WithLoadedAssembly elementAssy

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    elementAssy.DefinitionFullName
                    typeGenerics
                    methodGenerics
                    elementType

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx tokenElementHandle state with
        | Some raised -> raised
        | None ->

        let index, state = IlMachineState.popEvalStack thread state
        let arr, state = IlMachineState.popEvalStack thread state

        match ArrayElementOperands.resolve loggerFactory baseClassTypes index arr thread state with
        | ArrayElementOperands.Faulted (state, whatWeDid) -> state, whatWeDid
        | ArrayElementOperands.InRange (arrAddr, index) ->

        let arrAlloc = ManagedHeap.getArrayShape arrAddr state.ManagedHeap

        let arrayElementHandle =
            match arrAlloc.ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero element -> element
            | other -> failwith $"executeLdelema: array allocation has non-szarray type %O{other}"

        let buildResult (state : IlMachineState) : IlMachineState * WhatWeDid =
            let result =
                ManagedPointerSource.Byref
                    {
                        Root = ByrefRoot.ArrayElement (arrAddr, index)
                        Projections = []
                    }
                |> EvalStackValue.ManagedPointer

            let state =
                IlMachineState.pushToEvalStack' result thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        if wasReadonly then
            // The readonly. prefix suppresses the array-element-type check.
            buildResult state
        else
            // ECMA-335 III.4.10: the array's runtime element type must exactly equal the
            // metadata token; otherwise ArrayTypeMismatchException. For a value-type token
            // this is a tautology under valid IL (the verifier rejects mismatches), but the
            // check is cheap and the readonly. prefix is the only legal way to bypass it.
            // The exact-equality semantics — not assignment-compatibility — match CoreCLR
            // (interpexec.cpp INTOP_LDELEMA_REF, where the JIT-resolved expectedMT is
            // compared with `arr->GetArrayElementTypeHandle()`).
            //
            // The zero itself is discarded — this path only wants the handle — but computing it is
            // not: laying a type out reads its field types, which can bind assembly references
            // nothing else in the run names, and `cliTypeZeroOfHandle` returns those bindings in the
            // state. Dropping the call would drop the bindings, and would turn a type that cannot be
            // laid out into a silent ArrayTypeMismatchException below instead of the failure it is.
            let _zeroOfType, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes tokenElementHandle

            if tokenElementHandle <> arrayElementHandle then
                // Don't advance the PC: exception dispatch needs the faulting instruction's
                // offset for handler search and stack-trace construction.
                IlMachineStateExecution.raiseRuntimeException
                    loggerFactory
                    baseClassTypes
                    (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.ArrayTypeMismatch)
                    []
                    thread
                    state
            else
                buildResult state

    let executeStelem (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let state, concreteTypeHandle = elementTypeOperand ctx state

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx concreteTypeHandle state with
        | Some raised -> raised
        | None ->

        let contents, state = IlMachineState.popEvalStack thread state
        let index, state = IlMachineState.popEvalStack thread state
        let arr, state = IlMachineState.popEvalStack thread state

        // ECMA-335 III.4.x: bounds check fires before the array-store variance
        // check, matching CoreCLR ordering (e.g. `Store<object>(new string[0], 0, new object())`
        // must raise IndexOutOfRangeException, not ArrayTypeMismatchException).
        match ArrayElementOperands.resolve loggerFactory baseClassTypes index arr thread state with
        | ArrayElementOperands.Faulted (state, whatWeDid) -> state, whatWeDid
        | ArrayElementOperands.InRange (arr, index) ->

        let zeroOfType, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes concreteTypeHandle

        // ECMA-335 III.4.x runtime-assignment-compatibility gate (see
        // IlMachineStateExecution.checkArrayStoreVariance).
        match
            IlMachineStateExecution.checkArrayStoreVariance loggerFactory baseClassTypes thread arr contents state
        with
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Refused state ->
            IlMachineStateExecution.raiseRuntimeException
                loggerFactory
                baseClassTypes
                (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.ArrayTypeMismatch)
                []
                thread
                state
        | IlMachineStateExecution.ArrayStoreVarianceCheck.Allowed state ->

        let contents = EvalStackValue.toCliTypeCoerced zeroOfType contents

        IlMachineState.setArrayValue arr contents index state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeLdelem (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let state, concreteTypeHandle = elementTypeOperand ctx state

        match UnaryMetadataIlOpContext.refuseTypeOperand ctx concreteTypeHandle state with
        | Some raised -> raised
        | None ->

        let index, state = IlMachineState.popEvalStack thread state
        let arr, state = IlMachineState.popEvalStack thread state

        match ArrayElementOperands.resolve loggerFactory baseClassTypes index arr thread state with
        | ArrayElementOperands.Faulted (state, whatWeDid) -> state, whatWeDid
        | ArrayElementOperands.InRange (arr, index) ->

        let toPush = ManagedHeap.getArrayValue arr index state.ManagedHeap

        IlMachineState.pushToEvalStack toPush thread state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed
