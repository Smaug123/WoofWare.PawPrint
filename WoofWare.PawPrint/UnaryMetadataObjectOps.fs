namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataObjectOps =
    /// The type test that ECMA-335 III.4.3 (`castclass`) and III.4.6 (`isinst`) share once the
    /// operand is a non-null object: is an object of runtime type `objConcreteType` an instance of
    /// `targetConcreteType`?
    ///
    /// Both instructions read a `Nullable<T>` token as a boxed `T`, because a `Nullable<T>` boxes
    /// as a `T` or as null and never as itself. So a boxed `T` passes a `Nullable<T>` test even
    /// though `T` is not assignable to `Nullable<T>` structurally, and the plain assignability
    /// walk cannot answer this on its own. CoreCLR's `ObjIsInstanceOfCore` (`jithelpers.cpp`),
    /// which `CastHelpers.ChkCastAny` and `CastHelpers.IsInstanceOfAny` reach for a value-type
    /// token, asks `Nullable::IsNullableForType` before its structural walk for the same reason.
    let private isObjectInstanceOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objConcreteType : ConcreteTypeHandle)
        (targetConcreteType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if IlMachineState.isNullableForType baseClassTypes state targetConcreteType objConcreteType then
            state, true
        else
            IlMachineState.isConcreteTypeAssignableTo
                loggerFactory
                baseClassTypes
                state
                objConcreteType
                targetConcreteType

    /// ECMA-335 III.4.3 (`castclass`) and III.4.33 (`unbox.any` whose type token denotes a
    /// reference type) specify identical behaviour once the operand and the target type are in
    /// hand: a null operand passes through; an operand that is an instance of the target (per
    /// `isObjectInstanceOf`) passes through unchanged; anything else raises InvalidCastException.
    ///
    /// `opName` appears only in the diagnostic for eval-stack shapes we do not model.
    let private castToReferenceType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (thread : ThreadId)
        (targetConcreteType : ConcreteTypeHandle)
        (actualObj : EvalStackValue)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        match actualObj with
        | EvalStackValue.NullObjectRef ->
            // Per ECMA-335 III.4.3: null ref is always valid for a cast to a reference type.
            let state =
                state
                |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | EvalStackValue.ObjectRef addr ->
            // `getObjectConcreteType` consults both the array and the non-array side of the heap,
            // so array operands need no special-casing here; `isConcreteTypeAssignableTo` already
            // understands array handles (rank, element covariance, the SZ-array implicit generic
            // interfaces, and the `System.Array` base chain).
            let objConcreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            let state, isInstance =
                isObjectInstanceOf loggerFactory baseClassTypes state objConcreteType targetConcreteType

            if isInstance then
                let state =
                    state
                    |> IlMachineState.pushToEvalStack' actualObj thread
                    |> IlMachineState.advanceProgramCounter thread

                state, WhatWeDid.Executed
            else
                IlMachineStateExecution.raiseOpcodeFault
                    loggerFactory
                    baseClassTypes
                    OpcodeFault.InvalidCast
                    thread
                    state
        | other -> failwith $"%s{opName}: unexpected eval stack value {other}"

    let executeCastclass (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetConcreteType =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetType, _targetAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    targetType

        castToReferenceType loggerFactory baseClassTypes "Castclass" thread targetConcreteType actualObj state

    /// Implements `newobj` of one of an array type's runtime-synthesised constructors (ECMA-335
    /// II.14.2), which has no body to run. `arrayType` is the `MemberReference`'s parent, and
    /// `signature` is the constructor's as the `MemberReference` spells it, which picks the
    /// constructor out by its parameter count.
    ///
    /// The constructor's `int32` arguments are popped (the top of the stack is the last), and what
    /// CoreCLR's allocator would make of them is decided by `ArrayConstructor.plan` and allocated
    /// by `ArrayConstruction.allocate`, which is exactly what an `[UnsafeAccessor]` bound to the
    /// same constructor does.
    let private executeArrayNewobj
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        (arrayType : TypeDefn)
        (signature : MemberSignature)
        : IlMachineState * WhatWeDid
        =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let methodSig =
            match signature with
            | MemberSignature.Method m -> m
            | MemberSignature.Field _ ->
                failwith
                    "BUG: an array constructor's MemberReference had a field signature; expected a method signature"

        let state, arrayType =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                ctx.ActiveAssembly.DefinitionFullName
                ctx.CurrentMethod.DeclaringTypeGenerics
                ctx.CurrentMethod.Generics
                arrayType

        let ctor =
            let allInt32 =
                methodSig.ParameterTypes
                |> List.forall (fun parameter ->
                    match parameter with
                    | TypeDefn.PrimitiveType PrimitiveType.Int32 -> true
                    | _ -> false
                )

            match ArrayConstructor.withParameterCount arrayType methodSig.ParameterTypes.Length with
            | Some ctor when allInt32 -> ctor
            | _ ->
                let parameters = methodSig.ParameterTypes |> List.map string |> String.concat ", "

                failwith
                    $"TODO: newobj names a constructor of array type %O{arrayType} taking (%s{parameters}), which that type does not declare; CoreCLR raises MissingMethodException when it compiles the call"

        let count = ArrayConstructor.parameterCount ctor
        let arguments = Array.zeroCreate<int> count
        let mutable s = state

        for i = count - 1 downto 0 do
            let v, s' = IlMachineState.popEvalStack thread s

            match v with
            | EvalStackValue.Int32 (Int32Source.Verbatim n) ->
                arguments.[i] <- n
                s <- s'
            | other -> failwith $"unexpectedly popped non-Int32 value %O{other} as array constructor argument %d{i}"

        let state = s

        match ArrayConstructor.plan ctor (ImmutableArray.CreateRange arguments) with
        | Error error ->
            // The constructor is a callee -- CoreCLR's `CreateInstanceMDArray` helper -- so what it
            // raises is not `newobj`'s own fault, and does not go through `OpcodeFaults`. The helper
            // is `[StackTraceHidden]`, so the frame that reports it is this one, at this `newobj`.
            let exceptionType, message = ArrayConstructor.exceptionFor baseClassTypes error

            IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                loggerFactory
                baseClassTypes
                exceptionType
                message
                thread
                state
        | Ok allocation ->

        let alloc, state = ArrayConstruction.allocate baseClassTypes allocation state

        let state =
            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    /// Allocate the object a `newobj` names and enter its constructor: everything the opcode does
    /// once its operand has been resolved and concretized, value types included, and
    /// `System.String` included, whose constructors are redirected here to their managed `Ctor`
    /// siblings. This is the whole of the opcode's construction, so that a body the runtime
    /// synthesises as `newobj` (CoreCLR's `[UnsafeAccessor]` stubs, `vm/unsafeaccessors.cpp`)
    /// constructs exactly as the opcode does.
    ///
    /// The constructor arguments must already be on the calling frame's evaluation stack; the
    /// receiver is pushed beneath them. `advanceProgramCounterOfCaller` is passed through to
    /// `callMethod`: the opcode advances past itself, while a frame without IL has nothing to
    /// advance. `logger` is the caller's, so that a caller which already holds one per opcode does
    /// not pay for a new one on every construction.
    let constructObject
        (loggerFactory : ILoggerFactory)
        (logger : ILogger)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (concretizedCtor : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (declaringTypeHandle : ConcreteTypeHandle)
        (advanceProgramCounterOfCaller : bool)
        (state : IlMachineState)
        : IlMachineState
        =
        let heapValueByref (addr : ManagedHeapAddress) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        let ctorAssembly =
            state.LoadedAssembly concretizedCtor.DeclaringAssemblyFullName |> Option.get

        let ctorType =
            ctorAssembly.TypeDefs.[concretizedCtor.RequiredDeclaringType.Definition.Get]

        do
            logger.LogDebug (
                "Creating object of type {ConstructorAssembly}.{ConstructorType}",
                ctorAssembly.Name.Name,
                ctorType.Name
            )

        // The CLI's variable-size-object case: types whose instance size depends on the
        // constructor arguments, which CoreCLR flags `CORINFO_FLG_VAROBJSIZE` (set whenever
        // the MethodTable `HasComponentSize` — see `vm/jitinterface.cpp`). The runtime cannot
        // allocate before the constructor runs, so it allocates nothing and passes no `this`
        // (`jit/importer.cpp`, CEE_NEWOBJ: "At present this can only be String",
        // `newObjThisPtr = nullptr`; `interpreter/compiler.cpp`, `doCallInsteadOfNew = true`).
        //
        // Arrays are the CLI's only other variable-size case and never reach here: their
        // constructors have no body, and both routes to one -- `newobj` through
        // `executeArrayNewobj`, and an `[UnsafeAccessor]` -- allocate through `ArrayConstruction`
        // instead. So, exactly as CoreCLR asserts, this is System.String and nothing else.
        //
        // Every `System.String` constructor is declared `extern` with
        // `MethodImplOptions.InternalCall` and has an empty body; the *implementation* is the
        // sibling managed static `String.Ctor` of the same parameter signature, returning
        // `string`. CoreCLR wires the two together in `vm/ecall.cpp`
        // (`PopulateManagedStringConstructors`), which walks the nine `METHOD__STRING__CTORF_*`
        // binder entries and dynamically assigns each `Ctor` method's own compiled code as the
        // ctor's FCall implementation. So a `newobj` on String really does execute CoreLib IL —
        // `Ctor`'s — and we reproduce that by redirecting the call here rather than
        // hand-implementing each overload at the native boundary.
        //
        // The stack shapes line up exactly: `newobj` has pushed the N constructor arguments and
        // no `this`, which is precisely what a static N-ary `Ctor` pops, and `Ctor`'s `string`
        // return value is pushed to the caller by the ordinary `NotConstructing` return path —
        // which is what `newobj` must leave behind.
        if TypeInfo.NominallyEqual ctorType baseClassTypes.String then
            let ctorImplementation =
                ctorType.Methods
                |> List.filter (fun candidate ->
                    candidate.Name = "Ctor"
                    && candidate.IsStatic
                    && (MethodInfo.requireRawSignature "String ctor redirection" candidate).ParameterTypes = (MethodInfo.requireRawSignature
                        "String ctor redirection"
                        concretizedCtor)
                        .ParameterTypes
                )

            let describedSignature : string =
                (MethodInfo.requireRawSignature "String ctor redirection" concretizedCtor).ParameterTypes
                |> List.map string
                |> String.concat ", "

            let ctorImplementation =
                match ctorImplementation with
                | [ single ] -> single
                | [] ->
                    failwith
                        $"newobj on System.String::.ctor(%s{describedSignature}) found no matching static String.Ctor to redirect to. CoreCLR implements every string constructor as its same-signature `Ctor` sibling (vm/ecall.cpp, PopulateManagedStringConstructors); a missing one means this CoreLib declares a constructor overload we do not know about."
                | _ :: _ :: _ ->
                    failwith
                        $"newobj on System.String::.ctor(%s{describedSignature}) found several matching static String.Ctor overloads; the parameter signature should identify exactly one."

            match (MethodInfo.requireRawSignature "String ctor redirection" ctorImplementation).ReturnType with
            | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String) -> ()
            | other ->
                failwith
                    $"String.Ctor selected for newobj returns %O{other}; every String.Ctor overload must return String, because its return value is what newobj pushes."

            // String is non-generic, so there are no type generics to substitute, and no
            // `Ctor` overload is itself generic.
            let state, concretizedCtorImplementation, _ =
                ExecutionConcretization.concretizeMethodWithTypeGenerics
                    loggerFactory
                    baseClassTypes
                    ImmutableArray.Empty
                    ctorImplementation
                    None
                    ctorAssembly.DefinitionFullName
                    ImmutableArray.Empty
                    state

            let threadState = state.ThreadState.[thread]

            IlMachineStateExecution.callMethod
                loggerFactory
                baseClassTypes
                None
                ConstructionState.NotConstructing
                false
                false
                advanceProgramCounterOfCaller
                concretizedCtorImplementation.Generics
                concretizedCtorImplementation
                thread
                threadState
                None
                ReturnValueDisposition.PushToCaller
                false // wrapExceptionInTargetInvocation
                state
        else

        let state, fields =
            IlMachineState.buildInstanceStorage loggerFactory baseClassTypes state declaringTypeHandle

        // This is a bit unorthodox for value types, which *aren't* heap-allocated.
        // We'll perform their construction on the heap, though, to keep the interface
        // of Newobj uniform.
        // On completion of the constructor, we'll copy the value back off the heap,
        // and put it on the eval stack directly.
        let allocatedAddr, state =
            let ty =
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    concretizedCtor.RequiredDeclaringType.Identity
                    concretizedCtor.DeclaringTypeGenerics
                |> Option.get

            IlMachineState.allocateManagedObject ty fields state

        let state =
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ctorType then
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer (heapValueByref allocatedAddr)) thread
            else
                state
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocatedAddr)) thread

        let threadState = state.ThreadState.[thread]

        IlMachineStateExecution.callMethod
            loggerFactory
            baseClassTypes
            None
            (ConstructionState.Constructing allocatedAddr)
            false
            false
            advanceProgramCounterOfCaller
            concretizedCtor.Generics
            concretizedCtor
            thread
            threadState
            None
            ReturnValueDisposition.PushToCaller
            false // wrapExceptionInTargetInvocation
            state

    let executeNewobj (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let activeAssy = ctx.ActiveAssembly
        let metadataToken = ctx.MetadataToken
        let thread = ctx.Thread
        let logger = ctx.Logger

        // Array constructors are runtime-synthesized (ECMA-335 II.14.2): the metadata token is a
        // MemberReference whose parent is a TypeSpec of an array. There's no managed body to
        // resolve, so detect that shape up front and route to the array allocation path. C#
        // constructs a szarray with `newarr`, but `newobj` of one of its constructors is equally
        // valid IL, and is the only way to reach a jagged constructor.
        let arraySpec =
            match metadataToken with
            | MemberReference mrHandle ->
                match activeAssy.Members.TryGetValue mrHandle with
                | true, memberRef ->
                    match memberRef.Parent with
                    | MetadataToken.TypeSpecification specHandle ->
                        match activeAssy.TypeSpecs.TryGetValue specHandle with
                        | true, ts ->
                            match ts.Signature with
                            | TypeDefn.Array _
                            | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> Some (ts.Signature, memberRef.Signature)
                            | _ -> None
                        | false, _ -> None
                    | _ -> None
                | false, _ -> None
            | _ -> None

        match arraySpec with
        | Some (arrayType, sig0) -> executeArrayNewobj ctx state arrayType sig0
        | None ->

        let state, ctor, typeArgsFromMetadata =
            match metadataToken with
            | MethodDef md ->
                let method = activeAssy.Methods.[md]

                state, MethodInfo.mapTypeGenerics (fun _ -> failwith "non-generic method") method, None
            | MemberReference mr ->
                let state, _, method, extractedTypeArgs =
                    IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy mr state

                match method with
                | Choice1Of2 mr -> state, mr, Some extractedTypeArgs
                | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
            | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

        let state, concretizedCtor, declaringTypeHandle =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                ctor
                None
                typeArgsFromMetadata
                state

        // No class-initialisation check here: the object is allocated first and the constructor's
        // own prologue runs it, which is the order the CLR uses. Measured on .NET 10, a `.cctor`
        // that throws under a plain `newobj` gives its `TypeInitializationException` a trace
        // beginning `at T..ctor()`, so that frame is established before the initialiser runs, and
        // the allocation it belongs to has already happened.
        //
        // An allocation whose `.cctor` then throws is therefore garbage, exactly as on the real
        // runtime: the `newobj` never completes and nothing can reach the object.

        constructObject loggerFactory logger baseClassTypes thread concretizedCtor declaringTypeHandle true state,
        WhatWeDid.Executed

    let executeBox (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let state, typeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, ty, assy =
                    match metadataToken with
                    | MetadataToken.TypeDefinition h ->
                        let state, ty = IlMachineState.lookupTypeDefn baseClassTypes state activeAssy h
                        state, ty, activeAssy
                    | MetadataToken.TypeReference ref ->
                        IlMachineState.lookupTypeRef loggerFactory baseClassTypes state activeAssy ref
                    | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
                    | _ -> failwith $"unexpected token {metadataToken} in Box"

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    assy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    ty

        let toBox, state = state |> IlMachineState.popEvalStack thread

        // ECMA-335 III.4.1: structural reference-type tokens (szarrays and multi-dim arrays)
        // make `box` a no-op — the value already on the stack is a reference. A byref token is
        // rejected outright by the runtime; a pointer or function-pointer token, despite being
        // unverifiable, is *not* — measured on real .NET, `box int*` (and `unbox.any int*`) runs.
        // PawPrint does not implement a boxed pointer, which would be a heap object whose runtime
        // type is a structural handle; that is its own change, so refuse loudly and say so.
        match typeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            state
            |> IlMachineState.pushToEvalStack' toBox thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | ConcreteTypeHandle.Byref _ ->
            failwithf
                "Box: byref type %O cannot be boxed; real .NET rejects the method as an invalid program (measured)."
                typeHandle
        | ConcreteTypeHandle.Pointer _ ->
            failwithf
                "TODO: Box of pointer type %O is not implemented. Real .NET boxes one (measured), producing an object whose runtime type is the pointer type; PawPrint has no heap object with a structural type. Reachable from a dynamic method, whose `ILGenerator.Emit(OpCode, Type)` takes any RuntimeType, and from `typeof(int*)`."
                typeHandle
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwithf
                "TODO: Box of function pointer type %O is not implemented; see the pointer case above, which real .NET likewise permits."
                typeHandle
        | ConcreteTypeHandle.Concrete _ ->

        // A byref-like type is stack-only, so boxing one is not a program the runtime will accept:
        // measured on real .NET as InvalidProgramException, against `sizeof Span<int>` as a control
        // (legal, answers 16). Universe-independent, because the same IL is illegal either way; it
        // is only *reachable* from a `DynamicScope` operand, because no compiler emits it.
        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes typeHandle with
        | Some (_, boxedDefn) when DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies boxedDefn ->
            // Don't advance the PC: exception dispatch needs the faulting instruction's offset.
            IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                loggerFactory
                baseClassTypes
                baseClassTypes.InvalidProgramException
                // CoreCLR's own message here is InvalidProgramException's default (measured).
                None
                thread
                state
        | _ ->

        let targetType =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes |> Option.get

        let defn =
            (state._LoadedAssemblies.ByDefinitionName targetType.AssemblyFullName).TypeDefs.[targetType.Definition.Get]

        let toPush, state =
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn then
                // Boxing a value type: wrap it in a heap object and push an ObjectRef. A
                // `Nullable<T>` boxes to null or to a boxed `T`; `boxValue` owns that rule.
                Boxing.boxValue loggerFactory baseClassTypes typeHandle toBox state
            else
                // Reference type: box is a no-op, value passes through unchanged
                toBox, state

        state
        |> IlMachineState.pushToEvalStack' toPush thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeIsinst (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetConcreteType =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetType, _targetAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    targetType

        let isinstCheck
            (state : IlMachineState)
            (objConcreteType : ConcreteTypeHandle)
            (successValue : EvalStackValue)
            : IlMachineState * EvalStackValue
            =
            let state, isInstance =
                isObjectInstanceOf loggerFactory baseClassTypes state objConcreteType targetConcreteType

            if isInstance then
                state, successValue
            else
                state, EvalStackValue.NullObjectRef

        let state, returnObj =
            match actualObj with
            | EvalStackValue.NullObjectRef ->
                // null IsInstance check always succeeds and results in a null reference
                state, EvalStackValue.NullObjectRef
            | EvalStackValue.ObjectRef addr ->
                let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                isinstCheck state concreteType actualObj
            | EvalStackValue.ManagedPointer src ->
                match IlMachineState.readManagedByref baseClassTypes state src with
                | CliType.ObjectRef None -> state, EvalStackValue.NullObjectRef
                | CliType.ObjectRef (Some addr) ->
                    let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap
                    isinstCheck state concreteType (EvalStackValue.ObjectRef addr)
                | other -> failwith $"TODO: Isinst on managed pointer to non-object-ref {other}"
            | esv -> failwith $"TODO: Isinst on {esv}"

        let state =
            state
            |> IlMachineState.pushToEvalStack' returnObj thread
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    let executeUnboxAny (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        // ECMA-335 III.4.33
        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetConcreteTypeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetType, _targetAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    targetType

        // The type token need not denote a nominal type: `unbox.any !!T` with `T = int[]`
        // concretizes to a structural array handle, which by design has no row in
        // `AllConcreteTypes` and no TypeDef to interrogate. Dispatch on the shape of the handle
        // before touching any metadata.
        match targetConcreteTypeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Array types are reference types, and are never `Nullable<T>`, so III.4.33 reduces
            // to castclass.
            castToReferenceType
                loggerFactory
                baseClassTypes
                "Unbox_Any (reference-type target)"
                thread
                targetConcreteTypeHandle
                actualObj
                state
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // None of these can be a generic argument, so `unbox.any !!T` cannot reach here. A
            // byref token really is invalid IL; a *pointer* token is unverifiable but not invalid,
            // and `unbox.any int*` was measured to run on real .NET. PawPrint refuses both, because
            // the pointer case needs the boxed pointer `executeBox` does not implement either.
            failwith
                $"TODO: Unbox_Any of byref/pointer/function-pointer type %O{targetConcreteTypeHandle} is not implemented. A byref token is invalid IL; a pointer token is legal on real .NET (measured) and needs the same boxed pointer `box` does not implement."
        | ConcreteTypeHandle.Concrete _ ->

        let targetConcreteType, targetDefn =
            AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes targetConcreteTypeHandle
            |> Option.get

        let isNullable =
            InternalTypeKind.kind baseClassTypes targetConcreteType = InternalTypeKind.Nullable

        let isValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn

        if isNullable then
            // ECMA-335 III.4.33 / CoreCLR `Nullable::UnBox` (src/coreclr/vm/object.cpp). `box` of a
            // `Nullable<T>` never produces a boxed Nullable — it yields null, or a boxed `T` — so
            // unboxing has to reconstruct the Nullable from those two forms:
            //   - a null operand yields a zeroed Nullable (`hasValue = false`). This is the one
            //     value-typed `unbox.any` target that accepts null instead of raising
            //     NullReferenceException;
            //   - a boxed `T` yields `hasValue = true` with that value;
            //   - anything else is an InvalidCastException.
            // The match against `T` is exact equivalence, not assignability
            // (`Nullable::IsNullableForTypeHelper` compares against `GetInstantiation()[0]`).
            if targetConcreteType.Generics.Length <> 1 then
                failwith
                    $"Unbox_Any: %O{targetConcreteTypeHandle} classified as System.Nullable`1 but has %d{targetConcreteType.Generics.Length} generic arguments, expected exactly 1"

            let underlyingHandle = targetConcreteType.Generics.[0]

            // Built from the zero rather than hand-rolled, so the layout, field ids and offsets
            // match every other way a `Nullable<T>` comes into existence.
            let nullableZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes targetConcreteTypeHandle

            let zeroCvt =
                match nullableZero with
                | CliType.ValueType cvt -> cvt
                | other ->
                    failwith
                        $"Unbox_Any: zero of Nullable`1 %O{targetConcreteTypeHandle} was %O{other}, expected a value type"

            match actualObj with
            | EvalStackValue.NullObjectRef ->
                state
                |> IlMachineState.pushToEvalStack nullableZero thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed
            | EvalStackValue.ObjectRef addr ->
                let boxedOpt =
                    match ManagedHeap.tryGet addr state.ManagedHeap with
                    | Some v -> Some v
                    | None ->
                        // An array can never be a boxed T for any T that Nullable admits.
                        if ManagedHeap.isArray addr state.ManagedHeap then
                            None
                        else
                            failwith $"Unbox_Any: could not find managed object with address {addr}"

                match boxedOpt with
                | Some boxed when boxed.ConcreteType = underlyingHandle ->
                    let value, state =
                        BoxedValue.contents baseClassTypes underlyingHandle boxed.Contents state

                    // No coercion needed: `BoxedValue.contents` decides its shape from
                    // `cliTypeZeroOfHandle underlyingHandle`, which is the same computation that
                    // produced the zero of the `value` field we are overwriting.
                    let hasValueField =
                        IlMachineState.requiredOwnInstanceFieldId state zeroCvt.Declared "hasValue"

                    let valueField =
                        IlMachineState.requiredOwnInstanceFieldId state zeroCvt.Declared "value"

                    let result =
                        zeroCvt
                        |> CliValueType.WithFieldSetById hasValueField (CliType.ofBool true)
                        |> CliValueType.WithFieldSetById valueField value

                    state
                    |> IlMachineState.pushToEvalStack (CliType.ValueType result) thread
                    |> IlMachineState.advanceProgramCounter thread
                    |> Tuple.withRight WhatWeDid.Executed
                | Some boxed when boxed.ConcreteType = targetConcreteTypeHandle ->
                    // CoreCLR has a "for safety's sake" arm here that copies a genuinely boxed
                    // `Nullable<T>` straight through. Nothing in this interpreter can produce one:
                    // every path that boxes — `box`, the `constrained.` callvirt fallback in
                    // UnaryMetadataCallOps (reachable with a `Nullable<T>` receiver, since
                    // `Object::GetType` is not virtual and so is inherited by every value type),
                    // and the reflection-invocation QCall — goes through `Boxing.boxValue`, which
                    // boxes a `Nullable<T>` as null or as its `T`, and `Boxing.boxValueType`
                    // refuses a `Nullable<T>` handle outright.
                    // So this is unreachable rather than merely untested; fail loudly if that
                    // assumption ever stops holding, instead of silently answering
                    // InvalidCastException like the arm below.
                    failwith
                        $"Unbox_Any: operand at %O{addr} is a boxed Nullable`1 (%O{targetConcreteTypeHandle}), which no PawPrint boxing path can create; CoreCLR's Nullable::UnBox copies it through, but that arm is deliberately unmodelled here"
                | Some _
                | None ->
                    IlMachineStateExecution.raiseOpcodeFault
                        loggerFactory
                        baseClassTypes
                        OpcodeFault.InvalidCast
                        thread
                        state
            | other -> failwith $"Unbox_Any (Nullable`1 target): unexpected eval stack value {other}"
        elif not isValueType then
            // Reference-type target: behave exactly like castclass.
            castToReferenceType
                loggerFactory
                baseClassTypes
                "Unbox_Any (reference-type target)"
                thread
                targetConcreteTypeHandle
                actualObj
                state
        else
            // Value-type target, non-Nullable.
            let state, typeTest =
                BoxedValue.unboxTypeTest
                    loggerFactory
                    baseClassTypes
                    "Unbox_Any (value-type target)"
                    targetConcreteTypeHandle
                    actualObj
                    state

            match typeTest with
            | UnboxTypeTest.NullOperand ->
                IlMachineStateExecution.raiseOpcodeFault
                    loggerFactory
                    baseClassTypes
                    OpcodeFault.NullReference
                    thread
                    state
            | UnboxTypeTest.WrongType ->
                IlMachineStateExecution.raiseOpcodeFault
                    loggerFactory
                    baseClassTypes
                    OpcodeFault.InvalidCast
                    thread
                    state
            | UnboxTypeTest.Accepted (_addr, boxed) ->
                // Materialise using the *boxed object's* handle, not the target's: that is the
                // handle its `Contents` were built with, which is the precondition
                // `BoxedValue.contents` documents. Under the enum relaxation the two can differ,
                // and it is the push/store path that reconciles the result with the target.
                let toPush, state =
                    BoxedValue.contents baseClassTypes boxed.ConcreteType boxed.Contents state

                state
                |> IlMachineState.pushToEvalStack toPush thread
                |> IlMachineState.advanceProgramCounter thread
                |> Tuple.withRight WhatWeDid.Executed

    /// ECMA-335 III.4.32 (`unbox`). The type test is shared with the value-type arm of
    /// `unbox.any` — CoreCLR routes both through `CastHelpers.Unbox_Helper` — but the result
    /// differs: `unbox` pushes a managed pointer *into* the boxed object rather than a copy of
    /// its contents, so a `stobj`/`stfld` through the result is visible through the box.
    ///
    /// The `Nullable<T>` target is unimplemented; see `BoxedValue.unboxAddress`.
    let executeUnbox (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let currentMethod = ctx.CurrentMethod
        let thread = ctx.Thread

        let actualObj, state = IlMachineState.popEvalStack thread state

        let state, targetConcreteTypeHandle =
            match ctx.TypeOperand with
            | ResolvedTypeOperand.FromScope handle -> state, handle
            | ResolvedTypeOperand.FromMetadata (activeAssy, metadataToken) ->
                let state, targetType, _targetAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory baseClassTypes state activeAssy metadataToken

                IlMachineState.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    activeAssy.DefinitionFullName
                    currentMethod.DeclaringTypeGenerics
                    currentMethod.Generics
                    targetType

        let state, result =
            BoxedValue.unboxAddress loggerFactory baseClassTypes "Unbox" targetConcreteTypeHandle actualObj state

        match result with
        | UnboxAddress.Faulted fault ->
            IlMachineStateExecution.raiseOpcodeFault loggerFactory baseClassTypes fault thread state
        | UnboxAddress.Address ptr ->
            state
            |> IlMachineState.pushToEvalStack (CliType.RuntimePointer (CliRuntimePointer.Managed ptr)) thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
