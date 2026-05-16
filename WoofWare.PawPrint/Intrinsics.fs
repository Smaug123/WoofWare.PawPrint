namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Intrinsics =
    type IntrinsicMethodKey = IntrinsicMethodKeys.IntrinsicMethodKey

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        IntrinsicMethodKeys.methodKey state methodToCall

    let formatMethodKey (key : IntrinsicMethodKey) : string = IntrinsicMethodKeys.formatMethodKey key

    let isSafeIntrinsic (key : IntrinsicMethodKey) : bool = IntrinsicMethodKeys.isSafeIntrinsic key

    open IntrinsicHelpers

    let call
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (wasConstructing : ManagedHeapAddress option)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let intrinsicKey = methodKey state methodToCall

        // Predicates shared by the Interlocked.CompareExchange / Interlocked.Exchange intrinsic arms,
        // which both dispatch by the (location, value, [comparand]) shape of the overload.
        let isReferenceTypeHandle (handle : ConcreteTypeHandle) : bool =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> false
            | ConcreteTypeHandle.Concrete _ ->
                match IlMachineState.tryGetConcreteTypeInfo state handle with
                | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                | None ->
                    failwith $"Interlocked reference-type intrinsic: concrete type handle %O{handle} has no TypeDef row"

        let isNativeIntPrimitive (primitive : PrimitiveType) : bool =
            match primitive with
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> true
            | _ -> false

        // CIL widens Boolean (1-byte zero-extending) and Char (2-byte zero-extending)
        // to Int32 on the eval stack and `EvalStackValue.toCliTypeCoerced` already
        // rewraps from Int32 back to `CliType.Bool` / `CliType.Char`, so for atomic
        // Exchange / CompareExchange they behave identically to the scalar integers
        // here. Naming the predicate after the eval-stack shape rather than the spec
        // name "integer" keeps its contract truthful for the call sites that justify
        // dispatching to `executeScalarIntegerExchange` / `executeScalarInteger`.
        let isScalarIntegralLikePrimitive (primitive : PrimitiveType) : bool =
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64 -> true
            | _ -> false

        // In general, some implementations are in:
        // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L192
        match methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name with
        | "System.Private.CoreLib", _, "get_IsSupported" when
            scalarOnlyFalseIsSupportedIntrinsics.Contains intrinsicKey.DeclaringTypeFullName
            ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith $"bad signature for %s{formatMethodKey intrinsicKey}"

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool false) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), ".ctor" when
            intrinsicKey.ParameterShapes = [ "*" ; "System.Int32" ]
            && (intrinsicKey.DeclaringTypeFullName = "System.ReadOnlySpan`1"
                || intrinsicKey.DeclaringTypeFullName = "System.Span`1")
            ->
            writePointerBackedSpanConstructor
                loggerFactory
                baseClassTypes
                currentThread
                wasConstructing
                methodToCall
                state
            |> Some
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), "ToString" ->
            spanToString loggerFactory baseClassTypes currentThread methodToCall state
            |> Some
        | "System.Private.CoreLib", "MemoryExtensions", "Equals" ->
            memoryExtensionsEquals baseClassTypes currentThread methodToCall state |> Some
        | "System.Private.CoreLib", "SpanHelpers", "SequenceEqual" when
            isSpanHelpersByteSequenceEqual state methodToCall
            ->
            spanHelpersSequenceEqual baseClassTypes currentThread methodToCall state |> Some
        | "System.Private.CoreLib", ("Vector128" | "Vector256" | "Vector512"), "get_IsHardwareAccelerated"
        | "System.Private.CoreLib", "Vector", "get_IsHardwareAccelerated" when
            // System.Runtime.Intrinsics.Vector{128,256,512}.IsHardwareAccelerated and
            // System.Numerics.Vector.IsHardwareAccelerated are JIT intrinsic capability queries
            // whose IL bodies are recursive self-calls the JIT replaces with a constant. PawPrint
            // models a deterministic virtual CPU profile; the default scalar-only profile reports
            // them unavailable without consulting the host. The fully-qualified-name guard on the
            // "Vector" arm rejects any unrelated CoreLib type that happens to share the short name.
            methodToCall.DeclaringType.Name <> "Vector"
            || intrinsicKey.DeclaringTypeFullName = "System.Numerics.Vector"
            ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ ->
                failwith
                    $"bad signature for System.Private.CoreLib.%s{methodToCall.DeclaringType.Name}.get_IsHardwareAccelerated"

            let isAccelerated =
                vectorAccelerationAvailable methodToCall.DeclaringType.Name state.HardwareIntrinsics

            IlMachineState.pushToEvalStack (CliType.ofBool isAccelerated) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Object", "GetType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [],
              MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "System",
                                                                          "Type",
                                                                          generics)) when generics.IsEmpty -> ()
            | _ -> failwith "bad signature Object.GetType"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let concreteType, state =
                // Normal Object.GetType dispatch arrives here with an ObjectRef. The managed-pointer
                // arms are deliberately defensive for future receiver shapes and direct intrinsic use;
                // constrained.callvirt on value types boxes before dispatching this intrinsic.
                match arg with
                | EvalStackValue.ObjectRef addr -> ManagedHeap.getObjectConcreteType addr state.ManagedHeap, state
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.NullObjectRef ->
                    failwith "TODO: Object.GetType receiver was null; throw NullReferenceException"
                | EvalStackValue.ManagedPointer ptr ->
                    match IlMachineState.readManagedByref baseClassTypes state ptr with
                    | CliType.ObjectRef (Some addr) -> ManagedHeap.getObjectConcreteType addr state.ManagedHeap, state
                    | CliType.ObjectRef None ->
                        failwith "TODO: Object.GetType receiver was null; throw NullReferenceException"
                    | CliType.ValueType valueType -> valueType.Declared, state
                    | other -> failwith $"Object.GetType: expected object ref or value type receiver, got %O{other}"
                | other -> failwith $"Object.GetType: expected object ref or managed pointer receiver, got %O{other}"

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.Closed concreteType)
                    state

            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeTypeAddr)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_TypeHandle" ->
            // TODO: check return type is RuntimeTypeHandle
            match methodToCall.Signature.ParameterTypes with
            | _ :: _ -> failwith "bad signature Type.get_TypeHandle"
            | _ -> ()

            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L470

            // TODO: check return type is RuntimeTypeHandle
            match methodToCall.Signature.ParameterTypes with
            | _ :: _ -> failwith "bad signature Type.get_TypeHandle"
            | _ -> ()

            // no args, returns RuntimeTypeHandle, a struct with a single field (a RuntimeType class)
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L18

            // The thing on top of the stack will be a RuntimeType (an ObjectRef after the
            // primitive-like flatten invariant; primitive-like wrappers never reach the stack
            // as UserDefinedValueType).
            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg : ManagedHeapAddress option =
                match arg with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
                | EvalStackValue.ObjectRef addr -> Some addr
                | s -> failwith $"Type.get_TypeHandle: expected ObjectRef, got %O{s}"

            let state =
                let state, runtimeTypeHandleHandle =
                    DumpedAssembly.typeInfoToTypeDefn'
                        baseClassTypes
                        state._LoadedAssemblies
                        baseClassTypes.RuntimeTypeHandle
                    |> IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty

                let vt =
                    // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L92
                    let mTypeField =
                        FieldIdentity.requiredOwnInstanceField baseClassTypes.RuntimeTypeHandle "m_type"

                    FieldIdentity.cliField
                        runtimeTypeHandleHandle
                        mTypeField
                        (CliType.ObjectRef arg)
                        (AllConcreteTypes.findExistingNonGenericConcreteType
                            state.ConcreteTypes
                            baseClassTypes.RuntimeType.Identity
                         |> Option.get)
                    |> List.singleton
                    |> CliValueType.OfFields
                        baseClassTypes
                        state.ConcreteTypes
                        runtimeTypeHandleHandle
                        Layout.Default
                        (CharSetMetadata.ofTypeAttributes baseClassTypes.RuntimeTypeHandle.TypeAttributes)

                IlMachineState.pushToEvalStack (CliType.ValueType vt) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "RuntimeHelpers", "GetMethodTable" ->
            match methodToCall.Signature.ParameterTypes with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ] -> ()
            | _ -> failwith "bad signature RuntimeHelpers.GetMethodTable"

            match methodToCall.Signature.ReturnType with
            | MethodReturnType.Returns (ConcreteTypeHandle.Pointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                                                      "System.Runtime.CompilerServices",
                                                                                                      "MethodTable",
                                                                                                      generics))) when
                generics.IsEmpty
                ->
                ()
            | _ -> failwith "bad return type RuntimeHelpers.GetMethodTable"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let addr =
                match arg with
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NullReferenceException"
                | other -> failwith $"RuntimeHelpers.GetMethodTable: expected ObjectRef, got %O{other}"

            let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr concreteType))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_IsValueType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Type.get_IsValueType"

            let target, state = popRuntimeTypeHandle baseClassTypes currentThread state

            let isValueType =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    let ty =
                        match state.LoadedAssembly identity.Assembly with
                        | Some assembly -> assembly.TypeDefs.[identity.TypeDefinition.Get]
                        | None ->
                            failwith
                                $"Type.get_IsValueType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"

                    DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // CoreCLR derives IsValueType from generic-parameter constraints:
                    // gpNotNullableValueTypeConstraint => true; gpReferenceTypeConstraint => false;
                    // otherwise consults the parameter's base type, which is the most specific
                    // non-interface class constraint (System.Object if there is none). The flag
                    // cases are exhaustive for unconstrained `T`, `where T : struct`, and
                    // `where T : class`. For any other class constraint (including
                    // `where T : Enum`/`where T : ValueType`, which CoreCLR resolves to true)
                    // we'd need to walk GenericParamMetadata.Constraints and resolve each
                    // constraint type — fail loudly here rather than silently return the wrong
                    // answer.
                    let assembly =
                        state.LoadedAssembly declaringType.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Type.get_IsValueType: assembly for declaring type of generic parameter is not loaded: %s{declaringType.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

                    if position < 0 || position >= typeInfo.Generics.Length then
                        failwith
                            $"Type.get_IsValueType: generic parameter position %d{position} is out of range for %O{declaringType.TypeDefinition.Get} (declares %d{typeInfo.Generics.Length} parameters)"

                    let _, metadata = typeInfo.Generics.[position]

                    match metadata.Constraint with
                    | Some GenericConstraint.NonNullableValue -> true
                    | Some GenericConstraint.Reference -> false
                    | None when metadata.Constraints.IsEmpty -> false
                    | None ->
                        failwith
                            $"TODO: Type.get_IsValueType for generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} with %d{metadata.Constraints.Length} class/interface constraint(s); needs constraint-walk to honour `where T : Enum`/`where T : ValueType`"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: Type.get_IsValueType for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.Closed ty ->
                    match ty with
                    // Byref, pointer, function-pointer, single-dim szarray, and multi-dim array
                    // types are TypeDescs in CoreCLR; IsValueTypeImpl resolves to
                    // IsSubclassOf(typeof(ValueType)) for TypeDescs, which is false for all of
                    // these. They're absent from the nominal AllConcreteTypes mapping, so handle
                    // them explicitly here rather than failing the lookup.
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ -> false
                    | ConcreteTypeHandle.Concrete _ ->
                        let typeInfo =
                            match AllConcreteTypes.lookup ty state.ConcreteTypes with
                            | Some ty -> state.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get]
                            | None ->
                                failwith $"Type.get_IsValueType: expected nominal concrete type handle, got %O{ty}"

                        DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo

            IlMachineState.pushToEvalStack (CliType.ofBool isValueType) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_IsEnum" ->
            // CoreCLR semantics: a type IsEnum iff its immediate parent in the type hierarchy is
            // System.Enum. Enums cannot be generic, so an open generic type definition is never
            // an enum. Structural shapes (byref, pointer, single-dim szarray, multi-dim array)
            // never extend Enum either. CoreCLR additionally has an IsTypeDesc branch that
            // returns IsSubclassOf(Enum) for generic parameters with an Enum-shaped constraint —
            // implement that once constraint metadata reaches reflection paths.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Type.get_IsEnum"

            let target, state = popRuntimeTypeHandle baseClassTypes currentThread state

            let isEnum, state =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> false, state
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // CoreCLR returns IsSubclassOf(Enum) for generic parameters: false unless
                    // a class-style constraint resolves to System.Enum (or a specific enum).
                    // The flag-only cases (struct/class/unconstrained) are exhaustively handled
                    // — anything else needs the constraint walk we haven't yet implemented, so
                    // fail loudly rather than silently returning false for `where T : Enum`.
                    let assembly =
                        state.LoadedAssembly declaringType.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Type.get_IsEnum: assembly for declaring type of generic parameter is not loaded: %s{declaringType.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]

                    if position < 0 || position >= typeInfo.Generics.Length then
                        failwith
                            $"Type.get_IsEnum: generic parameter position %d{position} is out of range for %O{declaringType.TypeDefinition.Get} (declares %d{typeInfo.Generics.Length} parameters)"

                    let _, metadata = typeInfo.Generics.[position]

                    // `where T : unmanaged, Enum` (and similar combinations) sets the
                    // NotNullableValueTypeConstraint flag *and* emits an Enum class-constraint,
                    // and CoreCLR walks the constraints regardless of the flag — so ignoring
                    // Constraints when a flag is set would silently return false for an
                    // enum-shaped parameter. Guard non-empty Constraints uniformly.
                    if not metadata.Constraints.IsEmpty then
                        failwith
                            $"TODO: Type.get_IsEnum for generic parameter #%d{position} of %O{declaringType.TypeDefinition.Get} with %d{metadata.Constraints.Length} class/interface constraint(s); needs constraint-walk to honour `where T : Enum`"

                    false, state
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: Type.get_IsEnum for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ -> false, state
                    | ConcreteTypeHandle.Concrete _ ->
                        let state, baseHandle =
                            IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state handle

                        match baseHandle with
                        | None ->
                            // System.Object has no base type and is not an enum.
                            false, state
                        | Some baseHandle ->
                            let baseIsEnum =
                                match AllConcreteTypes.lookup baseHandle state.ConcreteTypes with
                                | Some baseTy -> baseTy.Identity = baseClassTypes.Enum.Identity
                                | None ->
                                    // Structural handles (byref/pointer/array) are never System.Enum,
                                    // and they're absent from the nominal AllConcreteTypes mapping.
                                    false

                            baseIsEnum, state

            IlMachineState.pushToEvalStack (CliType.ofBool isEnum) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_IsGenericType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Type.get_IsGenericType"

            let target, state = popRuntimeTypeHandle baseClassTypes currentThread state

            let isGenericType =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> true
                // A generic parameter is itself not a generic type — it's a placeholder
                // for one. Type.IsGenericType returns false on it in CoreCLR.
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false
                | RuntimeTypeHandleTarget.Closed ty ->
                    match ty with
                    | ConcreteTypeHandle.Concrete _ ->
                        match AllConcreteTypes.lookup ty state.ConcreteTypes with
                        | Some ty -> not ty.Generics.IsEmpty
                        | None -> failwith $"Type.get_IsGenericType: concrete type handle was not registered: %O{ty}"
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ -> false

            IlMachineState.pushToEvalStack (CliType.ofBool isGenericType) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "AsPointer" ->
            // Method signature: 1 generic parameter, we take a Byref of that parameter, and return a TypeDefn.Pointer(Void)
            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                match arg with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) ->
                    // The placeholder *is* a native-int bit pattern; the original
                    // `Unsafe.AsRef<T>(void* source)` round-trip is documented to
                    // recover `source`. Surface those bits as a verbatim native
                    // pointer so subsequent `conv.u`/`(IntPtr)` casts and direct
                    // comparisons (`p == (void*)1`) see the raw value rather
                    // than a managed-pointer wrapper that no other operand
                    // shape can match.
                    CliRuntimePointer.Verbatim bits
                | EvalStackValue.ManagedPointer ptr -> CliRuntimePointer.Managed ptr
                | x -> failwith $"TODO: Unsafe.AsPointer(%O{x})"

            IlMachineState.pushToEvalStack (CliType.RuntimePointer toPush) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "SkipInit" ->
            // `SkipInit<T>(out T)` is a JIT intrinsic that deliberately leaves
            // the byref target untouched. PawPrint's storage is already
            // deterministic, so the only observable effect is consuming the
            // byref argument and returning void.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.SkipInit"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tParam ], MethodReturnType.Void when tParam = t -> ()
            | _ -> failwith $"bad signature Unsafe.SkipInit: %A{methodToCall.Signature}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            match arg with
            | EvalStackValue.ManagedPointer _ -> ()
            | other -> failwith $"Unsafe.SkipInit: expected managed byref argument, got %O{other}"

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "Unsafe", "AsRef" ->
            // `AsRef<T>(ref readonly T)` and `AsRef<T>(void* source)` are JIT
            // intrinsics. The CoreLib bodies in this runtime throw
            // PlatformNotSupportedException; the intended intrinsic semantics
            // are the address-preserving `ldarg.0; ret`.
            //
            // The `void*` overload is invoked by BCL code like
            // `MemoryMarshal.GetNonNullPinnableReference` which fabricates
            // `Unsafe.AsRef<T>((void*)1)` for empty spans so the subsequent
            // `fixed` pins to a non-null pointer. Translate the native int back
            // through the managed-pointer view, normalising `0L` to `Null` and
            // existing managed-pointer provenance back to its underlying
            // source; raw verbatim bits become a `NativeIntPlaceholder` whose
            // contract is "must never be dereferenced".
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.AsRef"

            let isByrefOverload =
                match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                | [ ConcreteByref tParam ], MethodReturnType.Returns (ConcreteByref tRet) when tParam = t && tRet = t ->
                    true
                | [ ConcretePointer _ ], MethodReturnType.Returns (ConcreteByref tRet) when tRet = t -> false
                | _ -> failwith $"TODO: Unsafe.AsRef unsupported signature %A{methodToCall.Signature.ParameterTypes}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                if isByrefOverload then
                    match arg with
                    | EvalStackValue.ManagedPointer ptr -> EvalStackValue.ManagedPointer ptr
                    | x -> failwith $"TODO: Unsafe.AsRef(ref readonly T) on %O{x}"
                else
                    let placeholderOf (bits : int64) =
                        if bits = 0L then
                            ManagedPointerSource.Null
                        else
                            ManagedPointerSource.NativeIntPlaceholder bits

                    match arg with
                    | EvalStackValue.ManagedPointer ptr -> EvalStackValue.ManagedPointer ptr
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> EvalStackValue.ManagedPointer ptr
                    | EvalStackValue.NativeInt (NativeIntSource.Verbatim bits) ->
                        EvalStackValue.ManagedPointer (placeholderOf bits)
                    | EvalStackValue.Int32 bits -> EvalStackValue.ManagedPointer (placeholderOf (int64 bits))
                    | EvalStackValue.Int64 (Int64Source.Verbatim bits) ->
                        EvalStackValue.ManagedPointer (placeholderOf bits)
                    | EvalStackValue.NullObjectRef -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
                    | x -> failwith $"TODO: Unsafe.AsRef(void*) on %O{x}"

            state
            |> IlMachineState.pushToEvalStack' toPush currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "NullRef" ->
            // CoreCLR's UNSAFE__BYREF_NULLREF intrinsic replaces the CoreLib
            // body with a null managed byref (`ldc.i4.0; conv.u; ret`).
            let t =
                let generics = Seq.toList methodToCall.Generics

                match generics with
                | [ t ] -> t
                | _ -> failwith $"bad generics Unsafe.NullRef: expected exactly one generic argument, got %A{generics}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteByref ret) when ret = t -> ()
            | _ ->
                failwith
                    $"bad signature Unsafe.NullRef: expected no parameters and byref return matching %O{t}, got %A{methodToCall.Signature}"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "IsNullRef" ->
            // The JIT intrinsic compares the byref argument against the null
            // managed byref.
            let t =
                let generics = Seq.toList methodToCall.Generics

                match generics with
                | [ t ] -> t
                | _ ->
                    failwith $"bad generics Unsafe.IsNullRef: expected exactly one generic argument, got %A{generics}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref param ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) when param = t -> ()
            | _ ->
                failwith
                    $"bad signature Unsafe.IsNullRef: expected one byref parameter matching %O{t} and bool return, got %A{methodToCall.Signature}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let isNullRef =
                match arg with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> true
                | EvalStackValue.ManagedPointer _ -> false
                | other -> failwith $"Unsafe.IsNullRef: expected managed byref argument, got %O{other}"

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool isNullRef) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Interlocked", ("Add" | "ExchangeAdd") ->
            // `Add` returns the newly-stored sum; the private `ExchangeAdd`
            // primitive returns the original value. The read-modify-write
            // happens inside one intrinsic dispatch, so the scheduler cannot
            // interleave another guest thread between the read and write.
            let returnsOriginalValue = methodToCall.Name = "ExchangeAdd"

            let executeInt32 (operation : string) (state : IlMachineState) : IlMachineState =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    EvalStackValue.convToInt32 valueArg
                    |> Option.defaultWith (fun () -> failwith $"%s{operation}: expected int32 value, got %O{valueArg}")

                let byrefSrc = popManagedByrefArgument operation byrefArg
                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                let current =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.Int32 i -> i
                    | other -> failwith $"%s{operation}: expected int32 in target location, got %O{other}"

                // From the docs:
                // This method handles an overflow condition by wrapping:
                // if the value at location1 is Int32.MaxValue and value is 1, the result is Int32.MinValue;
                // if value is 2, the result is (Int32.MinValue + 1); and so on.
                // No exception is thrown.
                let updated = uint32<int32> current + uint32<int32> value |> int32<uint32>

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        baseClassTypes
                        state
                        byrefSrc
                        (EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.Int32 updated))

                let result = if returnsOriginalValue then current else updated

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            let executeInt64 (operation : string) (state : IlMachineState) : IlMachineState =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    EvalStackValue.convToInt64 valueArg
                    |> Option.defaultWith (fun () -> failwith $"%s{operation}: expected int64 value, got %O{valueArg}")

                let byrefSrc = popManagedByrefArgument operation byrefArg
                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                let current =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.Int64 i -> i
                    | other -> failwith $"%s{operation}: expected int64 in target location, got %O{other}"

                // From the docs:
                // This method handles an overflow condition by wrapping:
                // if the value at location1 is Int64.MaxValue and value is 1, the result is Int64.MinValue;
                // if value is 2, the result is (Int64.MinValue + 1); and so on.
                // No exception is thrown.
                let updated =
                    match current, value with
                    | Int64Source.Verbatim current, Int64Source.Verbatim value ->
                        uint64<int64> current + uint64<int64> value
                        |> int64<uint64>
                        |> Int64Source.Verbatim
                    | _, _ -> failwith "TODO"

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        baseClassTypes
                        state
                        byrefSrc
                        (EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.Int64 updated))

                let result = if returnsOriginalValue then current else updated

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcreteInt32 state.ConcreteTypes) ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes)
            | [ ConcreteByref (ConcreteUInt32 state.ConcreteTypes) ; ConcreteUInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteUInt32 state.ConcreteTypes) ->
                executeInt32 methodToCall.Name state |> Some
            | [ ConcreteByref (ConcreteInt64 state.ConcreteTypes) ; ConcreteInt64 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes)
            | [ ConcreteByref (ConcreteUInt64 state.ConcreteTypes) ; ConcreteUInt64 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteUInt64 state.ConcreteTypes) ->
                executeInt64 methodToCall.Name state |> Some
            | _ -> None

        | "System.Private.CoreLib", "Interlocked", "CompareExchange" ->
            // The native-int-shaped overloads need their own path: the shipped IL wrappers do
            // `Unsafe.As<_, long>` and delegate to the Int64 overload, which would destroy our
            // NativeIntSource provenance.
            // Narrow scalar and reference-type overloads are JIT intrinsic boundaries too; handle
            // those primitives here instead of executing their Unsafe.As / InternalCall wrappers.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L452
            let executeScalarInteger (operation : string) (state : IlMachineState) : IlMachineState =
                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc = popManagedByrefArgument operation byrefArg
                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc
                let currentEval = EvalStackValue.ofCliType currentValue
                let valueCli = EvalStackValue.toCliTypeCoerced currentValue value
                let comparandCli = EvalStackValue.toCliTypeCoerced currentValue comparand

                // The intrinsic bypasses normal method-frame construction, so coerce the eval-stack
                // operands to the signedness/width of the overload before comparing and writing.
                let state =
                    if EvalStackValueComparisons.ceq currentEval (EvalStackValue.ofCliType comparandCli) then
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli
                    else
                        state

                state
                |> IlMachineState.pushToEvalStack currentValue currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive
                ConcretePrimitive state.ConcreteTypes comparandPrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isNativeIntPrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = comparandPrimitive
                && locationPrimitive = returnPrimitive
                ->

                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc =
                    popManagedByrefArgument "Interlocked.CompareExchange(ref native-int,...)" byrefArg

                // Eval-stack IntPtr/UIntPtr arguments are flattened to the primitive by the push
                // boundary (see EvalStackValue.ofCliType), so a UserDefinedValueType IntPtr or
                // UIntPtr is unreachable here by invariant.
                let toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                    match v with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                    | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref native-int,...): unexpected native-int-shaped eval stack value %O{other}"

                let comparandSrc = toNativeIntSource comparand
                let valueSrc = toNativeIntSource value

                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                // `ref IntPtr` / `ref UIntPtr` derefs to a wrapper struct. Route the read/write through
                // the eval-stack flatten/rewrap boundary: `ofCliType` peels the primitive-like
                // wrapper to `NativeInt`, and `toCliTypeCoerced` reconstructs the wrapper shape
                // on write. The primitive-like registry is the single source of truth for shape.
                let currentSrc =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref native-int,...): expected NativeInt at byref target, got %O{other}"

                // Two representations of zero exist (`Verbatim 0L` for constructed zero native
                // ints and `ManagedPointer Null` for default-initialised IntPtr/UIntPtr); treat
                // them as equal, matching native-int `ceq` semantics.
                let nativeIntEq (a : NativeIntSource) (b : NativeIntSource) : bool =
                    EvalStackValueComparisons.ceq (EvalStackValue.NativeInt a) (EvalStackValue.NativeInt b)

                let state =
                    if nativeIntEq currentSrc comparandSrc then
                        let newValue =
                            EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.NativeInt valueSrc)

                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc newValue
                    else
                        state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt currentSrc) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive
                ConcretePrimitive state.ConcreteTypes comparandPrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isScalarIntegralLikePrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = comparandPrimitive
                && locationPrimitive = returnPrimitive
                ->
                executeScalarInteger "Interlocked.CompareExchange" state |> Some
            | [ ConcreteByref locationType ; valueType ; comparandType ], MethodReturnType.Returns returnType when
                locationType = valueType
                && locationType = comparandType
                && locationType = returnType
                && isReferenceTypeHandle locationType
                ->
                // Reference-typed CompareExchange overloads are JIT/runtime intrinsic boundaries
                // in CoreLib. Implement the object-reference primitive directly instead of trying
                // to execute the generic Unsafe.As<T, object> path or the non-generic
                // CompareExchangeObject InternalCall boundary.
                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc = popManagedByrefArgument "Interlocked.CompareExchange<T>" byrefArg

                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                let objectTarget (argName : string) (value : CliType) : ManagedHeapAddress option =
                    match value with
                    | CliType.ObjectRef target -> target
                    | other ->
                        failwith $"Interlocked.CompareExchange<T>: expected reference-type %s{argName}, got %O{other}"

                let currentTarget = objectTarget "location" currentValue

                let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                let comparandCli = EvalStackValue.toCliTypeCoerced currentValue comparand

                let comparandTarget = objectTarget "comparand" comparandCli

                let state =
                    if currentTarget = comparandTarget then
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli
                    else
                        state

                state
                |> IlMachineState.pushToEvalStack currentValue currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ ->
                // The float/double overloads are not yet intrinsified. Their shipped IL bodies
                // reinterpret-cast to integer overloads, so falling through would either re-enter
                // this intrinsic path or lose the bit-level shape of the floating-point value.
                // When a caller needs one of these, add a dedicated intrinsic arm.
                None
        | "System.Private.CoreLib", "Interlocked", "Exchange" ->
            // Same intrinsic-boundary motivation as CompareExchange: the shipped CoreLib
            // bodies for Exchange ride Unsafe.As / InternalCall paths that would either
            // destroy NativeIntSource provenance for IntPtr/UIntPtr or re-enter this
            // intrinsic at the wrong width. Implement the primitive directly.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L80
            let executeScalarIntegerExchange (operation : string) (state : IlMachineState) : IlMachineState =
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc = popManagedByrefArgument operation byrefArg
                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc
                let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                // The intrinsic bypasses normal method-frame construction, so coerce the
                // eval-stack value to the signedness/width of the overload before writing.
                let state =
                    IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli

                state
                |> IlMachineState.pushToEvalStack currentValue currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isNativeIntPrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = returnPrimitive
                ->

                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc =
                    popManagedByrefArgument "Interlocked.Exchange(ref native-int,...)" byrefArg

                // Eval-stack IntPtr/UIntPtr arguments are flattened to the primitive by the push
                // boundary (see EvalStackValue.ofCliType), so a UserDefinedValueType IntPtr or
                // UIntPtr is unreachable here by invariant.
                let toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                    match v with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                    | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                    | other ->
                        failwith
                            $"Interlocked.Exchange(ref native-int,...): unexpected native-int-shaped eval stack value %O{other}"

                let valueSrc = toNativeIntSource value

                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                // `ref IntPtr` / `ref UIntPtr` derefs to a wrapper struct. Route the read/write through
                // the eval-stack flatten/rewrap boundary: `ofCliType` peels the primitive-like
                // wrapper to `NativeInt`, and `toCliTypeCoerced` reconstructs the wrapper shape
                // on write. The primitive-like registry is the single source of truth for shape.
                let currentSrc =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | other ->
                        failwith
                            $"Interlocked.Exchange(ref native-int,...): expected NativeInt at byref target, got %O{other}"

                let newValue =
                    EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.NativeInt valueSrc)

                let state =
                    IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc newValue

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt currentSrc) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isScalarIntegralLikePrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = returnPrimitive
                ->
                executeScalarIntegerExchange "Interlocked.Exchange" state |> Some
            | [ ConcreteByref locationType ; valueType ], MethodReturnType.Returns returnType when
                locationType = valueType
                && locationType = returnType
                && isReferenceTypeHandle locationType
                ->
                // Reference-typed Exchange overloads are JIT/runtime intrinsic boundaries
                // in CoreLib. Implement the object-reference primitive directly instead of
                // trying to execute the generic Unsafe.As<T, object> path.
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc = popManagedByrefArgument "Interlocked.Exchange<T>" byrefArg

                let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                let state =
                    IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli

                state
                |> IlMachineState.pushToEvalStack currentValue currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ ->
                // The float/double overloads are not yet intrinsified, matching the
                // CompareExchange precedent above. Add a dedicated arm when first needed.
                None
        | "System.Private.CoreLib", "Thread", "FastPollGC" ->
            // [Intrinsic] internal static void Thread.FastPollGC() => Thread.FastPollGC();
            // The managed IL body is an infinite self-recursive call; the JIT replaces
            // every call site with an inline fast GC poll. PawPrint has no GC, so the
            // intrinsic is a pure no-op. This cannot live in safeIntrinsics because
            // executing the IL would loop forever.
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L389-L392
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"Thread.FastPollGC: unexpected signature %A{methodToCall.Signature}"

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "Volatile", ("ReadBarrier" | "WriteBarrier") ->
            // [Intrinsic] public static void Volatile.{Read,Write}Barrier() => Volatile.{Read,Write}Barrier();
            // Same shape as Thread.FastPollGC: the managed body is infinite self-recursion
            // and the JIT replaces the call with the appropriate processor fence. PawPrint
            // does not model memory-ordering effects across threads, and even if it did the
            // single-stepping interpreter has no instruction reordering to fence against,
            // so the no-op is correct. Cannot live in safeIntrinsics because the IL would
            // loop forever.
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/Threading/Volatile.cs#L236-L245
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"Volatile.%s{methodToCall.Name}: unexpected signature %A{methodToCall.Signature}"

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "BitConverter", "SingleToInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.SingleToInt32Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.SingleToInt32Bits (float32<float> f) |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int32BitsToSingle" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteSingle state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.Int64BitsToSingle"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int32BitsToSingle arg |> CliNumericType.Float32 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "DoubleToUInt64Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteUInt64 state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Float i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.DoubleToUInt64Bits arg
                |> int64<uint64>
                |> Int64Source.Verbatim
                |> CliNumericType.Int64
                |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "UInt64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 (Int64Source.Verbatim i) -> uint64<int64> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.UInt64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.Int64BitsToDouble"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 (Int64Source.Verbatim i) -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "DoubleToInt64Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.DoubleToInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f ->
                    BitConverter.DoubleToInt64Bits f |> Int64Source.Verbatim |> EvalStackValue.Int64
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "SingleToUInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], MethodReturnType.Returns (ConcreteUInt32 state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.SingleToUInt32Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f ->
                    BitConverter.SingleToUInt32Bits (float32<float> f)
                    |> int<uint32>
                    |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "UInt32BitsToSingle" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteSingle state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.UInt32BitsToSingle"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Int32 f ->
                    BitConverter.UInt32BitsToSingle (uint32<int> f)
                    |> float<float32>
                    |> EvalStackValue.Float
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitOperations", "Log2" ->
            // BitOperations.Log2 is a JIT intrinsic in the real CLR. The BCL IL body falls
            // through to a software fallback that reads from a De Bruijn lookup table backed
            // by a PE byte range, which collides with paths PawPrint does not yet model.
            // Model the boundary directly instead: delegate to the host BCL, which honours
            // the documented `Log2(0) = 0` contract.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    match arg with
                    | EvalStackValue.Int32 i -> uint32<int> i
                    | _ -> failwith $"BitOperations.Log2(uint): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | [ ConcreteUInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    match arg with
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> uint64<int64> i
                    | _ -> failwith $"BitOperations.Log2(ulong): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | [ ConcreteUIntPtr state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value : unativeint =
                    match arg with
                    | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> unativeint<int64> i
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> 0un
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> unativeint<int64> i
                    | EvalStackValue.Int32 i -> unativeint<int> i
                    | _ -> failwith $"BitOperations.Log2(nuint): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ -> failwith $"BitOperations.Log2: unexpected signature %s{formatMethodKey intrinsicKey}"
        | "System.Private.CoreLib", "String", "Equals" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteString state.ConcreteTypes ; ConcreteString state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) ->
                let arg1, state = IlMachineState.popEvalStack currentThread state

                let arg1 =
                    match arg1 with
                    | EvalStackValue.ObjectRef h -> Some h
                    | EvalStackValue.NullObjectRef -> None
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg1}"
                    | _ -> failwith $"TODO: %O{arg1}"

                let arg2, state = IlMachineState.popEvalStack currentThread state

                let arg2 =
                    match arg2 with
                    | EvalStackValue.ObjectRef h -> Some h
                    | EvalStackValue.NullObjectRef -> None
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg2}"
                    | _ -> failwith $"TODO: %O{arg2}"

                let areEqual =
                    match arg1, arg2 with
                    | None, None -> true
                    | Some _, None
                    | None, Some _ -> false
                    | Some arg1, Some arg2 -> ManagedHeap.stringsEqual arg1 arg2 state.ManagedHeap

                state
                |> IlMachineState.pushToEvalStack (CliType.ofBool areEqual) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ -> None
        | "System.Private.CoreLib", "Unsafe", "ReadUnaligned" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L558
            // Semantically this returns the T that would be read by
            // reinterpreting the pointer as `ref T` and dereferencing. The JIT
            // lowers it to `Unsafe.As<byte, T>(ref source)` + deref. Our heap
            // stores typed cells rather than raw bytes, so we model the read
            // by delegating the bytewise gather/reconstruction to managed
            // byref byte helpers.
            //
            // Two overloads exist: `ReadUnaligned<T>(ref byte source)` and
            // `ReadUnaligned<T>(void* source)`. PawPrint handles the pointer
            // overload only when the pointer has managed provenance, for
            // example a PE byte-range pointer produced by `ldsflda`.
            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.ReadUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t

                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> src
                    | EvalStackValue.NullObjectRef -> failwith "TODO: Unsafe.ReadUnaligned on null should throw NRE"
                    | _ -> failwith $"TODO: Unsafe.ReadUnaligned: expected ManagedPointer, got %O{ptr}"

                let v = IlMachineState.readManagedByrefBytesAs baseClassTypes state src tZero

                let state =
                    state
                    |> IlMachineState.pushToEvalStack v currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                Some state
            | [ ConcretePointer _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.ReadUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t

                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src = managedPointerOfPointerArgument "Unsafe.ReadUnaligned(void*)" ptr

                let v = IlMachineState.readManagedByrefBytesAs baseClassTypes state src tZero

                let state =
                    state
                    |> IlMachineState.pushToEvalStack v currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                Some state
            | _ -> None
        | "System.Private.CoreLib", "Unsafe", "WriteUnaligned" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L609
            // Symmetric to ReadUnaligned: writes a T through a byte-level
            // byref by delegating byte scattering to managed byref byte helpers.
            //
            // The `(void*, T)` overload is handled only for pointers with
            // managed provenance, symmetric with `ReadUnaligned`.
            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ; _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.WriteUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                let tSize = CliType.sizeOf tZero

                // Stack order: the ref byte goes on first (arg0), the value on
                // top (arg1). Pop value first.
                let value, state = IlMachineState.popEvalStack currentThread state
                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> src
                    | EvalStackValue.NullObjectRef -> failwith "TODO: Unsafe.WriteUnaligned on null should throw NRE"
                    | _ -> failwith $"TODO: Unsafe.WriteUnaligned: expected ManagedPointer, got %O{ptr}"

                // Coerce the stack value to a CliType shaped like T: sub-int
                // primitives arrive as Int32 and must narrow back to their
                // CliType flavour before the byte helpers write it.
                let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value

                let valueSize = CliType.sizeOf valueAsCli

                if valueSize <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned: coerced value has size %d{valueSize}, expected %d{tSize} for %O{valueAsCli}"

                let state =
                    IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state src valueAsCli

                let state = state |> IlMachineState.advanceProgramCounter currentThread
                Some state
            | [ ConcretePointer _ ; _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.WriteUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                let tSize = CliType.sizeOf tZero

                // Stack order: the pointer goes on first (arg0), the value on
                // top (arg1). Pop value first.
                let value, state = IlMachineState.popEvalStack currentThread state
                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src = managedPointerOfPointerArgument "Unsafe.WriteUnaligned(void*)" ptr

                let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value

                let valueSize = CliType.sizeOf valueAsCli

                if valueSize <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned(void*): coerced value has size %d{valueSize}, expected %d{tSize} for %O{valueAsCli}"

                let state =
                    IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state src valueAsCli

                let state = state |> IlMachineState.advanceProgramCounter currentThread
                Some state
            | _ -> None
        | "System.Private.CoreLib", "Unsafe", ("CopyBlock" | "CopyBlockUnaligned") ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L313
            // The CoreLib bodies throw PlatformNotSupportedException; the real JIT replaces
            // these with `cpblk` (optionally prefixed by `unaligned.`). Both overloads accept
            // the byref and pointer forms uniformly via managedPointerOfPointerArgument.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteUInt32 state.ConcreteTypes ],
              MethodReturnType.Void
            | [ ConcretePointer _ ; ConcretePointer _ ; ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Void ->
                let operation = $"Unsafe.%s{methodToCall.Name}"
                executeUnsafeCopyBlock baseClassTypes currentThread operation state |> Some
            | _ -> None
        | "System.Private.CoreLib", "String", "op_Implicit" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ par ], MethodReturnType.Returns ret ->
                let par = state.ConcreteTypes |> AllConcreteTypes.lookup par |> Option.get
                let ret = state.ConcreteTypes |> AllConcreteTypes.lookup ret |> Option.get

                if
                    par.Namespace = "System"
                    && par.Name = "String"
                    && ret.Namespace = "System"
                    && ret.Name = "ReadOnlySpan`1"
                then
                    match ret.Generics |> Seq.toList with
                    | [ gen ] ->
                        let gen = state.ConcreteTypes |> AllConcreteTypes.lookup gen |> Option.get

                        if gen.Namespace = "System" && gen.Name = "Char" then
                            // This is just an optimisation
                            // https://github.com/dotnet/runtime/blob/ab105b51f8b50ec5567d7cfe9001ca54dd6f64c3/src/libraries/System.Private.CoreLib/src/System/String.cs#L363-L366
                            None
                        else
                            failwith "TODO: unexpected params to String.op_Implicit"
                    | _ -> failwith "TODO: unexpected params to String.op_Implicit"
                else
                    failwith "TODO: unexpected params to String.op_Implicit"
            | _ -> failwith "TODO: unexpected params to String.op_Implicit"
        | "System.Private.CoreLib", "RuntimeHelpers", "IsReferenceOrContainsReferences" ->
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs#L207
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.IsReferenceOrContainsReference"

            let arg = Seq.exactlyOne methodToCall.Generics

            let state, result =
                // Some types appear circular, because they're hardcoded in the runtime. We have to special-case them.
                match arg with
                | ConcreteChar state.ConcreteTypes -> state, false
                | _ ->

                let generic = AllConcreteTypes.lookup arg state.ConcreteTypes

                let generic =
                    match generic with
                    | None -> failwith "somehow have not already concretised type in IsReferenceOrContainsReferences"
                    | Some generic -> generic

                let td =
                    state.LoadedAssembly generic.Assembly
                    |> Option.get
                    |> fun a -> a.TypeDefs.[generic.Definition.Get]

                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies td then
                    td
                    |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
                    |> typeInfoContainsReferences loggerFactory baseClassTypes state
                else
                    state, true

            let state =
                state
                |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "RuntimeHelpers", "InitializeArray" ->
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs#L18
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSystemArray state.ConcreteTypes ; ConcreteRuntimeFieldHandle state.ConcreteTypes ],
              MethodReturnType.Void -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.InitializeArray"

            // Pop args: arg1 (RuntimeFieldHandle) is on top, then arg0 (array ref)
            let fldHandle, state = IlMachineState.popEvalStack currentThread state
            let arrayRef, state = IlMachineState.popEvalStack currentThread state

            // Extract the array address
            let arrayAddr : ManagedHeapAddress =
                match arrayRef with
                | EvalStackValue.NullObjectRef ->
                    failwith "TODO: throw NullReferenceException for InitializeArray on null array"
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"InitializeArray: expected array object ref, got %O{other}"

            // RuntimeFieldHandle is primitive-like (FlattenToObjectRef): its single `m_ptr`
            // (an IRuntimeFieldInfo ref) arrives on the stack flattened to an ObjectRef,
            // including after box/unbox round-trips (Unbox_Any flattens primitive-like types).
            // The referenced object can be either a RuntimeFieldInfoStub (the form that
            // FieldHandleRegistry.getOrAllocate produces for ldtoken) or an RtFieldInfo
            // (the form reflection's RuntimeTypeHandle.GetFields populates from the IntPtr
            // ids returned by that QCall, https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/coreclr/System.Private.CoreLib/src/System/Reflection/RtFieldInfo.cs ).
            let runtimeFieldInfoAddr : ManagedHeapAddress =
                match fldHandle with
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.NullObjectRef ->
                    failwith "TODO: throw ArgumentException for InitializeArray with null field handle"
                | other -> failwith $"InitializeArray: expected RuntimeFieldHandle ObjectRef, got %O{other}"

            // The address-keyed registry index is populated when PawPrint allocates a
            // RuntimeFieldInfoStub. Reflection-produced RtFieldInfo objects are not in that
            // index — they are constructed in managed code from the IntPtr field ids that
            // RuntimeTypeHandle.GetFields returned, so we recover the FieldHandle by reading
            // the heap object's `m_fieldHandle` slot and resolving it against the id-keyed
            // index. Both RuntimeFieldInfoStub and RtFieldInfo declare a field with that name.
            let fieldHandle : FieldHandle =
                match FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoAddr state.FieldHandles with
                | Some fh -> fh
                | None ->

                let heapObj = ManagedHeap.get runtimeFieldInfoAddr state.ManagedHeap

                let typeInfo =
                    match IlMachineState.tryGetConcreteTypeInfo state heapObj.ConcreteType with
                    | Some (_, typeInfo) -> typeInfo
                    | None ->
                        failwith
                            $"InitializeArray: object at %O{runtimeFieldInfoAddr} has concrete type %O{heapObj.ConcreteType} with no TypeDef row"

                let fieldHandleField =
                    typeInfo.Fields
                    |> List.tryFind (fun field -> field.Name = "m_fieldHandle" && not field.IsStatic)
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"InitializeArray: object at %O{runtimeFieldInfoAddr} (type %s{typeInfo.Namespace}.%s{typeInfo.Name}) is not in the field handle registry and has no instance field 'm_fieldHandle' to recover the field id from"
                    )

                let fieldHandleId =
                    let fieldId = FieldIdentity.fieldId heapObj.ConcreteType fieldHandleField

                    match
                        AllocatedNonArrayObject.DereferenceFieldById fieldId heapObj
                        |> CliType.unwrapPrimitiveLikeDeep
                    with
                    | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> id
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)) -> id
                    | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) ->
                        failwith
                            "TODO: throw ArgumentException for InitializeArray with null field handle (m_fieldHandle was zero)"
                    | other ->
                        failwith
                            $"InitializeArray: m_fieldHandle on %s{typeInfo.Namespace}.%s{typeInfo.Name} did not contain a field-registry handle, got %O{other}"

                match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
                | Some fh -> fh
                | None ->
                    failwith
                        $"InitializeArray: m_fieldHandle id %d{fieldHandleId} on object at %O{runtimeFieldInfoAddr} (type %s{typeInfo.Namespace}.%s{typeInfo.Name}) was not present in the field handle registry"

            // Get the assembly and field definition
            let assemblyFullName = fieldHandle.GetAssemblyFullName ()
            let fieldDefHandle = fieldHandle.GetFieldDefinitionHandle().Get

            let assembly : DumpedAssembly =
                match state.LoadedAssembly' assemblyFullName with
                | Some a -> a
                | None -> failwith $"InitializeArray: assembly %s{assemblyFullName} not loaded"

            let fieldInfo = assembly.Fields.[fieldDefHandle]

            let rva : int =
                match fieldInfo.RelativeVirtualAddress with
                | Some rva -> rva
                | None -> failwith $"InitializeArray: field %s{fieldInfo.Name} has no RVA"

            // Read the raw bytes from the PE image
            let sectionData = assembly.PeReader.GetSectionData rva

            // Get the array and decode elements from the raw bytes
            let arr = state.ManagedHeap.Arrays.[arrayAddr]

            let state =
                if arr.Length = 0 then
                    state
                else
                    let reader = sectionData.GetReader ()
                    // Decode each element from raw bytes based on its current CliType
                    let firstElement = arr.Elements.[0]

                    let state =
                        (state, seq { 0 .. arr.Length - 1 })
                        ||> Seq.fold (fun (state : IlMachineState) (i : int) ->
                            let decoded : CliType =
                                match firstElement with
                                | CliType.Numeric (CliNumericType.Int8 _) ->
                                    CliType.Numeric (CliNumericType.Int8 (reader.ReadSByte ()))
                                | CliType.Numeric (CliNumericType.UInt8 _) ->
                                    CliType.Numeric (CliNumericType.UInt8 (reader.ReadByte ()))
                                | CliType.Numeric (CliNumericType.Int16 _) ->
                                    CliType.Numeric (CliNumericType.Int16 (reader.ReadInt16 ()))
                                | CliType.Numeric (CliNumericType.UInt16 _) ->
                                    CliType.Numeric (CliNumericType.UInt16 (reader.ReadUInt16 ()))
                                | CliType.Numeric (CliNumericType.Int32 _) ->
                                    CliType.Numeric (CliNumericType.Int32 (reader.ReadInt32 ()))
                                | CliType.Numeric (CliNumericType.Int64 _) ->
                                    CliType.Numeric (
                                        CliNumericType.Int64 (reader.ReadInt64 () |> Int64Source.Verbatim)
                                    )
                                | CliType.Numeric (CliNumericType.Float32 _) ->
                                    CliType.Numeric (CliNumericType.Float32 (reader.ReadSingle ()))
                                | CliType.Numeric (CliNumericType.Float64 _) ->
                                    CliType.Numeric (CliNumericType.Float64 (reader.ReadDouble ()))
                                | CliType.Bool _ -> CliType.Bool (reader.ReadByte ())
                                | CliType.Char _ ->
                                    let lo = reader.ReadByte ()
                                    let hi = reader.ReadByte ()
                                    CliType.Char (hi, lo)
                                | other ->
                                    failwith
                                        $"InitializeArray: unsupported array element type for RVA initialization: %O{other}"

                            IlMachineState.setArrayValue arrayAddr decoded i state
                        )

                    state

            let state = state |> IlMachineState.advanceProgramCounter currentThread
            Some state
        | "System.Private.CoreLib", "RuntimeHelpers", "IsBitwiseEquatable" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.IsBitwiseEquatable"

            let ty = Seq.exactlyOne methodToCall.Generics

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes ty

            let result =
                match CliType.unwrapPrimitiveLikeDeep zero with
                | CliType.Numeric numeric ->
                    match numeric with
                    | CliNumericType.Float32 _
                    | CliNumericType.Float64 _
                    | CliNumericType.NativeFloat _ -> false
                    | CliNumericType.Int32 _
                    | CliNumericType.Int64 _
                    | CliNumericType.Int8 _
                    | CliNumericType.Int16 _
                    | CliNumericType.UInt8 _
                    | CliNumericType.UInt16 _
                    | CliNumericType.NativeInt _ -> true
                | CliType.Bool _
                | CliType.Char _ -> true
                // Returning false is semantically safe: it only disables the BCL's bitwise
                // equality fast path. In PawPrint today that may still be observable for user
                // structs because the fallback SpanHelpers.SequenceEqual<T> path is not implemented.
                // TODO: Return true for eligible value types after implementing the same
                // override, field-recursion, and IEquatable<T> checks as the MethodTable QCall.
                | CliType.ValueType _
                | CliType.ObjectRef _
                | CliType.RuntimePointer _ -> false

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "GC", "KeepAlive" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ], MethodReturnType.Void -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.GC.KeepAlive"

            let _, state = IlMachineState.popEvalStack currentThread state

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "Unsafe", "As" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L64
            let byrefAs () =
                let inputType, retType =
                    match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                    | [ input ], MethodReturnType.Returns ret -> input, ret
                    | _ -> failwith "bad signature Unsafe.As"

                let from, to_ =
                    match Seq.toList methodToCall.Generics with
                    | [ from ; to_ ] -> from, to_
                    | _ -> failwith "bad generics"

                if ConcreteTypeHandle.Byref to_ <> retType then
                    failwith "bad return type"

                if ConcreteTypeHandle.Byref from <> inputType then
                    failwith "bad input type"

                let from =
                    match AllConcreteTypes.lookup from state.ConcreteTypes with
                    | None -> failwith "somehow have not concretised input type"
                    | Some t -> t

                let to_ =
                    match AllConcreteTypes.lookup to_ state.ConcreteTypes with
                    | None -> failwith "somehow have not concretised ret type"
                    | Some t -> t

                let inputAddr, state = IlMachineState.popEvalStack currentThread state

                let ptr =
                    match inputAddr with
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith "expected pointer type"
                    | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                    | EvalStackValue.NullObjectRef -> failwith "todo: Unsafe.As on null"
                    | EvalStackValue.ManagedPointer src when from = to_ ->
                        // Unsafe.As<T,T> is a no-op: same address and same type view.
                        // Skipping the projection keeps the representation canonical so
                        // that AreSame / ceq on the result compares equal to the input.
                        EvalStackValue.ManagedPointer src
                    | EvalStackValue.ManagedPointer src ->
                        ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs to_) src
                        |> EvalStackValue.ManagedPointer
                    | EvalStackValue.ObjectRef addr -> failwith "todo: Unsafe.As on ObjectRef"
                    | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"

                let state =
                    state
                    |> IlMachineState.pushToEvalStack' ptr currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                Some state

            match methodToCall.Signature.ParameterTypes, Seq.toList methodToCall.Generics with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ], [ target ] ->
                if methodToCall.Signature.ReturnType <> MethodReturnType.Returns target then
                    failwith "bad return type Unsafe.As<T>(object)"

                let obj, state = IlMachineState.popEvalStack currentThread state

                match obj with
                | EvalStackValue.ObjectRef _
                | EvalStackValue.NullObjectRef ->
                    state
                    |> IlMachineState.pushToEvalStack' obj currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> Some
                | other -> failwith $"Unsafe.As<T>(object): expected object reference, got %O{other}"
            | _ -> byrefAs ()
        | "System.Private.CoreLib", "Unsafe", "BitCast" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L259
            // BCL body:
            //   if (sizeof(TFrom) != sizeof(TTo)
            //       || !typeof(TFrom).IsValueType
            //       || !typeof(TTo).IsValueType)
            //       ThrowHelper.ThrowNotSupportedException();
            //   return ReadUnaligned<TTo>(ref As<TFrom, byte>(ref source));
            //
            // PawPrint models this as a primitive byte reinterpretation between
            // two byte-addressable storage shapes. We are stricter than the BCL:
            // a value type carrying provenance the byte model cannot render
            // (managed pointers, runtime/method/field handles, GC handles, ...)
            // is rejected via `CliType.ByteAddressability`. The BCL would happily
            // produce undefined garbage in those cases; refusing is consistent
            // with PawPrint's deterministic byte model and with the user-facing
            // contract "between equal-sized unmanaged storage shapes".
            let fromHandle, toHandle =
                match Seq.toList methodToCall.Generics with
                | [ f ; t ] -> f, t
                | _ -> failwith "bad generics Unsafe.BitCast: expected exactly two type arguments"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ paramTy ], MethodReturnType.Returns retTy when paramTy = fromHandle && retTy = toHandle -> ()
            | _ -> failwith $"bad signature Unsafe.BitCast: %A{methodToCall.Signature}"

            let fromZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes fromHandle

            let toZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes toHandle

            let fromSize = CliType.sizeOf fromZero
            let toSize = CliType.sizeOf toZero

            let popped, state = IlMachineState.popEvalStack currentThread state
            let inputCli = EvalStackValue.toCliTypeCoerced fromZero popped

            let inputAddressable =
                match CliType.ByteAddressability inputCli with
                | CliByteAddressability.ByteAddressable -> true
                | CliByteAddressability.Rejected _ -> false

            let targetAddressable =
                match CliType.ByteAddressability toZero with
                | CliByteAddressability.ByteAddressable -> true
                | CliByteAddressability.Rejected _ -> false

            if fromSize <> toSize || not inputAddressable || not targetAddressable then
                // The BCL throws `NotSupportedException` for these cases. Raising guest exceptions
                // from intrinsic dispatch is not yet wired (Intrinsics.fs compiles before
                // IlMachineStateExecution.fs, so `raiseRuntimeException` is not in scope here).
                // Host-fail for now with a precise diagnostic; mirrors the existing
                // `Unsafe.ReadUnaligned` null-target TODO above.
                let reason =
                    if fromSize <> toSize then
                        $"size mismatch (TFrom = %d{fromSize} bytes, TTo = %d{toSize} bytes)"
                    elif not inputAddressable then
                        $"input is not byte-addressable: %s{(CliType.ByteAddressability inputCli).Description}"
                    else
                        $"target is not byte-addressable: %s{(CliType.ByteAddressability toZero).Description}"

                failwith
                    $"TODO: Unsafe.BitCast<%O{fromHandle}, %O{toHandle}> should throw NotSupportedException (%s{reason})"
            else
                let bytes = CliType.ToBytes inputCli
                let result = CliType.OfBytesLike toZero bytes

                state
                |> IlMachineState.pushToEvalStack result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
        | "System.Private.CoreLib", "Unsafe", "SizeOf" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L51
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Unsafe.SizeOf"

            let ty =
                match Seq.toList methodToCall.Generics with
                | [ ty ] -> ty
                | _ -> failwith "bad generics"

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes ty

            let size = CliType.sizeOf zero

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "AreSame" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L55
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with ceq on two byrefs.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref _ ; ConcreteByref _ ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Unsafe.AreSame"

            let right, state = IlMachineState.popEvalStack currentThread state
            let left, state = IlMachineState.popEvalStack currentThread state

            let extractPtr (v : EvalStackValue) : ManagedPointerSource =
                match v with
                | EvalStackValue.ManagedPointer p -> p
                | _ -> failwith $"TODO: Unsafe.AreSame: expected ManagedPointer, got %O{v}"

            // `ReinterpretAs` projections are address-preserving, so two byrefs
            // that reach the same byte location by different reinterpret chains
            // must compare equal. Strip trailing reinterprets before comparison.
            // A `ReinterpretAs` followed by a `Field` would need a bytewise
            // layout comparison (a field at the same offset under different
            // type views still aliases); refuse rather than risk a silent false
            // negative.
            let leftPtr = extractPtr left
            let rightPtr = extractPtr right

            let normalisation =
                ManagedPointerByteView.normalisationContextForPointers baseClassTypes state [ leftPtr ; rightPtr ]

            let leftNormalised =
                ManagedPointerSource.normaliseForComparison normalisation leftPtr

            let rightNormalised =
                ManagedPointerSource.normaliseForComparison normalisation rightPtr

            if
                ManagedPointerSource.hasNonTrailingReinterpret leftNormalised
                || ManagedPointerSource.hasNonTrailingReinterpret rightNormalised
            then
                failwith
                    $"TODO: Unsafe.AreSame on byref with `ReinterpretAs` followed by `Field` needs a bytewise layout comparison; got %O{leftPtr} vs %O{rightPtr}"

            let strip = ManagedPointerSource.stripTrailingReinterprets
            let areSame = strip leftNormalised = strip rightNormalised

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool areSame) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "Add" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L99
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with sizeof + conv.i + mul + add.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.Add"

            // Three overloads: `(ref T, int32)`, `(ref T, IntPtr)`, `(ref T, UIntPtr)`.
            // The IntPtr/UIntPtr overloads exist for native-sized element indices
            // (e.g. `Unsafe.Add(ref T, (nint)n)`). All three are JIT-lowered to
            // `sizeof * offset + base`, so we treat them uniformly.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tFromParam ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
            | _ ->
                failwith
                    $"TODO: Unsafe.Add: only the (ref T, int32), (ref T, IntPtr), and (ref T, UIntPtr) overloads are implemented; got params %A{methodToCall.Signature.ParameterTypes}"

            let offset, state = IlMachineState.popEvalStack currentThread state
            let src, state = IlMachineState.popEvalStack currentThread state

            // `conv.i` / `conv.u` produce `EvalStackValue.NativeInt (Verbatim ...)`;
            // the IntPtr/UIntPtr overloads feed us one of those. The int32 overload
            // produces `EvalStackValue.Int32` directly. Both narrow safely to int
            // so long as the verbatim value fits; on a 64-bit host the C# compiler
            // never emits an out-of-range native-int offset for array arithmetic.
            let offset =
                match offset with
                | EvalStackValue.Int32 i -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    if i < int64<int> System.Int32.MinValue || i > int64<int> System.Int32.MaxValue then
                        failwith
                            $"TODO: Unsafe.Add: native-int offset %d{i} does not fit in Int32; byte-level arithmetic on array byrefs is not modelled"

                    int32<int64> i
                | _ -> failwith $"TODO: Unsafe.Add: expected Int32 or Verbatim NativeInt offset, got %O{offset}"

            let ptr, state = offsetManagedPointerByElements baseClassTypes state t offset src

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "AddByteOffset" ->
            // CoreCLR's managed body throws PlatformNotSupportedException; the JIT replaces
            // the call with raw byref + native-int addition. Both overloads (IntPtr and
            // UIntPtr) share the same semantics: advance the byref by `byteOffset` bytes,
            // preserving the static `T` view.
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L661
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.AddByteOffset"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
            | _ ->
                failwith
                    $"TODO: Unsafe.AddByteOffset: only the (ref T, IntPtr) and (ref T, UIntPtr) overloads are implemented; got params %A{methodToCall.Signature.ParameterTypes}"

            let offset, state = IlMachineState.popEvalStack currentThread state
            let src, state = IlMachineState.popEvalStack currentThread state

            let offset : int =
                match offset with
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    if i < int64<int> System.Int32.MinValue || i > int64<int> System.Int32.MaxValue then
                        failwith $"TODO: Unsafe.AddByteOffset: native-int byte offset %d{i} does not fit in Int32"

                    int32<int64> i
                | EvalStackValue.Int32 i -> i
                | _ ->
                    failwith
                        $"TODO: Unsafe.AddByteOffset: expected Verbatim NativeInt or Int32 byte offset, got %O{offset}"

            let srcPtr =
                match src with
                | EvalStackValue.ManagedPointer p -> p
                | _ -> failwith $"TODO: Unsafe.AddByteOffset on non-ManagedPointer source byref: %O{src}"

            // `addByteOffsetUnderReinterpret` anchors the byte cursor under `ReinterpretAs T`
            // before appending the offset, so it works regardless of whether the source byref
            // already carries a trailing byte-view tail. The trailing `ReinterpretAs T` is
            // address-preserving; the `appendProjection` collapse rules handle the common
            // case where the source already has a `ReinterpretAs T` (idempotent) or a
            // `[ReinterpretAs T; ByteOffset n]` tail whose `n` cancels the new offset (e.g.
            // `RawData::Data` on an array followed by the canonical `+sizeof(nint)` skip).
            //
            // The byte-view path requires the reinterpret target's storage to be
            // byte-addressable on read. Object references (and value types containing
            // them) deliberately are not, so a naturally-typed byref to such cells
            // must stay in its natural form. We short-circuit when (a) the source
            // is itself naturally-typed (no trailing byte-view tail) and (b) the
            // byte offset is a whole-cell multiple, so the result is still
            // expressible without a reinterpret tail. The general byte-view path
            // handles all other shapes.
            let normalisation =
                match srcPtr with
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), _) ->
                    let elementSize =
                        let obj = state.ManagedHeap.Arrays.[arr]

                        if obj.Length = 0 then
                            0
                        else
                            CliType.sizeOf obj.Elements.[0]

                    ByteOffsetNormalisationContext.withArrayElementSize arr elementSize
                | _ -> ByteOffsetNormalisationContext.fixedStrideRootsOnly

            let typedShortcut : ManagedPointerSource option =
                match srcPtr with
                | ManagedPointerSource.Byref (root, projs) ->
                    let hasByteViewTail =
                        match List.tryLast projs with
                        | Some (ByrefProjection.ReinterpretAs _)
                        | Some (ByrefProjection.ByteOffset _) -> true
                        | _ -> false

                    if hasByteViewTail then
                        None
                    elif offset = 0 then
                        // Zero-byte advance on a naturally-typed byref is the identity;
                        // returning the source preserves the typed view that the bytewise
                        // path would otherwise destroy by appending a `ReinterpretAs T`.
                        Some srcPtr
                    else
                        match root, projs with
                        | ByrefRoot.ArrayElement (arr, i), [] ->
                            let arrObj = state.ManagedHeap.Arrays.[arr]

                            if arrObj.Length = 0 then
                                None
                            else
                                let elementSize = CliType.sizeOf arrObj.Elements.[0]

                                if elementSize > 0 && offset % elementSize = 0 then
                                    Some (
                                        ManagedPointerSource.Byref (
                                            ByrefRoot.ArrayElement (arr, i + offset / elementSize),
                                            []
                                        )
                                    )
                                else
                                    None
                        | _ -> None
                | _ -> None

            // Concretising T is only required for the byte-view fallback (which
            // anchors a `ReinterpretAs T` tail). The typed shortcut never touches
            // T, so structural concrete-type handles (array, pointer, function
            // pointer) — which `AllConcreteTypes.lookup` doesn't store — can still
            // resolve cleanly through the shortcut.
            let ptr =
                match typedShortcut with
                | Some p -> p
                | None ->
                    let tConcrete =
                        match AllConcreteTypes.lookup t state.ConcreteTypes with
                        | Some c -> c
                        | None -> failwith $"Unsafe.AddByteOffset: T not concretised: %O{t}"

                    ManagedPointerSource.addByteOffsetUnderReinterpret normalisation tConcrete offset srcPtr

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Unsafe", "ByteOffset" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L69
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with sub on two byrefs.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.ByteOffset"

            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ; ConcreteByref _ ] -> ()
            | _ -> failwith "bad signature Unsafe.ByteOffset"

            let target, state = IlMachineState.popEvalStack currentThread state
            let origin, state = IlMachineState.popEvalStack currentThread state

            let tSize, state =
                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                CliType.sizeOf tZero, state

            // `Unsafe.AsRef<T>((void*)bits)` byrefs are bit patterns, not
            // anchored byrefs. `Unsafe.ByteOffset` on a pair of them is just
            // the bit-difference, matching the IL `sub` semantics implemented
            // in BinaryArithmetic. Null is the placeholder for bits=0, so
            // pairings with Null are still well-defined as bit subtraction.
            let asPlaceholderBits (v : EvalStackValue) : int64 option =
                match v with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> Some 0L
                | EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits) -> Some bits
                | _ -> None

            match asPlaceholderBits origin, asPlaceholderBits target with
            | Some originBits, Some targetBits ->
                let byteOffset = targetBits - originBits

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim byteOffset))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ ->

            // ByteOffset measures the byte distance between two byref address
            // targets. The generic T on the method is only the static view
            // through which each byref was declared; reinterpreting a byref
            // doesn't move it. Trailing `ByteOffset` projections contribute
            // to the absolute byte address; `ReinterpretAs` projections are
            // address-preserving.
            let extractByteLocation (v : EvalStackValue) : ByteStorageIdentity * int64 =
                let src =
                    match v with
                    | EvalStackValue.ManagedPointer p -> p
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer p) -> p
                    | _ -> failwith $"TODO: Unsafe.ByteOffset on non-ManagedPointer: %O{v}"

                let projectionByteOffset (projs : ByrefProjection list) : int64 =
                    let mutable byteOff = 0L

                    for p in projs do
                        match p with
                        | ByrefProjection.ReinterpretAs _ -> ()
                        | ByrefProjection.ByteOffset n -> byteOff <- byteOff + int64 n
                        | _ -> failwith $"TODO: Unsafe.ByteOffset on byref with non-ReinterpretAs projection: %O{p}"

                    byteOff

                match src with
                | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), projs) ->
                    ByteStorageIdentity.StackMemory (thread, frame, block),
                    int64 byteOffset + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), projs) ->
                    ByteStorageIdentity.NativeMemory block, int64 byteOffset + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local), projs) ->
                    ByteStorageIdentity.StackLocal (thread, frame, local), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg), projs) ->
                    ByteStorageIdentity.StackArgument (thread, frame, arg), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field), projs) ->
                    ByteStorageIdentity.StaticField (declaringType, field), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) ->
                    // `Array.Empty<T>()` carries no stored element to read a
                    // size from, but the statically-declared `T` on the method
                    // gives the same answer for any byref the caller could
                    // legally have obtained: both parameters are `ref T`.
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    let elementSize =
                        if arrObj.Length = 0 then
                            tSize
                        else
                            CliType.sizeOf arrObj.Elements.[0]

                    ByteStorageIdentity.Array arr, int64 i * int64 elementSize + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), projs) ->
                    ByteStorageIdentity.String str, int64 charIndex * 2L + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
                    ByteStorageIdentity.PeByteRange peByteRange, projectionByteOffset projs
                | _ -> failwith $"TODO: Unsafe.ByteOffset on unsupported byref: %O{v}"

            let storage1, originOffset = extractByteLocation origin
            let storage2, targetOffset = extractByteLocation target

            // Same-storage ByteOffset is an honest byte delta and composes
            // correctly with Unsafe.Add / further arithmetic. Cross-storage
            // ByteOffset has no principled byte distance in our model, so we
            // reuse the cross-storage helper to synthesise a
            // deterministic sentinel large enough to defeat the unsigned
            // overlap check `(nuint)offset < len` used by Memmove. The tag
            // makes any subsequent `add`/`sub` fail loudly via BinaryArithmetic.execute's
            // "refusing to operate on non-verbatim native int" branch, rather
            // than silently composing into a wrong answer.
            if storage1 = storage2 then
                let byteOffset = targetOffset - originOffset

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim byteOffset))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            else
                let byteOffset =
                    NativeIntSource.syntheticCrossStorageByteOffset storage1 originOffset storage2 targetOffset

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt byteOffset) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), "get_Item" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L141
            // The source-level body returns `ref Unsafe.Add(ref _reference, index)`;
            // the method is intrinsic so we model that primitive boundary directly.
            let spanTypeName : string = methodToCall.DeclaringType.Name

            let elementType : ConcreteTypeHandle =
                methodToCall.DeclaringType.Generics |> Seq.exactlyOne

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteByref ret) when ret = elementType ->
                ()
            | _ ->
                failwith
                    $"bad signature for System.Private.CoreLib.%s{spanTypeName}.get_Item: %A{methodToCall.Signature}"

            let index, state = IlMachineState.popEvalStack currentThread state
            let receiver, state = IlMachineState.popEvalStack currentThread state

            let index : int =
                match index with
                | EvalStackValue.Int32 i -> i
                | other -> failwith $"%s{spanTypeName}.get_Item expected Int32 index, got %O{other}"

            let span : CliValueType =
                match receiver with
                | EvalStackValue.ManagedPointer src ->
                    match IlMachineState.readManagedByref baseClassTypes state src with
                    | CliType.ValueType vt -> vt
                    | other ->
                        failwith $"%s{spanTypeName}.get_Item receiver byref read produced non-value-type %O{other}"
                | EvalStackValue.UserDefinedValueType vt -> vt
                | other -> failwith $"%s{spanTypeName}.get_Item expected span receiver byref, got %O{other}"

            let length : int =
                let lengthField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

                match
                    CliValueType.DereferenceFieldById lengthField span
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{spanTypeName}.get_Item expected _length to be int32, got %O{other}"

            if uint32<int32> index >= uint32<int32> length then
                failwith
                    $"TODO: %s{spanTypeName}.get_Item index %d{index} outside length %d{length}; throw IndexOutOfRangeException"

            let reference : EvalStackValue =
                let referenceField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

                match
                    CliValueType.DereferenceFieldById referenceField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> EvalStackValue.ManagedPointer src
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) ->
                    EvalStackValue.ManagedPointer src
                | other ->
                    failwith $"%s{spanTypeName}.get_Item expected _reference to be a managed byref, got %O{other}"

            let ptr, state =
                offsetManagedPointerByElements baseClassTypes state elementType index reference

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Span`1", "Clear" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L280
            // Span<T>.Clear is a JIT intrinsic; the BCL IL falls through to
            // SpanHelpers.ClearWithReferences / ClearWithoutReferences, the latter of
            // which has a P/Invoke fallback for long zeroings. Model the JIT semantics
            // directly: write default(T) to each of `_length` elements starting at
            // `_reference`, using the same byref-projection helpers as get_Item.
            let elementType : ConcreteTypeHandle =
                methodToCall.DeclaringType.Generics |> Seq.exactlyOne

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"bad signature for System.Span`1.Clear: %A{methodToCall.Signature}"

            let receiver, state = IlMachineState.popEvalStack currentThread state

            let span : CliValueType =
                match receiver with
                | EvalStackValue.ManagedPointer src ->
                    match IlMachineState.readManagedByref baseClassTypes state src with
                    | CliType.ValueType vt -> vt
                    | other -> failwith $"Span`1.Clear receiver byref read produced non-value-type %O{other}"
                | EvalStackValue.UserDefinedValueType vt -> vt
                | other -> failwith $"Span`1.Clear expected span receiver byref, got %O{other}"

            let length : int =
                let lengthField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

                match
                    CliValueType.DereferenceFieldById lengthField span
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"Span`1.Clear expected _length to be int32, got %O{other}"

            let reference : EvalStackValue =
                let referenceField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

                match
                    CliValueType.DereferenceFieldById referenceField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> EvalStackValue.ManagedPointer src
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) ->
                    EvalStackValue.ManagedPointer src
                | other -> failwith $"Span`1.Clear expected _reference to be a managed byref, got %O{other}"

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementType

            let state =
                (state, seq { 0 .. length - 1 })
                ||> Seq.fold (fun state i ->
                    let ptr, state =
                        offsetManagedPointerByElements baseClassTypes state elementType i reference

                    let byrefSrc =
                        match ptr with
                        | EvalStackValue.ManagedPointer src -> src
                        | other ->
                            failwith $"Span`1.Clear: offsetManagedPointerByElements returned non-byref %O{other}"

                    IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc zero
                )

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "RuntimeHelpers", "CreateSpan" ->
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            None
        | "System.Private.CoreLib", "MemoryMarshal", "GetArrayDataReference" ->
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/coreclr/System.Private.CoreLib/src/System/Runtime/InteropServices/MemoryMarshal.CoreCLR.cs#L20
            let generic = Seq.exactlyOne methodToCall.Generics

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteGenericArray state.ConcreteTypes generic ], MethodReturnType.Returns (ConcreteByref t) when
                t = generic
                ->
                ()
            | _ -> failwith "bad signature MemoryMarshal.GetArrayDataReference"

            let arr, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                match arr with
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _ -> failwith "expected reference"
                | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                | EvalStackValue.ObjectRef addr ->
                    if not (state.ManagedHeap.Arrays.ContainsKey addr) then
                        failwith "array not found"

                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (addr, 0), [])
                    |> EvalStackValue.ManagedPointer
                | EvalStackValue.NullObjectRef
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: raise NRE"
                | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"
                | EvalStackValue.ManagedPointer _ -> failwith "todo"

            state
            |> IlMachineState.pushToEvalStack' toPush currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Enum", "HasFlag" ->
            // https://github.com/dotnet/runtime/blob/dbd3e33df9ccf74b91045e095477726c2bf83916/src/libraries/System.Private.CoreLib/src/System/Enum.cs#L398
            // Enum.HasFlag(Enum flag) returns (thisValue & flagValue) == flagValue
            // The arguments are boxed enums (ObjectRef) since the method signature takes System.Enum.
            //
            // Peek first to check type compatibility. If types mismatch, raise ArgumentException
            // directly before consuming the boxed enum values for the raw bitwise comparison below.
            let evalStack = state.ThreadState.[currentThread].MethodState.EvaluationStack
            let flagPeek = EvalStack.PeekNthFromTop 0 evalStack
            let thisPeek = EvalStack.PeekNthFromTop 1 evalStack

            match thisPeek, flagPeek with
            | Some (EvalStackValue.ObjectRef thisAddr), Some (EvalStackValue.ObjectRef flagAddr) ->
                let thisObj = ManagedHeap.get thisAddr state.ManagedHeap
                let flagObj = ManagedHeap.get flagAddr state.ManagedHeap

                if thisObj.ConcreteType <> flagObj.ConcreteType then
                    // Type mismatch: raise ArgumentException.
                    // We must pop the two args before raising, so the eval stack is clean.
                    let _, state = IlMachineState.popEvalStack currentThread state
                    let _, state = IlMachineState.popEvalStack currentThread state

                    let exnAddr, exnTypeHandle, state =
                        ExceptionDispatching.allocateRuntimeException
                            loggerFactory
                            baseClassTypes
                            baseClassTypes.ArgumentException
                            state

                    let state =
                        ExceptionDispatching.overwriteHResultPostCtor baseClassTypes exnAddr exnTypeHandle state

                    match
                        ExceptionDispatching.throwExceptionObject
                            loggerFactory
                            baseClassTypes
                            state
                            currentThread
                            exnAddr
                            exnTypeHandle
                    with
                    | ExceptionDispatchResult.HandlerFound state -> Some state
                    | ExceptionDispatchResult.ExceptionUnhandled _ ->
                        failwith
                            "Enum.HasFlag type mismatch: ArgumentException was unhandled (no catch handler in caller)"
                else
                    let flag, state = IlMachineState.popEvalStack currentThread state
                    let thisVal, state = IlMachineState.popEvalStack currentThread state

                    let numericToInt64 (n : CliNumericType) : int64 =
                        match n with
                        | CliNumericType.Int32 i -> int64 i
                        | CliNumericType.Int64 (Int64Source.Verbatim i) -> i
                        | CliNumericType.Int8 i -> int64 i
                        | CliNumericType.UInt8 i -> int64 i
                        | CliNumericType.Int16 i -> int64 i
                        | CliNumericType.UInt16 i -> int64 i
                        | other -> failwith $"Enum.HasFlag: unexpected underlying numeric type %O{other}"

                    let extractInt (contents : CliValueType) : int64 =
                        match (CliValueType.PrimitiveLikeField contents).Contents with
                        | CliType.Numeric n -> numericToInt64 n
                        | other -> failwith $"Enum.HasFlag: unexpected underlying type %O{other}"

                    let thisInt = extractInt thisObj.Contents
                    let flagInt = extractInt flagObj.Contents
                    let result = (thisInt &&& flagInt) = flagInt

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (if result then 1 else 0)) currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> Some
            | Some _, Some EvalStackValue.NullObjectRef ->
                // Null flag: raise ArgumentNullException.
                let _, state = IlMachineState.popEvalStack currentThread state
                let _, state = IlMachineState.popEvalStack currentThread state

                let exnAddr, exnTypeHandle, state =
                    ExceptionDispatching.allocateRuntimeException
                        loggerFactory
                        baseClassTypes
                        baseClassTypes.ArgumentNullException
                        state

                let state =
                    ExceptionDispatching.overwriteHResultPostCtor baseClassTypes exnAddr exnTypeHandle state

                match
                    ExceptionDispatching.throwExceptionObject
                        loggerFactory
                        baseClassTypes
                        state
                        currentThread
                        exnAddr
                        exnTypeHandle
                with
                | ExceptionDispatchResult.HandlerFound state -> Some state
                | ExceptionDispatchResult.ExceptionUnhandled _ ->
                    failwith "Enum.HasFlag null flag: ArgumentNullException was unhandled (no catch handler in caller)"
            | _ -> failwith $"Enum.HasFlag: expected two ObjectRefs on eval stack"
        | _ -> None
