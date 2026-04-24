namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Intrinsics =
    let private safeIntrinsics =
        [
            // The IL implementation is fine: https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L677
            "System.Private.CoreLib", "Unsafe", "AsRef"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            "System.Private.CoreLib", "String", "get_Length"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            "System.Private.CoreLib", "ArgumentNullException", "ThrowIfNull"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            "System.Private.CoreLib", "Type", "GetTypeFromHandle"
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L161
            "System.Private.CoreLib", "ReadOnlySpan`1", "get_Length"
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            "System.Private.CoreLib", "RuntimeHelpers", "CreateSpan"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L127
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L137
            "System.Private.CoreLib", "Math", "Abs"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L965C10-L1062C19
            "System.Private.CoreLib", "Math", "Max"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Buffer.cs#L150
            "System.Private.CoreLib", "Buffer", "Memmove"
            // https://github.com/dotnet/runtime/blob/1c3221b63340d7f81dfd829f3bcd822e582324f6/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L799
            "System.Private.CoreLib", "Thread", "get_CurrentThread"
            // IL body is `ldarg.0; ldfld _managedThreadId; ret` — pure field access.
            "System.Private.CoreLib", "Thread", "get_ManagedThreadId"
        ]
        |> Set.ofList

    type private RefTypeProcessingStatus =
        | InProgress
        | Completed of bool

    let rec private containsRefType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (seenSoFar : ImmutableDictionary<TypeInfo<TypeDefn, TypeDefn>, RefTypeProcessingStatus>)
        (td : TypeInfo<TypeDefn, TypeDefn>)
        : IlMachineState * ImmutableDictionary<_, RefTypeProcessingStatus> * bool
        =
        match seenSoFar.TryGetValue td with
        | true, InProgress ->
            // We've hit a cycle. Optimistically assume this path does not introduce a reference type.
            // If another path finds a reference type, its 'true' will override this.
            state, seenSoFar, false
        | true, Completed v ->
            // We've already calculated this; return the memoized result.
            state, seenSoFar, v
        | false, _ ->
            if DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies td then
                // Short-circuit: if the type itself is a reference type, we're done.
                let seenSoFar = seenSoFar.Add (td, Completed true)
                state, seenSoFar, true
            else
                // It's a value type, so we must check its fields.
                // Mark as in progress before recursing.
                let seenSoFarWithInProgress = seenSoFar.Add (td, InProgress)

                let stateAfterFieldResolution, nonStaticFields =
                    ((state, []), td.Fields)
                    ||> List.fold (fun (currentState, acc) field ->
                        if field.IsStatic then
                            currentState, acc
                        else
                            // TODO: generics
                            let newState, _, info =
                                IlMachineState.resolveTypeFromDefn
                                    loggerFactory
                                    baseClassTypes
                                    field.Signature
                                    ImmutableArray.Empty
                                    ImmutableArray.Empty
                                    (currentState.LoadedAssembly (td.Assembly) |> Option.get)
                                    currentState

                            newState, info :: acc
                    )

                // Recurse through the fields, correctly propagating state.
                let finalState, finalSeenSoFar, fieldsContainRefType =
                    ((stateAfterFieldResolution, seenSoFarWithInProgress, false), nonStaticFields)
                    ||> List.fold (fun (currentState, currentSeenSoFar, currentResult) field ->
                        if currentResult then
                            (currentState, currentSeenSoFar, true) // Short-circuit
                        else
                            let newState, newSeenSoFar, fieldResult =
                                containsRefType loggerFactory baseClassTypes currentState currentSeenSoFar field

                            (newState, newSeenSoFar, currentResult || fieldResult)
                    )

                // Mark as completed with the final result before returning.
                let finalSeenSoFar = finalSeenSoFar.SetItem (td, Completed fieldsContainRefType)
                finalState, finalSeenSoFar, fieldsContainRefType

    let private popRuntimeTypeHandle
        (currentThread : ThreadId)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        let this, state = IlMachineState.popEvalStack currentThread state

        let this =
            match this with
            | EvalStackValue.ObjectRef ptr ->
                IlMachineState.readManagedByref state (ManagedPointerSource.Byref (ByrefRoot.HeapValue ptr, []))
            | EvalStackValue.ManagedPointer ptr -> IlMachineState.readManagedByref state ptr
            | EvalStackValue.NullObjectRef -> failwith "TODO: Type intrinsic receiver was null; throw NRE"
            | EvalStackValue.Float _
            | EvalStackValue.Int32 _
            | EvalStackValue.Int64 _ -> failwith "Type intrinsic receiver: refusing to dereference literal"
            | other -> failwith $"Type intrinsic receiver: expected RuntimeType object or byref, got %O{other}"

        let ty =
            match this with
            | CliType.ValueType cvt ->
                // `RuntimeType.m_handle` is IntPtr (primitive-like); unwrap to reach the inner NativeInt.
                match CliValueType.DereferenceField "m_handle" cvt |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr cth)) -> cth
                | other ->
                    failwith
                        $"Type intrinsic receiver: expected RuntimeType.m_handle to contain a TypeHandlePtr, got %O{other}"
            | other -> failwith $"Type intrinsic receiver: expected RuntimeType value contents, got %O{other}"

        ty, state

    let call
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let callerAssy =
            state.ThreadState.[currentThread].MethodState.ExecutingMethod.DeclaringType.Assembly

        if
            methodToCall.DeclaringType.Assembly.Name = "System.Private.CoreLib"
            && methodToCall.DeclaringType.Name = "Volatile"
        then
            // These are all safely implemented in IL, just inefficient.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Volatile.cs#L13
            None
        elif
            Set.contains
                (methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name)
                safeIntrinsics
        then
            None
        else

        // In general, some implementations are in:
        // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L192
        match methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name with
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
                    {
                        Name = "m_type"
                        Contents = CliType.ObjectRef arg
                        Offset = None
                        Type =
                            AllConcreteTypes.findExistingNonGenericConcreteType
                                state.ConcreteTypes
                                baseClassTypes.RuntimeType.Identity
                            |> Option.get
                    }
                    |> List.singleton
                    |> CliValueType.OfFields baseClassTypes state.ConcreteTypes runtimeTypeHandleHandle Layout.Default

                IlMachineState.pushToEvalStack (CliType.ValueType vt) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "RuntimeHelpers", "GetMethodTable" ->
            match methodToCall.Signature.ParameterTypes with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ] -> ()
            | _ -> failwith "bad signature RuntimeHelpers.GetMethodTable"

            match methodToCall.Signature.ReturnType with
            | ConcreteTypeHandle.Pointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                            "System.Runtime.CompilerServices",
                                                                            "MethodTable",
                                                                            generics)) when generics.IsEmpty -> ()
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
            | [], ConcreteBool state.ConcreteTypes -> ()
            | _ -> failwith "bad signature Type.get_IsValueType"

            let ty, state = popRuntimeTypeHandle currentThread state

            let ty =
                // TODO: structural handles such as typeof(int[]) still reach here as
                // ConcreteTypeHandle.OneDimArrayZero, but this branch only handles nominal types.
                match AllConcreteTypes.lookup ty state.ConcreteTypes with
                | Some ty -> ty
                | None -> failwith $"Type.get_IsValueType: expected nominal concrete type handle, got %O{ty}"

            let ty = state.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get]

            let isValueType =
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty

            IlMachineState.pushToEvalStack (CliType.ofBool isValueType) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_IsGenericType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], ConcreteBool state.ConcreteTypes -> ()
            | _ -> failwith "bad signature Type.get_IsGenericType"

            let ty, state = popRuntimeTypeHandle currentThread state

            let isGenericType =
                match ty with
                | ConcreteTypeHandle.Concrete _ ->
                    match AllConcreteTypes.lookup ty state.ConcreteTypes with
                    | Some ty -> not ty.Generics.IsEmpty
                    | None -> failwith $"Type.get_IsGenericType: concrete type handle was not registered: %O{ty}"
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
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
                | EvalStackValue.ManagedPointer ptr -> CliRuntimePointer.Managed ptr
                | x -> failwith $"TODO: Unsafe.AsPointer(%O{x})"

            IlMachineState.pushToEvalStack (CliType.RuntimePointer toPush) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Interlocked", "CompareExchange" ->
            // We only intercept the (ref IntPtr, IntPtr, IntPtr) -> IntPtr overload: the shipped IL
            // wrapper does `Unsafe.As<IntPtr,long>` + delegates to the Int64 overload, which would
            // destroy our NativeIntSource provenance. Other overloads fall through to their IL bodies.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L452
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
                ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
                ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
              ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ->

                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let byrefSrc =
                    match byrefArg with
                    | EvalStackValue.ManagedPointer ptr -> ptr
                    | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref IntPtr,...): expected ManagedPointer byref, got %O{other}"

                // Eval-stack IntPtr arguments are flattened to the primitive by the push
                // boundary (see EvalStackValue.ofCliType), so a UserDefinedValueType IntPtr
                // is unreachable here by invariant.
                let toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                    match v with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 i -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                    | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref IntPtr,...): unexpected IntPtr-shaped eval stack value %O{other}"

                let comparandSrc = toNativeIntSource comparand
                let valueSrc = toNativeIntSource value

                let currentValue = IlMachineState.readManagedByref state byrefSrc

                // `ref IntPtr` derefs to the IntPtr wrapper struct. Route the read/write through
                // the eval-stack flatten/rewrap boundary: `ofCliType` peels the primitive-like
                // wrapper to `NativeInt`, and `toCliTypeCoerced` reconstructs the wrapper shape
                // on write. The primitive-like registry is the single source of truth for shape.
                let currentSrc =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 i -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref IntPtr,...): expected NativeInt at byref target, got %O{other}"

                // Two representations of zero exist (`Verbatim 0L` for `new IntPtr(0)` and
                // `ManagedPointer Null` for default-initialised IntPtr / `IntPtr.Zero`); treat
                // them as equal, matching native-int `ceq` semantics.
                let nativeIntEq (a : NativeIntSource) (b : NativeIntSource) : bool =
                    a = b || (NativeIntSource.isZero a && NativeIntSource.isZero b)

                let state =
                    if nativeIntEq currentSrc comparandSrc then
                        let newValue =
                            EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.NativeInt valueSrc)

                        IlMachineState.writeManagedByref state byrefSrc newValue
                    else
                        state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt currentSrc) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ ->
                // Other Interlocked.CompareExchange overloads are not yet intrinsified.
                // The Int32/Int64 shipped IL bodies self-call (expecting the JIT to intrinsify),
                // so they will stack-overflow if we fall through here. The object overload
                // delegates to CompareExchangeObject which is InternalCall (no IL body).
                // When a caller needs one of these, it will need its own intrinsic arm.
                None
        | "System.Private.CoreLib", "BitConverter", "SingleToInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], ConcreteInt32 state.ConcreteTypes -> ()
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
            | [ ConcreteInt32 state.ConcreteTypes ], ConcreteSingle state.ConcreteTypes -> ()
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
            | [ ConcreteDouble state.ConcreteTypes ], ConcreteUInt64 state.ConcreteTypes -> ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Float i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.DoubleToUInt64Bits arg
                |> int64<uint64>
                |> CliNumericType.Int64
                |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "UInt64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt64 state.ConcreteTypes ], ConcreteDouble state.ConcreteTypes -> ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 i -> uint64 i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.UInt64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt64 state.ConcreteTypes ], ConcreteDouble state.ConcreteTypes -> ()
            | _ -> failwith "bad signature BitConverter.Int64BitsToDouble"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "DoubleToInt64Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], ConcreteInt64 state.ConcreteTypes -> ()
            | _ -> failwith "bad signature BitConverter.DoubleToInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.DoubleToInt64Bits f |> EvalStackValue.Int64
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "SingleToUInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], ConcreteUInt32 state.ConcreteTypes -> ()
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
            | [ ConcreteUInt32 state.ConcreteTypes ], ConcreteSingle state.ConcreteTypes -> ()
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
        | "System.Private.CoreLib", "String", "Equals" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteString state.ConcreteTypes ; ConcreteString state.ConcreteTypes ],
              ConcreteBool state.ConcreteTypes ->
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
            // as a bytewise gather across the pointed-to storage and then
            // reconstruct a T of the right shape via `ofBytesLike`.
            //
            // Two overloads exist: `ReadUnaligned<T>(ref byte source)` and
            // `ReadUnaligned<T>(void* source)`. Only intercept the byref one
            // here; the pointer overload has a different eval-stack shape
            // and falls through to its shipped IL body.
            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.ReadUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                let tSize = CliType.sizeOf tZero

                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> src
                    | EvalStackValue.NullObjectRef -> failwith "TODO: Unsafe.ReadUnaligned on null should throw NRE"
                    | _ -> failwith $"TODO: Unsafe.ReadUnaligned: expected ManagedPointer, got %O{ptr}"

                // Address-preserving `ReinterpretAs` projections don't change the
                // bytes we need to read. Strip trailing reinterprets so the byref
                // resolves to its underlying storage shape for the gather below.
                let src = ManagedPointerSource.stripTrailingReinterprets src

                // An array byref may carry a trailing ByteOffset (from byte-view
                // pointer arithmetic like `Unsafe.Add(ref byte, n)`); pull that
                // out so the gather below can start mid-cell when needed.
                let arrayStartByteOffset (projs : ByrefProjection list) : int =
                    match List.tryLast projs with
                    | Some (ByrefProjection.ByteOffset n) -> n
                    | _ -> 0

                let v : CliType =
                    match src with
                    | ManagedPointerSource.Null -> failwith "TODO: Unsafe.ReadUnaligned on null should throw NRE"
                    | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) when
                        (match projs with
                         | []
                         | [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset _ ] -> true
                         | _ -> false)
                        ->
                        // Gather tSize bytes from the array starting at cell i
                        // plus any accumulated byte offset from a byte-view
                        // cursor. Each cell contributes its own serialisation;
                        // the gather walks cell by cell, taking the remainder
                        // of each cell's bytes before advancing.
                        let arrObj = state.ManagedHeap.Arrays.[arr]

                        let arrElementSize =
                            if arrObj.Length = 0 then
                                0
                            else
                                CliType.sizeOf arrObj.Elements.[0]

                        let byteOffset = arrayStartByteOffset projs
                        let buf = Array.zeroCreate<byte> tSize
                        let mutable filled = 0
                        let mutable cell = i
                        let mutable inCellOffset = byteOffset

                        // Normalise a starting byte offset that spans whole
                        // cells into a cell advance so the main loop's
                        // `cellBytes.Length - inCellOffset` arithmetic stays
                        // well-defined. Only meaningful when cells have a
                        // uniform size (always true here: primitive arrays).
                        if arrElementSize > 0 && inCellOffset >= arrElementSize then
                            cell <- cell + (inCellOffset / arrElementSize)
                            inCellOffset <- inCellOffset % arrElementSize

                        while filled < tSize do
                            if cell >= arrObj.Length then
                                failwith
                                    $"TODO: Unsafe.ReadUnaligned: read past end of array at cell %d{cell} of length %d{arrObj.Length} while gathering %d{tSize} bytes"

                            let cellBytes = CliType.ToBytes arrObj.Elements.[cell]
                            let canTake = cellBytes.Length - inCellOffset
                            let take = min canTake (tSize - filled)
                            Array.blit cellBytes inCellOffset buf filled take
                            filled <- filled + take
                            cell <- cell + 1
                            inCellOffset <- 0

                        CliType.ofBytesLike tZero buf
                    | ManagedPointerSource.Byref _ ->
                        // Fallback: if the byref's natural read happens to match
                        // the target size exactly, the generic deref produces the
                        // right CliType directly (e.g. `Unsafe.ReadUnaligned<int>`
                        // on `ref int`). Anything else (cross-cell reads through
                        // non-array storage, byref with non-trailing projections)
                        // would need the byte-offset machinery that's out of
                        // scope for this PR.
                        let raw = IlMachineState.readManagedByref state src

                        if CliType.sizeOf raw = tSize then
                            let bytes = CliType.ToBytes raw
                            CliType.ofBytesLike tZero bytes
                        else
                            failwith
                                $"TODO: Unsafe.ReadUnaligned: byref shape %O{src} not yet supported for bytewise gather (size %d{CliType.sizeOf raw} vs %d{tSize})"

                let state =
                    state
                    |> IlMachineState.pushToEvalStack v currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                Some state
            | _ -> None
        | "System.Private.CoreLib", "Unsafe", "WriteUnaligned" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L609
            // Symmetric to ReadUnaligned: writes a T through a byte-level
            // byref by scattering `CliType.ToBytes` of the value across
            // consecutive cells of the pointed-to storage.
            //
            // Only intercept the `(ref byte, T)` overload; the `(void*, T)`
            // overload falls through to its IL body.
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

                let src = ManagedPointerSource.stripTrailingReinterprets src

                // Coerce the stack value to a CliType shaped like T: sub-int
                // primitives arrive as Int32 and must narrow back to their
                // CliType flavour before `ToBytes` produces a correct byte image.
                let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value
                let bytes = CliType.ToBytes valueAsCli

                if bytes.Length <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned: ToBytes produced %d{bytes.Length} bytes, expected %d{tSize} for %O{valueAsCli}"

                let arrayStartByteOffsetW (projs : ByrefProjection list) : int =
                    match List.tryLast projs with
                    | Some (ByrefProjection.ByteOffset n) -> n
                    | _ -> 0

                let state =
                    match src with
                    | ManagedPointerSource.Null -> failwith "TODO: Unsafe.WriteUnaligned on null should throw NRE"
                    | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) when
                        (match projs with
                         | []
                         | [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset _ ] -> true
                         | _ -> false)
                        ->
                        let arrObj = state.ManagedHeap.Arrays.[arr]

                        let arrElementSize =
                            if arrObj.Length = 0 then
                                0
                            else
                                CliType.sizeOf arrObj.Elements.[0]

                        let byteOffset = arrayStartByteOffsetW projs
                        let mutable state = state
                        let mutable filled = 0
                        let mutable cell = i
                        let mutable inCellOffset = byteOffset

                        if arrElementSize > 0 && inCellOffset >= arrElementSize then
                            cell <- cell + (inCellOffset / arrElementSize)
                            inCellOffset <- inCellOffset % arrElementSize

                        while filled < tSize do
                            if cell >= arrObj.Length then
                                failwith
                                    $"TODO: Unsafe.WriteUnaligned: write past end of array at cell %d{cell} of length %d{arrObj.Length}"

                            let existing = arrObj.Elements.[cell]
                            let existingBytes = CliType.ToBytes existing
                            let cellSize = existingBytes.Length
                            let canTake = cellSize - inCellOffset
                            let take = min canTake (tSize - filled)
                            // Splice new bytes into a copy of the existing cell
                            // serialisation, then rebuild the cell's CliType.
                            let newCellBytes = Array.copy existingBytes
                            Array.blit bytes filled newCellBytes inCellOffset take
                            let newCell = CliType.ofBytesLike existing newCellBytes
                            state <- IlMachineState.setArrayValue arr newCell cell state
                            filled <- filled + take
                            cell <- cell + 1
                            inCellOffset <- 0

                        state
                    | ManagedPointerSource.Byref _ ->
                        // Size-matched write to a non-array byref: fall through
                        // to the generic projection write. Larger / cross-cell
                        // writes are out of scope for this PR.
                        let existing = IlMachineState.readManagedByref state src

                        if CliType.sizeOf existing = tSize then
                            IlMachineState.writeManagedByref state src valueAsCli
                        else
                            failwith
                                $"TODO: Unsafe.WriteUnaligned: byref shape %O{src} not yet supported for bytewise scatter (cell size %d{CliType.sizeOf existing} vs T size %d{tSize})"

                let state = state |> IlMachineState.advanceProgramCounter currentThread
                Some state
            | _ -> None
        | "System.Private.CoreLib", "String", "op_Implicit" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ par ], ret ->
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
            | [], ConcreteBool state.ConcreteTypes -> ()
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
                    |> containsRefType loggerFactory baseClassTypes state ImmutableDictionary.Empty
                    |> fun (state, _, result) -> state, result
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
            | [ ConcreteNonGenericArray state.ConcreteTypes ; ConcreteRuntimeFieldHandle state.ConcreteTypes ],
              ConcreteVoid state.ConcreteTypes -> ()
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
            let runtimeFieldInfoStubAddr : ManagedHeapAddress =
                match fldHandle with
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.NullObjectRef ->
                    failwith "TODO: throw ArgumentException for InitializeArray with null field handle"
                | other -> failwith $"InitializeArray: expected RuntimeFieldHandle ObjectRef, got %O{other}"

            // Look up the FieldHandle from the registry using the RuntimeFieldInfoStub address
            let fieldHandle : FieldHandle =
                match FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoStubAddr state.FieldHandles with
                | Some fh -> fh
                | None ->
                    failwith
                        $"InitializeArray: RuntimeFieldInfoStub at %O{runtimeFieldInfoStubAddr} not found in field handle registry"

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
                                    CliType.Numeric (CliNumericType.Int64 (reader.ReadInt64 ()))
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
        | "System.Private.CoreLib", "Unsafe", "As" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L64
            let byrefAs () =
                let inputType, retType =
                    match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                    | [ input ], ret -> input, ret
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
                if methodToCall.Signature.ReturnType <> target then
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
        | "System.Private.CoreLib", "Unsafe", "SizeOf" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L51
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], ConcreteInt32 state.ConcreteTypes -> ()
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
            | [ ConcreteByref _ ; ConcreteByref _ ], ConcreteBool state.ConcreteTypes -> ()
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

            if
                ManagedPointerSource.hasNonTrailingReinterpret leftPtr
                || ManagedPointerSource.hasNonTrailingReinterpret rightPtr
            then
                failwith
                    $"TODO: Unsafe.AreSame on byref with `ReinterpretAs` followed by `Field` needs a bytewise layout comparison; got %O{leftPtr} vs %O{rightPtr}"

            let strip = ManagedPointerSource.stripTrailingReinterprets
            let areSame = strip leftPtr = strip rightPtr

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
            | [ ConcreteByref tFromParam ; ConcreteInt32 state.ConcreteTypes ], ConcreteByref tFromRet
            | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ], ConcreteByref tFromRet
            | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ], ConcreteByref tFromRet when
                tFromParam = t && tFromRet = t
                ->
                ()
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

            // The input byref may or may not carry an address-preserving
            // `ReinterpretAs` projection (from an `Unsafe.As` or a round-trip).
            // We can only do element-index arithmetic if `sizeof(T)` matches the
            // array's true element size: otherwise advancing by `offset` elements
            // of T is not a whole-element step in the underlying array. Any
            // existing trailing reinterprets must also only be size-preserving,
            // and they stay on the result so that later field access / As chains
            // still see the type view the caller set up.
            // Thread the state returned by `cliTypeZeroOfHandle`: for a struct T
            // it can concretise additional types, and discarding the update
            // would drop that work from the machine state.
            let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
            let tSize = CliType.sizeOf tZero

            let ptr : EvalStackValue =
                match src with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs)) ->
                    let arrElementSize =
                        let arrObj = state.ManagedHeap.Arrays.[arr]

                        if arrObj.Length = 0 then
                            tSize
                        else
                            CliType.sizeOf arrObj.Elements.[0]

                    // Choose between cell-index and byte-cursor walks:
                    //   - If the byref already carries a `ByteOffset` tail, we
                    //     must stay in the byte cursor (accumulate).
                    //   - If `sizeof(T)` matches the underlying array's cell
                    //     stride, cell-index arithmetic is exact and keeps the
                    //     byref in a form the generic projection fold can
                    //     dereference — preferred even when there's a trailing
                    //     `ReinterpretAs`.
                    //   - Otherwise we need a byte cursor; this requires a
                    //     trailing `ReinterpretAs` to anchor the view, since
                    //     plain cell byrefs aren't byte-addressable.
                    let trailingIsByteOffset =
                        match List.tryLast projs with
                        | Some (ByrefProjection.ByteOffset _) -> true
                        | _ -> false

                    let trailingIsReinterpretAs =
                        match List.tryLast projs with
                        | Some (ByrefProjection.ReinterpretAs _) -> true
                        | _ -> false

                    // The byte-cursor branch produces pointers of shape
                    // `[ReinterpretAs ...; ByteOffset n]` that the bytewise
                    // consumers (`ReadUnaligned`, `WriteUnaligned`,
                    // `ByteOffset`) handle. If the existing projection list
                    // contains anything other than `ReinterpretAs` or
                    // `ByteOffset` — e.g. a `Field` for
                    // `Unsafe.As<int, byte>(ref arr[i].Field)` — appending
                    // another `ByteOffset` would manufacture a pointer the
                    // downstream code can't consume. Refuse here so failure
                    // surfaces at the arithmetic, not at the next load/store.
                    let projectionsAreByteViewCompatible =
                        projs
                        |> List.forall (fun p ->
                            match p with
                            | ByrefProjection.ReinterpretAs _
                            | ByrefProjection.ByteOffset _ -> true
                            | _ -> false
                        )

                    if
                        projectionsAreByteViewCompatible
                        && (trailingIsByteOffset || (tSize <> arrElementSize && trailingIsReinterpretAs))
                    then
                        let byteDelta = tSize * offset
                        let baseSrc = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs)

                        let cellSizeOf (addr : ManagedHeapAddress) : int =
                            let obj = state.ManagedHeap.Arrays.[addr]

                            if obj.Length = 0 then
                                0
                            else
                                CliType.sizeOf obj.Elements.[0]

                        baseSrc
                        |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset byteDelta)
                        |> ManagedPointerSource.normaliseArrayByteOffset cellSizeOf
                        |> EvalStackValue.ManagedPointer
                    else
                        if tSize <> arrElementSize then
                            failwith
                                $"TODO: Unsafe.Add where element size of T (%d{tSize}) differs from underlying array element size (%d{arrElementSize}) without a trailing ReinterpretAs projection"

                        for p in projs do
                            match p with
                            | ByrefProjection.ReinterpretAs _ -> ()
                            | _ -> failwith $"TODO: Unsafe.Add on byref with non-ReinterpretAs projection: %O{p}"

                        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i + offset), projs)
                        |> EvalStackValue.ManagedPointer
                | _ -> failwith $"TODO: Unsafe.Add on non-plain-array-element byref: %O{src}"

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
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

            // ByteOffset measures the byte distance between two byref address
            // targets. The generic T on the method is only the static view
            // through which each byref was declared; reinterpreting a byref
            // doesn't move it, so the size used here must come from the
            // underlying array's true element size, not T. Trailing
            // address-preserving `ReinterpretAs` projections are therefore safe
            // to ignore when extracting `(arr, index)`.
            // Returns (arr, cellIndex, byteOffset). A trailing ByteOffset (from
            // byte-view pointer arithmetic) contributes to the absolute byte
            // address; interior projections must still be plain
            // `ReinterpretAs` (address-preserving) for the answer to be the
            // simple stride*delta formula below.
            let extractArrayElement (v : EvalStackValue) : ManagedHeapAddress * int * int =
                let src =
                    match v with
                    | EvalStackValue.ManagedPointer p -> p
                    | _ -> failwith $"TODO: Unsafe.ByteOffset on non-ManagedPointer: %O{v}"

                match src with
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) ->
                    let mutable byteOff = 0

                    for p in projs do
                        match p with
                        | ByrefProjection.ReinterpretAs _ -> ()
                        | ByrefProjection.ByteOffset n -> byteOff <- byteOff + n
                        | _ -> failwith $"TODO: Unsafe.ByteOffset on byref with non-ReinterpretAs projection: %O{p}"

                    arr, i, byteOff
                | _ -> failwith $"TODO: Unsafe.ByteOffset on non-plain-array-element byref: %O{v}"

            let arr1, i1, byteOff1 = extractArrayElement origin
            let arr2, i2, byteOff2 = extractArrayElement target

            // `Array.Empty<T>()` carries no stored element to read a size from,
            // but the statically-declared `T` on the method gives the same
            // answer for any byref the caller could legally have obtained: both
            // parameters are `ref T`, so the natural per-element stride is
            // `sizeof(T)`. `MemoryMarshal.GetArrayDataReference` and zero-length
            // span helpers rely on `ByteOffset` working for empty arrays.
            let tSize, state =
                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                CliType.sizeOf tZero, state

            let arrElementSize (arr : ManagedHeapAddress) : int =
                let arrObj = state.ManagedHeap.Arrays.[arr]

                if arrObj.Length = 0 then
                    tSize
                else
                    CliType.sizeOf arrObj.Elements.[0]

            // Same-array ByteOffset is an honest byte delta and composes
            // correctly with Unsafe.Add / further arithmetic. Cross-array
            // ByteOffset has no principled byte distance in our model (we
            // don't map heap addresses to integers), so we synthesise a
            // deterministic sentinel large enough to defeat the unsigned
            // overlap check `(nuint)offset < len` used by Memmove, and mark
            // it as `SyntheticCrossArrayOffset`. The tag makes any subsequent
            // `add`/`sub` fail loudly via BinaryArithmetic.execute's
            // "refusing to operate on non-verbatim native int" branch, rather
            // than silently composing into a wrong answer.
            if arr1 = arr2 then
                let byteOffset =
                    int64 (i2 - i1) * int64 (arrElementSize arr1) + int64 (byteOff2 - byteOff1)

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim byteOffset))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            else
                let originOffset = int64 i1 * int64 (arrElementSize arr1) + int64 byteOff1
                let targetOffset = int64 i2 * int64 (arrElementSize arr2) + int64 byteOff2

                let byteOffset =
                    NativeIntSource.syntheticCrossArrayByteOffset arr1 originOffset arr2 targetOffset

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt byteOffset) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
        | "System.Private.CoreLib", "RuntimeHelpers", "CreateSpan" ->
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            None
        | "System.Private.CoreLib", "Type", "op_Equality" ->
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L703
            None
        | "System.Private.CoreLib", "MemoryMarshal", "GetArrayDataReference" ->
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/coreclr/System.Private.CoreLib/src/System/Runtime/InteropServices/MemoryMarshal.CoreCLR.cs#L20
            let generic = Seq.exactlyOne methodToCall.Generics

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteGenericArray state.ConcreteTypes generic ], ConcreteByref t when t = generic -> ()
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
            // directly (the IL body calls Object.GetType() which we don't implement).
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
                        | CliNumericType.Int64 i -> i
                        | CliNumericType.Int8 i -> int64 i
                        | CliNumericType.UInt8 i -> int64 i
                        | CliNumericType.Int16 i -> int64 i
                        | CliNumericType.UInt16 i -> int64 i
                        | other -> failwith $"Enum.HasFlag: unexpected underlying numeric type %O{other}"

                    let extractInt (contents : CliValueType) : int64 =
                        match CliValueType.DereferenceField "value__" contents with
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
        | a, b, c -> failwith $"TODO: implement JIT intrinsic {a}.{b}.{c}"
        |> Option.map (fun s -> s.WithThreadSwitchedToAssembly callerAssy currentThread |> fst)
