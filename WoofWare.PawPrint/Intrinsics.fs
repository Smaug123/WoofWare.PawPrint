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

            // The thing on top of the stack will be a RuntimeType.
            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                let rec go (arg : EvalStackValue) =
                    match arg with
                    | EvalStackValue.UserDefinedValueType vt ->
                        match CliValueType.TryExactlyOneField vt with
                        | None -> failwith "TODO"
                        | Some field -> go (EvalStackValue.ofCliType field.Contents)
                    | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                    | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
                    | EvalStackValue.ObjectRef addr -> Some addr
                    | s -> failwith $"TODO: called with unrecognised arg %O{s}"

                go arg

            let state =
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
                    |> CliValueType.OfFields
                        (AllConcreteTypes.findExistingNonGenericConcreteType
                            state.ConcreteTypes
                            baseClassTypes.RuntimeTypeHandle.Identity
                         |> Option.get)
                        Layout.Default

                IlMachineState.pushToEvalStack (CliType.ValueType vt) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "Type", "get_IsValueType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], ConcreteBool state.ConcreteTypes -> ()
            | _ -> failwith "bad signature Type.get_IsValueType"

            let this, state = IlMachineState.popEvalStack currentThread state

            let this =
                match this with
                | EvalStackValue.ObjectRef ptr ->
                    IlMachineState.readManagedByref state (ManagedPointerSource.Byref (ByrefRoot.HeapValue ptr, []))
                | EvalStackValue.ManagedPointer ptr -> IlMachineState.readManagedByref state ptr
                | EvalStackValue.NullObjectRef -> failwith "TODO: throw NRE"
                | EvalStackValue.Float _
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _ -> failwith "refusing to dereference literal"
                | _ -> failwith "TODO"
            // `this` should be of type Type
            let ty =
                match this with
                | CliType.ValueType cvt ->
                    match CliValueType.DereferenceField "m_handle" cvt with
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr cth)) -> cth
                    | _ -> failwith ""
                | _ -> failwith "expected a Type"

            let ty = AllConcreteTypes.lookup ty state.ConcreteTypes |> Option.get
            let ty = state.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get]

            let isValueType =
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty

            IlMachineState.pushToEvalStack (CliType.ofBool isValueType) currentThread state
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

                let rec toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                    match v with
                    | EvalStackValue.NativeInt src -> src
                    | EvalStackValue.Int64 i -> NativeIntSource.Verbatim i
                    | EvalStackValue.Int32 i -> NativeIntSource.Verbatim (int64<int> i)
                    | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                    | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                    | EvalStackValue.UserDefinedValueType vt ->
                        // An IntPtr struct wrapping a single native-int field.
                        match CliValueType.TryExactlyOneField vt with
                        | Some field -> toNativeIntSource (EvalStackValue.ofCliType field.Contents)
                        | None ->
                            failwith
                                $"Interlocked.CompareExchange(ref IntPtr,...): expected IntPtr struct with one field, got %O{vt}"
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref IntPtr,...): unexpected IntPtr-shaped eval stack value %O{other}"

                let comparandSrc = toNativeIntSource comparand
                let valueSrc = toNativeIntSource value

                let currentValue = IlMachineState.readManagedByref state byrefSrc

                // `ref IntPtr` derefs to the IntPtr struct. Dig through to the underlying
                // NativeInt field for CAS, then rewrap on write.
                let rec extractNativeInt (v : CliType) : NativeIntSource =
                    match v with
                    | CliType.Numeric (CliNumericType.NativeInt src) -> src
                    | CliType.Numeric (CliNumericType.Int64 i) -> NativeIntSource.Verbatim i
                    | CliType.ValueType vt ->
                        match CliValueType.TryExactlyOneField vt with
                        | Some field -> extractNativeInt field.Contents
                        | None ->
                            failwith
                                $"Interlocked.CompareExchange(ref IntPtr,...): expected IntPtr struct with one field at byref target, got %O{vt}"
                    | other ->
                        failwith
                            $"Interlocked.CompareExchange(ref IntPtr,...): expected NativeInt at byref target, got %O{other}"

                let rec rewrapNativeInt (shape : CliType) (newSrc : NativeIntSource) : CliType =
                    match shape with
                    | CliType.Numeric (CliNumericType.NativeInt _) -> CliType.Numeric (CliNumericType.NativeInt newSrc)
                    | CliType.Numeric (CliNumericType.Int64 _) ->
                        match newSrc with
                        | NativeIntSource.Verbatim i -> CliType.Numeric (CliNumericType.Int64 i)
                        | _ ->
                            failwith
                                "Interlocked.CompareExchange(ref IntPtr,...): refusing to downcast non-verbatim NativeIntSource to Int64"
                    | CliType.ValueType vt ->
                        match CliValueType.TryExactlyOneField vt with
                        | Some field ->
                            let updated = rewrapNativeInt field.Contents newSrc

                            CliType.ValueType (CliValueType.WithFieldSet field.Name updated vt)
                        | None ->
                            failwith
                                $"Interlocked.CompareExchange(ref IntPtr,...): cannot rewrap into non-single-field struct %O{vt}"
                    | other -> failwith $"Interlocked.CompareExchange(ref IntPtr,...): cannot rewrap into %O{other}"

                let currentSrc = extractNativeInt currentValue

                // Two representations of zero exist (`Verbatim 0L` for `new IntPtr(0)` and
                // `ManagedPointer Null` for default-initialised IntPtr / `IntPtr.Zero`); treat
                // them as equal, matching native-int `ceq` semantics.
                let nativeIntEq (a : NativeIntSource) (b : NativeIntSource) : bool =
                    a = b || (NativeIntSource.isZero a && NativeIntSource.isZero b)

                let state =
                    if nativeIntEq currentSrc comparandSrc then
                        let newValue = rewrapNativeInt currentValue valueSrc
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
            let ptr, state = IlMachineState.popEvalStack currentThread state

            let v : CliType =
                let rec go ptr =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref state src
                    | EvalStackValue.NativeInt src -> failwith "TODO"
                    | EvalStackValue.NullObjectRef -> failwith "TODO: ReadUnaligned on null"
                    | EvalStackValue.ObjectRef ptr -> failwith "TODO"
                    | EvalStackValue.UserDefinedValueType vt ->
                        match CliValueType.TryExactlyOneField vt with
                        | None -> failwith "TODO"
                        | Some field -> go (EvalStackValue.ofCliType field.Contents)
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a pointer! {ptr}"

                go ptr

            let state =
                state
                |> IlMachineState.pushToEvalStack v currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
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

            // Navigate the RuntimeFieldHandle struct to find the RuntimeFieldInfoStub address.
            // RuntimeFieldHandle is a struct with one field: m_ptr (an ObjectRef to RuntimeFieldInfoStub)
            let runtimeFieldInfoStubAddr : ManagedHeapAddress =
                match fldHandle with
                | EvalStackValue.UserDefinedValueType vt ->
                    match CliValueType.DereferenceField "m_ptr" vt with
                    | CliType.ObjectRef (Some addr) -> addr
                    | CliType.ObjectRef None ->
                        failwith "TODO: throw ArgumentException for InitializeArray with null field handle"
                    | other -> failwith $"InitializeArray: unexpected m_ptr contents: %O{other}"
                | other -> failwith $"InitializeArray: expected RuntimeFieldHandle value type, got %O{other}"

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
