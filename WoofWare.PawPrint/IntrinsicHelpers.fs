namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

module internal IntrinsicHelpers =
    type RefTypeProcessingStatus =
        | InProgress
        | Completed of bool

    type RefTypeKey =
        {
            Identity : ResolvedTypeIdentity
            Generics : TypeDefn list
        }

    let refTypeKey (td : TypeInfo<TypeDefn, TypeDefn>) : RefTypeKey =
        {
            Identity = td.Identity
            Generics = Seq.toList td.Generics
        }

    let rec containsRefType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (seenSoFar : ImmutableDictionary<RefTypeKey, RefTypeProcessingStatus>)
        (td : TypeInfo<TypeDefn, TypeDefn>)
        : IlMachineState * ImmutableDictionary<_, RefTypeProcessingStatus> * bool
        =
        let key = refTypeKey td

        match seenSoFar.TryGetValue key with
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
                let seenSoFar = seenSoFar.Add (key, Completed true)
                state, seenSoFar, true
            else
                // It's a value type, so we must check its fields.
                // Mark as in progress before recursing.
                let seenSoFarWithInProgress = seenSoFar.Add (key, InProgress)

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
                let finalSeenSoFar = finalSeenSoFar.SetItem (key, Completed fieldsContainRefType)
                finalState, finalSeenSoFar, fieldsContainRefType

    let concreteTypeContainsReferences
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> state, false
        | ConcreteTypeHandle.Concrete _ ->
            let concrete =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"type was not registered: %O{handle}")

            let primitiveValueTypeNames =
                set
                    [
                        "Boolean"
                        "Byte"
                        "SByte"
                        "Char"
                        "Int16"
                        "UInt16"
                        "Int32"
                        "UInt32"
                        "Int64"
                        "UInt64"
                        "IntPtr"
                        "UIntPtr"
                        "Single"
                        "Double"
                    ]

            if
                concrete.Assembly.Name = "System.Private.CoreLib"
                && concrete.Namespace = "System"
                && primitiveValueTypeNames.Contains concrete.Name
            then
                state, false
            else
                let td =
                    state.LoadedAssembly concrete.Assembly
                    |> Option.get
                    |> fun a -> a.TypeDefs.[concrete.Definition.Get]

                if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies td then
                    td
                    |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
                    |> containsRefType loggerFactory baseClassTypes state ImmutableDictionary.Empty
                    |> fun (state, _, result) -> state, result
                else
                    state, true

    let typeInfoContainsReferences
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<TypeDefn, TypeDefn>)
        : IlMachineState * bool
        =
        typeInfo
        |> containsRefType loggerFactory baseClassTypes state ImmutableDictionary.Empty
        |> fun (state, _, result) -> state, result

    let popRuntimeTypeHandle
        (currentThread : ThreadId)
        (state : IlMachineState)
        : RuntimeTypeHandleTarget * IlMachineState
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
                let handleField =
                    IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "m_handle"

                match CliValueType.DereferenceFieldById handleField cvt |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
                | other ->
                    failwith
                        $"Type intrinsic receiver: expected RuntimeType.m_handle to contain a TypeHandlePtr, got %O{other}"
            | other -> failwith $"Type intrinsic receiver: expected RuntimeType value contents, got %O{other}"

        ty, state

    /// Compute `src + offset` worth of element-T steps over a byref source.
    /// The input byref may or may not carry an address-preserving
    /// `ReinterpretAs` projection (from an `Unsafe.As` or a round-trip).
    /// We can only do element-index arithmetic if `sizeof(T)` matches the
    /// underlying storage's true cell size (the array's element size, or
    /// 2 bytes for a string char): otherwise advancing by `offset` elements
    /// of T is not a whole-cell step in the underlying storage. Any
    /// existing trailing reinterprets must also only be size-preserving,
    /// and they stay on the result so that later field access / As chains
    /// still see the type view the caller set up.
    let offsetManagedPointerByElements
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (elementType : ConcreteTypeHandle)
        (offset : int)
        (src : EvalStackValue)
        : EvalStackValue * IlMachineState
        =
        // Thread the state returned by `cliTypeZeroOfHandle`: for a struct T
        // it can concretise additional types, and discarding the update would
        // drop that work from the machine state.
        let tZero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementType

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
                //     trailing `ReinterpretAs` to anchor the view, since plain
                //     cell byrefs aren't byte-addressable.
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
                // consumers (`ReadUnaligned`, `WriteUnaligned`, `ByteOffset`)
                // handle. If the existing projection list contains anything
                // other than `ReinterpretAs` or `ByteOffset`, appending another
                // `ByteOffset` would manufacture a pointer the downstream code
                // can't consume.
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

                    let normalisationElementSize =
                        let obj = state.ManagedHeap.Arrays.[arr]

                        if obj.Length = 0 then 0 else arrElementSize

                    let normalisation =
                        ByteOffsetNormalisationContext.withArrayElementSize arr normalisationElementSize

                    baseSrc
                    |> ManagedPointerSource.addByteOffsetToByteView normalisation byteDelta
                    |> EvalStackValue.ManagedPointer
                else
                    if tSize <> arrElementSize then
                        failwith
                            $"TODO: byref element offset where element size of T (%d{tSize}) differs from underlying array element size (%d{arrElementSize}) without a trailing ReinterpretAs projection"

                    for p in projs do
                        match p with
                        | ByrefProjection.ReinterpretAs _ -> ()
                        | _ -> failwith $"TODO: byref element offset on byref with non-ReinterpretAs projection: %O{p}"

                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i + offset), projs)
                    |> EvalStackValue.ManagedPointer
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, i), projs) as src) ->
                let stringCharSize = 2

                let trailingIsByteOffset =
                    match List.tryLast projs with
                    | Some (ByrefProjection.ByteOffset _) -> true
                    | _ -> false

                let trailingIsReinterpretAs =
                    match List.tryLast projs with
                    | Some (ByrefProjection.ReinterpretAs _) -> true
                    | _ -> false

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
                    && (trailingIsByteOffset || (tSize <> stringCharSize && trailingIsReinterpretAs))
                then
                    let normalisation = ByteOffsetNormalisationContext.nonArrayRootsOnly

                    src
                    |> ManagedPointerSource.addByteOffsetToByteView normalisation (tSize * offset)
                    |> EvalStackValue.ManagedPointer
                else
                    if tSize <> stringCharSize then
                        failwith
                            $"TODO: byref element offset where element size of T (%d{tSize}) differs from string char size (%d{stringCharSize}) without a trailing ReinterpretAs projection"

                    for p in projs do
                        match p with
                        | ByrefProjection.ReinterpretAs _ -> ()
                        | _ ->
                            failwith
                                $"TODO: byref element offset on string byref with non-ReinterpretAs projection: %O{p}"

                    ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, i + offset), projs)
                    |> EvalStackValue.ManagedPointer
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread,
                                                                                                    frame,
                                                                                                    block,
                                                                                                    byteOffset),
                                                                         [])) ->
                ManagedPointerSource.Byref (
                    ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset + (tSize * offset)),
                    []
                )
                |> EvalStackValue.ManagedPointer
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (_, projs) as src) ->
                let projectionsAreByteViewCompatible =
                    projs
                    |> List.forall (fun p ->
                        match p with
                        | ByrefProjection.ReinterpretAs _
                        | ByrefProjection.ByteOffset _ -> true
                        | _ -> false
                    )

                if projs <> [] && projectionsAreByteViewCompatible then
                    let normalisation = ByteOffsetNormalisationContext.nonArrayRootsOnly

                    src
                    |> ManagedPointerSource.addByteOffsetToByteView normalisation (tSize * offset)
                    |> EvalStackValue.ManagedPointer
                elif offset = 0 then
                    EvalStackValue.ManagedPointer src
                else
                    failwith
                        $"TODO: byref element offset on non-array byref without a trailing byte-view ReinterpretAs projection: %O{src}"
            | _ -> failwith $"TODO: byref element offset on non-managed-pointer: %O{src}"

        ptr, state

    let vectorAccelerationAvailable (declaringTypeName : string) (profile : HardwareIntrinsicsProfile) : bool =
        match declaringTypeName with
        | "Vector128" -> profile.Vector128
        | "Vector256" -> profile.Vector256
        | "Vector512" -> profile.Vector512
        | other -> failwith $"Unexpected vector intrinsic type name: %s{other}"

    let scalarOnlyFalseIsSupportedIntrinsics =
        set
            [
                "System.Runtime.Intrinsics.Arm.AdvSimd"
                "System.Runtime.Intrinsics.Arm.AdvSimd.Arm64"
                "System.Runtime.Intrinsics.Arm.Rdm"
                "System.Runtime.Intrinsics.Arm.Rdm.Arm64"
                "System.Runtime.Intrinsics.X86.Ssse3"
            ]

    let byteTemplate : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

    let byteConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Byte handle %O{handle} not found")

    let checkedByteCount (operation : string) (count : int64) : int =
        if count < 0L then
            failwith $"%s{operation}: byte count %d{count} is negative"

        if count > int64 System.Int32.MaxValue then
            failwith $"%s{operation}: byte count %d{count} exceeds the interpreter Int32 byte-offset model"

        int count

    let byteCountOfStackValue (operation : string) (arg : EvalStackValue) : int =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim count) -> checkedByteCount operation count
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %d{count}, which is not a valid UIntPtr length"
        | EvalStackValue.Int64 count -> checkedByteCount operation count
        | EvalStackValue.Int32 count -> checkedByteCount operation (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr byte count, got %O{other}"

    let splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
        match src with
        | ManagedPointerSource.Null -> ValueNone
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revPrefix ->
                ValueSome (root, List.rev revPrefix, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs _ :: revPrefix -> ValueSome (root, List.rev revPrefix, 0)
            | _ -> ValueNone

    let byteAtOffset (operation : string) (src : ManagedPointerSource) (byteOffset : int) (value : CliType) : byte =
        if byteOffset < 0 then
            failwith $"%s{operation}: negative byte offset %d{byteOffset} through %O{src}"

        match value with
        | CliType.ValueType vt when not (CliValueType.IsTightlyPacked vt) ->
            failwith $"%s{operation}: refusing to byte-compare non-tightly-packed value type %O{vt.Declared}"
        | _ -> ()

        try
            CliType.BytesAt byteOffset 1 value |> Array.exactlyOne
        with ex ->
            failwith $"%s{operation}: %s{ex.Message}"

    let readSpanHelpersSequenceEqualByte
        (operation : string)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        : byte
        =
        let readPrimitiveByteView () : byte =
            match IlMachineState.readManagedByrefBytesAs state src byteTemplate with
            | CliType.Numeric (CliNumericType.UInt8 b) -> b
            | other -> failwith $"%s{operation}: byte-view read returned non-byte value %O{other}"

        match src with
        | ManagedPointerSource.Null -> failwith $"%s{operation}: attempted to dereference null byref"
        | ManagedPointerSource.Byref (root, projs) ->
            match splitTrailingByteView src with
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                match byteViewRoot, prefixProjs with
                | ByrefRoot.ArrayElement _, []
                | ByrefRoot.LocalMemoryByte _, []
                | ByrefRoot.PeByteRange _, []
                | ByrefRoot.StringCharAt _, [] -> readPrimitiveByteView ()
                | _ ->
                    let basePtr = ManagedPointerSource.Byref (byteViewRoot, prefixProjs)
                    let value = IlMachineState.readManagedByref state basePtr

                    match value with
                    | CliType.ValueType _ -> byteAtOffset operation src byteOffset value
                    | _ -> readPrimitiveByteView ()
            | ValueNone ->
                let value =
                    IlMachineState.readManagedByref state (ManagedPointerSource.Byref (root, projs))

                byteAtOffset operation src 0 value

    let managedPointerOfPointerArgument (operation : string) (arg : EvalStackValue) : ManagedPointerSource =
        match arg with
        | EvalStackValue.ManagedPointer ptr -> ptr
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> ptr
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
            failwith $"%s{operation}: refusing to dereference unmanaged pointer value %d{i}"
        | other -> failwith $"%s{operation}: expected a pointer argument, got %O{other}"

    let isSpanHelpersByteSequenceEqual
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : bool
        =
        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> true
        | _ -> false

    let spanHelpersSequenceEqual
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
        | _ -> failwith $"bad signature for SpanHelpers.SequenceEqual: %A{methodToCall.Signature}"

        let operation = "SpanHelpers.SequenceEqual"

        let byteCountArg, state = IlMachineState.popEvalStack currentThread state
        let rightArg, state = IlMachineState.popEvalStack currentThread state
        let leftArg, state = IlMachineState.popEvalStack currentThread state

        let byteCount = byteCountOfStackValue operation byteCountArg

        let result =
            if byteCount = 0 then
                true
            else
                let byteType = byteConcreteType operation baseClassTypes state
                let leftPtr = managedPointerOfPointerArgument operation leftArg
                let rightPtr = managedPointerOfPointerArgument operation rightArg
                let mutable equal = true
                let mutable i = 0

                while equal && i < byteCount do
                    let left =
                        ManagedPointerByteView.addByteOffset baseClassTypes state byteType i leftPtr

                    let right =
                        ManagedPointerByteView.addByteOffset baseClassTypes state byteType i rightPtr

                    equal <-
                        readSpanHelpersSequenceEqualByte operation state left = readSpanHelpersSequenceEqualByte
                            operation
                            state
                            right

                    i <- i + 1

                equal

        state
        |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
        |> IlMachineState.advanceProgramCounter currentThread

    let popPointerBackedSpanConstructorArgs
        (currentThread : ThreadId)
        (wasConstructing : ManagedHeapAddress option)
        (state : IlMachineState)
        : ManagedPointerSource * ManagedPointerSource * int * IlMachineState
        =
        match wasConstructing with
        | Some _ ->
            let thisArg, state = IlMachineState.popEvalStack currentThread state
            let lengthArg, state = IlMachineState.popEvalStack currentThread state
            let sourceArg, state = IlMachineState.popEvalStack currentThread state

            let thisPtr =
                match thisArg with
                | EvalStackValue.ManagedPointer ptr -> ptr
                | other -> failwith $"Span pointer constructor expected managed byref `this`, got %O{other}"

            let length =
                match lengthArg with
                | EvalStackValue.Int32 i -> i
                | other -> failwith $"Span pointer constructor expected int length, got %O{other}"

            let sourcePtr = managedPointerOfPointerArgument "Span pointer constructor" sourceArg

            thisPtr, sourcePtr, length, state
        | None ->
            let lengthArg, state = IlMachineState.popEvalStack currentThread state
            let sourceArg, state = IlMachineState.popEvalStack currentThread state
            let thisArg, state = IlMachineState.popEvalStack currentThread state

            let thisPtr =
                match thisArg with
                | EvalStackValue.ManagedPointer ptr -> ptr
                | other -> failwith $"Span pointer constructor expected managed byref `this`, got %O{other}"

            let length =
                match lengthArg with
                | EvalStackValue.Int32 i -> i
                | other -> failwith $"Span pointer constructor expected int length, got %O{other}"

            let sourcePtr = managedPointerOfPointerArgument "Span pointer constructor" sourceArg

            thisPtr, sourcePtr, length, state

    let intrinsicDeclaringTypeHandle
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : ConcreteTypeHandle
        =
        AllConcreteTypes.findExistingConcreteType
            state.ConcreteTypes
            methodToCall.DeclaringType.Identity
            methodToCall.DeclaringType.Generics
        |> Option.defaultWith (fun () ->
            failwith
                $"Intrinsic method declaring type was not registered: %s{methodToCall.DeclaringType.Namespace}.%s{methodToCall.DeclaringType.Name}"
        )

    let popManagedByrefArgument (operation : string) (arg : EvalStackValue) : ManagedPointerSource =
        match arg with
        | EvalStackValue.ManagedPointer ptr -> ptr
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
        | other -> failwith $"%s{operation}: expected managed byref argument, got %O{other}"

    let writePointerBackedSpanConstructor
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (wasConstructing : ManagedHeapAddress option)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        let elementType = methodToCall.DeclaringType.Generics |> Seq.exactlyOne

        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [ ConcretePointer _ ; ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Void -> ()
        | _ ->
            failwith
                $"bad signature for %s{IntrinsicMethodKeys.formatMethodKey (IntrinsicMethodKeys.methodKey state methodToCall)}"

        let state, elementContainsRefs =
            concreteTypeContainsReferences loggerFactory baseClassTypes state elementType

        if elementContainsRefs then
            failwith
                $"TODO: %s{methodToCall.DeclaringType.Name}(void*, int) with reference-containing element type should throw ArgumentException"

        let thisPtr, sourcePtr, length, state =
            popPointerBackedSpanConstructorArgs currentThread wasConstructing state

        if length < 0 then
            failwith
                $"TODO: %s{methodToCall.DeclaringType.Name}(void*, int) with negative length should throw ArgumentOutOfRangeException"

        let elementTypeInfo =
            match AllConcreteTypes.lookup elementType state.ConcreteTypes with
            | Some info -> info
            | None -> failwith $"Span pointer constructor element type was not registered: %O{elementType}"

        let sourcePtr =
            match sourcePtr with
            | ManagedPointerSource.Null -> ManagedPointerSource.Null
            | sourcePtr ->
                ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs elementTypeInfo) sourcePtr

        let declaringTypeHandle = intrinsicDeclaringTypeHandle state methodToCall

        let span =
            match IlMachineState.readManagedByref state thisPtr with
            | CliType.ValueType vt when vt.Declared = declaringTypeHandle -> vt
            | CliType.ValueType vt ->
                failwith
                    $"Span pointer constructor `this` pointed at value type %O{vt.Declared}, expected %O{declaringTypeHandle}"
            | other -> failwith $"Span pointer constructor `this` pointed at non-value-type %O{other}"

        let referenceField =
            IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

        let lengthField =
            IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

        let referenceValue =
            EvalStackValue.toCliTypeCoerced
                (CliValueType.DereferenceFieldById referenceField span)
                (EvalStackValue.ManagedPointer sourcePtr)

        let lengthValue =
            EvalStackValue.toCliTypeCoerced
                (CliValueType.DereferenceFieldById lengthField span)
                (EvalStackValue.Int32 length)

        let span =
            span
            |> CliValueType.WithFieldSetById referenceField referenceValue
            |> CliValueType.WithFieldSetById lengthField lengthValue

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state thisPtr (CliType.ValueType span)

        let state =
            match wasConstructing with
            | None -> state
            | Some constructing ->
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

                state
                |> IlMachineState.pushToEvalStack (CliType.ValueType constructed.Contents) currentThread

        state |> IlMachineState.advanceProgramCounter currentThread

    let charOfCliType (operation : string) (value : CliType) : char =
        match CliType.unwrapPrimitiveLikeDeep value with
        | CliType.Char (high, low) -> char (int high * 256 + int low)
        | CliType.Numeric (CliNumericType.UInt16 i) -> char (int<uint16> i)
        | CliType.Numeric (CliNumericType.Int16 i) -> char (int<uint16> (uint16<int16> i))
        | other -> failwith $"%s{operation}: expected char-compatible value, got %O{other}"

    let int32OfEvalStackValue (operation : string) (value : EvalStackValue) : int =
        match value with
        | EvalStackValue.Int32 i -> i
        | EvalStackValue.UserDefinedValueType vt ->
            match (CliValueType.PrimitiveLikeField vt).Contents |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.Numeric (CliNumericType.Int32 i) -> i
            | other -> failwith $"%s{operation}: expected int32-like value, got %O{other}"
        | other -> failwith $"%s{operation}: expected int32-like value, got %O{other}"

    let isCorelibConcreteType
        (state : IlMachineState)
        (ns : string)
        (name : string)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | Some ty ->
            ty.Assembly.Name = "System.Private.CoreLib"
            && ty.Namespace = ns
            && ty.Name = name
        | None -> false

    let isReadOnlySpanOfChar (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | Some ty ->
            ty.Assembly.Name = "System.Private.CoreLib"
            && ty.Namespace = "System"
            && ty.Name = "ReadOnlySpan`1"
            && ty.Generics.Length = 1
            && isCorelibConcreteType state "System" "Char" ty.Generics.[0]
        | None -> false

    let spanReceiverValue (operation : string) (state : IlMachineState) (receiver : EvalStackValue) : CliValueType =
        match receiver with
        | EvalStackValue.ManagedPointer src ->
            match IlMachineState.readManagedByref state src with
            | CliType.ValueType vt -> vt
            | other -> failwith $"%s{operation}: receiver byref read produced non-value-type %O{other}"
        | EvalStackValue.UserDefinedValueType vt -> vt
        | other -> failwith $"%s{operation}: expected span receiver byref, got %O{other}"

    let spanReferenceAndLength
        (operation : string)
        (state : IlMachineState)
        (span : CliValueType)
        : EvalStackValue * int
        =
        let referenceField =
            IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

        let reference =
            match
                CliValueType.DereferenceFieldById referenceField span
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> EvalStackValue.ManagedPointer src
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) ->
                EvalStackValue.ManagedPointer src
            | other -> failwith $"%s{operation}: expected _reference to be a managed byref, got %O{other}"

        let lengthField =
            IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

        let length =
            match
                CliValueType.DereferenceFieldById lengthField span
                |> CliType.unwrapPrimitiveLike
            with
            | CliType.Numeric (CliNumericType.Int32 i) -> i
            | other -> failwith $"%s{operation}: expected _length to be int32, got %O{other}"

        reference, length

    let readCharSpanContents
        (baseClassTypes : BaseClassTypes<_>)
        (operation : string)
        (state : IlMachineState)
        (span : CliValueType)
        : string * IlMachineState
        =
        let spanType =
            AllConcreteTypes.lookup span.Declared state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: span type %O{span.Declared} was not registered")

        if
            spanType.Assembly.Name <> "System.Private.CoreLib"
            || spanType.Namespace <> "System"
            || (spanType.Name <> "ReadOnlySpan`1" && spanType.Name <> "Span`1")
            || spanType.Generics.Length <> 1
            || not (isCorelibConcreteType state "System" "Char" spanType.Generics.[0])
        then
            failwith $"%s{operation}: expected ReadOnlySpan<char> or Span<char>, got %O{spanType}"

        let reference, length = spanReferenceAndLength operation state span

        if length < 0 then
            failwith $"%s{operation}: span length was negative: %d{length}"

        let contents, state =
            (([], state), [ 0 .. length - 1 ])
            ||> List.fold (fun (chars, state) index ->
                let ptr, state =
                    offsetManagedPointerByElements baseClassTypes state spanType.Generics.[0] index reference

                let value =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref state src
                    | other -> failwith $"%s{operation}: element pointer was not a managed pointer: %O{other}"

                charOfCliType operation value :: chars, state
            )

        System.String (contents |> List.rev |> List.toArray), state

    let spanToString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [], MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) -> ()
        | _ ->
            failwith
                $"bad signature for %s{IntrinsicMethodKeys.formatMethodKey (IntrinsicMethodKeys.methodKey state methodToCall)}"

        let operation = $"{methodToCall.DeclaringType.Name}.ToString"
        let elementType = methodToCall.DeclaringType.Generics |> Seq.exactlyOne
        let receiver, state = IlMachineState.popEvalStack currentThread state
        let span = spanReceiverValue operation state receiver
        let reference, length = spanReferenceAndLength operation state span

        if length < 0 then
            failwith $"%s{operation}: span length was negative: %d{length}"

        let elementTypeInfo =
            AllConcreteTypes.lookup elementType state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: element type %O{elementType} was not registered")

        let contents, state =
            if
                elementTypeInfo.Assembly.Name = "System.Private.CoreLib"
                && elementTypeInfo.Namespace = "System"
                && elementTypeInfo.Name = "Char"
            then
                (([], state), [ 0 .. length - 1 ])
                ||> List.fold (fun (chars, state) index ->
                    let ptr, state =
                        offsetManagedPointerByElements baseClassTypes state elementType index reference

                    let value =
                        match ptr with
                        | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref state src
                        | other -> failwith $"%s{operation}: element pointer was not a managed pointer: %O{other}"

                    charOfCliType operation value :: chars, state
                )
                |> fun (chars, state) -> System.String (chars |> List.rev |> List.toArray), state
            else
                let typeKind =
                    if methodToCall.DeclaringType.Name = "ReadOnlySpan`1" then
                        "ReadOnlySpan"
                    else
                        "Span"

                $"System.%s{typeKind}<%s{elementTypeInfo.Name}>[%d{length}]", state

        let stringAddr, state =
            IlMachineState.allocateManagedString loggerFactory baseClassTypes contents state

        state
        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some stringAddr)) currentThread
        |> IlMachineState.advanceProgramCounter currentThread

    let memoryExtensionsEquals
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [ leftSpan ; rightSpan ; comparisonType ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) when
            isReadOnlySpanOfChar state leftSpan
            && isReadOnlySpanOfChar state rightSpan
            && isCorelibConcreteType state "System" "StringComparison" comparisonType
            ->
            ()
        | _ ->
            failwith
                $"bad signature for %s{IntrinsicMethodKeys.formatMethodKey (IntrinsicMethodKeys.methodKey state methodToCall)}"

        let operation =
            "MemoryExtensions.Equals(ReadOnlySpan<char>, ReadOnlySpan<char>, StringComparison)"

        let comparisonType, state = IlMachineState.popEvalStack currentThread state
        let right, state = IlMachineState.popEvalStack currentThread state
        let left, state = IlMachineState.popEvalStack currentThread state

        let comparisonType = int32OfEvalStackValue operation comparisonType
        let left = spanReceiverValue operation state left
        let right = spanReceiverValue operation state right
        let left, state = readCharSpanContents baseClassTypes operation state left
        let right, state = readCharSpanContents baseClassTypes operation state right

        let result =
            match comparisonType with
            | 0
            | 1
            | 2
            | 3 ->
                failwith
                    $"TODO: %s{operation} with culture-sensitive StringComparison %d{comparisonType} requires deterministic culture modelling"
            | 4 -> String.Equals (left, right, StringComparison.Ordinal)
            | 5 -> String.Equals (left, right, StringComparison.OrdinalIgnoreCase)
            | _ ->
                failwith
                    $"TODO: %s{operation} with invalid StringComparison %d{comparisonType} should throw ArgumentException"

        state
        |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
        |> IlMachineState.advanceProgramCounter currentThread
