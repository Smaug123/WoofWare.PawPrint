namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Intrinsics =
    type IntrinsicMethodKey =
        {
            AssemblyName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterShapes : string list
        }

    [<RequireQualifiedAccess>]
    type private IntrinsicParameterPattern =
        | Any
        | Exact of string
        | Byref
        | Pointer
        | SzArray
        | Array

    type private IntrinsicMethodPattern =
        {
            AssemblyName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterPatterns : IntrinsicParameterPattern list option
        }

    let private pattern
        (assemblyName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        (parameterPatterns : IntrinsicParameterPattern list)
        : IntrinsicMethodPattern
        =
        {
            AssemblyName = assemblyName
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterPatterns = Some parameterPatterns
        }

    let private anyParams
        (assemblyName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        : IntrinsicMethodPattern
        =
        {
            AssemblyName = assemblyName
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterPatterns = None
        }

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringType.Assembly with
            | Some assy -> assy
            | None ->
                failwith
                    $"Intrinsic method key requested for method whose declaring assembly is not loaded: %O{methodToCall}"

        let declaringType =
            declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

        let concreteTypeShape (handle : ConcreteTypeHandle) : string =
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some ct ->
                    if String.IsNullOrEmpty ct.Namespace then
                        ct.Name
                    else
                        $"%s{ct.Namespace}.%s{ct.Name}"
                | None -> failwith $"Intrinsic method key requested for unknown concrete type handle: %O{handle}"
            | ConcreteTypeHandle.Byref _ -> "&"
            | ConcreteTypeHandle.Pointer _ -> "*"
            | ConcreteTypeHandle.OneDimArrayZero _ -> "[]"
            | ConcreteTypeHandle.Array (_, rank) -> $"[%i{rank}]"

        {
            AssemblyName = methodToCall.DeclaringType.Assembly.Name
            DeclaringTypeFullName = TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) declaringType
            MethodName = methodToCall.Name
            ParameterShapes = methodToCall.Signature.ParameterTypes |> List.map concreteTypeShape
        }

    let formatMethodKey (key : IntrinsicMethodKey) : string =
        let parameters = key.ParameterShapes |> String.concat ", "
        $"%s{key.AssemblyName} %s{key.DeclaringTypeFullName}.%s{key.MethodName}(%s{parameters})"

    let private parameterPatternMatches (pattern : IntrinsicParameterPattern) (actual : string) : bool =
        match pattern with
        | IntrinsicParameterPattern.Any -> true
        | IntrinsicParameterPattern.Exact expected -> expected = actual
        | IntrinsicParameterPattern.Byref -> actual = "&"
        | IntrinsicParameterPattern.Pointer -> actual = "*"
        | IntrinsicParameterPattern.SzArray -> actual = "[]"
        | IntrinsicParameterPattern.Array -> actual.StartsWith ("[", StringComparison.Ordinal)

    let private methodPatternMatches (pattern : IntrinsicMethodPattern) (key : IntrinsicMethodKey) : bool =
        pattern.AssemblyName = key.AssemblyName
        && pattern.DeclaringTypeFullName = key.DeclaringTypeFullName
        && pattern.MethodName = key.MethodName
        && match pattern.ParameterPatterns with
           | None -> true
           | Some patterns ->
               List.length patterns = List.length key.ParameterShapes
               && List.forall2 parameterPatternMatches patterns key.ParameterShapes

    let private safeIntrinsics =
        [
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            pattern "System.Private.CoreLib" "System.String" "get_Length" []
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L728-L737
            pattern
                "System.Private.CoreLib"
                "System.String"
                "get_Chars"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            // IL body is `ldarg.0; ldflda _firstChar; ret`; PawPrint projects `_firstChar`
            // to the string character data side-table.
            pattern "System.Private.CoreLib" "System.String" "GetRawStringData" []
            pattern "System.Private.CoreLib" "System.String" "GetRawStringDataAsUInt16" []
            // IL body constructs a span over the string contents; PawPrint's string field
            // projection handles the `_firstChar` boundary it depends on.
            pattern
                "System.Private.CoreLib"
                "System.String"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.String" ]
            // String overloads bottom out in String.GetRawStringData plus ReadOnlySpan construction.
            anyParams "System.Private.CoreLib" "System.MemoryExtensions" "AsSpan"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            anyParams "System.Private.CoreLib" "System.ArgumentNullException" "ThrowIfNull"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "GetTypeFromHandle"
                [ IntrinsicParameterPattern.Exact "System.RuntimeTypeHandle" ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L703
            // Managed IL bodies with RuntimeType fast paths before Equals; op_Inequality delegates to op_Equality.
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.Type"
                    IntrinsicParameterPattern.Exact "System.Type"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Type"
                    IntrinsicParameterPattern.Exact "System.Type"
                ]
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L161
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_Length" []
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_IsEmpty" []
            // Reviewed constructors initialise `_reference` / `_length` through already-modelled
            // array and byref boundaries. The `(void*, int)` constructor is an explicit
            // intrinsic implementation below because it crosses the unmanaged-pointer boundary.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            // IL body delegates to the array-backed constructor above.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "op_Implicit" [ IntrinsicParameterPattern.SzArray ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // ReadOnlySpan<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "ToArray" []
            // IL body is `ldarg.0; ldfld _length; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_Length" []
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_IsEmpty" []
            // Same constructor shape as ReadOnlySpan<T>; the `(void*, int)` constructor is
            // handled explicitly below.
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // IL body constructs ReadOnlySpan<T> over this span's `_reference` and `_length`.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // Span<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern "System.Private.CoreLib" "System.Span`1" "Slice" [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.Span`1" "ToArray" []
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            anyParams "System.Private.CoreLib" "System.Runtime.CompilerServices.RuntimeHelpers" "CreateSpan"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L127
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L137
            anyParams "System.Private.CoreLib" "System.Math" "Abs"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L965C10-L1062C19
            anyParams "System.Private.CoreLib" "System.Math" "Max"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Buffer.cs#L150
            anyParams "System.Private.CoreLib" "System.Buffer" "Memmove"
            // Managed fast paths use Unsafe.ReadUnaligned/WriteUnaligned; the native fallback remains
            // a future boundary.
            anyParams "System.Private.CoreLib" "System.SpanHelpers" "Memmove"
            // https://github.com/dotnet/runtime/blob/1c3221b63340d7f81dfd829f3bcd822e582324f6/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L799
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_CurrentThread" []
            // IL body is `ldarg.0; ldfld _managedThreadId; ret` — pure field access.
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_ManagedThreadId" []
            // IL body is `ldsfld <Default>k__BackingField; ret`; the .cctor constructs the comparer.
            pattern "System.Private.CoreLib" "System.Collections.Generic.EqualityComparer`1" "get_Default" []
            // Volatile.Read/Write wrappers are managed field accesses through volatile struct
            // views. PawPrint does not currently model memory-ordering effects, but executing
            // the IL is deterministic and preserves the accessed value.
            pattern "System.Private.CoreLib" "System.Threading.Volatile" "Read" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.Threading.Volatile"
                "Write"
                [ IntrinsicParameterPattern.Byref ; IntrinsicParameterPattern.Any ]
            pattern "System.Private.CoreLib" "System.Threading.Volatile" "ReadBarrier" []
            pattern "System.Private.CoreLib" "System.Threading.Volatile" "WriteBarrier" []
        ]

    let isSafeIntrinsic (key : IntrinsicMethodKey) : bool =
        safeIntrinsics |> List.exists (fun pattern -> methodPatternMatches pattern key)

    type private RefTypeProcessingStatus =
        | InProgress
        | Completed of bool

    type private RefTypeKey =
        {
            Identity : ResolvedTypeIdentity
            Generics : TypeDefn list
        }

    let private refTypeKey (td : TypeInfo<TypeDefn, TypeDefn>) : RefTypeKey =
        {
            Identity = td.Identity
            Generics = Seq.toList td.Generics
        }

    let rec private containsRefType
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

    let private concreteTypeContainsReferences
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

    let private popRuntimeTypeHandle
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
    let private offsetManagedPointerByElements
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
                    src
                    |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset (tSize * offset))
                    |> ManagedPointerSource.normaliseStringByteOffset
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
                    // Non-array byte views have no repeatable cell stride, so
                    // normaliseArrayByteOffset is only meaningful for array roots.
                    src
                    |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset (tSize * offset))
                    |> EvalStackValue.ManagedPointer
                elif offset = 0 then
                    EvalStackValue.ManagedPointer src
                else
                    failwith
                        $"TODO: byref element offset on non-array byref without a trailing byte-view ReinterpretAs projection: %O{src}"
            | _ -> failwith $"TODO: byref element offset on non-managed-pointer: %O{src}"

        ptr, state

    let private vectorAccelerationAvailable (declaringTypeName : string) (profile : HardwareIntrinsicsProfile) : bool =
        match declaringTypeName with
        | "Vector128" -> profile.Vector128
        | "Vector256" -> profile.Vector256
        | "Vector512" -> profile.Vector512
        | other -> failwith $"Unexpected vector intrinsic type name: %s{other}"

    let private scalarOnlyFalseIsSupportedIntrinsics =
        set
            [
                "System.Runtime.Intrinsics.Arm.AdvSimd"
                "System.Runtime.Intrinsics.Arm.AdvSimd.Arm64"
                "System.Runtime.Intrinsics.Arm.Rdm"
                "System.Runtime.Intrinsics.Arm.Rdm.Arm64"
            ]

    let private byteTemplate : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

    let private byteConcreteType
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

    let private checkedByteCount (operation : string) (count : int64) : int =
        if count < 0L then
            failwith $"%s{operation}: byte count %d{count} is negative"

        if count > int64 System.Int32.MaxValue then
            failwith $"%s{operation}: byte count %d{count} exceeds the interpreter Int32 byte-offset model"

        int count

    let private byteCountOfStackValue (operation : string) (arg : EvalStackValue) : int =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim count) -> checkedByteCount operation count
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %d{count}, which is not a valid UIntPtr length"
        | EvalStackValue.Int64 count -> checkedByteCount operation count
        | EvalStackValue.Int32 count -> checkedByteCount operation (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr byte count, got %O{other}"

    let private splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
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

    let private byteAtOffset
        (operation : string)
        (src : ManagedPointerSource)
        (byteOffset : int)
        (value : CliType)
        : byte
        =
        if byteOffset < 0 then
            failwith $"%s{operation}: negative byte offset %d{byteOffset} through %O{src}"

        if CliType.ContainsObjectReferences value then
            failwith $"%s{operation}: refusing to byte-compare value containing object references: %O{value}"

        match value with
        | CliType.ValueType vt when not (CliValueType.IsTightlyPacked vt) ->
            failwith $"%s{operation}: refusing to byte-compare non-tightly-packed value type %O{vt.Declared}"
        | CliType.RuntimePointer _ ->
            failwith $"%s{operation}: refusing to byte-compare runtime pointer value %O{value}"
        | _ -> ()

        let bytes = CliType.ToBytes value

        if byteOffset >= bytes.Length then
            failwith $"%s{operation}: byte offset %d{byteOffset} is outside %d{bytes.Length}-byte value at %O{src}"

        bytes.[byteOffset]

    let private isRawDataDataField (state : IlMachineState) (field : FieldId) : bool =
        match field with
        | FieldId.Metadata (declaringType, _, "Data") ->
            match AllConcreteTypes.lookup declaringType state.ConcreteTypes with
            | Some declaringType ->
                declaringType.Assembly.Name = "System.Private.CoreLib"
                && declaringType.Namespace = "System.Runtime.CompilerServices"
                && declaringType.Name = "RawData"
                && declaringType.Generics.IsEmpty
            | None -> false
        | FieldId.Metadata _
        | FieldId.Named _ -> false

    let private boxedValueTypeRawDataByte
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        : byte
        =
        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
        | false, _ -> failwith $"%s{operation}: RawData.Data byte view for array object %O{addr} is not modelled"
        | true, obj ->
            let concrete =
                AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: heap object %O{addr} concrete type %O{obj.ConcreteType} is not registered"
                )

            let typeDef =
                state.LoadedAssembly concrete.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: heap object %O{addr} assembly %O{concrete.Assembly} is not loaded"
                )
                |> fun assembly -> assembly.TypeDefs.[concrete.Definition.Get]

            if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeDef) then
                failwith $"%s{operation}: refusing RawData.Data byte view over reference type %O{obj.ConcreteType}"

            byteAtOffset operation src byteOffset (CliType.ValueType obj.Contents)

    let private readSpanHelpersSequenceEqualByte
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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
                | ByrefRoot.RvaData _, []
                | ByrefRoot.StringCharAt _, [] -> readPrimitiveByteView ()
                | ByrefRoot.HeapObjectField (addr, field), [] when isRawDataDataField state field ->
                    boxedValueTypeRawDataByte operation baseClassTypes state src addr byteOffset
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

    let private managedPointerOfPointerArgument (operation : string) (arg : EvalStackValue) : ManagedPointerSource =
        match arg with
        | EvalStackValue.ManagedPointer ptr -> ptr
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> ptr
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
        | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
            failwith $"%s{operation}: refusing to dereference unmanaged pointer value %d{i}"
        | other -> failwith $"%s{operation}: expected a pointer argument, got %O{other}"

    let private isSpanHelpersByteSequenceEqual
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

    let private spanHelpersSequenceEqual
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
                        readSpanHelpersSequenceEqualByte baseClassTypes operation state left = readSpanHelpersSequenceEqualByte
                            baseClassTypes
                            operation
                            state
                            right

                    i <- i + 1

                equal

        state
        |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
        |> IlMachineState.advanceProgramCounter currentThread

    let private popPointerBackedSpanConstructorArgs
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

    let private intrinsicDeclaringTypeHandle
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

    let private writePointerBackedSpanConstructor
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
        | _ -> failwith $"bad signature for %s{formatMethodKey (methodKey state methodToCall)}"

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

        let state = IlMachineState.writeManagedByref state thisPtr (CliType.ValueType span)

        let state =
            match wasConstructing with
            | None -> state
            | Some constructing ->
                let constructed = state.ManagedHeap.NonArrayObjects.[constructing]

                state
                |> IlMachineState.pushToEvalStack (CliType.ValueType constructed.Contents) currentThread

        state |> IlMachineState.advanceProgramCounter currentThread

    let private charOfCliType (operation : string) (value : CliType) : char =
        match CliType.unwrapPrimitiveLikeDeep value with
        | CliType.Char (high, low) -> char (int high * 256 + int low)
        | CliType.Numeric (CliNumericType.UInt16 i) -> char (int<uint16> i)
        | CliType.Numeric (CliNumericType.Int16 i) -> char (int<uint16> (uint16<int16> i))
        | other -> failwith $"%s{operation}: expected char-compatible value, got %O{other}"

    let private int32OfEvalStackValue (operation : string) (value : EvalStackValue) : int =
        match value with
        | EvalStackValue.Int32 i -> i
        | EvalStackValue.UserDefinedValueType vt ->
            match (CliValueType.PrimitiveLikeField vt).Contents |> CliType.unwrapPrimitiveLikeDeep with
            | CliType.Numeric (CliNumericType.Int32 i) -> i
            | other -> failwith $"%s{operation}: expected int32-like value, got %O{other}"
        | other -> failwith $"%s{operation}: expected int32-like value, got %O{other}"

    let private isCorelibConcreteType
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

    let private isReadOnlySpanOfChar (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | Some ty ->
            ty.Assembly.Name = "System.Private.CoreLib"
            && ty.Namespace = "System"
            && ty.Name = "ReadOnlySpan`1"
            && ty.Generics.Length = 1
            && isCorelibConcreteType state "System" "Char" ty.Generics.[0]
        | None -> false

    let private spanReceiverValue
        (operation : string)
        (state : IlMachineState)
        (receiver : EvalStackValue)
        : CliValueType
        =
        match receiver with
        | EvalStackValue.ManagedPointer src ->
            match IlMachineState.readManagedByref state src with
            | CliType.ValueType vt -> vt
            | other -> failwith $"%s{operation}: receiver byref read produced non-value-type %O{other}"
        | EvalStackValue.UserDefinedValueType vt -> vt
        | other -> failwith $"%s{operation}: expected span receiver byref, got %O{other}"

    let private spanReferenceAndLength
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

    let private readCharSpanContents
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

    let private spanToString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [], MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) -> ()
        | _ -> failwith $"bad signature for %s{formatMethodKey (methodKey state methodToCall)}"

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

    let private memoryExtensionsEquals
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
        | _ -> failwith $"bad signature for %s{formatMethodKey (methodKey state methodToCall)}"

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

        // In general, some implementations are in:
        // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L192
        match methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name with
        | "System.Private.CoreLib", ("AdvSimd" | "Rdm" | "Arm64"), "get_IsSupported" when
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
        | "System.Private.CoreLib", ("Vector128" | "Vector256" | "Vector512"), "get_IsHardwareAccelerated" ->
            // System.Runtime.Intrinsics.Vector{128,256,512}.IsHardwareAccelerated are JIT
            // intrinsic capability queries. PawPrint models a deterministic virtual CPU profile;
            // the default scalar-only profile reports them unavailable without consulting the host.
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
                    match IlMachineState.readManagedByref state ptr with
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
                    |> CliValueType.OfFields baseClassTypes state.ConcreteTypes runtimeTypeHandleHandle Layout.Default

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

            let target, state = popRuntimeTypeHandle currentThread state

            let ty =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    match state.LoadedAssembly identity.Assembly with
                    | Some assembly -> assembly.TypeDefs.[identity.TypeDefinition.Get]
                    | None ->
                        failwith
                            $"Type.get_IsValueType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                | RuntimeTypeHandleTarget.Closed ty ->
                    // TODO: structural handles such as typeof(int[]) still reach here as
                    // ConcreteTypeHandle.OneDimArrayZero, but this branch only handles nominal types.
                    match AllConcreteTypes.lookup ty state.ConcreteTypes with
                    | Some ty -> state.LoadedAssembly(ty.Assembly).Value.TypeDefs.[ty.Definition.Get]
                    | None -> failwith $"Type.get_IsValueType: expected nominal concrete type handle, got %O{ty}"

            let isValueType =
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies ty

            IlMachineState.pushToEvalStack (CliType.ofBool isValueType) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "Type", "get_IsGenericType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Type.get_IsGenericType"

            let target, state = popRuntimeTypeHandle currentThread state

            let isGenericType =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ -> true
                | RuntimeTypeHandleTarget.Closed ty ->
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
            | EvalStackValue.ManagedPointer _
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer _) -> ()
            | other -> failwith $"Unsafe.SkipInit: expected managed byref argument, got %O{other}"

            state |> IlMachineState.advanceProgramCounter currentThread |> Some
        | "System.Private.CoreLib", "Unsafe", "AsRef" ->
            // `AsRef<T>(ref readonly T)` is a JIT intrinsic. The CoreLib body in
            // this runtime throws PlatformNotSupportedException; the intended
            // intrinsic semantics are the address-preserving `ldarg.0; ret`.
            // Keep the void* overload out of this arm until native pointers are
            // modelled here deliberately.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.AsRef"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tParam ], MethodReturnType.Returns (ConcreteByref tRet) when tParam = t && tRet = t -> ()
            | _ -> failwith $"TODO: Unsafe.AsRef unsupported signature %A{methodToCall.Signature.ParameterTypes}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                match arg with
                | EvalStackValue.ManagedPointer ptr -> EvalStackValue.ManagedPointer ptr
                | x -> failwith $"TODO: Unsafe.AsRef(%O{x})"

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

            let popByref (operation : string) (arg : EvalStackValue) : ManagedPointerSource =
                match arg with
                | EvalStackValue.ManagedPointer ptr -> ptr
                | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
                | other -> failwith $"%s{operation}: expected managed byref argument, got %O{other}"

            let executeInt32 (operation : string) (state : IlMachineState) : IlMachineState =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    EvalStackValue.convToInt32 valueArg
                    |> Option.defaultWith (fun () -> failwith $"%s{operation}: expected int32 value, got %O{valueArg}")

                let byrefSrc = popByref operation byrefArg
                let currentValue = IlMachineState.readManagedByref state byrefSrc

                let current =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.Int32 i -> i
                    | other -> failwith $"%s{operation}: expected int32 location, got %O{other}"

                let updated = uint32<int32> current + uint32<int32> value |> int32<uint32>

                let state =
                    IlMachineState.writeManagedByref
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

                let byrefSrc = popByref operation byrefArg
                let currentValue = IlMachineState.readManagedByref state byrefSrc

                let current =
                    match EvalStackValue.ofCliType currentValue with
                    | EvalStackValue.Int64 i -> i
                    | other -> failwith $"%s{operation}: expected int64 location, got %O{other}"

                let updated = uint64<int64> current + uint64<int64> value |> int64<uint64>

                let state =
                    IlMachineState.writeManagedByref
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
            // The (ref IntPtr, IntPtr, IntPtr) -> IntPtr overload needs its own path: the shipped
            // IL wrapper does `Unsafe.As<IntPtr,long>` + delegates to the Int64 overload, which
            // would destroy our NativeIntSource provenance.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L452
            let isReferenceTypeHandle (handle : ConcreteTypeHandle) : bool =
                match handle with
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> true
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _ -> false
                | ConcreteTypeHandle.Concrete _ ->
                    match IlMachineState.tryGetConcreteTypeInfo state handle with
                    | Some (_, typeInfo) ->
                        DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                    | None ->
                        failwith $"Interlocked.CompareExchange<T>: concrete type handle %O{handle} has no TypeDef row"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
                ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
                ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->

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
                    EvalStackValueComparisons.ceq (EvalStackValue.NativeInt a) (EvalStackValue.NativeInt b)

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

                let byrefSrc =
                    match byrefArg with
                    | EvalStackValue.ManagedPointer ptr -> ptr
                    | EvalStackValue.NullObjectRef -> ManagedPointerSource.Null
                    | other -> failwith $"Interlocked.CompareExchange<T>: expected ManagedPointer byref, got %O{other}"

                let currentValue = IlMachineState.readManagedByref state byrefSrc

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
                        IlMachineState.writeManagedByref state byrefSrc valueCli
                    else
                        state

                state
                |> IlMachineState.pushToEvalStack currentValue currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            | _ ->
                // Other Interlocked.CompareExchange overloads are not yet intrinsified.
                // The Int32/Int64 shipped IL bodies self-call (expecting the JIT to intrinsify),
                // so they will stack-overflow if we fall through here.
                // When a caller needs one of these, it will need its own intrinsic arm.
                None
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
            | [ ConcreteInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) -> ()
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
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes) -> ()
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
            // as a bytewise gather across the pointed-to storage and then
            // reconstruct a T of the right shape via `ofBytesLike`.
            //
            // Two overloads exist: `ReadUnaligned<T>(ref byte source)` and
            // `ReadUnaligned<T>(void* source)`. PawPrint handles the pointer
            // overload only when the pointer has managed provenance, for
            // example an RVA data pointer produced by `ldsflda`.
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

                let v = IlMachineState.readManagedByrefBytesAs state src tZero

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

                let v = IlMachineState.readManagedByrefBytesAs state src tZero

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
                // CliType flavour before `ToBytes` produces a correct byte image.
                let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value
                let bytes = CliType.ToBytes valueAsCli

                if bytes.Length <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned: ToBytes produced %d{bytes.Length} bytes, expected %d{tSize} for %O{valueAsCli}"

                let state = IlMachineState.writeManagedByrefBytes state src valueAsCli

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
                let bytes = CliType.ToBytes valueAsCli

                if bytes.Length <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned(void*): ToBytes produced %d{bytes.Length} bytes, expected %d{tSize} for %O{valueAsCli}"

                let state = IlMachineState.writeManagedByrefBytes state src valueAsCli

                let state = state |> IlMachineState.advanceProgramCounter currentThread
                Some state
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
            let runtimeFieldInfoStubAddr : ManagedHeapAddress =
                match fldHandle with
                | EvalStackValue.ObjectRef addr -> addr
                | EvalStackValue.NullObjectRef ->
                    failwith "TODO: throw ArgumentException for InitializeArray with null field handle"
                | other -> failwith $"InitializeArray: expected RuntimeFieldHandle ObjectRef, got %O{other}"

            let tryResolveFieldHandleFromRuntimeFieldInfoObject
                (runtimeFieldInfoAddr : ManagedHeapAddress)
                (state : IlMachineState)
                : FieldHandle option
                =
                let heapObj = ManagedHeap.get runtimeFieldInfoAddr state.ManagedHeap

                match IlMachineState.tryGetConcreteTypeInfo state heapObj.ConcreteType with
                | None -> None
                | Some (_, typeInfo) ->
                    typeInfo.Fields
                    |> List.tryFind (fun field -> field.Name = "m_fieldHandle")
                    |> Option.bind (fun field ->
                        let fieldId = FieldIdentity.fieldId heapObj.ConcreteType field

                        let fieldHandleId =
                            match
                                AllocatedNonArrayObject.DereferenceFieldById fieldId heapObj
                                |> CliType.unwrapPrimitiveLikeDeep
                            with
                            | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> Some id
                            | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> None
                            | CliType.RuntimePointer (CliRuntimePointer.Verbatim id) -> Some id
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)) -> Some id
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
                            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim id)) -> Some id
                            | other ->
                                failwith
                                    $"InitializeArray: expected RuntimeFieldHandleInternal-compatible m_fieldHandle on reflected field info, got %O{other}"

                        fieldHandleId
                        |> Option.bind (fun fieldHandleId ->
                            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
                        )
                    )

            // Look up the FieldHandle from the registry using either the RuntimeFieldInfoStub
            // address produced by ldtoken, or the RtFieldInfo object used by reflection.
            let fieldHandle : FieldHandle =
                match FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoStubAddr state.FieldHandles with
                | Some fh -> fh
                | None ->
                    match tryResolveFieldHandleFromRuntimeFieldInfoObject runtimeFieldInfoStubAddr state with
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
        | "System.Private.CoreLib", "RuntimeHelpers", "IsBitwiseEquatable" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.IsBitwiseEquatable"

            let ty =
                match Seq.toList methodToCall.Generics with
                | [ ty ] -> ty
                | _ -> failwith "bad generics RuntimeHelpers.IsBitwiseEquatable"

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
                // equality fast path. The positive value-type cases need the same override,
                // field-recursion, and IEquatable<T> checks as the MethodTable QCall.
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
                | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), projs) ->
                    ByteStorageIdentity.LocalMemory (thread, frame, block),
                    int64 byteOffset + projectionByteOffset projs
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
                    match IlMachineState.readManagedByref state src with
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
                        | CliNumericType.Int64 i -> i
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
