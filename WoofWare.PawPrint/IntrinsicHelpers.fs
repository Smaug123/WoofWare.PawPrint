namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

module internal IntrinsicHelpers =
    /// CoreCLR's `MethodTable::IsValueTypeImpl`, as the reflection surface sees it.
    ///
    /// Byrefs, pointers, function pointers and arrays are TypeDescs, for which it resolves to
    /// `IsSubclassOf(typeof(ValueType))` and so answers false; they are absent from the nominal
    /// `AllConcreteTypes` mapping, so they must be answered from the shape rather than by
    /// failing the lookup. `operation` names the caller in the diagnostic raised when a
    /// `Concrete` handle turns out to have no row, which is a broken interpreter invariant
    /// rather than anything the guest did.
    let isValueTypeHandleAsCoreClr
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (operation : string)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> false
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.tryIsValueType baseClassTypes state._LoadedAssemblies state.ConcreteTypes handle with
            | Some isValueType -> isValueType
            | None -> failwith $"%s{operation}: expected nominal concrete type handle, got %O{handle}"

    type private RefTypeProcessingStatus =
        | InProgress
        | Completed of bool

    /// CoreLib's primitive value types. Each is defined with a single instance field whose
    /// signature is the primitive type itself (`System.Int32.m_value : int32`), so walking their
    /// fields bottoms out only on the cycle check below. Short-circuiting is both cheaper and
    /// clearer than relying on that.
    let private primitiveValueTypeNames =
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

    /// Decide whether the storage of `handle` contains any managed references, walking the fields
    /// of value types transitively.
    ///
    /// The walk is in the *concrete* domain: `handle` names an exact instantiation, so a field
    /// typed `T` is concretized against the declaring type's own generic arguments rather than
    /// against a manufactured `GenericTypeParameter` placeholder. That is what makes
    /// `Box<string>` distinguishable from `Box<int>`, and it is why the memo table is keyed on
    /// the handle: `ConcreteTypeHandle` already identifies identity *and* instantiation.
    let rec private containsRefType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (seenSoFar : ImmutableDictionary<ConcreteTypeHandle, RefTypeProcessingStatus>)
        (handle : ConcreteTypeHandle)
        : IlMachineState * ImmutableDictionary<ConcreteTypeHandle, RefTypeProcessingStatus> * bool
        =
        match handle with
        // An array is an object reference.
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, seenSoFar, true
        // A byref is a GC-tracked pointer, and CoreCLR counts it: the JIT answers
        // `IsReferenceOrContainsReferences<T>` with `varTypeIsGC(fromType) || fromLayout->HasGCPtr()`
        // (jit/importercalls.cpp:3624), and a layout's `m_gcPtrCount` includes its `TYP_BYREF`
        // slots (jit/layout.cpp:576). Hence the API's documented contract: "a value type that
        // contains references *or by-refs*".
        | ConcreteTypeHandle.Byref _ -> state, seenSoFar, true
        // Unmanaged pointers are not GC-tracked.
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, seenSoFar, false
        | ConcreteTypeHandle.Concrete _ ->

        match seenSoFar.TryGetValue handle with
        | true, InProgress ->
            // We've hit a cycle. Optimistically assume this path does not introduce a reference type.
            // If another path finds a reference type, its 'true' will override this.
            state, seenSoFar, false
        | true, Completed v ->
            // We've already calculated this; return the memoized result.
            state, seenSoFar, v
        | false, _ ->
            let concrete =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"type was not registered: %O{handle}")

            if
                concrete.Assembly.Name = "System.Private.CoreLib"
                && concrete.Namespace = "System"
                && primitiveValueTypeNames.Contains concrete.Name
            then
                state, seenSoFar.Add (handle, Completed false), false
            else

            let assy =
                state.LoadedAssembly concrete.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"assembly %O{concrete.Assembly} of concrete type %O{handle} is not loaded"
                )

            let td = assy.TypeDefs.[concrete.Definition.Get]

            if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies td) then
                // Short-circuit: if the type itself is a reference type, we're done.
                state, seenSoFar.Add (handle, Completed true), true
            else

            // It's a value type, so we must check its fields.
            // Mark as in progress before recursing.
            let seenSoFarWithInProgress = seenSoFar.Add (handle, InProgress)

            // Concretize each instance field's signature against *this* instantiation's generic
            // arguments. A field is declared in the same assembly as its declaring type, so that
            // assembly is the resolution scope for the signature's tokens. There is no method
            // generic context: a field signature cannot mention one.
            let stateAfterFieldResolution, nonStaticFields =
                ((state, []), td.Fields)
                ||> List.fold (fun (currentState, acc) field ->
                    if field.IsStatic then
                        currentState, acc
                    else
                        let newState, fieldHandle =
                            IlMachineState.concretizeType
                                loggerFactory
                                baseClassTypes
                                currentState
                                concrete.Assembly
                                concrete.Generics
                                ImmutableArray.Empty
                                field.Signature

                        newState, fieldHandle :: acc
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
            let finalSeenSoFar = finalSeenSoFar.SetItem (handle, Completed fieldsContainRefType)

            finalState, finalSeenSoFar, fieldsContainRefType

    let concreteTypeContainsReferences
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        containsRefType loggerFactory baseClassTypes state ImmutableDictionary.Empty handle
        |> fun (state, _, result) -> state, result

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
        (offset : int64)
        (src : EvalStackValue)
        : EvalStackValue * IlMachineState
        =
        // Thread the state returned by `cliTypeZeroOfHandle`: for a struct T
        // it can concretise additional types, and discarding the update would
        // drop that work from the machine state.
        let tZero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementType

        let tSize = CliType.sizeOf tZero

        // `Unsafe.AsRef<T>((void*)bits)` byrefs are bit patterns, not
        // anchored byrefs. `Unsafe.Add<T>(ref placeholder, n)` advances
        // by `n * sizeof(T)` bytes; if the result lands on zero,
        // normalise to Null so `IsNullRef` agrees with the bit-pattern
        // definition (mirrors `BinaryArithmetic.addOffsetToManagedPtr`).
        // `Null` is the bit pattern `0`, so adding to it must follow the
        // same bit-arithmetic route — otherwise a chained
        // `Unsafe.Add(Unsafe.Add(placeholder, -n), n)` whose middle step
        // normalised to `Null` would fall into the byref path and try to
        // project off a null managed pointer.
        let placeholderBits =
            match src with
            | EvalStackValue.ManagedPointer ptr -> ManagedPointerSource.tryBitPatternBits ptr
            | _ -> ValueNone

        match placeholderBits with
        | ValueSome bits ->
            let ptrSrc = bits + offset * int64<int> tSize |> ManagedPointerSource.ofBitPattern

            EvalStackValue.ManagedPointer ptrSrc, state
        | ValueNone ->

        // Every anchored-byref branch below combines `offset` with a stored index or byte
        // offset that PawPrint represents as an int32. The IL this models does the same
        // arithmetic at native-int width (`sizeof !!T; conv.i; mul; add`), so an int32 result
        // that wraps is a limit of *our* representation, not the CLI's — and it does not merely
        // lose precision, it puts the byref on the wrong side of the source address
        // (`Unsafe.Add(ref a[1], Int32.MaxValue)` would report -8589934592 bytes instead of
        // +8589934592). The bit-pattern branch above needs none of this: it already carries its
        // whole address in an int64. Refuse what we cannot represent rather than answer wrongly.
        // An anchored byref stores an int32 cell index or byte offset, so a walk whose element
        // count alone exceeds the width of an int32 *difference* can never land on one PawPrint
        // can represent: `index` and `index + offset` are both int32, so `offset` is confined to
        // +/-(2^32 - 1). Refusing here also bounds the products below — |offset| < 2^32 and
        // tSize <= Int32.MaxValue < 2^31 give |tSize * offset| < 2^63 — so none of the int64
        // arithmetic that follows can itself overflow.
        if offset < -4294967295L || offset > 4294967295L then
            failwith
                $"TODO: byref element offset: a walk of %d{offset} elements cannot reach a byref PawPrint can represent, whose roots store int32 indices and byte offsets"

        let representable (what : string) (value : int64) : int =
            if
                value < int64<int> System.Int32.MinValue
                || value > int64<int> System.Int32.MaxValue
            then
                failwith
                    $"TODO: byref element offset: %s{what} is %d{value}, which does not fit in the int32 PawPrint stores for it; a byref this far from its root is not modelled"

            int32<int64> value

        /// `sizeof(T) * offset`: the byte distance this element walk covers. Deferred rather than
        /// computed up front, because the branches that step whole cells never form it, and their
        /// index arithmetic can be representable when this product is not.
        let byteDelta () : int =
            representable $"a walk of %d{offset} elements of size %d{tSize}" (int64<int> tSize * offset)

        /// `index + offset` on a root that stores a cell index rather than a byte offset.
        let offsetIndex (what : string) (index : int) : int =
            representable $"%s{what} %d{index} advanced by %d{offset}" (int64<int> index + offset)

        /// `byteOffset + sizeof(T) * offset` on a root that stores a byte offset directly.
        let offsetByteOffset (what : string) (byteOffset : int) : int =
            representable
                $"%s{what} %d{byteOffset} advanced by %d{offset} elements of size %d{tSize}"
                (int64<int> byteOffset + int64<int> tSize * offset)

        let ptr : EvalStackValue =
            match src with
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs)) ->
                // The array's own stride, for an empty array as much as a populated one:
                // substituting `sizeof(T)` for an empty array would make the
                // `tSize <> arrElementSize` test below trivially false, silently choosing
                // cell-index arithmetic that is only a correct byte position when `T` *is*
                // the element type.
                let arrElementSize = (ManagedHeap.getArrayShape arr state.ManagedHeap).ElementStride

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
                    let byteDelta = byteDelta ()
                    let baseSrc = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs)

                    // Zero here would mean "do not normalise" (the fold guards on
                    // `cellSize > 0`), leaving an empty array with a raw byte cursor where a
                    // populated one of the same element type folds it into the cell index.
                    let normalisation =
                        ByteOffsetNormalisationContext.withArrayElementSize arr arrElementSize

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

                    ManagedPointerSource.Byref (
                        ByrefRoot.ArrayElement (arr, offsetIndex "array element index" i),
                        projs
                    )
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
                    |> ManagedPointerSource.addByteOffsetToByteView normalisation (byteDelta ())
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

                    ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, offsetIndex "string char index" i), projs)
                    |> EvalStackValue.ManagedPointer
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread,
                                                                                                    frame,
                                                                                                    block,
                                                                                                    byteOffset),
                                                                         [])) ->
                ManagedPointerSource.Byref (
                    ByrefRoot.StackMemoryByte (
                        thread,
                        frame,
                        block,
                        offsetByteOffset "stack memory byte offset" byteOffset
                    ),
                    []
                )
                |> EvalStackValue.ManagedPointer
            | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset),
                                                                         [])) ->
                ManagedPointerSource.Byref (
                    ByrefRoot.NativeMemoryByte (block, offsetByteOffset "native memory byte offset" byteOffset),
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
                    |> ManagedPointerSource.addByteOffsetToByteView normalisation (byteDelta ())
                    |> EvalStackValue.ManagedPointer
                elif offset = 0L then
                    EvalStackValue.ManagedPointer src
                else
                    // The projection chain contains structural navigations (e.g. Field)
                    // that aren't byte-view compatible. Transition into a byte-view by
                    // appending ReinterpretAs(T) + ByteOffset(sizeof(T) * offset).
                    let elementTypeInfo =
                        AllConcreteTypes.lookup elementType state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith $"byref element offset: element type %O{elementType} was not registered"
                        )

                    let normalisation = ByteOffsetNormalisationContext.nonArrayRootsOnly

                    src
                    |> ManagedPointerSource.addByteOffsetUnderReinterpret normalisation elementTypeInfo (byteDelta ())
                    |> EvalStackValue.ManagedPointer
            | _ -> failwith $"TODO: byref element offset on non-managed-pointer: %O{src}"

        ptr, state

    let vectorAccelerationAvailable (declaringTypeName : string) (profile : HardwareIntrinsicsProfile) : bool =
        match declaringTypeName with
        | "Vector128" -> profile.Vector128
        | "Vector256" -> profile.Vector256
        | "Vector512" -> profile.Vector512
        // System.Numerics.Vector.IsHardwareAccelerated is the JIT capability query for the
        // non-generic numerics helper (and is the value Vector<T>.IsHardwareAccelerated forwards
        // to via its static interface impl). CoreCLR reports it true iff the JIT can accelerate
        // at least 128-bit SIMD, so it shares the Vector128 bit on PawPrint's profile.
        | "Vector" -> profile.Vector128
        | other -> failwith $"Unexpected vector intrinsic type name: %s{other}"

    // PawPrint emulates a deterministic scalar virtual CPU: every hardware-intrinsic
    // family reports IsSupported = false so the BCL falls through to its scalar/portable
    // path. Most of the IL bodies are a recursive `return IsSupported;` stub that the JIT
    // replaces with a constant; without an explicit fold here it would recurse forever.
    // A few entries (e.g. `System.Numerics.Vector\`1`) have honest terminating bodies that
    // would return `true` for primitive `T` — we still fold them to `false` here because the
    // scalar profile has no implementation of the SIMD ops a `true` answer would commit us to.
    // New ISAs landing in CoreLib must be added to this set.
    //
    // Coverage source: src/libraries/System.Private.CoreLib/src/System/Runtime/Intrinsics
    // (and System/Numerics for `Vector\`1`) in the dotnet/runtime tree. Listing
    // harmless-but-unmatched names is fine; the lookup is keyed off the fully qualified
    // declaring type name, so types absent from the running CoreLib simply never trigger a
    // match.
    //
    // Note the spelling of the per-ISA nested classes (`Arm64`, `X64`, `VL`, `V256`, `V512`):
    // a nesting chain is joined with `+`, matching `Type.FullName` and `TypeInfo.fullName`.
    // A `.` here silently fails to match and the ISA's `IsSupported` then recurses forever
    // instead of folding to false.
    let scalarOnlyFalseIsSupportedIntrinsics =
        set
            [
                // System.Numerics
                "System.Numerics.Vector`1"
                // System.Runtime.Intrinsics.Arm
                "System.Runtime.Intrinsics.Arm.AdvSimd"
                "System.Runtime.Intrinsics.Arm.AdvSimd+Arm64"
                "System.Runtime.Intrinsics.Arm.Aes"
                "System.Runtime.Intrinsics.Arm.Aes+Arm64"
                "System.Runtime.Intrinsics.Arm.ArmBase"
                "System.Runtime.Intrinsics.Arm.ArmBase+Arm64"
                "System.Runtime.Intrinsics.Arm.Crc32"
                "System.Runtime.Intrinsics.Arm.Crc32+Arm64"
                "System.Runtime.Intrinsics.Arm.Dp"
                "System.Runtime.Intrinsics.Arm.Dp+Arm64"
                "System.Runtime.Intrinsics.Arm.Rdm"
                "System.Runtime.Intrinsics.Arm.Rdm+Arm64"
                "System.Runtime.Intrinsics.Arm.Sha1"
                "System.Runtime.Intrinsics.Arm.Sha1+Arm64"
                "System.Runtime.Intrinsics.Arm.Sha256"
                "System.Runtime.Intrinsics.Arm.Sha256+Arm64"
                "System.Runtime.Intrinsics.Arm.Sve"
                "System.Runtime.Intrinsics.Arm.Sve+Arm64"
                "System.Runtime.Intrinsics.Arm.Sve2"
                "System.Runtime.Intrinsics.Arm.Sve2+Arm64"
                // System.Runtime.Intrinsics.Wasm
                "System.Runtime.Intrinsics.Wasm.PackedSimd"
                "System.Runtime.Intrinsics.Wasm.WasmBase"
                // System.Runtime.Intrinsics.X86
                "System.Runtime.Intrinsics.X86.Aes"
                "System.Runtime.Intrinsics.X86.Aes+X64"
                "System.Runtime.Intrinsics.X86.Avx"
                "System.Runtime.Intrinsics.X86.Avx+X64"
                "System.Runtime.Intrinsics.X86.Avx10v1"
                "System.Runtime.Intrinsics.X86.Avx10v1+X64"
                "System.Runtime.Intrinsics.X86.Avx10v1+V512"
                "System.Runtime.Intrinsics.X86.Avx10v1+V512+X64"
                "System.Runtime.Intrinsics.X86.Avx10v2"
                "System.Runtime.Intrinsics.X86.Avx10v2+X64"
                "System.Runtime.Intrinsics.X86.Avx10v2+V512"
                "System.Runtime.Intrinsics.X86.Avx10v2+V512+X64"
                "System.Runtime.Intrinsics.X86.Avx2"
                "System.Runtime.Intrinsics.X86.Avx2+X64"
                "System.Runtime.Intrinsics.X86.Avx512BW"
                "System.Runtime.Intrinsics.X86.Avx512BW+VL"
                "System.Runtime.Intrinsics.X86.Avx512BW+X64"
                "System.Runtime.Intrinsics.X86.Avx512CD"
                "System.Runtime.Intrinsics.X86.Avx512CD+VL"
                "System.Runtime.Intrinsics.X86.Avx512CD+X64"
                "System.Runtime.Intrinsics.X86.Avx512DQ"
                "System.Runtime.Intrinsics.X86.Avx512DQ+VL"
                "System.Runtime.Intrinsics.X86.Avx512DQ+X64"
                "System.Runtime.Intrinsics.X86.Avx512F"
                "System.Runtime.Intrinsics.X86.Avx512F+VL"
                "System.Runtime.Intrinsics.X86.Avx512F+X64"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi+VL"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi+X64"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi2"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi2+VL"
                "System.Runtime.Intrinsics.X86.Avx512Vbmi2+X64"
                "System.Runtime.Intrinsics.X86.AvxVnni"
                "System.Runtime.Intrinsics.X86.AvxVnni+X64"
                "System.Runtime.Intrinsics.X86.Bmi1"
                "System.Runtime.Intrinsics.X86.Bmi1+X64"
                "System.Runtime.Intrinsics.X86.Bmi2"
                "System.Runtime.Intrinsics.X86.Bmi2+X64"
                "System.Runtime.Intrinsics.X86.Fma"
                "System.Runtime.Intrinsics.X86.Fma+X64"
                "System.Runtime.Intrinsics.X86.Gfni"
                "System.Runtime.Intrinsics.X86.Gfni+V256"
                "System.Runtime.Intrinsics.X86.Gfni+V512"
                "System.Runtime.Intrinsics.X86.Gfni+X64"
                "System.Runtime.Intrinsics.X86.Lzcnt"
                "System.Runtime.Intrinsics.X86.Lzcnt+X64"
                "System.Runtime.Intrinsics.X86.Pclmulqdq"
                "System.Runtime.Intrinsics.X86.Pclmulqdq+V256"
                "System.Runtime.Intrinsics.X86.Pclmulqdq+V512"
                "System.Runtime.Intrinsics.X86.Pclmulqdq+X64"
                "System.Runtime.Intrinsics.X86.Popcnt"
                "System.Runtime.Intrinsics.X86.Popcnt+X64"
                "System.Runtime.Intrinsics.X86.Sse"
                "System.Runtime.Intrinsics.X86.Sse+X64"
                "System.Runtime.Intrinsics.X86.Sse2"
                "System.Runtime.Intrinsics.X86.Sse2+X64"
                "System.Runtime.Intrinsics.X86.Sse3"
                "System.Runtime.Intrinsics.X86.Sse3+X64"
                "System.Runtime.Intrinsics.X86.Sse41"
                "System.Runtime.Intrinsics.X86.Sse41+X64"
                "System.Runtime.Intrinsics.X86.Sse42"
                "System.Runtime.Intrinsics.X86.Sse42+X64"
                "System.Runtime.Intrinsics.X86.Ssse3"
                "System.Runtime.Intrinsics.X86.Ssse3+X64"
                "System.Runtime.Intrinsics.X86.X86Base"
                "System.Runtime.Intrinsics.X86.X86Base+X64"
                "System.Runtime.Intrinsics.X86.X86Serialize"
                "System.Runtime.Intrinsics.X86.X86Serialize+X64"
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

        if count > int64 Int32.MaxValue then
            failwith $"%s{operation}: byte count %d{count} exceeds the interpreter Int32 byte-offset model"

        int count

    let byteCountOfStackValue (operation : string) (arg : EvalStackValue) : int =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim count) -> checkedByteCount operation count
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %O{count}, which is not a valid UIntPtr length"
        | EvalStackValue.Int64 count ->
            match Int64Source.isNonnegative count with
            | Some true ->
                match count with
                | Int64Source.SyntheticCrossArrayOffset _ ->
                    failwith "refusing to interpret memory address difference as byte count"
                | Int64Source.WidenedNativeInt (src, _) ->
                    failwith
                        $"%s{operation}: byte count came from a widened native int %O{src}; refusing to interpret pointer-shaped int64 as byte count"
                | Int64Source.OpaqueHashBits bits ->
                    failwith
                        $"%s{operation}: byte count came from synthesised pointer-hash bits 0x%x{bits}; refusing to interpret hashed pointer provenance as byte count"
                | Int64Source.Verbatim count -> checkedByteCount operation count
            | _ -> failwith "unexpectedly got negative byte count"
        | EvalStackValue.Int32 (Int32Source.Verbatim count) -> checkedByteCount operation (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr byte count, got %O{other}"

    let splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
        match src with
        | ManagedPointerSource.Null -> ValueNone
        | ManagedPointerSource.NativeIntPlaceholder _ -> ValueNone
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
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        : byte
        =
        let readPrimitiveByteView () : byte =
            match IlMachineState.readManagedByrefBytesAs baseClassTypes state src byteTemplate with
            | CliType.Numeric (CliNumericType.UInt8 b) -> b
            | other -> failwith $"%s{operation}: byte-view read returned non-byte value %O{other}"

        match src with
        | ManagedPointerSource.Null -> failwith $"%s{operation}: attempted to dereference null byref"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (root, projs) ->
            match splitTrailingByteView src with
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                match byteViewRoot, prefixProjs with
                | ByrefRoot.ArrayElement _, []
                | ByrefRoot.StackMemoryByte _, []
                | ByrefRoot.NativeMemoryByte _, []
                | ByrefRoot.PeByteRange _, []
                | ByrefRoot.StringCharAt _, [] -> readPrimitiveByteView ()
                | _ ->
                    let basePtr = ManagedPointerSource.Byref (byteViewRoot, prefixProjs)
                    let value = IlMachineState.readManagedByref baseClassTypes state basePtr

                    match value with
                    | CliType.ValueType _ -> byteAtOffset operation src byteOffset value
                    | _ -> readPrimitiveByteView ()
            | ValueNone ->
                let value =
                    IlMachineState.readManagedByref baseClassTypes state (ManagedPointerSource.Byref (root, projs))

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
                    let left = ManagedPointerByteView.addByteOffset state byteType i leftPtr

                    let right = ManagedPointerByteView.addByteOffset state byteType i rightPtr

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

    let popPointerBackedSpanConstructorArgs
        (currentThread : ThreadId)
        (wasConstructing : ConstructionState)
        (state : IlMachineState)
        : ManagedPointerSource * ManagedPointerSource * int * IlMachineState
        =
        match wasConstructing with
        | ConstructionState.Constructing _ ->
            let thisArg, state = IlMachineState.popEvalStack currentThread state
            let lengthArg, state = IlMachineState.popEvalStack currentThread state
            let sourceArg, state = IlMachineState.popEvalStack currentThread state

            let thisPtr =
                match thisArg with
                | EvalStackValue.ManagedPointer ptr -> ptr
                | other -> failwith $"Span pointer constructor expected managed byref `this`, got %O{other}"

            let length =
                match lengthArg with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                | other -> failwith $"Span pointer constructor expected int length, got %O{other}"

            let sourcePtr = managedPointerOfPointerArgument "Span pointer constructor" sourceArg

            thisPtr, sourcePtr, length, state
        | ConstructionState.NotConstructing ->
            let lengthArg, state = IlMachineState.popEvalStack currentThread state
            let sourceArg, state = IlMachineState.popEvalStack currentThread state
            let thisArg, state = IlMachineState.popEvalStack currentThread state

            let thisPtr =
                match thisArg with
                | EvalStackValue.ManagedPointer ptr -> ptr
                | other -> failwith $"Span pointer constructor expected managed byref `this`, got %O{other}"

            let length =
                match lengthArg with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
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
            methodToCall.RequiredDeclaringType.Identity
            methodToCall.DeclaringTypeGenerics
        |> Option.defaultWith (fun () ->
            failwith $"Intrinsic method declaring type was not registered: %s{MethodOwner.describe methodToCall.Owner}"
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
        (wasConstructing : ConstructionState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState
        =
        let elementType = methodToCall.DeclaringTypeGenerics |> Seq.exactlyOne

        match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
        | [ ConcretePointer _ ; ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Void -> ()
        | _ ->
            failwith
                $"bad signature for %s{IntrinsicMethodKeys.formatMethodKey (IntrinsicMethodKeys.methodKey state methodToCall)}"

        let state, elementContainsRefs =
            concreteTypeContainsReferences loggerFactory baseClassTypes state elementType

        if elementContainsRefs then
            failwith
                $"TODO: %s{MethodOwner.describe methodToCall.Owner}(void*, int) with reference-containing element type should throw ArgumentException"

        let thisPtr, sourcePtr, length, state =
            popPointerBackedSpanConstructorArgs currentThread wasConstructing state

        if length < 0 then
            failwith
                $"TODO: %s{MethodOwner.describe methodToCall.Owner}(void*, int) with negative length should throw ArgumentOutOfRangeException"

        let elementTypeInfo =
            match AllConcreteTypes.lookup elementType state.ConcreteTypes with
            | Some info -> info
            | None -> failwith $"Span pointer constructor element type was not registered: %O{elementType}"

        // `Unsafe.AsRef<T>((void*)bits)` placeholders are bit patterns, not
        // anchored byrefs; `appendProjection` rightly refuses to project off
        // them. The CLR permits arbitrary non-null pointers for zero-length
        // pointer-backed spans (the source must never be dereferenced), so
        // for `length = 0` we skip the `ReinterpretAs` and keep the
        // placeholder verbatim. For `length > 0` over a placeholder we
        // refuse: any indexing would have to project off the placeholder,
        // which is undefined.
        let sourcePtr =
            match sourcePtr with
            | ManagedPointerSource.Null -> ManagedPointerSource.Null
            | ManagedPointerSource.NativeIntPlaceholder _ when length = 0 -> sourcePtr
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"TODO: %s{MethodOwner.describe methodToCall.Owner}(void*, int) with non-zero length %d{length} over placeholder pointer 0x%x{bits}"
            | sourcePtr ->
                ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs elementTypeInfo) sourcePtr

        let declaringTypeHandle = intrinsicDeclaringTypeHandle state methodToCall

        let span =
            match IlMachineState.readManagedByref baseClassTypes state thisPtr with
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
                (EvalStackValue.Int32 (Int32Source.Verbatim length))

        let span =
            span
            |> CliValueType.WithFieldSetById referenceField referenceValue
            |> CliValueType.WithFieldSetById lengthField lengthValue

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state thisPtr (CliType.ValueType span)

        let state =
            match wasConstructing with
            | ConstructionState.NotConstructing -> state
            | ConstructionState.Constructing constructing ->
                let constructed = ManagedHeap.get constructing state.ManagedHeap

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
        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
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

    let spanReceiverValue
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (receiver : EvalStackValue)
        : CliValueType
        =
        match receiver with
        | EvalStackValue.ManagedPointer src ->
            match IlMachineState.readManagedByref baseClassTypes state src with
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
                    offsetManagedPointerByElements
                        baseClassTypes
                        state
                        spanType.Generics.[0]
                        (int64<int> index)
                        reference

                let value =
                    match ptr with
                    | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref baseClassTypes state src
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

        let operation = $"{MethodOwner.describe methodToCall.Owner}.ToString"
        let elementType = methodToCall.DeclaringTypeGenerics |> Seq.exactlyOne
        let receiver, state = IlMachineState.popEvalStack currentThread state
        let span = spanReceiverValue baseClassTypes operation state receiver
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
                        offsetManagedPointerByElements baseClassTypes state elementType (int64<int> index) reference

                    let value =
                        match ptr with
                        | EvalStackValue.ManagedPointer src -> IlMachineState.readManagedByref baseClassTypes state src
                        | other -> failwith $"%s{operation}: element pointer was not a managed pointer: %O{other}"

                    charOfCliType operation value :: chars, state
                )
                |> fun (chars, state) -> System.String (chars |> List.rev |> List.toArray), state
            else
                let typeKind =
                    if methodToCall.RequiredDeclaringType.Name = "ReadOnlySpan`1" then
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
        let left = spanReceiverValue baseClassTypes operation state left
        let right = spanReceiverValue baseClassTypes operation state right
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

    let executeUnsafeCopyBlock
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState
        =
        // Stack order: destination (arg0) pushed first, source (arg1), byteCount (arg2) on top.
        let byteCountArg, state = IlMachineState.popEvalStack currentThread state
        let sourceArg, state = IlMachineState.popEvalStack currentThread state
        let destArg, state = IlMachineState.popEvalStack currentThread state

        let byteCount = byteCountOfStackValue operation byteCountArg

        let state =
            if byteCount = 0 then
                state
            else
                let sourcePtr = managedPointerOfPointerArgument operation sourceArg
                let destPtr = managedPointerOfPointerArgument operation destArg

                match sourcePtr, destPtr with
                | ManagedPointerSource.Null, _ -> failwith $"%s{operation}: refusing nonzero byte copy from null source"
                | _, ManagedPointerSource.Null ->
                    failwith $"%s{operation}: refusing nonzero byte copy to null destination"
                | _ ->

                // cpblk is undefined for overlapping ranges (ECMA-335 III.3.30),
                // so a forward walk is per-spec correct; we don't need the
                // Memmove-style overlap handling. The shared cell-aware primitive
                // preserves non-byte-addressable cell shapes (object references,
                // runtime pointers, value-types containing those) and
                // non-`Verbatim` numeric provenance (e.g. `TypeHandlePtr`-tagged
                // `IntPtr`s) that the byte-walk fallback cannot serialise.
                CellAwareMemOps.copy
                    baseClassTypes
                    operation
                    CellAwareCopyPolicy.CpblkForward
                    state
                    destPtr
                    sourcePtr
                    byteCount

        state |> IlMachineState.advanceProgramCounter currentThread

    let executeSpanHelpersMemmove
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState
        =
        // `SpanHelpers.Memmove(ref byte dest, ref byte src, nuint len)` — stack
        // order: destination (arg0) pushed first, source (arg1), byteCount
        // (arg2) on top.
        let byteCountArg, state = IlMachineState.popEvalStack currentThread state
        let sourceArg, state = IlMachineState.popEvalStack currentThread state
        let destArg, state = IlMachineState.popEvalStack currentThread state

        let byteCount = byteCountOfStackValue operation byteCountArg

        let state =
            if byteCount = 0 then
                state
            else
                // Accepts both strict managed byrefs and `NativeInt`-wrapped
                // managed pointers (`ref *(byte*)ptr` patterns produced by
                // `Marshal.StructureToPtr` / `Marshal.PtrToStructure` against
                // `AllocHGlobal`'d memory; see Marshal.CoreCLR.cs:270, 295).
                let sourcePtr = managedPointerOfPointerArgument operation sourceArg
                let destPtr = managedPointerOfPointerArgument operation destArg

                match sourcePtr, destPtr with
                | ManagedPointerSource.Null, _ -> failwith $"%s{operation}: refusing nonzero byte copy from null source"
                | _, ManagedPointerSource.Null ->
                    failwith $"%s{operation}: refusing nonzero byte copy to null destination"
                | _ when sourcePtr = destPtr ->
                    // CoreCLR's `SpanHelpers.Memmove` short-circuits perfectly
                    // overlapping buffers (`Unsafe.AreSame(ref dest, ref src)`)
                    // without copying — see SpanHelpers.ByteMemOps.cs:230. We
                    // match that behaviour here so self-copies of spans whose
                    // backing cells aren't byte-renderable (provenance-carrying
                    // `NativeInt`s, field-projected residuals whose flat byte
                    // offset is undecidable) don't trip the cell-aware path
                    // for a copy that has no observable effect.
                    state
                | _ ->

                // The shared cell-aware primitive preserves non-byte-addressable
                // cell shapes (object references, runtime pointers, value types
                // containing those) and non-`Verbatim` numeric provenance (e.g.
                // `TypeHandlePtr`-tagged `IntPtr`s) that the byte-walk fallback
                // cannot serialise. `CellAwareCopyPolicy.Memmove` mirrors the
                // BCL's overlap handling by walking backwards when src strictly
                // precedes dest in the same flat byte storage, matching the
                // intent of CoreCLR's `Memmove` (which P/Invokes into native
                // memmove on overlap; see SpanHelpers.ByteMemOps.cs:37).
                CellAwareMemOps.copy
                    baseClassTypes
                    operation
                    CellAwareCopyPolicy.Memmove
                    state
                    destPtr
                    sourcePtr
                    byteCount

        state |> IlMachineState.advanceProgramCounter currentThread

    let executeSpanHelpersClearWithoutReferences
        (baseClassTypes : BaseClassTypes<_>)
        (currentThread : ThreadId)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState
        =
        // `SpanHelpers.ClearWithoutReferences(ref byte dest, nuint len)` — stack
        // order: destination (arg0) pushed first, byteCount (arg1) on top.
        let byteCountArg, state = IlMachineState.popEvalStack currentThread state
        let destArg, state = IlMachineState.popEvalStack currentThread state

        let byteCount = byteCountOfStackValue operation byteCountArg

        let state =
            // CoreCLR returns immediately for `len == 0` (SpanHelpers.ByteMemOps.cs:248).
            // Honouring that explicitly matters here rather than falling out of an empty
            // loop: `Array.Clear` on a zero-length array hands us the byref to where
            // element 0 *would* have been, which must never be dereferenced.
            if byteCount = 0 then
                state
            else
                // Accepts both strict managed byrefs and `NativeInt`-wrapped managed
                // pointers, as `executeSpanHelpersMemmove` does; `NativeMemory.Clear`
                // reaches this helper as `ref *(byte*)ptr` (NativeMemory.cs:51).
                let destPtr = managedPointerOfPointerArgument operation destArg

                match destPtr with
                | ManagedPointerSource.Null ->
                    failwith $"%s{operation}: refusing nonzero byte clear of null destination"
                | _ -> CellAwareMemOps.clear baseClassTypes operation state destPtr byteCount

        state |> IlMachineState.advanceProgramCounter currentThread
