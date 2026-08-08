namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEvalStack =

    let private runtimePointerTarget : CliType =
        CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)

    let private assertDoesNotReturnObjectRef (popped : EvalStackValue) : unit =
        let outcome : Choice<CliType, exn> =
            try
                EvalStackValue.toCliTypeCoerced runtimePointerTarget popped |> Choice1Of2
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 (CliType.ObjectRef returned) ->
            failwith $"Bug: coercing %O{popped} to RuntimePointer returned ObjectRef(%O{returned})"
        | Choice1Of2 (CliType.RuntimePointer _) -> ()
        | Choice1Of2 other -> failwith $"Unexpected result from RuntimePointer coercion: %O{other}"
        | Choice2Of2 _ -> ()

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target does not return ObjectRef for NullObjectRef`` () : unit =
        assertDoesNotReturnObjectRef EvalStackValue.NullObjectRef

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target does not return ObjectRef for ObjectRef`` () : unit =
        ManagedHeapAddress.ManagedHeapAddress 42
        |> EvalStackValue.ObjectRef
        |> assertDoesNotReturnObjectRef

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target preserves method table pointer provenance`` () : unit =
        let typeHandle = RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42)

        match
            EvalStackValue.toCliTypeCoerced
                runtimePointerTarget
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr typeHandle))
        with
        | CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected RuntimePointer(MethodTablePtr %O{typeHandle}), got %O{other}"

    [<Test>]
    let ``RuntimePointer carrying method table pointer flattens back to native int`` () : unit =
        let typeHandle = RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42)

        match EvalStackValue.ofCliType (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr typeHandle)) with
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected NativeInt(MethodTablePtr %O{typeHandle}), got %O{other}"

    // A GC handle's tag bits are part of the value managed code is holding — a
    // `WeakReference` keeps `handle | TracksResurrectionBit` in a field for the
    // object's whole lifetime — so they have to survive storage round-trips
    // alongside the handle's identity, not just the identity on its own.
    [<TestCase(0L)>]
    [<TestCase(1L)>]
    [<TestCase(3L)>]
    let ``toCliTypeCoerced RuntimePointer target preserves GC handle pointer provenance`` (tag : int64) : unit =
        let handle = GcHandleAddress.GcHandleAddress 42

        match
            EvalStackValue.toCliTypeCoerced
                runtimePointerTarget
                (EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag)))
        with
        | CliType.RuntimePointer (CliRuntimePointer.GcHandlePtr (actual, actualTag)) when
            actual = handle && actualTag = tag
            ->
            ()
        | other -> failwith $"Expected RuntimePointer(GcHandlePtr %O{handle}, tag 0x%x{tag}), got %O{other}"

    [<TestCase(0L)>]
    [<TestCase(1L)>]
    [<TestCase(3L)>]
    let ``RuntimePointer carrying GC handle pointer flattens back to native int`` (tag : int64) : unit =
        let handle = GcHandleAddress.GcHandleAddress 42

        match EvalStackValue.ofCliType (CliType.RuntimePointer (CliRuntimePointer.GcHandlePtr (handle, tag))) with
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (actual, actualTag)) when
            actual = handle && actualTag = tag
            ->
            ()
        | other -> failwith $"Expected NativeInt(GcHandlePtr %O{handle}, tag 0x%x{tag}), got %O{other}"

    [<Test>]
    let ``Conv_U preserves PE byte-range managed pointer provenance`` () : unit =
        let peByteRange =
            {
                AssemblyFullName = "Example"
                Source =
                    PeByteRangePointerSource.FieldRva (
                        ComparableFieldDefinitionHandle.Make (
                            Unchecked.defaultof<System.Reflection.Metadata.FieldDefinitionHandle>
                        )
                    )
                RelativeVirtualAddress = 4096
                Size = 8
            }

        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, [ ByrefProjection.ByteOffset 4 ])

        match EvalStackValue.toUnsignedNativeInt (EvalStackValue.ManagedPointer ptr) with
        | UnsignedNativeIntSource.FromManagedPointer actual ->
            match actual with
            | ManagedPointerSource.Byref (ByrefRoot.PeByteRange actualPeByteRange, [ ByrefProjection.ByteOffset 4 ]) when
                actualPeByteRange = peByteRange
                ->
                ()
            | other -> failwith $"Expected Conv_U to preserve PE byte-range pointer provenance, got %O{other}"
        | other -> failwith $"Expected Conv_U to return FromManagedPointer for PE byte-range pointer, got %O{other}"

    [<Test>]
    let ``ceq compares method table pointers by concrete type identity`` () : unit =
        let methodTable =
            EvalStackValue.NativeInt (
                NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))
            )

        let sameMethodTable =
            EvalStackValue.NativeInt (
                NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))
            )

        let otherMethodTable =
            EvalStackValue.NativeInt (
                NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 43))
            )

        let sameRuntimeTypeHandle =
            EvalStackValue.NativeInt (
                NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))
            )

        let otherRuntimeTypeHandle =
            EvalStackValue.NativeInt (
                NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 43))
            )

        let openGenericRuntimeTypeHandle =
            let identity =
                ResolvedTypeIdentity.ofTypeDefinition
                    (System.Reflection.AssemblyName "TestAssembly")
                    (System.Reflection.Metadata.Ecma335.MetadataTokens.TypeDefinitionHandle 1)

            EvalStackValue.NativeInt (
                NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)
            )

        // Array handles have a MethodTable in CoreCLR, so they should alias a MethodTablePtr.
        let arrayHandle =
            ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 42)

        let arrayMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed arrayHandle))

        let arrayRuntimeTypeHandle =
            EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed arrayHandle))

        // Pointer/Byref/FunctionPointer are TypeDescs in CoreCLR — they have no MethodTable, so
        // a TypeHandlePtr to one must NEVER alias a synthetic MethodTablePtr for the same handle.
        let pointerHandle = ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 42)

        let pointerMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed pointerHandle))

        let pointerRuntimeTypeHandle =
            EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed pointerHandle))

        let byrefHandle = ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 42)

        let byrefMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed byrefHandle))

        let byrefRuntimeTypeHandle =
            EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed byrefHandle))

        if not (EvalStackValueComparisons.ceq methodTable sameMethodTable) then
            failwith "Expected matching MethodTablePtr values to compare equal"

        if EvalStackValueComparisons.ceq methodTable otherMethodTable then
            failwith "Expected different MethodTablePtr values to compare unequal"

        // CoreCLR patterns like RuntimeHelpers.GetMethodTable(obj) == TypeHandleOf<T>().AsMethodTable()
        // require these two encodings to compare equal when they reference the same concrete type.
        if not (EvalStackValueComparisons.ceq methodTable sameRuntimeTypeHandle) then
            failwith
                "Expected MethodTablePtr to compare equal to TypeHandlePtr(Closed) wrapping the same concrete handle"

        if not (EvalStackValueComparisons.ceq sameRuntimeTypeHandle methodTable) then
            failwith "Expected MethodTablePtr/TypeHandlePtr equality to be symmetric"

        if EvalStackValueComparisons.ceq methodTable otherRuntimeTypeHandle then
            failwith
                "Expected MethodTablePtr to compare unequal to TypeHandlePtr(Closed) of a different concrete handle"

        // Open generic type definitions don't have a closed MethodTable, so MethodTablePtr never aliases them.
        if EvalStackValueComparisons.ceq methodTable openGenericRuntimeTypeHandle then
            failwith "Expected MethodTablePtr to remain distinct from TypeHandlePtr(OpenGenericTypeDefinition)"

        // Array MethodTables are real in CoreCLR, so the cross-arm must alias them.
        if not (EvalStackValueComparisons.ceq arrayMethodTable arrayRuntimeTypeHandle) then
            failwith "Expected MethodTablePtr to alias TypeHandlePtr(Closed) for array handles"

        // Pointer/Byref/FunctionPointer are TypeDesc-only; aliasing would let TypeDesc handles
        // take the MethodTable branch in cast/equality patterns.
        if EvalStackValueComparisons.ceq pointerMethodTable pointerRuntimeTypeHandle then
            failwith "Expected TypeDesc Pointer handles to remain distinct from MethodTablePtr"

        if EvalStackValueComparisons.ceq byrefMethodTable byrefRuntimeTypeHandle then
            failwith "Expected TypeDesc Byref handles to remain distinct from MethodTablePtr"

    [<Test>]
    let ``ceq of WidenedNativeInt vs OpaqueHashBits fails loudly rather than returning silently wrong`` () : unit =
        // Under the counter-based pointer-hash scheme, an identity bit op such as `x ^ 0UL`
        // materialises the WidenedNativeInt's bits into OpaqueHashBits. A subsequent
        // `x == y` would then ask: do `WidenedNativeInt x`'s materialised bits equal `b`?
        // Answering that requires the PointerHashCounters map which ceq does not currently
        // thread; the silent-false answer that prior versions returned is wrong under
        // identity ops, so this case fails loudly until ceq is taught to look it up.
        let widened =
            EvalStackValue.Int64 (
                Int64Source.WidenedNativeInt (
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42)),
                    true
                )
            )

        let hashBits = EvalStackValue.Int64 (Int64Source.OpaqueHashBits 4L)

        let ex =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.ceq widened hashBits |> ignore)

        ex.Message |> shouldContainText "WidenedNativeInt"
        ex.Message |> shouldContainText "OpaqueHashBits"
        ex.Message |> shouldContainText "PointerHashCounters"

        // And symmetrically the other direction.
        let exSym =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.ceq hashBits widened |> ignore)

        exSym.Message |> shouldContainText "PointerHashCounters"

    [<Test>]
    let ``ceq of SyntheticCrossArrayOffset vs OpaqueHashBits returns false (cross-shape)`` () : unit =
        // SyntheticCrossArrayOffset is a delta between two distinct byte-storage roots;
        // OpaqueHashBits is synthesised pointer hash bits. The two shapes are not
        // comparable as numeric values — there is no bit pattern at which they could
        // sensibly be considered equal — so ceq returns false rather than failing.
        let offset =
            SyntheticCrossArrayOffset.make
                (ByteStorageIdentity.Array (ManagedHeapAddress 1))
                0L
                (ByteStorageIdentity.Array (ManagedHeapAddress 2))
                0L

        let offsetEsv = EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset offset)
        let hashBits = EvalStackValue.Int64 (Int64Source.OpaqueHashBits 12L)

        if EvalStackValueComparisons.ceq offsetEsv hashBits then
            failwith "Expected ceq(SyntheticCrossArrayOffset, OpaqueHashBits) to be false"

        if EvalStackValueComparisons.ceq hashBits offsetEsv then
            failwith "Expected ceq(OpaqueHashBits, SyntheticCrossArrayOffset) to be false (symmetric)"

    [<Test>]
    let ``ceq compares managed pointers with native-int pointer forms`` () : unit =
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress 707), [])

        let managedPtr = EvalStackValue.ManagedPointer ptr
        let nativePtr = EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
        let nativeZero = EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
        let managedNull = EvalStackValue.ManagedPointer ManagedPointerSource.Null

        if not (EvalStackValueComparisons.ceq managedPtr nativePtr) then
            failwith "Expected a managed pointer to compare equal to the native-int form of the same pointer"

        if not (EvalStackValueComparisons.ceq nativePtr managedPtr) then
            failwith "Expected native-int pointer comparison to be symmetric"

        if EvalStackValueComparisons.ceq managedPtr nativeZero then
            failwith "Expected a non-null managed pointer to compare unequal to native zero"

        if EvalStackValueComparisons.ceq nativeZero managedPtr then
            failwith "Expected native zero to compare unequal to a non-null managed pointer"

        if not (EvalStackValueComparisons.ceq managedNull nativeZero) then
            failwith "Expected a null managed pointer to compare equal to native zero"

        if not (EvalStackValueComparisons.ceq nativeZero managedNull) then
            failwith "Expected native zero to compare equal to a null managed pointer"

    [<Test>]
    let ``unsigned-or-unordered branch comparisons treat NaN as true`` () : unit =
        let nan = EvalStackValue.Float System.Double.NaN
        let one = EvalStackValue.Float 1.0

        if not (EvalStackValueComparisons.cgtUn nan one) then
            failwith "Expected cgt.un-style float comparison to be true when left operand is NaN"

        if not (EvalStackValueComparisons.cltUn one nan) then
            failwith "Expected clt.un-style float comparison to be true when right operand is NaN"

        if not (EvalStackValueComparisons.cgeUn nan one) then
            failwith "Expected bge.un-style float comparison to be true when left operand is NaN"

        if not (EvalStackValueComparisons.cleUn nan one) then
            failwith "Expected ble.un-style float comparison to be true when left operand is NaN"

        if not (EvalStackValueComparisons.cgeUn one nan) then
            failwith "Expected bge.un-style float comparison to be true when right operand is NaN"

        if not (EvalStackValueComparisons.cleUn one nan) then
            failwith "Expected ble.un-style float comparison to be true when right operand is NaN"

    // Tag bits never make a handle look null: `base` is non-zero on its own, so
    // `WeakReference.get_Target`'s `if (th == 0) return default` must not fire for
    // a stripped-but-still-live handle either.
    [<TestCase(0L)>]
    [<TestCase(1L)>]
    [<TestCase(3L)>]
    let ``unsigned comparisons treat GcHandlePtr as strictly greater than zero`` (tag : int64) : unit =
        // GC handle addresses are minted starting from 1 by GcHandleRegistry, so a
        // GcHandlePtr is never null. `cgt.un` is the unsigned greater-than
        // comparison; on native-int operands it's emitted by `nuint`/`UIntPtr`
        // ordering and is also the canonical CIL idiom for "non-null" checks
        // against object refs. With a GC handle on the eval stack, `cgt.un`
        // against zero must answer truthfully rather than falling through to
        // the generic non-Verbatim TODO. The symmetric `clt.un` direction (and
        // therefore `cge.un` / `cle.un`, which are derived from the two) must
        // also answer truthfully.
        let handle =
            EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 42, tag))

        let zero = EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)

        if not (EvalStackValueComparisons.cgtUn handle zero) then
            failwith "Expected cgt.un to report a GcHandlePtr as strictly greater than zero"

        if EvalStackValueComparisons.cgtUn zero handle then
            failwith "Expected cgt.un to report zero as not strictly greater than a GcHandlePtr"

        if EvalStackValueComparisons.cltUn handle zero then
            failwith "Expected clt.un to report a GcHandlePtr as not strictly less than zero"

        if not (EvalStackValueComparisons.cltUn zero handle) then
            failwith "Expected clt.un to report zero as strictly less than a GcHandlePtr"

        // bge.un / ble.un are derived from cltUn / cgtUn respectively; check
        // them too so a regression that re-breaks the underlying arms is
        // caught regardless of which entry point the runtime calls.
        if not (EvalStackValueComparisons.cgeUn handle zero) then
            failwith "Expected bge.un to report a GcHandlePtr as >= zero"

        if EvalStackValueComparisons.cleUn handle zero then
            failwith "Expected ble.un to report a GcHandlePtr as not <= zero"

    [<Test>]
    let ``unsigned comparisons treat ManagedPointer Null as zero against Verbatim`` () : unit =
        // `IntPtr.Zero` / `UIntPtr.Zero` are `[Intrinsic]` static fields with no IL
        // initialiser. `cliTypeZeroOf` populates their slots with
        // `NativeIntSource.ManagedPointer ManagedPointerSource.Null`, which represents the
        // value 0 but in a different shape from `Verbatim 0L`. The C# pattern
        // `if (IntPtr.Zero != default(IntPtr))` lowers (in Debug mode) to
        // `ldsfld; ldc.i4.0; conv.i; cgt.un`, so `cgtUn` must relate
        // `ManagedPointer Null` and `Verbatim 0L` correctly. `clt.un` / `cge.un` / `cle.un`
        // must answer symmetrically.
        let nullPtr =
            EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)

        let verbatimZero = EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
        let verbatimOne = EvalStackValue.NativeInt (NativeIntSource.Verbatim 1L)
        // `-1L` reinterprets as `0xFFFF_FFFF_FFFF_FFFF`, the maximum uint64, so it
        // exercises the wrap-around case that a naive signed implementation would
        // get wrong.
        let verbatimMaxU = EvalStackValue.NativeInt (NativeIntSource.Verbatim -1L)

        if EvalStackValueComparisons.cgtUn nullPtr verbatimZero then
            failwith "cgt.un should report ManagedPointer Null as not strictly greater than Verbatim 0L"

        if EvalStackValueComparisons.cgtUn verbatimZero nullPtr then
            failwith "cgt.un should report Verbatim 0L as not strictly greater than ManagedPointer Null"

        if EvalStackValueComparisons.cltUn nullPtr verbatimZero then
            failwith "clt.un should report ManagedPointer Null as not strictly less than Verbatim 0L"

        if EvalStackValueComparisons.cltUn verbatimZero nullPtr then
            failwith "clt.un should report Verbatim 0L as not strictly less than ManagedPointer Null"

        // Non-zero Verbatim against Null: Null is "0", so the Verbatim side
        // strictly dominates under unsigned comparison whenever it is non-zero.
        if not (EvalStackValueComparisons.cgtUn verbatimOne nullPtr) then
            failwith "cgt.un should report Verbatim 1L as strictly greater than ManagedPointer Null"

        if EvalStackValueComparisons.cgtUn nullPtr verbatimOne then
            failwith "cgt.un should report ManagedPointer Null as not strictly greater than Verbatim 1L"

        if not (EvalStackValueComparisons.cltUn nullPtr verbatimOne) then
            failwith "clt.un should report ManagedPointer Null as strictly less than Verbatim 1L"

        if EvalStackValueComparisons.cltUn verbatimOne nullPtr then
            failwith "clt.un should report Verbatim 1L as not strictly less than ManagedPointer Null"

        // Signed-negative Verbatim reinterprets as the max unsigned value, so
        // the relation is still "Null = 0 < max_uint64" even though signed `-1 < 0`.
        if not (EvalStackValueComparisons.cgtUn verbatimMaxU nullPtr) then
            failwith "cgt.un should report Verbatim -1L (max unsigned) as strictly greater than ManagedPointer Null"

        if EvalStackValueComparisons.cgtUn nullPtr verbatimMaxU then
            failwith "cgt.un should report ManagedPointer Null as not strictly greater than Verbatim -1L"

        // bge.un / ble.un derived comparisons must agree.
        if not (EvalStackValueComparisons.cgeUn nullPtr verbatimZero) then
            failwith "bge.un should report ManagedPointer Null as >= Verbatim 0L"

        if not (EvalStackValueComparisons.cleUn nullPtr verbatimZero) then
            failwith "ble.un should report ManagedPointer Null as <= Verbatim 0L"

    [<Test>]
    let ``unsigned comparisons treat non-null ManagedPointer as strictly greater than Verbatim zero`` () : unit =
        // A non-null managed pointer is some live address, which we model as an
        // unknown but strictly non-zero value (cf. the GcHandlePtr arms). That
        // makes the comparisons against `Verbatim 0L` well-defined: the pointer
        // is greater. Comparisons against arbitrary non-zero Verbatims remain
        // refused because the actual address is not known in our model.
        let ptr =
            EvalStackValue.NativeInt (
                NativeIntSource.ManagedPointer (
                    ManagedPointerSource.Byref (
                        ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 0, 0us),
                        []
                    )
                )
            )

        let verbatimZero = EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)

        if not (EvalStackValueComparisons.cgtUn ptr verbatimZero) then
            failwith "cgt.un should report non-null ManagedPointer as strictly greater than Verbatim 0L"

        if EvalStackValueComparisons.cgtUn verbatimZero ptr then
            failwith "cgt.un should report Verbatim 0L as not strictly greater than a non-null ManagedPointer"

        if EvalStackValueComparisons.cltUn ptr verbatimZero then
            failwith "clt.un should report non-null ManagedPointer as not strictly less than Verbatim 0L"

        if not (EvalStackValueComparisons.cltUn verbatimZero ptr) then
            failwith "clt.un should report Verbatim 0L as strictly less than a non-null ManagedPointer"

    [<Test>]
    let ``toCliTypeCoerced Int64 target preserves SyntheticCrossArrayOffset provenance`` () : unit =
        // Regression: Int64-target slots used to widen synthetic cross-array offsets to NativeInt,
        // erasing the Int64Source wrapper. The coercion must preserve the variant unchanged so the
        // value can flow back through arithmetic that's only defined for Int64Source.
        let synthetic : SyntheticCrossArrayOffset =
            SyntheticCrossArrayOffset.make
                (ByteStorageIdentity.Array (ManagedHeapAddress 11))
                7L
                (ByteStorageIdentity.String (ManagedHeapAddress 13))
                3L

        let target : CliType =
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

        let popped : EvalStackValue =
            EvalStackValue.Int64 (Int64Source.SyntheticCrossArrayOffset synthetic)

        match EvalStackValue.toCliTypeCoerced target popped with
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.SyntheticCrossArrayOffset actual)) ->
            if actual <> synthetic then
                failwith $"expected synthetic to round-trip unchanged, got %O{actual}"
        | other -> failwith $"expected Int64 target to preserve synthetic, got %O{other}"

    [<Test>]
    let ``unsigned comparisons order native-memory byrefs by byte offset within the same block`` () : unit =
        let block = NativeMemoryBlockId.NativeMemoryBlockId 0

        let pointerAt (byteOffset : int) : EvalStackValue =
            EvalStackValue.NativeInt (
                NativeIntSource.ManagedPointer (
                    ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), [])
                )
            )

        let lo = pointerAt 4
        let hi = pointerAt 16

        if not (EvalStackValueComparisons.cgtUn hi lo) then
            failwith
                "cgt.un should report higher native-memory offset as strictly greater than a lower one in the same block"

        if EvalStackValueComparisons.cgtUn lo hi then
            failwith
                "cgt.un should report lower native-memory offset as not strictly greater than a higher one in the same block"

        if not (EvalStackValueComparisons.cltUn lo hi) then
            failwith
                "clt.un should report lower native-memory offset as strictly less than a higher one in the same block"

        if EvalStackValueComparisons.cltUn hi lo then
            failwith
                "clt.un should report higher native-memory offset as not strictly less than a lower one in the same block"

        // Equality: same byte offset must not be reported as strictly ordered.
        let alsoLo = pointerAt 4

        if EvalStackValueComparisons.cgtUn lo alsoLo then
            failwith "cgt.un should report equal native-memory offsets as not strictly ordered"

        if EvalStackValueComparisons.cltUn lo alsoLo then
            failwith "clt.un should report equal native-memory offsets as not strictly ordered"

    // Sentinel floats covering the regimes the ordered ble/bge arms must respect.
    // ECMA-335 III.3.7 specifies "ordered" for ble/bge: NaN comparisons must report the
    // branch as *not* taken, which corresponds to IEEE `<=` / `>=` returning false on NaN.
    let private nan : float = System.Double.NaN
    let private pInf : float = System.Double.PositiveInfinity
    let private nInf : float = System.Double.NegativeInfinity
    let private pZero : float = 0.0
    let private nZero : float = -0.0
    let private subnormal : float = System.Double.Epsilon

    let private floatEsv (v : float) : EvalStackValue = EvalStackValue.Float v

    [<Test>]
    let ``cle on Float × Float matches IEEE <= (ordered semantics)`` () : unit =
        EvalStackValueComparisons.cle (floatEsv 1.0) (floatEsv 2.0) |> shouldEqual true
        EvalStackValueComparisons.cle (floatEsv 2.0) (floatEsv 1.0) |> shouldEqual false
        EvalStackValueComparisons.cle (floatEsv 1.0) (floatEsv 1.0) |> shouldEqual true

        // ±0 compare equal.
        EvalStackValueComparisons.cle (floatEsv pZero) (floatEsv nZero)
        |> shouldEqual true

        EvalStackValueComparisons.cle (floatEsv nZero) (floatEsv pZero)
        |> shouldEqual true

        // ±Inf.
        EvalStackValueComparisons.cle (floatEsv nInf) (floatEsv 0.0) |> shouldEqual true
        EvalStackValueComparisons.cle (floatEsv 0.0) (floatEsv pInf) |> shouldEqual true

        EvalStackValueComparisons.cle (floatEsv pInf) (floatEsv pInf)
        |> shouldEqual true

        EvalStackValueComparisons.cle (floatEsv pInf) (floatEsv 0.0)
        |> shouldEqual false

        // Subnormals are real, finite values; ordering is well-defined.
        EvalStackValueComparisons.cle (floatEsv subnormal) (floatEsv 1.0)
        |> shouldEqual true

        EvalStackValueComparisons.cle (floatEsv 0.0) (floatEsv subnormal)
        |> shouldEqual true

        // Ordered: NaN compared to anything (including itself) must report false, so the
        // branch is not taken.
        EvalStackValueComparisons.cle (floatEsv nan) (floatEsv 1.0) |> shouldEqual false
        EvalStackValueComparisons.cle (floatEsv 1.0) (floatEsv nan) |> shouldEqual false
        EvalStackValueComparisons.cle (floatEsv nan) (floatEsv nan) |> shouldEqual false

        EvalStackValueComparisons.cle (floatEsv nan) (floatEsv pInf)
        |> shouldEqual false

        EvalStackValueComparisons.cle (floatEsv nInf) (floatEsv nan)
        |> shouldEqual false

    [<Test>]
    let ``cge on Float × Float matches IEEE >= (ordered semantics)`` () : unit =
        EvalStackValueComparisons.cge (floatEsv 2.0) (floatEsv 1.0) |> shouldEqual true
        EvalStackValueComparisons.cge (floatEsv 1.0) (floatEsv 2.0) |> shouldEqual false
        EvalStackValueComparisons.cge (floatEsv 1.0) (floatEsv 1.0) |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv pZero) (floatEsv nZero)
        |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv nZero) (floatEsv pZero)
        |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv pInf) (floatEsv 0.0) |> shouldEqual true
        EvalStackValueComparisons.cge (floatEsv 0.0) (floatEsv nInf) |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv nInf) (floatEsv nInf)
        |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv 0.0) (floatEsv pInf)
        |> shouldEqual false

        EvalStackValueComparisons.cge (floatEsv 1.0) (floatEsv subnormal)
        |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv subnormal) (floatEsv 0.0)
        |> shouldEqual true

        EvalStackValueComparisons.cge (floatEsv nan) (floatEsv 1.0) |> shouldEqual false
        EvalStackValueComparisons.cge (floatEsv 1.0) (floatEsv nan) |> shouldEqual false
        EvalStackValueComparisons.cge (floatEsv nan) (floatEsv nan) |> shouldEqual false

        EvalStackValueComparisons.cge (floatEsv pInf) (floatEsv nan)
        |> shouldEqual false

        EvalStackValueComparisons.cge (floatEsv nan) (floatEsv nInf)
        |> shouldEqual false

    [<Test>]
    let ``cle and cge on Int × Int agree with the obvious arithmetic`` () : unit =
        let i32 (v : int32) =
            EvalStackValue.Int32 (Int32Source.Verbatim v)

        let i64 (v : int64) =
            EvalStackValue.Int64 (Int64Source.Verbatim v)

        EvalStackValueComparisons.cle (i32 1) (i32 2) |> shouldEqual true
        EvalStackValueComparisons.cle (i32 2) (i32 2) |> shouldEqual true
        EvalStackValueComparisons.cle (i32 3) (i32 2) |> shouldEqual false

        EvalStackValueComparisons.cge (i32 2) (i32 1) |> shouldEqual true
        EvalStackValueComparisons.cge (i32 2) (i32 2) |> shouldEqual true
        EvalStackValueComparisons.cge (i32 1) (i32 2) |> shouldEqual false

        EvalStackValueComparisons.cle (i64 1L) (i64 2L) |> shouldEqual true
        EvalStackValueComparisons.cle (i64 -5L) (i64 -5L) |> shouldEqual true
        EvalStackValueComparisons.cge (i64 -1L) (i64 -5L) |> shouldEqual true
        EvalStackValueComparisons.cge (i64 -5L) (i64 -1L) |> shouldEqual false

    [<Test>]
    let ``cle and cge fail on Float × Int (cross-type guard inherited from cgt/clt)`` () : unit =
        // ECMA-335 leaves cross-shape numeric comparisons unverifiable; we keep the
        // classifier honest by failing loudly rather than coercing one side. cle defers
        // non-Float×Float to `not cgt`, so the Float × Int case re-uses cgt's existing
        // "invalid comparison" failwith.
        let f = EvalStackValue.Float 1.0
        let i = EvalStackValue.Int32 (Int32Source.Verbatim 1)
        let n = EvalStackValue.NativeInt (NativeIntSource.Verbatim 1L)

        let exFI =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cle f i |> ignore)

        exFI.Message |> shouldContainText "invalid comparison"

        let exIF =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cle i f |> ignore)

        exIF.Message |> shouldContainText "invalid comparison"

        let exFN =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cle f n |> ignore)

        exFN.Message |> shouldContainText "invalid comparison"

        let exNF =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cle n f |> ignore)

        exNF.Message |> shouldContainText "invalid comparison"

        // And symmetrically for cge (defers to `not clt`).
        let gxFI =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cge f i |> ignore)

        gxFI.Message |> shouldContainText "invalid comparison"

        let gxIF =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cge i f |> ignore)

        gxIF.Message |> shouldContainText "invalid comparison"

        let gxFN =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cge f n |> ignore)

        gxFN.Message |> shouldContainText "invalid comparison"

        let gxNF =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cge n f |> ignore)

        gxNF.Message |> shouldContainText "invalid comparison"

    [<Test>]
    let ``unsigned comparisons order two byrefs into the same string by character index`` () : unit =
        // `EventSource`'s manifest handling compares a byref to the start of a name
        // against one part-way into the *same* string, which reaches `clt.un` as two
        // `StringCharAt` byrefs sharing an object. Refusing that was needlessly
        // conservative: they plainly have a common root, and two characters of one
        // string sit at known, ordered addresses.
        //
        // The justification mirrors the `ArrayElement` arm of
        // `tryByteAddressDeltaSign`: a char cell is two bytes — strictly positive —
        // and canonicalisation keeps each pointer's byte effect within its own cell,
        // so index order is address order.
        let str = ManagedHeapAddress.ManagedHeapAddress 105

        let at (charIndex : int) : EvalStackValue =
            EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []))

        let atStart = at 0
        let later = at 28

        if not (EvalStackValueComparisons.cltUn atStart later) then
            failwith "Expected clt.un to report char 0 as strictly below char 28 of the same string"

        if EvalStackValueComparisons.cltUn later atStart then
            failwith "Expected clt.un to report char 28 as not strictly below char 0 of the same string"

        if not (EvalStackValueComparisons.cgtUn later atStart) then
            failwith "Expected cgt.un to report char 28 as strictly above char 0 of the same string"

        if EvalStackValueComparisons.cgtUn atStart later then
            failwith "Expected cgt.un to report char 0 as not strictly above char 28 of the same string"

        // The same character is neither above nor below itself. This case already
        // worked, via the identical-root path; it is here so that a fix which
        // answers only for *differing* indices is still caught.
        if EvalStackValueComparisons.cltUn atStart (at 0) then
            failwith "Expected clt.un to report a character as not strictly below itself"

        if EvalStackValueComparisons.cgtUn atStart (at 0) then
            failwith "Expected cgt.un to report a character as not strictly above itself"

    [<Test>]
    let ``unsigned comparisons still refuse byrefs into two different strings`` () : unit =
        // The counterpart to the arm above, and the reason it is keyed on the string
        // object rather than on the root shape: two separately allocated strings have
        // no defensible ordering, so refusing loudly stays the right answer. Without
        // this, widening the arm to any pair of `StringCharAt` byrefs would look
        // correct.
        let first =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (ByrefRoot.StringCharAt (ManagedHeapAddress.ManagedHeapAddress 105, 0), [])
            )

        let second =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (ByrefRoot.StringCharAt (ManagedHeapAddress.ManagedHeapAddress 106, 4), [])
            )

        let lt =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cltUn first second |> ignore)

        lt.Message |> shouldContainText "without a common root"

        let gt =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cgtUn first second |> ignore)

        gt.Message |> shouldContainText "without a common root"

    [<Test>]
    let ``unsigned comparisons refuse two string byrefs whose projections differ`` () : unit =
        // The ordering arm above holds `addr = base + 2*index + d`, where `d` is the byte
        // effect of the projections. That gives `compare index2 index1` the sign of the
        // address delta only when `d` agrees on both sides — which is why the arm requires
        // the projection lists to be equal rather than merely canonical.
        //
        // Canonicality alone does not bound `d` within the two-byte char cell: it permits a
        // reinterpret to a wider type with a byte cursor past that cell, or a field
        // projection at any offset. So a byref at char 0 carrying such a projection can
        // genuinely sit *above* a bare byref at char 1, and index order would report the
        // opposite. Refusing is the honest answer; the array arm gets to be more permissive
        // only because a field offset there is bounded by the element's own layout.
        let str = ManagedHeapAddress.ManagedHeapAddress 105

        let projected =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.StringCharAt (str, 0),
                    [ ByrefProjection.Field (FieldId.Named "someField") ]
                )
            )

        let bare =
            EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, 1), []))

        let lt =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cltUn projected bare |> ignore)

        lt.Message |> shouldContainText "without a common root"

        let gt =
            Assert.Throws<System.Exception> (fun () -> EvalStackValueComparisons.cgtUn bare projected |> ignore)

        gt.Message |> shouldContainText "without a common root"

        // Equal projections are fine, though: `d` cancels, whatever it is. This is the
        // generalisation the arm does allow, and it is why the check is equality rather
        // than emptiness.
        let alsoProjected =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.StringCharAt (str, 1),
                    [ ByrefProjection.Field (FieldId.Named "someField") ]
                )
            )

        if not (EvalStackValueComparisons.cltUn projected alsoProjected) then
            failwith "Expected clt.un to order two identically-projected string byrefs by index"

    // ---------------------------------------------------------------------------
    // Narrowing integer conversions of pointer-shaped values.
    //
    // `conv.i1` / `conv.i2` / `conv.i4` and their unsigned counterparts narrow to a
    // destination smaller than a pointer, so the result cannot be a pointer and the
    // honest answer is the source's bits — synthesised from the pointer's identity
    // when PawPrint models no address for it. These tests pin that the synthesis
    // happens at the conversion, is stable, and is reached identically whether the
    // pointer arrives in the native-int slot or already widened to int64.
    // ---------------------------------------------------------------------------

    /// One of the six unchecked narrowing conversions, normalised so a single
    /// property can range over all of them: the four sub-32-bit conversions return the
    /// pushed int32 directly, while `conv.i4` / `conv.u4` return the `EvalStackValue`
    /// they push, so the latter are unwrapped to the same int32.
    type private NarrowingConv =
        {
            Name : string
            /// Width of the destination in bits, for the cross-width coherence property.
            DestinationBits : int
            Apply : EvalStackValue -> PointerHashCounters -> int32 * PointerHashCounters
        }

    let private ofInt32Returning
        (name : string)
        (destinationBits : int)
        (f : EvalStackValue -> PointerHashCounters -> int32 * PointerHashCounters)
        : NarrowingConv
        =
        {
            Name = name
            DestinationBits = destinationBits
            Apply = f
        }

    let private ofEvalStackReturning
        (name : string)
        (f : EvalStackValue -> PointerHashCounters -> EvalStackValue * PointerHashCounters)
        : NarrowingConv
        =
        {
            Name = name
            DestinationBits = 32
            Apply =
                fun value counters ->
                    match f value counters with
                    | EvalStackValue.Int32 (Int32Source.Verbatim i), counters -> i, counters
                    | other, _ -> failwith $"%s{name}: expected a verbatim int32, got %O{other}"
        }

    let private narrowingConversions : NarrowingConv list =
        [
            ofInt32Returning "conv.i1" 8 EvalStackValue.convToInt8
            ofInt32Returning "conv.u1" 8 EvalStackValue.convToUInt8
            ofInt32Returning "conv.i2" 16 EvalStackValue.convToInt16
            ofInt32Returning "conv.u2" 16 EvalStackValue.convToUInt16
            ofEvalStackReturning "conv.i4" EvalStackValue.convToInt32
            ofEvalStackReturning "conv.u4" EvalStackValue.convToUInt32
        ]

    /// Pointer-shaped `NativeIntSource`s that `materialiseHashBits` can canonicalise,
    /// spanning the interesting axes: a MethodTable-keyed shape and the TypeHandle
    /// encoding that aliases it, a TypeDesc-keyed shape and the TypeDescPtr that
    /// shares its identity but not its tag, a tagged GC handle, and the opaque
    /// integer- and string-keyed handle families.
    ///
    /// `FunctionPointer` is absent because constructing one needs a whole
    /// `WoofWare.PawPrint.MethodInfo`; `PerInstInfoPtr` / `PerInstDictPtr` and the
    /// generic-parameter `MethodTablePtr` targets are absent because
    /// `materialiseHashBits` refuses them by design.
    let private genPointerShapedSource : Gen<NativeIntSource> =
        Gen.oneof
            [
                Gen.choose (0, 5)
                |> Gen.map (fun n ->
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete n))
                )
                Gen.choose (0, 5)
                |> Gen.map (fun n ->
                    NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete n))
                )
                Gen.choose (0, 5)
                |> Gen.map (fun n ->
                    NativeIntSource.TypeHandlePtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete n))
                    )
                )
                Gen.choose (0, 5)
                |> Gen.map (fun n ->
                    NativeIntSource.TypeDescPtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete n))
                    )
                )
                Gen.choose (0, 5)
                |> Gen.map (fun n ->
                    NativeIntSource.MethodTableAuxiliaryDataPtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete n)
                    )
                )
                Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.MethodHandlePtr
                Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.FieldHandlePtr
                Gen.zip (Gen.choose (0, 5)) (Gen.choose64 (0L, 3L))
                |> Gen.map (fun (addr, tag) -> NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress addr, tag))
                Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.EventPipeProviderPtr
                Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.EventPipeEventPtr
                Gen.elements [ "A" ; "B" ; "C" ] |> Gen.map NativeIntSource.AssemblyHandle
                Gen.elements [ "M1" ; "M2" ] |> Gen.map NativeIntSource.ModuleHandle
                Gen.elements [ "I1" ; "I2" ] |> Gen.map NativeIntSource.MetadataImportHandle
            ]

    let private genConversionAndSource : Gen<NarrowingConv * NativeIntSource> =
        Gen.zip (Gen.elements narrowingConversions) genPointerShapedSource

    let private narrowingPropertyConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 500

    [<Test>]
    let ``narrowing a widened pointer equals narrowing its materialised bits`` () : unit =
        // The oracle for the whole feature: the widened-native-int arm introduces no
        // truncation logic of its own, it only materialises and then defers to the
        // arm that already handled synthesised bits.
        let property ((conv, src) : NarrowingConv * NativeIntSource) : unit =
            let viaWidened, countersAfter =
                conv.Apply (EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, true))) PointerHashCounters.empty

            let bits, expectedCounters =
                PointerHashSynthesis.materialiseHashBits "oracle" src PointerHashCounters.empty

            let viaHashBits, _ =
                conv.Apply (EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits)) expectedCounters

            if viaWidened <> viaHashBits then
                failwith
                    $"%s{conv.Name} of widened %O{src} gave %i{viaWidened}, but of its materialised bits gave %i{viaHashBits}"

            countersAfter.Assigned |> shouldEqual expectedCounters.Assigned
            countersAfter.NextCounter |> shouldEqual expectedCounters.NextCounter

        Check.One (narrowingPropertyConfig, Prop.forAll (Arb.fromGen genConversionAndSource) property)

    [<Test>]
    let ``both spellings of narrowing a pointer agree`` () : unit =
        // `(int)ptr` and `(int)(long)ptr` are one guest operation; CoreLib picks
        // between the spellings by `#if TARGET_64BIT` inside `IntPtr.GetHashCode`.
        let property ((conv, src) : NarrowingConv * NativeIntSource) : unit =
            let direct, directCounters =
                conv.Apply (EvalStackValue.NativeInt src) PointerHashCounters.empty

            let widened, widenedCounters =
                conv.Apply (EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, true))) PointerHashCounters.empty

            if direct <> widened then
                failwith $"%s{conv.Name} of %O{src} gave %i{direct} in the native-int slot but %i{widened} once widened"

            directCounters.Assigned |> shouldEqual widenedCounters.Assigned

            // Signedness of the widening is a property of the int64 slot, not of the
            // pointer, so it cannot change the bits.
            let widenedUnsigned, _ =
                conv.Apply (EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, false))) PointerHashCounters.empty

            widenedUnsigned |> shouldEqual widened

        Check.One (narrowingPropertyConfig, Prop.forAll (Arb.fromGen genConversionAndSource) property)

    [<Test>]
    let ``narrowing the same pointer twice is stable and assigns no second counter`` () : unit =
        let property ((conv, src) : NarrowingConv * NativeIntSource) : unit =
            let first, counters =
                conv.Apply (EvalStackValue.NativeInt src) PointerHashCounters.empty

            let second, counters' = conv.Apply (EvalStackValue.NativeInt src) counters

            second |> shouldEqual first
            counters'.NextCounter |> shouldEqual counters.NextCounter
            counters'.Assigned |> shouldEqual counters.Assigned

        Check.One (narrowingPropertyConfig, Prop.forAll (Arb.fromGen genConversionAndSource) property)

    [<Test>]
    let ``distinct pointers narrow to distinct int32s`` () : unit =
        // The counter scheme stores `(n + 1) <<< 2`, so distinctness survives
        // truncation to 32 bits for any counter count PawPrint can reach. Sub-32-bit
        // widths deliberately make no such claim: a byte cannot hold 2^30 identities.
        let property ((a, b) : NativeIntSource * NativeIntSource) : unit =
            let bitsA, counters =
                PointerHashSynthesis.materialiseHashBits "oracle" a PointerHashCounters.empty

            let bitsB, _ = PointerHashSynthesis.materialiseHashBits "oracle" b counters

            // Only claim distinctness when the two sources really are distinct
            // identities; `MethodTablePtr (Closed h)` and `TypeHandlePtr (Closed h)`
            // alias deliberately, and share bits.
            if bitsA <> bitsB then
                let narrowedA, counters =
                    EvalStackValue.convToInt32 (EvalStackValue.NativeInt a) PointerHashCounters.empty

                let narrowedB, _ = EvalStackValue.convToInt32 (EvalStackValue.NativeInt b) counters

                if narrowedA = narrowedB then
                    failwith $"conv.i4 collapsed distinct pointers %O{a} and %O{b} onto %O{narrowedA}"

        Check.One (
            narrowingPropertyConfig,
            Prop.forAll (Arb.fromGen (Gen.zip genPointerShapedSource genPointerShapedSource)) property
        )

    [<Test>]
    let ``narrow widths are the low bytes of the int32 narrowing`` () : unit =
        let property (src : NativeIntSource) : unit =
            let asInt32, _ =
                EvalStackValue.convToInt32 (EvalStackValue.NativeInt src) PointerHashCounters.empty

            let full =
                match asInt32 with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                | other -> failwith $"expected verbatim int32, got %O{other}"

            for conv in narrowingConversions do
                let narrowed, _ =
                    conv.Apply (EvalStackValue.NativeInt src) PointerHashCounters.empty

                let mask =
                    if conv.DestinationBits = 32 then
                        -1
                    else
                        (1 <<< conv.DestinationBits) - 1

                if (narrowed &&& mask) <> (full &&& mask) then
                    failwith
                        $"%s{conv.Name} of %O{src} gave %i{narrowed}, whose low %i{conv.DestinationBits} bits disagree with conv.i4's %i{full}"

        Check.One (narrowingPropertyConfig, Prop.forAll (Arb.fromGen genPointerShapedSource) property)

    [<Test>]
    let ``narrowing refuses a byref whose address is not modelled`` () : unit =
        let byref =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 21), [])

        // At 32 bits the byref survives as a narrowed pointer rather than becoming a
        // number: a mask against it is still answerable, which is what managed code
        // narrowing an address is usually about.
        for value in
            [
                EvalStackValue.NativeInt (NativeIntSource.ManagedPointer byref)
                EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer byref, true))
            ] do
            for conv, expected in
                [
                    EvalStackValue.convToInt32, "conv.i4"
                    EvalStackValue.convToUInt32, "conv.u4"
                ] do
                match conv value PointerHashCounters.empty with
                | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer p), counters ->
                    p |> shouldEqual byref
                    // No identity was registered: a byref is not a synthesisable handle.
                    counters |> shouldEqual PointerHashCounters.empty
                | other, _ -> failwith $"%s{expected} of %O{value} should keep byref provenance, got %O{other}"

            // Below 32 bits there is no representation for a narrowed byref, so the
            // conversion must refuse rather than invent bits.
            for conv in narrowingConversions |> List.filter (fun c -> c.DestinationBits < 32) do
                let exn =
                    Assert.Throws<System.Exception> (fun () -> conv.Apply value PointerHashCounters.empty |> ignore)

                exn.Message |> shouldContainText "refusing"

    [<Test>]
    let ``narrowing refuses a cross-array offset`` () : unit =
        let offset =
            SyntheticCrossArrayOffset.make
                (ByteStorageIdentity.Array (ManagedHeapAddress 1))
                0L
                (ByteStorageIdentity.Array (ManagedHeapAddress 2))
                0L

        for conv in narrowingConversions do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    conv.Apply
                        (EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset offset))
                        PointerHashCounters.empty
                    |> ignore
                )

            exn.Message |> shouldContainText "cross-array offset"

    [<Test>]
    let ``narrowing a verbatim native int assigns no counter`` () : unit =
        // Controls for the properties above: a value whose bits PawPrint knows
        // exactly must not be routed through counter assignment, or every ordinary
        // arithmetic narrowing would perturb the synthesised-bit sequence.
        let property (i : int64) : unit =
            for conv in narrowingConversions do
                for value in
                    [
                        EvalStackValue.NativeInt (NativeIntSource.Verbatim i)
                        EvalStackValue.Int64 (Int64Source.Verbatim i)
                    ] do
                    let _, counters = conv.Apply value PointerHashCounters.empty
                    counters |> shouldEqual PointerHashCounters.empty

        Check.One (narrowingPropertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``narrowing a native-int placeholder truncates its own bits`` () : unit =
        // `Unsafe.AsRef<T>((void*)bits)` placeholders ARE bit patterns rather than
        // addresses, so they narrow verbatim and register no identity. All three of
        // `Int64Source.widenedNativeInt`, the float-conversion helper, and
        // `materialiseHashBits` have to agree about this, because a placeholder can
        // arrive already-widened or straight from the native-int slot.
        let property (bits : int64) : unit =
            let placeholder =
                NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)

            let narrowed, counters =
                EvalStackValue.convToInt32 (EvalStackValue.NativeInt placeholder) PointerHashCounters.empty

            narrowed
            |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int32 (uint32 (uint64 bits)))))

            counters |> shouldEqual PointerHashCounters.empty

            let viaHash, hashCounters =
                PointerHashSynthesis.materialiseHashBits "placeholder" placeholder PointerHashCounters.empty

            viaHash |> shouldEqual bits
            hashCounters |> shouldEqual PointerHashCounters.empty

        Check.One (narrowingPropertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``Int64 GetHashCode of a method handle xors two halves of one materialisation`` () : unit =
        // The guest shape this change exists for. `RuntimeMethodInfo.GetHashCode` ->
        // `IntPtr.GetHashCode` -> `Int64.GetHashCode`, which on 64-bit is
        // `(int)l ^ (int)(l >> 32)` (Int64.cs:106-109). Both halves must derive from
        // the *same* materialisation, or the hash would not be a function of the handle.
        let handle = NativeIntSource.MethodHandlePtr 17L
        let widened = EvalStackValue.Int64 (Int64Source.WidenedNativeInt (handle, true))

        // `(int)l`
        let low, counters = EvalStackValue.convToInt32 widened PointerHashCounters.empty

        // `(int)(l >> 32)`
        let shifted, counters =
            Int64Source.shr "GetHashCode" (Int64Source.WidenedNativeInt (handle, true)) 32 counters

        let high, counters =
            EvalStackValue.convToInt32 (EvalStackValue.Int64 shifted) counters

        // One identity registered across all three steps.
        counters.Assigned.Count |> shouldEqual 1

        let expectedBits, _ =
            PointerHashSynthesis.materialiseHashBits "oracle" handle PointerHashCounters.empty

        low
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int32 (uint32 (uint64 expectedBits)))))

        high
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int32 (uint32 (uint64 (expectedBits >>> 32))))))
