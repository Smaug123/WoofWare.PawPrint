namespace WoofWare.PawPrint.Test

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
        let typeHandle = ConcreteTypeHandle.Concrete 42

        match
            EvalStackValue.toCliTypeCoerced
                runtimePointerTarget
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr typeHandle))
        with
        | CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected RuntimePointer(MethodTablePtr %O{typeHandle}), got %O{other}"

    [<Test>]
    let ``RuntimePointer carrying method table pointer flattens back to native int`` () : unit =
        let typeHandle = ConcreteTypeHandle.Concrete 42

        match EvalStackValue.ofCliType (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr typeHandle)) with
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected NativeInt(MethodTablePtr %O{typeHandle}), got %O{other}"

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target preserves GC handle pointer provenance`` () : unit =
        let handle = GcHandleAddress.GcHandleAddress 42

        match
            EvalStackValue.toCliTypeCoerced
                runtimePointerTarget
                (EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle))
        with
        | CliType.RuntimePointer (CliRuntimePointer.GcHandlePtr actual) when actual = handle -> ()
        | other -> failwith $"Expected RuntimePointer(GcHandlePtr %O{handle}), got %O{other}"

    [<Test>]
    let ``RuntimePointer carrying GC handle pointer flattens back to native int`` () : unit =
        let handle = GcHandleAddress.GcHandleAddress 42

        match EvalStackValue.ofCliType (CliType.RuntimePointer (CliRuntimePointer.GcHandlePtr handle)) with
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr actual) when actual = handle -> ()
        | other -> failwith $"Expected NativeInt(GcHandlePtr %O{handle}), got %O{other}"

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
        | Some (UnsignedNativeIntSource.FromManagedPointer actual) ->
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
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (ConcreteTypeHandle.Concrete 42))

        let sameMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (ConcreteTypeHandle.Concrete 42))

        let otherMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (ConcreteTypeHandle.Concrete 43))

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
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr arrayHandle)

        let arrayRuntimeTypeHandle =
            EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed arrayHandle))

        // Pointer/Byref/FunctionPointer are TypeDescs in CoreCLR — they have no MethodTable, so
        // a TypeHandlePtr to one must NEVER alias a synthetic MethodTablePtr for the same handle.
        let pointerHandle = ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 42)

        let pointerMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr pointerHandle)

        let pointerRuntimeTypeHandle =
            EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed pointerHandle))

        let byrefHandle = ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 42)

        let byrefMethodTable =
            EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr byrefHandle)

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

    [<Test>]
    let ``unsigned comparisons treat GcHandlePtr as strictly greater than zero`` () : unit =
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
            EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 42))

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
