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

        let runtimeTypeHandle =
            EvalStackValue.NativeInt (
                NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))
            )

        if not (EvalStackValueComparisons.ceq methodTable sameMethodTable) then
            failwith "Expected matching MethodTablePtr values to compare equal"

        if EvalStackValueComparisons.ceq methodTable otherMethodTable then
            failwith "Expected different MethodTablePtr values to compare unequal"

        if EvalStackValueComparisons.ceq methodTable runtimeTypeHandle then
            failwith "Expected MethodTablePtr and TypeHandlePtr values to remain distinct"

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
