namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests pinning the counter-based pointer-hash synthesis contract. These
/// assertions are what makes the synthesised bits a faithful guest-observable
/// surrogate for real pointer bits: same key → same bits; distinct keys → distinct
/// bits; MethodTablePtr/TypeHandlePtr alias for Concrete/OneDimArrayZero/Array
/// shapes collapses to identical bits; the low-bit shape contract is upheld.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPointerHashSynthesis =

    let private materialise (src : NativeIntSource) (counters : PointerHashCounters) : int64 * PointerHashCounters =
        PointerHashSynthesis.materialiseHashBits "test" src counters

    [<Test>]
    let ``same source materialised twice returns same bits and bumps counter only once`` () : unit =
        let src =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))

        let bits1, counters1 = materialise src PointerHashCounters.empty
        counters1.NextCounter |> shouldEqual 1UL
        counters1.Assigned.Count |> shouldEqual 1

        let bits2, counters2 = materialise src counters1
        bits2 |> shouldEqual bits1
        counters2.NextCounter |> shouldEqual 1UL
        counters2.Assigned |> shouldEqual counters1.Assigned

    [<Test>]
    let ``distinct sources get distinct bits`` () : unit =
        let a =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1))

        let b =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 2))

        let bitsA, counters = materialise a PointerHashCounters.empty
        let bitsB, counters = materialise b counters

        bitsA |> shouldNotEqual bitsB
        counters.NextCounter |> shouldEqual 2UL
        counters.Assigned.Count |> shouldEqual 2

    [<Test>]
    let ``order-stable assignment - same sequence on two fresh fixtures produces same bits`` () : unit =
        let sources : NativeIntSource list =
            [
                NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 7))
                NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 8))
                NativeIntSource.TypeHandlePtr (
                    RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 9))
                )
                NativeIntSource.MethodHandlePtr 100L
                NativeIntSource.FieldHandlePtr 200L
            ]

        let materialiseAll (counters : PointerHashCounters) =
            ((counters, []), sources)
            ||> List.fold (fun (counters, bitsSoFar) src ->
                let bits, counters = materialise src counters
                counters, bits :: bitsSoFar
            )
            |> snd
            |> List.rev

        let bitsRunA = materialiseAll PointerHashCounters.empty
        let bitsRunB = materialiseAll PointerHashCounters.empty
        bitsRunA |> shouldEqual bitsRunB

    [<Test>]
    let ``registration order assigns counters in order; first-registered gets smaller bits`` () : unit =
        let a =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 11))

        let b =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 12))

        // First registration in any sequence gets counter 0 → bits = ((0+1) <<< 2) | 0 = 4.
        let bitsAAlone, _ = materialise a PointerHashCounters.empty
        let bitsBAlone, _ = materialise b PointerHashCounters.empty
        bitsAAlone |> shouldEqual 4L
        bitsBAlone |> shouldEqual 4L

        // Register a then b: a gets counter 0, b gets counter 1.
        let bitsA_AB, ab = materialise a PointerHashCounters.empty
        let bitsB_AB, _ = materialise b ab
        bitsA_AB |> shouldEqual 4L
        bitsB_AB |> shouldEqual 8L

        // Register b then a: b gets counter 0, a gets counter 1. The bits depend only on
        // registration order, not on the source identity — that is the load-bearing
        // determinism contract.
        let bitsB_BA, ba = materialise b PointerHashCounters.empty
        let bitsA_BA, _ = materialise a ba
        bitsB_BA |> shouldEqual 4L
        bitsA_BA |> shouldEqual 8L

        // Same-counters → same bits regardless of which source occupies it.
        bitsA_AB |> shouldEqual bitsB_BA
        bitsB_AB |> shouldEqual bitsA_BA

    [<Test>]
    let ``MethodTablePtr and TypeHandlePtr(Closed _) alias for Concrete shape`` () : unit =
        let handle = ConcreteTypeHandle.Concrete 99
        let mtSrc = NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)
        let thSrc = NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)

        let bitsMt, counters = materialise mtSrc PointerHashCounters.empty
        let bitsTh, counters = materialise thSrc counters

        bitsMt |> shouldEqual bitsTh
        // The two encodings share a canonical key, so the second materialisation
        // must reuse the first counter — no new assignment.
        counters.NextCounter |> shouldEqual 1UL
        counters.Assigned.Count |> shouldEqual 1

    [<Test>]
    let ``MethodTablePtr and TypeHandlePtr(Closed _) alias for OneDimArrayZero shape`` () : unit =
        let handle = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 5)

        let bitsMt, counters =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle))
                PointerHashCounters.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) counters

        bitsMt |> shouldEqual bitsTh
        counters.NextCounter |> shouldEqual 1UL

    [<Test>]
    let ``MethodTablePtr and TypeHandlePtr(Closed _) alias for Array shape`` () : unit =
        let handle = ConcreteTypeHandle.Array (ConcreteTypeHandle.Concrete 3, 2)

        let bitsMt, counters =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle))
                PointerHashCounters.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) counters

        bitsMt |> shouldEqual bitsTh
        counters.NextCounter |> shouldEqual 1UL

    [<Test>]
    let ``TypeHandlePtr(Closed Pointer _) does NOT alias to MethodTablePtr - distinct canonical keys`` () : unit =
        // Pointer-shaped TypeHandles are TypeDesc-shaped in CoreCLR; they live in a
        // different memory region from MethodTables, so the two encodings must NOT collapse.
        let element = ConcreteTypeHandle.Concrete 13
        let pointerHandle = ConcreteTypeHandle.Pointer element

        let bitsMt, counters =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed element))
                PointerHashCounters.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed pointerHandle)) counters

        bitsMt |> shouldNotEqual bitsTh
        counters.NextCounter |> shouldEqual 2UL

    [<Test>]
    let ``low bits are clear for MethodTablePtr`` () : unit =
        let bits, _ =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 7)))
                PointerHashCounters.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for MethodTablePtr of OneDimArrayZero`` () : unit =
        let handle = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle))
                PointerHashCounters.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``bit 1 is set for TypeHandlePtr of Pointer-shaped (TypeDesc)`` () : unit =
        let handle = ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise
                (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle))
                PointerHashCounters.empty

        bits &&& 2L |> shouldEqual 2L

    [<Test>]
    let ``bit 1 is set for TypeHandlePtr of Byref-shaped (TypeDesc)`` () : unit =
        let handle = ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise
                (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle))
                PointerHashCounters.empty

        bits &&& 2L |> shouldEqual 2L

    [<Test>]
    let ``low bits are clear for MethodHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.MethodHandlePtr 0xCAFEL) PointerHashCounters.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for FieldHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.FieldHandlePtr 0xBEEFL) PointerHashCounters.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for GcHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 17, 0L)) PointerHashCounters.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``a GC handle's tag bits land in the low bits and leave its identity alone`` () : unit =
        // Tag bits are a view over one identity, so all three views must agree
        // above the tag region and differ exactly within it — which is what
        // happens for real, where the tag really is stored in the pointer's spare
        // low bits.
        let handle = GcHandleAddress.GcHandleAddress 17

        let untagged, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 0L)) PointerHashCounters.empty

        let tagged1, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 1L)) counters

        let tagged3, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 3L)) counters

        tagged1 |> shouldEqual (untagged ||| 1L)
        tagged3 |> shouldEqual (untagged ||| 3L)

        // One identity, so only one counter was ever spent.
        counters.NextCounter |> shouldEqual 1UL

    [<Test>]
    let ``low bits are clear for AssemblyHandle / ModuleHandle / MetadataImportHandle`` () : unit =
        let assyBits, counters =
            materialise (NativeIntSource.AssemblyHandle "Foo") PointerHashCounters.empty

        let modBits, counters = materialise (NativeIntSource.ModuleHandle "Bar") counters
        let midBits, _ = materialise (NativeIntSource.MetadataImportHandle "Baz") counters

        assyBits &&& 3L |> shouldEqual 0L
        modBits &&& 3L |> shouldEqual 0L
        midBits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``Verbatim is returned unchanged and does not touch counters`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.Verbatim 12345L) PointerHashCounters.empty

        bits |> shouldEqual 12345L
        counters |> shouldEqual PointerHashCounters.empty

    [<Test>]
    let ``Verbatim works for negative values without sign mangling`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.Verbatim -7L) PointerHashCounters.empty

        bits |> shouldEqual -7L
        counters |> shouldEqual PointerHashCounters.empty

    [<Test>]
    let ``null managed pointer is materialised to 0L and does not touch counters`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.ManagedPointer ManagedPointerSource.Null) PointerHashCounters.empty

        bits |> shouldEqual 0L
        counters |> shouldEqual PointerHashCounters.empty

    [<Test>]
    let ``non-null managed pointer is refused with reason embedded in message`` () : unit =
        let src =
            NativeIntSource.ManagedPointer (
                ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 21), [])
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                PointerHashSynthesis.materialiseHashBits "my-call-site" src PointerHashCounters.empty
                |> ignore
            )

        ex.Message |> shouldContainText "my-call-site"
        ex.Message |> shouldContainText "managed pointer"

    [<Test>]
    let ``SyntheticCrossArrayOffset is refused with reason embedded in message`` () : unit =
        let offset =
            SyntheticCrossArrayOffset.make
                (ByteStorageIdentity.Array (ManagedHeapAddress.ManagedHeapAddress 1))
                0L
                (ByteStorageIdentity.Array (ManagedHeapAddress.ManagedHeapAddress 2))
                0L

        let src = NativeIntSource.SyntheticCrossArrayOffset offset

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                PointerHashSynthesis.materialiseHashBits "cross-array-callsite" src PointerHashCounters.empty
                |> ignore
            )

        ex.Message |> shouldContainText "cross-array-callsite"
        ex.Message |> shouldContainText "cross-array offset"

    [<Test>]
    let ``no collisions across 100 distinct canonical keys of varying shape`` () : unit =
        // Drawn from a mix of shapes so we cover the major canonical-key arms:
        // MethodTable, TypeHandle (TypeDesc-shaped), MethodTableAuxiliaryData,
        // MethodHandle, FieldHandle, GcHandle, EventPipeProvider, EventPipeEvent,
        // AssemblyHandle, ModuleHandle, MetadataImportHandle.
        let sources : NativeIntSource list =
            [
                for i in 0..9 ->
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete i))
                for i in 0..9 ->
                    NativeIntSource.MethodTablePtr (
                        RuntimeTypeHandleTarget.Closed (
                            ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete i)
                        )
                    )
                for i in 0..9 ->
                    NativeIntSource.TypeHandlePtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete i))
                    )
                for i in 0..9 ->
                    NativeIntSource.TypeHandlePtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete i))
                    )
                for i in 0..9 ->
                    NativeIntSource.MethodTableAuxiliaryDataPtr (
                        RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete i)
                    )
                for i in 0..9 -> NativeIntSource.MethodHandlePtr (int64 i)
                for i in 0..9 -> NativeIntSource.FieldHandlePtr (int64 i)
                for i in 0..9 -> NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress i, 0L)
                for i in 0..9 -> NativeIntSource.EventPipeProviderPtr (int64 i)
                for i in 0..9 -> NativeIntSource.EventPipeEventPtr (int64 i)
            ]

        sources.Length |> shouldEqual 100

        let _, allBits =
            ((PointerHashCounters.empty, []), sources)
            ||> List.fold (fun (counters, acc) src ->
                let bits, counters = materialise src counters
                counters, bits :: acc
            )

        let distinct = allBits |> List.distinct

        if distinct.Length <> allBits.Length then
            let collisionCount = allBits.Length - distinct.Length

            failwith
                $"counter-based synthesis produced %d{collisionCount} colliding bit pattern(s) across %d{allBits.Length} distinct canonical keys"

    [<Test>]
    let ``aliased encodings materialised separately do not double-bump the counter`` () : unit =
        let handle = ConcreteTypeHandle.Concrete 50
        let mt = NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)
        let th = NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)

        // Register the alias under one encoding, then via the other; total assigned should remain 1.
        let _, counters = materialise mt PointerHashCounters.empty
        let _, counters = materialise th counters
        let _, counters = materialise mt counters
        let _, counters = materialise th counters

        counters.NextCounter |> shouldEqual 1UL
        counters.Assigned.Count |> shouldEqual 1

    [<Test>]
    let ``MethodTableAuxiliaryDataPtr is canonicalised distinctly from MethodTablePtr`` () : unit =
        let handle = ConcreteTypeHandle.Concrete 77
        let mt = NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)

        let aux =
            NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed handle)

        let bitsMt, counters = materialise mt PointerHashCounters.empty
        let bitsAux, counters = materialise aux counters

        bitsMt |> shouldNotEqual bitsAux
        counters.NextCounter |> shouldEqual 2UL

    [<Test>]
    let ``EventPipeProviderPtr and EventPipeEventPtr with the same id are distinct canonical keys`` () : unit =
        let bitsProv, counters =
            materialise (NativeIntSource.EventPipeProviderPtr 5L) PointerHashCounters.empty

        let bitsEvt, counters = materialise (NativeIntSource.EventPipeEventPtr 5L) counters

        bitsProv |> shouldNotEqual bitsEvt
        counters.NextCounter |> shouldEqual 2UL

    [<Test>]
    let ``AssemblyHandle ModuleHandle MetadataImportHandle with same name are distinct canonical keys`` () : unit =
        let name = "X"

        let bitsAssy, counters =
            materialise (NativeIntSource.AssemblyHandle name) PointerHashCounters.empty

        let bitsMod, counters = materialise (NativeIntSource.ModuleHandle name) counters

        let bitsMid, counters =
            materialise (NativeIntSource.MetadataImportHandle name) counters

        bitsAssy |> shouldNotEqual bitsMod
        bitsAssy |> shouldNotEqual bitsMid
        bitsMod |> shouldNotEqual bitsMid
        counters.NextCounter |> shouldEqual 3UL
