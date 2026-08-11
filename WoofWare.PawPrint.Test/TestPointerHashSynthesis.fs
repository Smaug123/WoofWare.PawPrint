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

    let private materialise (src : NativeIntSource) (counters : PointerHashState) : int64 * PointerHashState =
        PointerHashSynthesis.materialiseHashBits "test" src counters

    [<Test>]
    let ``a fresh fixture assigns bits by first-touch order, with nothing assigned yet`` () : unit =
        // Pins which rule `empty` selects, and that it starts from a clean slate.
        // The rule choice is vacuous while `PointerHashState` has one case; it
        // earns its keep the moment a second lands, because the default is part of
        // the replay contract — switching it silently would change every synthesised
        // pointer value the guest observes. The rule named here is what the
        // `registration order assigns counters in order` test below spells out.
        PointerHashState.empty
        |> shouldEqual (PointerHashState.SequentialFirstTouch (0UL, Map.empty))

    [<Test>]
    let ``same source materialised twice returns same bits and bumps counter only once`` () : unit =
        let src =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 42))

        let bits1, counters1 = materialise src PointerHashState.empty
        PointerHashTestHelpers.nextCounter counters1 |> shouldEqual 1UL
        PointerHashTestHelpers.assignedCount counters1 |> shouldEqual 1

        let bits2, counters2 = materialise src counters1
        bits2 |> shouldEqual bits1
        PointerHashTestHelpers.nextCounter counters2 |> shouldEqual 1UL

        PointerHashTestHelpers.assigned counters2
        |> shouldEqual (PointerHashTestHelpers.assigned counters1)

    [<Test>]
    let ``distinct sources get distinct bits`` () : unit =
        let a =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1))

        let b =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 2))

        let bitsA, counters = materialise a PointerHashState.empty
        let bitsB, counters = materialise b counters

        bitsA |> shouldNotEqual bitsB
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 2UL
        PointerHashTestHelpers.assignedCount counters |> shouldEqual 2

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

        let materialiseAll (counters : PointerHashState) =
            ((counters, []), sources)
            ||> List.fold (fun (counters, bitsSoFar) src ->
                let bits, counters = materialise src counters
                counters, bits :: bitsSoFar
            )
            |> snd
            |> List.rev

        let bitsRunA = materialiseAll PointerHashState.empty
        let bitsRunB = materialiseAll PointerHashState.empty
        bitsRunA |> shouldEqual bitsRunB

    [<Test>]
    let ``registration order assigns counters in order; first-registered gets smaller bits`` () : unit =
        let a =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 11))

        let b =
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 12))

        // First registration in any sequence gets counter 0 → bits = ((0+1) <<< 2) | 0 = 4.
        let bitsAAlone, _ = materialise a PointerHashState.empty
        let bitsBAlone, _ = materialise b PointerHashState.empty
        bitsAAlone |> shouldEqual 4L
        bitsBAlone |> shouldEqual 4L

        // Register a then b: a gets counter 0, b gets counter 1.
        let bitsA_AB, ab = materialise a PointerHashState.empty
        let bitsB_AB, _ = materialise b ab
        bitsA_AB |> shouldEqual 4L
        bitsB_AB |> shouldEqual 8L

        // Register b then a: b gets counter 0, a gets counter 1. The bits depend only on
        // registration order, not on the source identity — that is the load-bearing
        // determinism contract.
        let bitsB_BA, ba = materialise b PointerHashState.empty
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

        let bitsMt, counters = materialise mtSrc PointerHashState.empty
        let bitsTh, counters = materialise thSrc counters

        bitsMt |> shouldEqual bitsTh
        // The two encodings share a canonical key, so the second materialisation
        // must reuse the first counter — no new assignment.
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL
        PointerHashTestHelpers.assignedCount counters |> shouldEqual 1

    [<Test>]
    let ``MethodTablePtr and TypeHandlePtr(Closed _) alias for OneDimArrayZero shape`` () : unit =
        let handle = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 5)

        let bitsMt, counters =
            materialise (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)) PointerHashState.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) counters

        bitsMt |> shouldEqual bitsTh
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL

    [<Test>]
    let ``MethodTablePtr and TypeHandlePtr(Closed _) alias for Array shape`` () : unit =
        let handle = ConcreteTypeHandle.Array (ConcreteTypeHandle.Concrete 3, 2)

        let bitsMt, counters =
            materialise (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)) PointerHashState.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) counters

        bitsMt |> shouldEqual bitsTh
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL

    [<Test>]
    let ``TypeHandlePtr(Closed Pointer _) does NOT alias to MethodTablePtr - distinct canonical keys`` () : unit =
        // Pointer-shaped TypeHandles are TypeDesc-shaped in CoreCLR; they live in a
        // different memory region from MethodTables, so the two encodings must NOT collapse.
        let element = ConcreteTypeHandle.Concrete 13
        let pointerHandle = ConcreteTypeHandle.Pointer element

        let bitsMt, counters =
            materialise (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed element)) PointerHashState.empty

        let bitsTh, counters =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed pointerHandle)) counters

        bitsMt |> shouldNotEqual bitsTh
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 2UL

    [<Test>]
    let ``low bits are clear for MethodTablePtr`` () : unit =
        let bits, _ =
            materialise
                (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 7)))
                PointerHashState.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for MethodTablePtr of OneDimArrayZero`` () : unit =
        let handle = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)) PointerHashState.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``bit 1 is set for TypeHandlePtr of Pointer-shaped (TypeDesc)`` () : unit =
        let handle = ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) PointerHashState.empty

        bits &&& 2L |> shouldEqual 2L

    [<Test>]
    let ``bit 1 is set for TypeHandlePtr of Byref-shaped (TypeDesc)`` () : unit =
        let handle = ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 7)

        let bits, _ =
            materialise (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed handle)) PointerHashState.empty

        bits &&& 2L |> shouldEqual 2L

    [<Test>]
    let ``low bits are clear for MethodHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.MethodHandlePtr 0xCAFEL) PointerHashState.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for FieldHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.FieldHandlePtr 0xBEEFL) PointerHashState.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``low bits are clear for GcHandlePtr`` () : unit =
        let bits, _ =
            materialise (NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 17, 0L)) PointerHashState.empty

        bits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``a GC handle's tag bits land in the low bits and leave its identity alone`` () : unit =
        // Tag bits are a view over one identity, so all three views must agree
        // above the tag region and differ exactly within it — which is what
        // happens for real, where the tag really is stored in the pointer's spare
        // low bits.
        let handle = GcHandleAddress.GcHandleAddress 17

        let untagged, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 0L)) PointerHashState.empty

        let tagged1, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 1L)) counters

        let tagged3, counters =
            materialise (NativeIntSource.GcHandlePtr (handle, 3L)) counters

        tagged1 |> shouldEqual (untagged ||| 1L)
        tagged3 |> shouldEqual (untagged ||| 3L)

        // One identity, so only one counter was ever spent.
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL

    [<Test>]
    let ``a TypeDesc pointer differs from its type handle by exactly the tag bit`` () : unit =
        // `AsTypeDesc` clears bit 1 of the same address, so the synthesised bits
        // must relate the same way: same identity above the tag region, differing
        // in exactly that bit. Otherwise a `ceq` between a handle and the TypeDesc
        // masked out of it would give an answer unrelated to the real one.
        let target =
            RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 1))

        let handleBits, counters =
            materialise (NativeIntSource.TypeHandlePtr target) PointerHashState.empty

        let typeDescBits, counters =
            materialise (NativeIntSource.TypeDescPtr target) counters

        handleBits &&& 3L |> shouldEqual 2L
        typeDescBits |> shouldEqual (handleBits &&& ~~~2L)

        // One identity between them, so only one counter was spent.
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL

    [<Test>]
    let ``a MethodTable-shaped type handle has no tag to strip`` () : unit =
        // The control for the case above: a MethodTable-backed handle is already
        // untagged, so its low bits are clear and it aliases its MethodTable.
        let target = RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1)

        let handleBits, counters =
            materialise (NativeIntSource.TypeHandlePtr target) PointerHashState.empty

        let methodTableBits, counters =
            materialise (NativeIntSource.MethodTablePtr target) counters

        handleBits &&& 3L |> shouldEqual 0L
        methodTableBits |> shouldEqual handleBits
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL

    [<Test>]
    let ``low bits are clear for AssemblyHandle / ModuleHandle / MetadataImportHandle`` () : unit =
        let assyBits, counters =
            materialise (NativeIntSource.AssemblyHandle "Foo") PointerHashState.empty

        let modBits, counters = materialise (NativeIntSource.ModuleHandle "Bar") counters
        let midBits, _ = materialise (NativeIntSource.MetadataImportHandle "Baz") counters

        assyBits &&& 3L |> shouldEqual 0L
        modBits &&& 3L |> shouldEqual 0L
        midBits &&& 3L |> shouldEqual 0L

    [<Test>]
    let ``Verbatim is returned unchanged and does not touch counters`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.Verbatim 12345L) PointerHashState.empty

        bits |> shouldEqual 12345L
        counters |> shouldEqual PointerHashState.empty

    [<Test>]
    let ``Verbatim works for negative values without sign mangling`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.Verbatim -7L) PointerHashState.empty

        bits |> shouldEqual -7L
        counters |> shouldEqual PointerHashState.empty

    [<Test>]
    let ``null managed pointer is materialised to 0L and does not touch counters`` () : unit =
        let bits, counters =
            materialise (NativeIntSource.ManagedPointer ManagedPointerSource.Null) PointerHashState.empty

        bits |> shouldEqual 0L
        counters |> shouldEqual PointerHashState.empty

    [<Test>]
    let ``non-null managed pointer is refused with reason embedded in message`` () : unit =
        let src =
            NativeIntSource.ManagedPointer (
                ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 21), [])
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                PointerHashSynthesis.materialiseHashBits "my-call-site" src PointerHashState.empty
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
                PointerHashSynthesis.materialiseHashBits "cross-array-callsite" src PointerHashState.empty
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
            ((PointerHashState.empty, []), sources)
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
        let _, counters = materialise mt PointerHashState.empty
        let _, counters = materialise th counters
        let _, counters = materialise mt counters
        let _, counters = materialise th counters

        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL
        PointerHashTestHelpers.assignedCount counters |> shouldEqual 1

    [<Test>]
    let ``MethodTableAuxiliaryDataPtr is canonicalised distinctly from MethodTablePtr`` () : unit =
        let handle = ConcreteTypeHandle.Concrete 77
        let mt = NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed handle)

        let aux =
            NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed handle)

        let bitsMt, counters = materialise mt PointerHashState.empty
        let bitsAux, counters = materialise aux counters

        bitsMt |> shouldNotEqual bitsAux
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 2UL

    [<Test>]
    let ``EventPipeProviderPtr and EventPipeEventPtr with the same id are distinct canonical keys`` () : unit =
        let bitsProv, counters =
            materialise (NativeIntSource.EventPipeProviderPtr 5L) PointerHashState.empty

        let bitsEvt, counters = materialise (NativeIntSource.EventPipeEventPtr 5L) counters

        bitsProv |> shouldNotEqual bitsEvt
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 2UL

    [<Test>]
    let ``AssemblyHandle ModuleHandle MetadataImportHandle with same name are distinct canonical keys`` () : unit =
        let name = "X"

        let bitsAssy, counters =
            materialise (NativeIntSource.AssemblyHandle name) PointerHashState.empty

        let bitsMod, counters = materialise (NativeIntSource.ModuleHandle name) counters

        let bitsMid, counters =
            materialise (NativeIntSource.MetadataImportHandle name) counters

        bitsAssy |> shouldNotEqual bitsMod
        bitsAssy |> shouldNotEqual bitsMid
        bitsMod |> shouldNotEqual bitsMid
        PointerHashTestHelpers.nextCounter counters |> shouldEqual 3UL

    // --- `tryExistingHashBits`: the read-only counterpart ---
    //
    // `ceq` recognises "are these synthesised bits that handle's address?" without minting,
    // which is what keeps a comparison from perturbing the numbering every later synthesised
    // value depends on. The contract is that it agrees with `materialiseHashBits` exactly —
    // including the low tag bits, which are OR-ed on per source rather than stored, so a
    // lookup that returned the bare assigned bits would silently disagree for every tagged
    // view.

    /// Every canonicalisable shape, chosen to include the two that carry tag bits
    /// (a TypeDesc-shaped `TypeHandlePtr`, and a tagged `GcHandlePtr`) — a generator left to
    /// its own devices does not reach those DU corners, and they are the only inputs that can
    /// tell a tag-preserving lookup from a bare one.
    let private canonicalisableSources : NativeIntSource list =
        [
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 1))
            NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 2))
            // TypeDesc-shaped: `TypeHandleTag.forTarget` gives this one a non-zero tag.
            NativeIntSource.TypeHandlePtr (
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 3))
            )
            NativeIntSource.TypeHandlePtr (
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 4))
            )
            NativeIntSource.TypeDescPtr (
                RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer (ConcreteTypeHandle.Concrete 3))
            )
            NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete 5))
            NativeIntSource.MethodHandlePtr 11L
            NativeIntSource.FieldHandlePtr 12L
            NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 13, 0L)
            // Tagged: the tag is a view over the same identity, so it must reach different bits.
            NativeIntSource.GcHandlePtr (GcHandleAddress.GcHandleAddress 13, 1L)
            NativeIntSource.EventPipeProviderPtr 14L
            NativeIntSource.EventPipeEventPtr 15L
            NativeIntSource.AssemblyHandle "A"
            NativeIntSource.ModuleHandle "A"
            NativeIntSource.MetadataImportHandle "A"
        ]

    [<Test>]
    let ``tryExistingHashBits agrees with materialiseHashBits once assigned`` () : unit =
        // Assign every source in one state, then check each lookup against the bits
        // materialisation handed out. Doing it in one accumulated state also checks that a
        // lookup is not confused by the other assignments around it.
        let assigned, counters =
            canonicalisableSources
            |> List.fold
                (fun (acc, counters) src ->
                    let bits, counters = materialise src counters
                    (src, bits) :: acc, counters
                )
                ([], PointerHashState.empty)

        for src, expected in assigned do
            PointerHashSynthesis.tryExistingHashBits counters src
            |> shouldEqual (Some expected)

        // Distinctness, so a lookup that returned some *other* source's bits would fail here
        // rather than agreeing vacuously.
        assigned
        |> List.map snd
        |> List.distinct
        |> List.length
        |> shouldEqual (List.length assigned)

    [<Test>]
    let ``tryExistingHashBits assigns nothing`` () : unit =
        // The load-bearing half: `ceq` reads this, and `ContextSwitchPrior` bands comparisons
        // as never mutating `PointerHashState`. A lookup that minted would make that banding
        // false and turn every comparison into a scheduling-visible side effect.
        for src in canonicalisableSources do
            PointerHashSynthesis.tryExistingHashBits PointerHashState.empty src
            |> shouldEqual None

        let _, counters = materialise canonicalisableSources.Head PointerHashState.empty

        for src in canonicalisableSources do
            PointerHashSynthesis.tryExistingHashBits counters src |> ignore<int64 option>

        PointerHashTestHelpers.nextCounter counters |> shouldEqual 1UL
        PointerHashTestHelpers.assignedCount counters |> shouldEqual 1

    [<Test>]
    let ``tryExistingHashBits refuses sources that have no assigned identity`` () : unit =
        // Domain restriction, shared with `canonicalKey`: a verbatim number, already-synthesised
        // bits, a managed pointer and a cross-array offset are values whose bits are known (or
        // knowably absent) without any assignment, so asking this question about them is a
        // category error rather than a miss.
        for src in
            [
                NativeIntSource.Verbatim 4L
                NativeIntSource.OpaqueHashBits 4L
                NativeIntSource.ManagedPointer ManagedPointerSource.Null
            ] do
            let exn =
                Assert.Throws (fun () ->
                    PointerHashSynthesis.tryExistingHashBits PointerHashState.empty src
                    |> ignore<int64 option>
                )

            exn.Message |> shouldContainText "not a canonicalisable pointer shape"
