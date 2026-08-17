namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeCall.tryResolveNativeHeapFreeTarget` decides which pointers a
/// `free`-shaped entry point may release. It is shared by `SystemNative_Free`
/// (reachable from a guest through `Marshal.FreeHGlobal` / `NativeMemory.Free`)
/// and by `FreeEnvironmentStringsW` (whose only caller, CoreLib's
/// `Environment.GetEnvironmentVariables`, always hands back the base pointer it
/// was given — so its refusal arms have no guest that can reach them).
///
/// Testing the classifier rather than either handler is what makes those arms
/// exercisable at all: it is a pure function of the pointer, so every shape it
/// distinguishes can be built here directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeHeapFree =
    let private blockA : NativeMemoryBlockId = NativeMemoryBlockId.NativeMemoryBlockId 7
    let private blockB : NativeMemoryBlockId = NativeMemoryBlockId.NativeMemoryBlockId 8

    let private baseOf (block : NativeMemoryBlockId) : ManagedPointerSource =
        ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, 0), [])

    let private expectRefused (ptr : ManagedPointerSource) : string =
        match NativeCall.tryResolveNativeHeapFreeTarget ptr with
        | Ok resolved -> failwith $"expected %O{ptr} to be refused, but it resolved to %O{resolved}"
        | Error reason -> reason

    [<Test>]
    let ``null is the documented no-op`` () : unit =
        // C `free(NULL)` does nothing. `NativeMemory.Free` filters null before
        // the P/Invoke, but `Marshal.FreeHGlobal` does not, so the classifier has
        // to answer for it.
        NativeCall.tryResolveNativeHeapFreeTarget ManagedPointerSource.Null
        |> shouldEqual (Ok None)

    [<Test>]
    let ``an allocation's base address resolves to its block`` () : unit =
        NativeCall.tryResolveNativeHeapFreeTarget (baseOf blockA)
        |> shouldEqual (Ok (Some blockA))

        NativeCall.tryResolveNativeHeapFreeTarget (baseOf blockB)
        |> shouldEqual (Ok (Some blockB))

    [<Test>]
    let ``a base address reached through reinterpretation still resolves`` () : unit =
        // `Unsafe.As` changes the view, not the address, so a pointer that has
        // been reinterpreted but not advanced is still the allocation base — which
        // is what `NativeMemoryAllocFree.cs` relies on when it writes through a
        // typed view and then frees the original pointer.
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockA, 0), [ ByrefProjection.ByteOffset 0 ])

        NativeCall.tryResolveNativeHeapFreeTarget ptr |> shouldEqual (Ok (Some blockA))

    [<Test>]
    let ``an interior pointer is refused, and named`` () : unit =
        // Freeing `base + n` is undefined in C. Resolving it to the whole block
        // would silently accept the guest memory-corruption bug that produced it.
        //
        // Both spellings of "advanced": the offset baked into the root, and the
        // offset accumulated as a projection. A classifier that folded only one
        // would accept the other.
        expectRefused (ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockA, 4), []))
        |> shouldContainText "interior"

        expectRefused (
            ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockA, 0), [ ByrefProjection.ByteOffset 4 ])
        )
        |> shouldContainText "interior"

        // And the two combining to a non-zero total, which is the case a
        // classifier that checked the root and the projections separately would
        // get right only by accident.
        expectRefused (
            ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockA, 2), [ ByrefProjection.ByteOffset 2 ])
        )
        |> shouldContainText "interior"

    [<Test>]
    let ``offsets that cancel to the base are accepted`` () : unit =
        // The counterpart of the refusals above: the rule is about the resolved
        // address, not about whether any arithmetic happened. A guest that walked
        // forward and back is pointing at the base.
        let ptr =
            ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockA, 8), [ ByrefProjection.ByteOffset -8 ])

        NativeCall.tryResolveNativeHeapFreeTarget ptr |> shouldEqual (Ok (Some blockA))

    [<Test>]
    let ``a field projection is refused rather than folded`` () : unit =
        // A field offset is not a byte count the classifier can add up, so it
        // cannot decide whether the address is the base. Refusing is the honest
        // answer; guessing "probably the base" would free a live block from an
        // interior address.
        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.NativeMemoryByte (blockA, 0),
                [ ByrefProjection.Field (FieldId.Named "X") ]
            )

        expectRefused ptr |> shouldContainText "projection"

    [<Test>]
    let ``a pointer that does not address the native heap is refused`` () : unit =
        // Only the native-heap pool has an allocator whose result may be freed.
        //
        // A byref into a managed `byte[]` is the realistic wrong input: it is what
        // `NativeCall.allocateBlobByteArray` hands back, and freeing one would be
        // nonsense — which is exactly why entry points modelled on a function
        // returning `malloc`'d memory must use `allocateNativeHeapBlob` instead.
        expectRefused (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (ManagedHeapAddress 1, 0), []))
        |> shouldContainText "native-heap"

        // A bare bit pattern carried through a managed reference names no storage
        // at all, so it cannot be the base of anything.
        expectRefused (ManagedPointerSource.NativeIntPlaceholder 0x1000L)
        |> shouldContainText "native-heap"

    [<Test>]
    let ``an interior offset cannot wrap around to the base`` () : unit =
        // Issue #993: the fold accumulates in int64 precisely so that a byte
        // offset cannot wrap back onto zero and be mistaken for the allocation
        // base, which would free a live block from an interior address.
        //
        // `ByteOffset` is documented to appear only as the final projection, so a
        // legal pointer contributes exactly two addends: the root's offset and
        // that one projection. Both are `int`, so their true sum lies in
        // [-2^32, 2^32 - 2] — and the *only* value in that range which is a
        // non-zero multiple of 2^32 is -2^32, from Int32.MinValue twice. So this
        // single pair is the whole of what an `int` fold gets wrong, and the
        // near-2^31 pairs one might reach for first (MaxValue + MaxValue is
        // 2^32 - 2, MaxValue + 1 is 2^31) do not exercise the guard at all.
        let rootOffset = System.Int32.MinValue
        let projectionOffset = System.Int32.MinValue

        int64 rootOffset + int64 projectionOffset |> shouldEqual -4294967296L

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.NativeMemoryByte (blockA, rootOffset),
                [ ByrefProjection.ByteOffset projectionOffset ]
            )

        expectRefused ptr |> shouldContainText "interior"

        // The non-wrapping extremes, so this test also covers the offsets an
        // `int` fold happens to get right — a fix that clamped instead of
        // widening would fail here.
        for root, projection in
            [
                System.Int32.MaxValue, System.Int32.MaxValue
                System.Int32.MaxValue, 1
                System.Int32.MinValue, -1
            ] do
            ManagedPointerSource.Byref (
                ByrefRoot.NativeMemoryByte (blockA, root),
                [ ByrefProjection.ByteOffset projection ]
            )
            |> expectRefused
            |> shouldContainText "interior"

    /// Offsets spanning the whole `int32` range, not the roughly [-100, 100] that
    /// FsCheck's default `int` generator produces — the interesting inputs for a
    /// wrap-around guard all live at the extremes.
    let private genByteOffset : Gen<int> =
        Gen.frequency
            [
                2, Gen.choose (-8, 8)
                1, Gen.choose (System.Int32.MinValue, System.Int32.MinValue + 64)
                1, Gen.choose (System.Int32.MaxValue - 64, System.Int32.MaxValue)
                2, Gen.choose (System.Int32.MinValue, System.Int32.MaxValue)
            ]

    /// Root and projection offsets to try together.
    ///
    /// Cancelling pairs are generated deliberately rather than left to chance:
    /// two independent draws land on a sum of zero far too rarely to exercise the
    /// accepting arm, so the property would have become a claim about refusals
    /// only. (`Int32.MinValue` is excluded from the cancelling branch because its
    /// negation is not representable, so the pair would not in fact cancel.)
    let private genOffsetPair : Gen<int * int> =
        Gen.frequency
            [
                1,
                Gen.choose (System.Int32.MinValue + 1, System.Int32.MaxValue)
                |> Gen.map (fun n -> n, -n)
                3, Gen.zip genByteOffset genByteOffset
            ]

    [<Test>]
    let ``a byte-offset pointer is accepted exactly when its offsets sum to zero`` () : unit =
        let mutable accepted = 0
        let mutable refused = 0

        let property (rootOffset : int, projectionOffset : int) : unit =
            let ptr =
                ManagedPointerSource.Byref (
                    ByrefRoot.NativeMemoryByte (blockA, rootOffset),
                    [ ByrefProjection.ByteOffset projectionOffset ]
                )

            // The oracle is int64 arithmetic done here, independently of the
            // fold under test: the address is the base iff the true sum is zero,
            // whatever it does modulo 2^32.
            let isBase = int64 rootOffset + int64 projectionOffset = 0L

            match NativeCall.tryResolveNativeHeapFreeTarget ptr with
            | Ok (Some block) ->
                accepted <- accepted + 1
                isBase |> shouldEqual true
                block |> shouldEqual blockA
            | Ok None -> failwith $"a native-heap pointer resolved to the null no-op: %O{ptr}"
            | Error _ ->
                refused <- refused + 1
                isBase |> shouldEqual false

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen genOffsetPair) property)

        // Both verdicts really occurred: without this, a generator that never
        // produced a cancelling pair would leave the accepting arm unexercised —
        // which is exactly what an earlier version of `genOffsetPair` did.
        accepted > 100 |> shouldEqual true
        refused > 100 |> shouldEqual true
