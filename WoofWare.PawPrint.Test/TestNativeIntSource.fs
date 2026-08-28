namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeIntSource =

    let private storageIdentities : ByteStorageIdentity array =
        [|
            ByteStorageIdentity.Array (ManagedHeapAddress 301)
            ByteStorageIdentity.String (ManagedHeapAddress 302)
            ByteStorageIdentity.StackMemory (ThreadId 0, FrameId 30, StackMemoryBlockId 0)
            ByteStorageIdentity.StackLocal (ThreadId 0, FrameId 31, 1us)
            ByteStorageIdentity.StackArgument (ThreadId 0, FrameId 32, 2us)
        |]

    /// Build a SyntheticCrossArrayOffset whose source and target identities are guaranteed distinct,
    /// drawn uniformly from `storageIdentities`.
    let private genSyntheticCrossArrayOffset : Gen<SyntheticCrossArrayOffset> =
        gen {
            let! sourceIndex = Gen.choose (0, storageIdentities.Length - 1)
            let! distance = Gen.choose (1, storageIdentities.Length - 1)
            let targetIndex = (sourceIndex + distance) % storageIdentities.Length
            let! sourceOffset = ArbMap.defaults |> ArbMap.generate<int64>
            let! targetOffset = ArbMap.defaults |> ArbMap.generate<int64>

            return
                SyntheticCrossArrayOffset.make
                    storageIdentities.[targetIndex]
                    targetOffset
                    storageIdentities.[sourceIndex]
                    sourceOffset
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    [<Test>]
    let ``negate is involutive on SyntheticCrossArrayOffset`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            SyntheticCrossArrayOffset.negate (SyntheticCrossArrayOffset.negate s)
            |> shouldEqual s

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``negate swaps target and source roots`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            let negated = SyntheticCrossArrayOffset.negate s

            SyntheticCrossArrayOffset.targetRoot negated
            |> shouldEqual (SyntheticCrossArrayOffset.sourceRoot s)

            SyntheticCrossArrayOffset.sourceRoot negated
            |> shouldEqual (SyntheticCrossArrayOffset.targetRoot s)

            SyntheticCrossArrayOffset.targetOffset negated
            |> shouldEqual (SyntheticCrossArrayOffset.sourceOffset s)

            SyntheticCrossArrayOffset.sourceOffset negated
            |> shouldEqual (SyntheticCrossArrayOffset.targetOffset s)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``make rejects identical source and target storage`` () : unit =
        let property (storage : int) (sourceOffset : int64) (targetOffset : int64) : unit =
            let identity = storageIdentities.[abs storage % storageIdentities.Length]

            let ex =
                Assert.Throws<System.Exception> (fun () ->
                    SyntheticCrossArrayOffset.make identity targetOffset identity sourceOffset
                    |> ignore
                )

            ex.Message |> shouldContainText "not a cross-array offset"

        Check.One (
            propertyConfig,
            Prop.forAll
                (ArbMap.defaults |> ArbMap.generate<int * int64 * int64> |> Arb.fromGen)
                (fun (storage, src, tgt) -> property storage src tgt)
        )

    /// Comparands drawn from the allowed magnitude band (-2^40, 2^40), in both signs, mixing
    /// small magnitudes (< 1024) with ones spread across the whole band: a uniform draw over
    /// the band almost never lands near zero, so we explicitly weight the buckets.
    let private allowedComparandGen : Gen<int64> =
        Gen.frequency
            [
                1, Gen.choose64 (-1023L, 1023L)
                1, Gen.choose64 (0L, (1L <<< 40) - 1L)
                1, Gen.choose64 (-((1L <<< 40) - 1L), 0L)
            ]

    [<Test>]
    let ``cltUnVerbatim places the delta by the comparand's sign`` () : unit =
        // Track buckets to confirm both signs actually arrive.
        let mutable nonnegativeComparands = 0
        let mutable negativeComparands = 0

        let property (s : SyntheticCrossArrayOffset) (comparand : int64) : unit =
            if comparand >= 0L then
                nonnegativeComparands <- nonnegativeComparands + 1
            else
                negativeComparands <- negativeComparands + 1

            // A nonnegative comparand's unsigned image is below the delta's band, so the delta
            // is not unsigned-less than it; a negative comparand's is above the band.
            SyntheticCrossArrayOffset.cltUnVerbatim s comparand
            |> shouldEqual (comparand < 0L)

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset allowedComparandGen |> Arb.fromGen)
                (fun (s, c) -> property s c)
        )

        // Each sign carries at least a third of the generator's weight, so with 500 cases the
        // probability of either bucket being empty is far below 2^-200. Asserting both buckets
        // fire guards against a future generator change silently dropping a sign.
        if nonnegativeComparands = 0 || negativeComparands = 0 then
            failwith $"generator missed a sign: nonnegative=%d{nonnegativeComparands}, negative=%d{negativeComparands}"

    [<Test>]
    let ``cgtUnVerbatim places the delta by the comparand's sign`` () : unit =
        let property (s : SyntheticCrossArrayOffset) (comparand : int64) : unit =
            SyntheticCrossArrayOffset.cgtUnVerbatim s comparand
            |> shouldEqual (comparand >= 0L)

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset allowedComparandGen |> Arb.fromGen)
                (fun (s, c) -> property s c)
        )

    [<Test>]
    let ``unsigned comparisons at the comparand band edges`` () : unit =
        let s =
            SyntheticCrossArrayOffset.make storageIdentities.[0] 4L storageIdentities.[1] 8L

        // Zero sits below the delta's band, minus one above it (its unsigned image is
        // 2^64 - 1); the extreme allowed magnitudes stay answerable on both sides.
        SyntheticCrossArrayOffset.cgtUnVerbatim s 0L |> shouldEqual true
        SyntheticCrossArrayOffset.cltUnVerbatim s 0L |> shouldEqual false
        SyntheticCrossArrayOffset.cgtUnVerbatim s (-1L) |> shouldEqual false
        SyntheticCrossArrayOffset.cltUnVerbatim s (-1L) |> shouldEqual true

        SyntheticCrossArrayOffset.cgtUnVerbatim s ((1L <<< 40) - 1L) |> shouldEqual true

        SyntheticCrossArrayOffset.cltUnVerbatim s ((1L <<< 40) - 1L)
        |> shouldEqual false

        SyntheticCrossArrayOffset.cgtUnVerbatim s (-((1L <<< 40) - 1L))
        |> shouldEqual false

        SyntheticCrossArrayOffset.cltUnVerbatim s (-((1L <<< 40) - 1L))
        |> shouldEqual true

    [<Test>]
    let ``cltUnVerbatim refuses comparands at or beyond the synthetic separation`` () : unit =
        let property (s : SyntheticCrossArrayOffset) (comparand : int64) : unit =
            let largeMagnitude = (1L <<< 40) + (comparand &&& 0x3F_FFFF_FFFFL)

            for outOfBand in [ largeMagnitude ; -largeMagnitude ] do
                let ex =
                    Assert.Throws<System.Exception> (fun () ->
                        SyntheticCrossArrayOffset.cltUnVerbatim s outOfBand |> ignore
                    )

                ex.Message |> shouldContainText "magnitude below 2^40"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset (ArbMap.defaults |> ArbMap.generate<int64>)
                 |> Arb.fromGen)
                (fun (s, c) -> property s c)
        )

    [<Test>]
    let ``cgtUnVerbatim refuses comparands at or beyond the synthetic separation`` () : unit =
        let property (s : SyntheticCrossArrayOffset) (comparand : int64) : unit =
            let largeMagnitude = (1L <<< 40) + (comparand &&& 0x3F_FFFF_FFFFL)

            for outOfBand in [ largeMagnitude ; -largeMagnitude ] do
                let ex =
                    Assert.Throws<System.Exception> (fun () ->
                        SyntheticCrossArrayOffset.cgtUnVerbatim s outOfBand |> ignore
                    )

                ex.Message |> shouldContainText "magnitude below 2^40"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset (ArbMap.defaults |> ArbMap.generate<int64>)
                 |> Arb.fromGen)
                (fun (s, c) -> property s c)
        )

    [<Test>]
    let ``unsigned native-int comparison places a cross-storage offset by the comparand's sign`` () : unit =
        let property (s : SyntheticCrossArrayOffset) (comparand : int64) : unit =
            let synthetic =
                EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset s)

            let verbatim = EvalStackValue.NativeInt (NativeIntSource.Verbatim comparand)

            // The delta's unsigned image lies inside the band (2^40, 2^64 - 2^40); a small
            // nonnegative comparand sits below it, a small-magnitude negative one above it.
            // Equality between the two is impossible, so cgt.un and clt.un with the operands
            // swapped must agree, and with the operands fixed must be complementary.
            EvalStackValueComparisons.cgtUn synthetic verbatim
            |> shouldEqual (comparand >= 0L)

            EvalStackValueComparisons.cltUn synthetic verbatim
            |> shouldEqual (comparand < 0L)

            EvalStackValueComparisons.cgtUn verbatim synthetic
            |> shouldEqual (comparand < 0L)

            EvalStackValueComparisons.cltUn verbatim synthetic
            |> shouldEqual (comparand >= 0L)

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset allowedComparandGen |> Arb.fromGen)
                (fun (s, c) -> property s c)
        )

    [<Test>]
    let ``NativeIntSource.isZero is false for any cross-storage offset`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            NativeIntSource.SyntheticCrossArrayOffset s
            |> NativeIntSource.isZero
            |> shouldEqual false

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``Int64Source.negate on a cross-storage offset returns the negated synthetic`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            match Int64Source.negate "test" (Int64Source.SyntheticCrossArrayOffset s) PointerHashState.empty with
            | Some (Int64Source.SyntheticCrossArrayOffset negated, _) ->
                negated |> shouldEqual (SyntheticCrossArrayOffset.negate s)
            | other -> failwith $"expected negate to return Some (synthetic), got %O{other}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``Int64Source.negate on a cross-storage offset is involutive`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            let original = Int64Source.SyntheticCrossArrayOffset s

            match Int64Source.negate "test" original PointerHashState.empty with
            | None -> failwith "negate of synthetic returned None"
            | Some (onceNegated, counters) ->
                match Int64Source.negate "test" onceNegated counters with
                | None -> failwith "double-negate of synthetic returned None"
                | Some (twiceNegated, _) -> twiceNegated |> shouldEqual original

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``equality on NativeIntSource.SyntheticCrossArrayOffset is structural`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            let lifted = NativeIntSource.SyntheticCrossArrayOffset s

            let recreated =
                NativeIntSource.SyntheticCrossArrayOffset (
                    SyntheticCrossArrayOffset.make
                        (SyntheticCrossArrayOffset.targetRoot s)
                        (SyntheticCrossArrayOffset.targetOffset s)
                        (SyntheticCrossArrayOffset.sourceRoot s)
                        (SyntheticCrossArrayOffset.sourceOffset s)
                )

            lifted |> shouldEqual recreated
            lifted.GetHashCode () |> shouldEqual (recreated.GetHashCode ())

            // Negation produces a distinct value (sources/targets swap), and equality must reflect that.
            let negated =
                NativeIntSource.SyntheticCrossArrayOffset (SyntheticCrossArrayOffset.negate s)

            if lifted = negated then
                failwith $"expected synthetic and its negation to compare unequal: %O{s}"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    /// Pair of synthetic cross-array offsets that differ in at least one root or offset, so binary
    /// arithmetic operations that demand a specific shape (e.g. negate or equal) reliably hit the
    /// failure branch.
    let private genDistinctOffsetPair : Gen<SyntheticCrossArrayOffset * SyntheticCrossArrayOffset> =
        gen {
            let! a = genSyntheticCrossArrayOffset
            let! b = genSyntheticCrossArrayOffset
            // Reject cases where b coincides with a or with -a; the rejection rate is tiny because
            // each offset carries two independent int64 fields, so the chance of collision per draw
            // is on the order of 2^-64.
            if b = a || b = SyntheticCrossArrayOffset.negate a then
                // XOR with a nonzero constant to guarantee the offset changes without risk of
                // arithmetic overflow wrapping back to the original value.
                let tweakedTargetOffset = SyntheticCrossArrayOffset.targetOffset b ^^^ 1L

                return
                    a,
                    SyntheticCrossArrayOffset.make
                        (SyntheticCrossArrayOffset.targetRoot b)
                        tweakedTargetOffset
                        (SyntheticCrossArrayOffset.sourceRoot b)
                        (SyntheticCrossArrayOffset.sourceOffset b)
            else
                return a, b
        }

    [<Test>]
    let ``ArithmeticOperation.add of opposite cross-array offsets returns int64 zero`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            ArithmeticOperation.add.CrossArrayOffsets s (SyntheticCrossArrayOffset.negate s)
            |> shouldEqual 0L

            ArithmeticOperation.add.CrossArrayOffsets (SyntheticCrossArrayOffset.negate s) s
            |> shouldEqual 0L

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``ArithmeticOperation.addOvf of opposite cross-array offsets returns int64 zero`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            ArithmeticOperation.addOvf.Op.CrossArrayOffsets s (SyntheticCrossArrayOffset.negate s)
            |> shouldEqual 0L

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``ArithmeticOperation.add refuses unrelated cross-array offsets`` () : unit =
        let property (a : SyntheticCrossArrayOffset, b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.add.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to add SyntheticCrossArrayOffsets"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDistinctOffsetPair) property)

    [<Test>]
    let ``ArithmeticOperation.sub of equal cross-array offsets returns int64 zero`` () : unit =
        let property (s : SyntheticCrossArrayOffset) : unit =
            ArithmeticOperation.sub.CrossArrayOffsets s s |> shouldEqual 0L

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSyntheticCrossArrayOffset) property)

    [<Test>]
    let ``ArithmeticOperation.sub refuses unrelated cross-array offsets`` () : unit =
        let property (a : SyntheticCrossArrayOffset, b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.sub.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to sub SyntheticCrossArrayOffsets"

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDistinctOffsetPair) property)

    [<Test>]
    let ``ArithmeticOperation.mul refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.mul.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to mul SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``ArithmeticOperation.div refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.div.Op.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to div SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``ArithmeticOperation.rem refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.rem.Op.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to rem SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``ArithmeticOperation.remUn refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () -> ArithmeticOperation.remUn.Op.CrossArrayOffsets a b |> ignore)

            ex.Message |> shouldContainText "refusing to rem_un SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``ArithmeticOperation.mulOvf refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () ->
                    ArithmeticOperation.mulOvf.Op.CrossArrayOffsets a b |> ignore
                )

            ex.Message |> shouldContainText "refusing to mul_ovf SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )

    [<Test>]
    let ``ArithmeticOperation.mulOvfUn refuses any cross-array offset pair`` () : unit =
        let property (a : SyntheticCrossArrayOffset) (b : SyntheticCrossArrayOffset) : unit =
            let ex =
                Assert.Throws<System.Exception> (fun () ->
                    ArithmeticOperation.mulOvfUn.Op.CrossArrayOffsets a b |> ignore
                )

            ex.Message
            |> shouldContainText "refusing to mul_ovf_un SyntheticCrossArrayOffsets"

        Check.One (
            propertyConfig,
            Prop.forAll
                (Gen.zip genSyntheticCrossArrayOffset genSyntheticCrossArrayOffset |> Arb.fromGen)
                (fun (a, b) -> property a b)
        )
