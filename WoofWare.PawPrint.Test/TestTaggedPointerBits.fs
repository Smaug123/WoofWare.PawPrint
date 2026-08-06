namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `TaggedPointerBits` decides whether a bitwise operation against a pointer of
/// the form `base ||| tag` — `base` unknown, non-zero, low bits clear — has an
/// answer that holds for *every* admissible base.
///
/// The oracle here is deliberately derived a different way from the
/// implementation. The implementation tests the operand's high region against the
/// two knife-edge values `0` and `~tagMask` in closed form; the oracle instead
/// walks bit positions one at a time, asking of each "does this operation preserve
/// this base bit, force it to a constant, or neither", and reads the concrete
/// answer off probe evaluations. That keeps the two from sharing a mistake.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTaggedPointerBits =

    /// The three operations under test, each paired with its per-bit behaviour on
    /// a single unknown base bit.
    type private Op =
        | And
        | Or
        | Xor

        member this.Apply (a : int64, b : int64) : int64 =
            match this with
            | Op.And -> a &&& b
            | Op.Or -> a ||| b
            | Op.Xor -> a ^^^ b

        /// Does this operation leave an unknown base bit unchanged, given the
        /// operand's bit at the same position?
        member this.PreservesBaseBit (operandBit : bool) : bool =
            match this with
            // b &&& 1 = b; b &&& 0 = 0
            | Op.And -> operandBit
            // b ||| 0 = b; b ||| 1 = 1
            | Op.Or -> not operandBit
            // b ^^^ 0 = b; b ^^^ 1 = ~b
            | Op.Xor -> not operandBit

        /// Does this operation force an unknown base bit to a constant, given the
        /// operand's bit at the same position?
        member this.ForcesBaseBit (operandBit : bool) : bool =
            match this with
            | Op.And -> not operandBit
            | Op.Or -> operandBit
            // Inverting an unknown bit is still unknown.
            | Op.Xor -> false

        member this.Invoke (tagWidthBits : int) (tag : int64) (operand : int64) : TaggedPointerBitsResult =
            match this with
            | Op.And -> TaggedPointerBits.bitAnd tagWidthBits tag operand
            | Op.Or -> TaggedPointerBits.bitOr tagWidthBits tag operand
            | Op.Xor -> TaggedPointerBits.bitXor tagWidthBits tag operand

    /// Bit positions of the unknown base: everything at or above the tag region.
    /// Bit 63 is included; all three operations are bit-parallel, so the sign bit
    /// is not special.
    let private baseBitPositions (tagWidthBits : int) : int list = [ tagWidthBits..63 ]

    let private bitIsSet (value : int64) (position : int) : bool = (value >>> position) &&& 1L = 1L

    /// A base that is admissible under the model: non-zero, low `tagWidthBits`
    /// bits clear. Used as a probe to read off the concrete answer once the
    /// per-bit analysis has established that one exists.
    let private probeBase (tagWidthBits : int) : int64 = 1L <<< tagWidthBits

    /// The oracle: classify by per-bit reasoning, and read the concrete payload
    /// off a probe evaluation.
    let private expected (op : Op) (tagWidthBits : int) (tag : int64) (operand : int64) : TaggedPointerBitsResult =
        let positions = baseBitPositions tagWidthBits

        let allPreserved =
            positions |> List.forall (fun i -> op.PreservesBaseBit (bitIsSet operand i))

        let allForced =
            positions |> List.forall (fun i -> op.ForcesBaseBit (bitIsSet operand i))

        let b = probeBase tagWidthBits
        let probeResult = op.Apply (b ||| tag, operand)

        if allPreserved then
            TaggedPointerBitsResult.Retagged (probeResult &&& TaggedPointerBits.tagMask tagWidthBits)
        elif allForced then
            TaggedPointerBitsResult.TagBitsOnly probeResult
        else
            TaggedPointerBitsResult.NotStatable

    /// Admissible bases to check a stated answer against. Includes each individual
    /// base bit set on its own, so that any single mishandled bit position shows up.
    let private sampleBases (tagWidthBits : int) : int64 list =
        let singles = baseBitPositions tagWidthBits |> List.map (fun i -> 1L <<< i)
        let mask = ~~~(TaggedPointerBits.tagMask tagWidthBits)

        singles
        @ [
            mask
            0x0123456789ABCDEFL &&& mask ||| (1L <<< tagWidthBits)
            0x7EDCBA9876543210L &&& mask ||| (1L <<< tagWidthBits)
        ]

    type private Scenario =
        {
            TagWidthBits : int
            Tag : int64
            Operand : int64
            Op : Op
        }

        override this.ToString () : string =
            $"{this.Op} width=%i{this.TagWidthBits} tag=0x%x{this.Tag} operand=0x%x{this.Operand}"

    /// `Retagged` / `TagBitsOnly` only ever fire on the knife-edge conditions
    /// "operand's high region is all zeros" and "all ones". A uniformly sampled
    /// `int64` essentially never hits either, so those branches would be tested
    /// vacuously; the generator constructs the high region explicitly instead.
    let private genScenario : Gen<Scenario> =
        gen {
            let! tagWidthBits = Gen.choose (1, 8)
            let low = TaggedPointerBits.tagMask tagWidthBits
            let! tagRaw = ArbMap.defaults |> ArbMap.generate<int64>
            let! operandLow = ArbMap.defaults |> ArbMap.generate<int64>
            let! arbitraryHigh = ArbMap.defaults |> ArbMap.generate<int64>

            let! high =
                Gen.frequency
                    [
                        // Every base bit preserved / forced: the two statable shapes.
                        3, Gen.constant 0L
                        3, Gen.constant ~~~low
                        // Partial high regions, including near-misses at either end.
                        2, Gen.constant (arbitraryHigh &&& ~~~low)
                        1, Gen.constant (~~~low &&& ~~~(1L <<< tagWidthBits))
                        1, Gen.constant (1L <<< tagWidthBits)
                        1, Gen.constant System.Int64.MinValue
                        1, Gen.constant (~~~low &&& ~~~System.Int64.MinValue)
                    ]

            let! op = Gen.elements [ Op.And ; Op.Or ; Op.Xor ]

            return
                {
                    TagWidthBits = tagWidthBits
                    Tag = tagRaw &&& low
                    Operand = (operandLow &&& low) ||| high
                    Op = op
                }
        }

    type private Arbitraries =
        static member Scenario () : Arbitrary<Scenario> = Arb.fromGen genScenario

    /// FsCheck reports a raised exception as the counterexample's failure, which
    /// keeps the scenario visible without needing labelled properties.
    let private holds (message : string) (condition : bool) : bool =
        if not condition then
            failwith message

        true

    let private config : Config =
        Config.QuickThrowOnFailure.WithMaxTest(2000).WithArbitrary [ typeof<Arbitraries> ]

    [<Test>]
    let ``classification matches the per-bit oracle`` () : unit =
        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen genScenario)
                (fun scenario ->
                    let actual = scenario.Op.Invoke scenario.TagWidthBits scenario.Tag scenario.Operand

                    let expected =
                        expected scenario.Op scenario.TagWidthBits scenario.Tag scenario.Operand

                    holds $"%O{scenario}: got %O{actual}, oracle says %O{expected}" (actual = expected)
                )
        )

    [<Test>]
    let ``a stated answer holds for every admissible base`` () : unit =
        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen genScenario)
                (fun scenario ->
                    let low = TaggedPointerBits.tagMask scenario.TagWidthBits

                    match scenario.Op.Invoke scenario.TagWidthBits scenario.Tag scenario.Operand with
                    | TaggedPointerBitsResult.NotStatable -> true
                    | TaggedPointerBitsResult.Retagged tag ->
                        // The tag must stay inside the tag region, or the result is not
                        // a well-formed tagged pointer.
                        let inRange = tag &&& ~~~low = 0L

                        let agrees =
                            sampleBases scenario.TagWidthBits
                            |> List.forall (fun b ->
                                scenario.Op.Apply (b ||| scenario.Tag, scenario.Operand) = (b ||| tag)
                            )

                        holds
                            $"%O{scenario}: claimed Retagged 0x%x{tag} (in range: %b{inRange}, agrees: %b{agrees})"
                            (inRange && agrees)
                    | TaggedPointerBitsResult.TagBitsOnly bits ->
                        sampleBases scenario.TagWidthBits
                        |> List.forall (fun b -> scenario.Op.Apply (b ||| scenario.Tag, scenario.Operand) = bits)
                        |> holds $"%O{scenario}: claimed TagBitsOnly 0x%x{bits}"
                )
        )

    [<Test>]
    let ``a refusal is a refusal of something genuinely unanswerable`` () : unit =
        // Completeness: we must never decline to answer a question we could have
        // answered. Whenever the implementation says NotStatable, exhibit concrete
        // admissible bases ruling out both answer shapes.
        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen genScenario)
                (fun scenario ->
                    match scenario.Op.Invoke scenario.TagWidthBits scenario.Tag scenario.Operand with
                    | TaggedPointerBitsResult.Retagged _
                    | TaggedPointerBitsResult.TagBitsOnly _ -> true
                    | TaggedPointerBitsResult.NotStatable ->
                        let low = TaggedPointerBits.tagMask scenario.TagWidthBits

                        let results =
                            sampleBases scenario.TagWidthBits
                            |> List.map (fun b -> b, scenario.Op.Apply (b ||| scenario.Tag, scenario.Operand))

                        // No TagBitsOnly answer: two bases disagree.
                        let notConstant = results |> List.map snd |> List.distinct |> List.length > 1

                        // No Retagged answer: some base does not survive into its own
                        // bit positions in the result.
                        let notPreserving =
                            results |> List.exists (fun (b, result) -> result &&& ~~~low <> b)

                        holds
                            $"%O{scenario}: refused, but notConstant=%b{notConstant} notPreserving=%b{notPreserving}"
                            (notConstant && notPreserving)
                )
        )

    // The specific operations the CoreLib IL performs on a tagged GC handle. See
    // docs/plans/2026-08-06-tagged-gc-handles.md for the IL these come from.

    let private w : int = TaggedPointerBits.gcHandleTagWidthBits

    [<Test>]
    let ``WeakReference IsTrackResurrection reads the tag`` () : unit =
        // `_taggedHandle & 1`
        TaggedPointerBits.bitAnd w 0L 1L
        |> shouldEqual (TaggedPointerBitsResult.TagBitsOnly 0L)

        TaggedPointerBits.bitAnd w 1L 1L
        |> shouldEqual (TaggedPointerBitsResult.TagBitsOnly 1L)

    [<Test>]
    let ``WeakReference Create sets the tag`` () : unit =
        // `h | TracksResurrectionBit`
        TaggedPointerBits.bitOr w 0L 1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 1L)

    [<Test>]
    let ``WeakReference get_Target strips the resurrection bit and keeps the handle`` () : unit =
        // `_taggedHandle & ~TracksResurrectionBit`
        TaggedPointerBits.bitAnd w 1L ~~~1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 0L)

        // A COM-aware bit, had one been set, must survive that mask.
        TaggedPointerBits.bitAnd w 3L ~~~1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 2L)

    [<Test>]
    let ``WeakReference get_WeakHandle strips every tag bit`` () : unit =
        // `_taggedHandle & ~HandleTagBits`
        TaggedPointerBits.bitAnd w 3L ~~~3L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 0L)

    [<Test>]
    let ``GCHandle pinned marker round-trips`` () : unit =
        // `handle |= 1`, then `handle & 1` and `handle & ~1`.
        TaggedPointerBits.bitOr w 0L 1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 1L)

        TaggedPointerBits.bitAnd w 1L 1L
        |> shouldEqual (TaggedPointerBitsResult.TagBitsOnly 1L)

        TaggedPointerBits.bitAnd w 1L ~~~1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 0L)

    [<Test>]
    let ``masks reaching outside the tag region are refused`` () : unit =
        // 4 is the first bit above the tag region: `handle & 4` is a question
        // about the unknown base.
        TaggedPointerBits.bitAnd w 1L 4L
        |> shouldEqual TaggedPointerBitsResult.NotStatable

        TaggedPointerBits.bitOr w 1L 8L
        |> shouldEqual TaggedPointerBitsResult.NotStatable

        TaggedPointerBits.bitXor w 1L 4L
        |> shouldEqual TaggedPointerBitsResult.NotStatable

    [<Test>]
    let ``an all-ones operand is answerable for and and or but not xor`` () : unit =
        // `handle & -1` is the identity; `handle | -1` is -1 regardless of base;
        // `handle ^ -1` is the complement of an unknown value.
        TaggedPointerBits.bitAnd w 2L -1L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 2L)

        TaggedPointerBits.bitOr w 2L -1L
        |> shouldEqual (TaggedPointerBitsResult.TagBitsOnly -1L)

        TaggedPointerBits.bitXor w 2L -1L
        |> shouldEqual TaggedPointerBitsResult.NotStatable

    [<Test>]
    let ``a zero operand is answerable for or and xor but not and`` () : unit =
        // `handle & 0` is 0 regardless of base; `handle | 0` and `handle ^ 0` are
        // the identity.
        TaggedPointerBits.bitAnd w 2L 0L
        |> shouldEqual (TaggedPointerBitsResult.TagBitsOnly 0L)

        TaggedPointerBits.bitOr w 2L 0L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 2L)

        TaggedPointerBits.bitXor w 2L 0L
        |> shouldEqual (TaggedPointerBitsResult.Retagged 2L)
