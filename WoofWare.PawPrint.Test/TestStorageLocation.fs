namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Laws of `StorageLocation.overlapVerdict`.
///
/// `overlapVerdict` is pure over `LocationResolution`, which is the whole point of splitting
/// it out of `shouldCopyBackwards`: the interesting failure mode has nothing to do with how a
/// pointer resolves and everything to do with how two resolutions of *differing precision*
/// combine. Testing it here needs no `IlMachineState`, so the laws can be stated directly
/// rather than smuggled through a guest program.
///
/// Note what is deliberately *not* asserted: that `resolve` produces any particular
/// resolution. That would restate `byteLocation`, whose behaviour this stage does not change.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStorageLocation =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// A deliberately tiny alphabet. Every law below is about *equality* of coarse keys and
    /// *ordering* of byte offsets, never about a key's content, so three distinct coarse keys
    /// and two distinct byte storages suffice to reach both the equal and unequal cases. What
    /// proves the cases are actually reached is the distribution check at the end of each
    /// property, not the width of the alphabet.
    let private coarseAlphabet : StorageLocation.SharedStorageKey list =
        [
            StorageLocation.SharedStorageKey.ArrayCell (ManagedHeapAddress 1, 0)
            StorageLocation.SharedStorageKey.ArrayCell (ManagedHeapAddress 1, 1)
            StorageLocation.SharedStorageKey.HeapValue (ManagedHeapAddress 2)
        ]

    let private storageAlphabet : ByteStorageIdentity list =
        [
            ByteStorageIdentity.Array (ManagedHeapAddress 1)
            ByteStorageIdentity.Array (ManagedHeapAddress 2)
        ]

    /// Offsets and byte counts are drawn with an explicit `Gen.choose`, not from FsCheck's
    /// default `int`: under `Quick` that is size-bounded to roughly [-100, 100], and deriving
    /// a whole case from one such value would explore only ~100 distinct cases however high
    /// `MaxTest` is set. The distribution checks below caught exactly that — the first draft
    /// of this file drove everything from one `NonNegativeInt` seed and never once produced a
    /// `CopyBackwards` verdict.
    let private genPrecise : Gen<(ByteStorageIdentity * int64) option> =
        Gen.frequency
            [
                1, Gen.constant None
                2,
                gen {
                    let! storage = Gen.elements storageAlphabet
                    let! offset = Gen.choose (0, 11)
                    return Some (storage, int64 offset)
                }
            ]

    let private genResolution : Gen<StorageLocation.LocationResolution> =
        Gen.frequency
            [
                1, Gen.constant StorageLocation.LocationResolution.Unrelatable
                6,
                gen {
                    let! coarse = Gen.elements coarseAlphabet
                    let! precise = genPrecise
                    return StorageLocation.LocationResolution.Located (coarse, precise)
                }
            ]

    let private genCase : Gen<StorageLocation.LocationResolution * StorageLocation.LocationResolution * int> =
        gen {
            let! src = genResolution
            let! dest = genResolution
            let! byteCount = Gen.choose (0, 11)
            return src, dest, byteCount
        }

    let private preciseOf (r : StorageLocation.LocationResolution) : (ByteStorageIdentity * int64) option =
        match r with
        | StorageLocation.LocationResolution.Located (_, precise) -> precise
        | StorageLocation.LocationResolution.Unrelatable -> None

    /// The pre-refactor decision, transcribed from `CellAwareMemOps.shouldCopyBackwards` as it
    /// stood before this stage. An independent statement of the arithmetic, so that a slip in
    /// rewriting the `match` shows up as a disagreement rather than as a silently reordered
    /// guard.
    let private referenceBackwards
        (src : (ByteStorageIdentity * int64) option)
        (dest : (ByteStorageIdentity * int64) option)
        (byteCount : int)
        : bool
        =
        match src, dest with
        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) when srcStorage = destStorage ->
            srcOffset < destOffset && destOffset < srcOffset + int64 byteCount
        | _ -> false

    /// The law Codex's review of the plan was about: when two byrefs share a coarse key but
    /// either lacks a flat coordinate, the direction is *not derivable*, and the verdict must
    /// say so. A resolution type that dropped its coarse key once a precise one was available
    /// could not state this law at all, because the pair would be incomparable.
    [<Test>]
    let ``equal coarse keys with either side imprecise is undecidable`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match src, dest with
                    | StorageLocation.LocationResolution.Located (srcCoarse, srcPrecise),
                      StorageLocation.LocationResolution.Located (destCoarse, destPrecise) when
                        srcCoarse = destCoarse && (srcPrecise.IsNone || destPrecise.IsNone)
                        ->
                        observed <- observed + 1

                        match StorageLocation.overlapVerdict src dest byteCount with
                        | StorageLocation.OverlapVerdict.Undecidable key -> key |> shouldEqual srcCoarse
                        | other ->
                            failwith
                                $"expected Undecidable for shared coarse key %A{srcCoarse} with an imprecise side, got %A{other}"
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        // Distribution check: the law is vacuous unless the shared-key-imprecise shape is
        // actually generated. If this were 0 the property above would pass on an
        // implementation that never returns `Undecidable` at all.
        if observed = 0 then
            failwith "property never generated a shared-coarse-key pair with an imprecise side"

    /// `CopyBackwards` is the only verdict that can corrupt data if wrong, so it must be
    /// earned: both sides precise, one byte storage, and `dest` starting strictly inside
    /// `src`'s range.
    [<Test>]
    let ``backwards is claimed only for a genuine forward overlap`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match StorageLocation.overlapVerdict src dest byteCount with
                    | StorageLocation.OverlapVerdict.CopyBackwards ->
                        observed <- observed + 1

                        match preciseOf src, preciseOf dest with
                        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) ->
                            srcStorage |> shouldEqual destStorage
                            (srcOffset < destOffset) |> shouldEqual true
                            (destOffset < srcOffset + int64 byteCount) |> shouldEqual true
                        | _ ->
                            failwith
                                $"CopyBackwards claimed without a precise offset on both sides: %A{src} / %A{dest}"
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        if observed = 0 then
            failwith "property never produced a CopyBackwards verdict, so the law is vacuous"

    /// A non-byref endpoint shares storage with nothing, so a copy involving one can always
    /// run forwards. This is the arm that must never reach `Undecidable`.
    [<Test>]
    let ``an unrelatable endpoint always copies forwards`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    if
                        src = StorageLocation.LocationResolution.Unrelatable
                        || dest = StorageLocation.LocationResolution.Unrelatable
                    then
                        observed <- observed + 1

                        StorageLocation.overlapVerdict src dest byteCount
                        |> shouldEqual StorageLocation.OverlapVerdict.CopyForwards
                )

        Check.One (propertyConfig, property)

        if observed = 0 then
            failwith "property never generated an Unrelatable endpoint"

    /// Behaviour preservation: for the both-precise case, the new verdict must agree with the
    /// arithmetic the pre-refactor `shouldCopyBackwards` performed.
    [<Test>]
    let ``both-precise agrees with the pre-refactor decision`` () : unit =
        let mutable observedBackwards = 0
        let mutable observedForwards = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match preciseOf src, preciseOf dest with
                    | Some _, Some _ ->
                        let expected = referenceBackwards (preciseOf src) (preciseOf dest) byteCount

                        let actual =
                            match StorageLocation.overlapVerdict src dest byteCount with
                            | StorageLocation.OverlapVerdict.CopyBackwards -> true
                            | StorageLocation.OverlapVerdict.CopyForwards -> false
                            | StorageLocation.OverlapVerdict.Undecidable key ->
                                failwith $"both sides precise, yet the verdict was Undecidable %A{key}"

                        actual |> shouldEqual expected

                        if expected then
                            observedBackwards <- observedBackwards + 1
                        else
                            observedForwards <- observedForwards + 1
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        // Both outcomes must occur, or the agreement is only being checked on one branch.
        if observedBackwards = 0 then
            failwith "never observed a both-precise overlapping pair"

        if observedForwards = 0 then
            failwith "never observed a both-precise non-overlapping pair"
