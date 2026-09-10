namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestPersistentVector =

    /// The trie's shape changes at each power of 32, so lengths are drawn per regime rather
    /// than uniformly: a uniform draw up to 70000 would almost never produce a leaf-only
    /// vector, and never an empty one.
    [<RequireQualifiedAccess>]
    type private Regime =
        | Empty
        | Leaf
        | OneBranch
        | TwoBranch
        | ThreeBranch

    let private lengthOf (regime : Regime) : Gen<int> =
        match regime with
        | Regime.Empty -> Gen.constant 0
        | Regime.Leaf -> Gen.choose (1, 32)
        | Regime.OneBranch -> Gen.choose (33, 1024)
        | Regime.TwoBranch -> Gen.choose (1025, 32768)
        | Regime.ThreeBranch -> Gen.choose (32769, 70000)

    let private regimes =
        [
            Regime.Empty
            Regime.Leaf
            Regime.OneBranch
            Regime.TwoBranch
            Regime.ThreeBranch
        ]

    let private regimeGen : Gen<Regime> = Gen.elements regimes

    let private assertEveryRegimeSeen (seen : Collections.Generic.Dictionary<Regime, int>) : unit =
        for regime in regimes do
            let count =
                match seen.TryGetValue regime with
                | true, n -> n
                | false, _ -> 0

            if count < 10 then
                failwith $"regime %A{regime} was generated only %d{count} times; the generator is not exploring it"

    /// A vector and the array it was built from, plus a sequence of in-range replacements.
    type private Scenario =
        {
            Regime : Regime
            Source : int[]
            Ops : (int * int) list
        }

    let private scenarioGen : Gen<Scenario> =
        gen {
            let! regime = regimeGen
            let! length = lengthOf regime
            let! source = Gen.arrayOfLength length (Gen.choose (-1000, 1000))

            let! ops =
                if length = 0 then
                    Gen.constant []
                else
                    Gen.listOf (Gen.zip (Gen.choose (0, length - 1)) (Gen.choose (-1000, 1000)))
                    |> Gen.map (List.truncate 20)

            return
                {
                    Regime = regime
                    Source = source
                    Ops = ops
                }
        }

    [<Test>]
    let ``Every version agrees with a mutable array snapshot taken at the same point`` () =
        let seen = Collections.Generic.Dictionary<Regime, int> ()

        let property (scenario : Scenario) : unit =
            seen.[scenario.Regime] <-
                (match seen.TryGetValue scenario.Regime with
                 | true, n -> n
                 | false, _ -> 0)
                + 1

            let initial = PersistentVector.ofArray scenario.Source
            let reference = Array.copy scenario.Source

            // Every version is kept so that a later `set` which wrongly mutated a shared
            // node would show up as an earlier version changing.
            let versions =
                (([ initial, Array.copy reference ], reference), scenario.Ops)
                ||> List.fold (fun (acc, reference) (index, value) ->
                    let latest, _ = List.head acc
                    let next = PersistentVector.set index value latest
                    reference.[index] <- value
                    (next, Array.copy reference) :: acc, reference
                )
                |> fst

            // Enumeration is checked on the final version only: it walks the same nodes as
            // `toArray`, and doing it per version doubles a test already dominated by the
            // 70000-element regime.
            let (latest, latestSnapshot) = List.head versions
            Seq.toList latest |> shouldEqual (List.ofArray latestSnapshot)

            for (vector, snapshot) in versions do
                vector.Length |> shouldEqual snapshot.Length
                vector.IsEmpty |> shouldEqual (snapshot.Length = 0)
                PersistentVector.toArray vector |> shouldEqual snapshot

                for (index, _) in scenario.Ops do
                    PersistentVector.item index vector |> shouldEqual snapshot.[index]

                if snapshot.Length > 0 then
                    vector.[0] |> shouldEqual snapshot.[0]
                    vector.[snapshot.Length - 1] |> shouldEqual snapshot.[snapshot.Length - 1]

        let config = Config.QuickThrowOnFailure.WithMaxTest 300
        Check.One (config, Prop.forAll (Arb.fromGen scenarioGen) property)
        assertEveryRegimeSeen seen

    [<Test>]
    let ``ofSeq and init agree with ofArray`` () =
        let property (regime : Regime, length : int) : unit =
            let source = Array.init length (fun i -> i * 7 - 3)
            let expected = PersistentVector.ofArray source
            PersistentVector.ofSeq (Seq.ofArray source) |> shouldEqual expected

            let calls = ResizeArray<int> ()

            let built =
                PersistentVector.init
                    length
                    (fun i ->
                        calls.Add i
                        i * 7 - 3
                    )

            built |> shouldEqual expected
            List.ofSeq calls |> shouldEqual [ 0 .. length - 1 ]

        let gen =
            gen {
                let! regime = regimeGen
                let! length = lengthOf regime
                return regime, length
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``Reading or replacing outside the vector raises IndexOutOfRangeException`` () =
        let gen =
            gen {
                let! regime = regimeGen
                let! length = lengthOf regime
                let! distance = Gen.choose (0, 100)
                let! below = Gen.elements [ true ; false ]
                let index = if below then -1 - distance else length + distance
                return length, index
            }

        let property (length : int, index : int) : unit =
            let vector = PersistentVector.init length id

            (fun () -> PersistentVector.item index vector |> ignore<int>)
            |> shouldFail<IndexOutOfRangeException>

            (fun () -> PersistentVector.set index 0 vector |> ignore<PersistentVector<int>>)
            |> shouldFail<IndexOutOfRangeException>

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``Equality and hashing are structural`` () =
        let mutable equalCases = 0
        let mutable unequalCases = 0

        let gen =
            gen {
                let! regime = regimeGen
                let! length = lengthOf regime
                let! source = Gen.arrayOfLength length (Gen.choose (-5, 5))
                let! sameContents = Gen.elements [ true ; false ]

                let! other =
                    if sameContents || length = 0 then
                        Gen.constant (Array.copy source)
                    else
                        gen {
                            let! index = Gen.choose (0, length - 1)
                            let copy = Array.copy source
                            // Guaranteed to differ: the alphabet is -5..5 and this is outside it.
                            copy.[index] <- 100
                            return copy
                        }

                return source, other
            }

        let property (source : int[], other : int[]) : unit =
            let a = PersistentVector.ofArray source
            let b = PersistentVector.ofArray other

            if source = other then
                equalCases <- equalCases + 1
                a |> shouldEqual b
                a.GetHashCode () |> shouldEqual (b.GetHashCode ())
            else
                unequalCases <- unequalCases + 1
                a |> shouldNotEqual b

            // A replacement that is undone gives back an equal vector.
            if source.Length > 0 then
                let index = source.Length / 2

                let roundTrip =
                    a |> PersistentVector.set index 999 |> PersistentVector.set index source.[index]

                roundTrip |> shouldEqual a
                roundTrip.GetHashCode () |> shouldEqual (a.GetHashCode ())

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen gen) property)

        if equalCases < 30 || unequalCases < 30 then
            failwith $"saw %d{equalCases} equal and %d{unequalCases} unequal pairs; the generator is not exploring both"

    /// Equal hashes for unequal vectors are legal, so the structural test above cannot tell a
    /// hash that reads the elements from one that reads only the length. This can: two
    /// same-length vectors differing in one element hash apart in all but a negligible
    /// fraction of cases.
    [<Test>]
    let ``Vectors that differ in one element almost always hash differently`` () =
        let mutable pairs = 0
        let mutable collisions = 0

        let gen =
            gen {
                let! regime = Gen.elements [ Regime.Leaf ; Regime.OneBranch ; Regime.TwoBranch ]
                let! length = lengthOf regime
                let! source = Gen.arrayOfLength length (Gen.choose (-5, 5))
                let! index = Gen.choose (0, length - 1)
                let other = Array.copy source
                // Outside the alphabet, so the pair is guaranteed unequal.
                other.[index] <- 100
                return source, other
            }

        let property (source : int[], other : int[]) : unit =
            pairs <- pairs + 1

            if (PersistentVector.ofArray source).GetHashCode () = (PersistentVector.ofArray other).GetHashCode () then
                collisions <- collisions + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen gen) property)

        // `HashCode` mixes every element, so a collision is a 2^-32 event; a hash that ignored
        // the elements would collide on every pair.
        if collisions * 10 > pairs then
            failwith $"%d{collisions} of %d{pairs} one-element-different pairs hashed alike"

    /// The reason the structure exists: a replacement copies one root-to-leaf path and shares
    /// every other node by reference. Observed through the internal node type, since the
    /// public API cannot distinguish sharing from copying.
    [<Test>]
    let ``A replacement shares every node off the replaced element's path`` () =
        let rec sharedAndCopied (before : PersistentVectorNode<int>) (after : PersistentVectorNode<int>) : int * int =
            if obj.ReferenceEquals (before, after) then
                1, 0
            else
                match before, after with
                | PersistentVectorNode.Leaf _, PersistentVectorNode.Leaf _ -> 0, 1
                | PersistentVectorNode.Branch b, PersistentVectorNode.Branch a ->
                    a.Length |> shouldEqual b.Length

                    let shared, copied =
                        Array.fold2
                            (fun (shared, copied) b a ->
                                let s, c = sharedAndCopied b a
                                shared + s, copied + c
                            )
                            (0, 0)
                            b
                            a

                    shared, copied + 1
                | _ -> failwith "a replacement changed the trie's shape"

        let rec depth (node : PersistentVectorNode<int>) : int =
            match node with
            | PersistentVectorNode.Leaf _ -> 1
            | PersistentVectorNode.Branch children -> 1 + depth children.[0]

        let gen =
            gen {
                let! regime = Gen.elements [ Regime.Leaf ; Regime.OneBranch ; Regime.TwoBranch ; Regime.ThreeBranch ]
                let! length = lengthOf regime
                let! index = Gen.choose (0, length - 1)
                return length, index
            }

        let property (length : int, index : int) : unit =
            let before = PersistentVector.init length id
            let after = PersistentVector.set index -1 before
            let _, copied = sharedAndCopied before.Root after.Root
            // Exactly the path: one node per level, no more.
            copied |> shouldEqual (depth before.Root)
            after.Shift |> shouldEqual before.Shift

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)
