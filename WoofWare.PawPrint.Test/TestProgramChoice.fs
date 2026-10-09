namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `ProgramChoice.choose`, which picks the program a driver of several runs at a tick, is a
/// pure function of the choice, the tick, the launch order, the program chosen last and the
/// programs that can run: these hold it to a reference for round robin, and to determinism and
/// uniformity for a seed.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProgramChoice =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A launch order of one to six programs, the program chosen last, and a non-empty set of
    /// candidates among them, in launch order.
    let private situation : Arbitrary<ProcessId list * ProcessId * ProcessId list> =
        gen {
            let! count = Gen.choose (1, 6)

            let launched =
                [ for i in 1..count -> ProcessId.parseOrFail "TestProgramChoice" (100 + 7 * i) ]

            let! last = Gen.elements launched
            let! mask = Gen.listOfLength count (Gen.elements [ true ; false ])
            let! forced = Gen.elements launched

            let candidates =
                List.zip launched mask
                |> List.filter (fun (pid, keep) -> keep || pid = forced)
                |> List.map fst

            return launched, last, candidates
        }
        |> Arb.fromGen

    /// Round robin's reference: the launch order rotated to start just after `last`, and the
    /// first candidate in it.
    let private nextAfter (launched : ProcessId list) (last : ProcessId) (candidates : ProcessId list) : ProcessId =
        let index = List.findIndex ((=) last) launched
        let rotated = List.skip (index + 1) launched @ List.take (index + 1) launched
        rotated |> List.find (fun pid -> List.contains pid candidates)

    [<Test>]
    let ``round robin takes the next program that can run after the one chosen last`` () : unit =
        let property ((launched, last, candidates) : ProcessId list * ProcessId * ProcessId list) (tick : int64) =
            ProgramChoice.choose ProgramChoice.RoundRobin tick launched last candidates
            |> shouldEqual (nextAfter launched last candidates)

        Check.One (propertyConfig, Prop.forAll situation property)

    [<Test>]
    let ``a seeded choice is a candidate, and the same at the same seed and tick`` () : unit =
        let property
            ((launched, last, candidates) : ProcessId list * ProcessId * ProcessId list)
            (seed : uint64)
            (tick : int64)
            =
            let chosen =
                ProgramChoice.choose (ProgramChoice.Seeded seed) tick launched last candidates

            List.contains chosen candidates |> shouldEqual true

            // Whichever program ran last: the seed and the tick alone decide.
            for other in launched do
                ProgramChoice.choose (ProgramChoice.Seeded seed) tick launched other candidates
                |> shouldEqual chosen

        Check.One (propertyConfig, Prop.forAll situation property)

    [<Test>]
    let ``a seeded choice between two programs takes each about half the time`` () : unit =
        let launched =
            [
                ProcessId.parseOrFail "TestProgramChoice" 100
                ProcessId.parseOrFail "TestProgramChoice" 101
            ]

        let property (seed : uint64) (start : int64) =
            let ticks = 2000

            let first =
                [ 0 .. ticks - 1 ]
                |> List.filter (fun offset ->
                    ProgramChoice.choose
                        (ProgramChoice.Seeded seed)
                        (start + int64 offset)
                        launched
                        launched.[0]
                        launched = launched.[0]
                )
                |> List.length

            // Five standard deviations either side of a fair coin's 1000.
            first |> shouldBeGreaterThan 888
            first |> shouldBeSmallerThan 1112

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property
        )

    [<Test>]
    let ``two seeds choose differently somewhere in a run`` () : unit =
        let launched =
            [
                ProcessId.parseOrFail "TestProgramChoice" 100
                ProcessId.parseOrFail "TestProgramChoice" 101
            ]

        let property (seed : uint64) (other : uint64) =
            let run (s : uint64) =
                [ 0L .. 199L ]
                |> List.map (fun tick ->
                    ProgramChoice.choose (ProgramChoice.Seeded s) tick launched launched.[0] launched
                )

            seed = other || run seed <> run other

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property)

    [<Test>]
    let ``no candidate, one never launched, or a last program never launched, is refused`` () : unit =
        let launched = [ ProcessId.parseOrFail "TestProgramChoice" 100 ]
        let stranger = ProcessId.parseOrFail "TestProgramChoice" 200

        for choice in [ ProgramChoice.RoundRobin ; ProgramChoice.Seeded 7UL ] do
            (fun () -> ProgramChoice.choose choice 0L launched launched.[0] [] |> ignore)
            |> shouldFail<exn>

            // One candidate is no choice, but is refused all the same if it was never launched.
            (fun () -> ProgramChoice.choose choice 0L launched launched.[0] [ stranger ] |> ignore)
            |> shouldFail<exn>

            (fun () ->
                ProgramChoice.choose choice 0L launched launched.[0] [ launched.[0] ; stranger ]
                |> ignore
            )
            |> shouldFail<exn>

            (fun () -> ProgramChoice.choose choice 0L launched stranger [ launched.[0] ] |> ignore)
            |> shouldFail<exn>
