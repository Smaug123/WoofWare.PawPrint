namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A single `leave` may exit several nested protected regions at once, and ECMA-335 III.3.55
/// requires every one of their `finally` handlers to run, innermost first.
///
/// `leave` enters only the innermost; each handler's `endfinally` then asks
/// `ExceptionHandling.nextFinallyToRun` for its successor, rather than the chain being computed
/// once and carried in the continuation. These tests pin that the walk reconstructs exactly the
/// chain `findFinallyBlocksToRun` describes — the walk is the implementation, the list is the
/// specification, and they must not be able to disagree.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFinallyChain =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A tower of properly nested `try` regions, outermost first.
    ///
    /// Nesting is the only shape the CLI permits: ECMA-335 II.12.4.2.7 forbids protected
    /// regions from partially overlapping, so two that share a point are always one inside the
    /// other. Successive levels may share a start offset, an end offset, or neither, but must
    /// differ somewhere — the share-a-start case is the one that makes `TryOffset` alone an
    /// insufficient sort key, and it is why the generator emits it.
    ///
    /// Built from the innermost region outward, each level growing by a non-zero amount at one
    /// or both ends. Constructing it this way makes proper nesting structural rather than
    /// something the generator has to check: growing outward cannot overshoot, whereas shrinking
    /// inward has to be clamped against an empty `try`, and a clamped level can escape its
    /// parent's end — which an earlier version of this generator did, producing towers that
    /// were not nested at all and a property failure that was the generator's fault.
    let private towerGen : Gen<ExceptionOffset list> =
        gen {
            let! depth = Gen.choose (1, 6)
            // Enough headroom that growing leftwards by at most 2 per level stays non-negative.
            let! innerStart = Gen.choose (2 * depth, 2 * depth + 20)
            let! innerLength = Gen.choose (1, 10)

            let! deltas =
                Gen.zip (Gen.choose (0, 2)) (Gen.choose (0, 2))
                // Grow somewhere, or the two regions would have the same extent and neither
                // would enclose the other.
                |> Gen.filter (fun (startDelta, endDelta) -> startDelta + endDelta > 0)
                |> Gen.listOfLength (depth - 1)

            let innerToOuter =
                deltas
                |> List.scan
                    (fun (start, len) (startDelta, endDelta) -> start - startDelta, len + startDelta + endDelta)
                    (innerStart, innerLength)

            let outerStart, outerLength = List.last innerToOuter

            // Handlers live outside every try, so a handler offset can never be mistaken for a
            // point inside a protected region.
            let beyond = outerStart + outerLength + 1

            return
                innerToOuter
                |> List.mapi (fun i (start, len) ->
                    {
                        TryOffset = start
                        TryLength = len
                        HandlerOffset = beyond + 2 * i
                        HandlerLength = 1
                    }
                )
                // Callers below take the tower outermost-first.
                |> List.rev
        }

    /// The innermost region of a tower is the last one built.
    let private innermost (tower : ExceptionOffset list) : ExceptionOffset = List.last tower

    /// A point outside every `try` in the tower, so a leave to it exits all of them.
    let private outsideEverything (tower : ExceptionOffset list) : int =
        (tower |> List.map (fun r -> r.TryOffset + r.TryLength) |> List.max) + 1000

    [<Test>]
    let ``a leave out of the whole tower runs every handler, innermost first`` () : unit =
        let property (tower : ExceptionOffset list) : unit =
            let regions = tower |> List.map ExceptionRegion.Finally
            let leaveSite = (innermost tower).TryOffset
            let target = outsideEverything tower

            ExceptionHandling.finallyBlocksBetween regions leaveSite target
            |> shouldEqual (List.rev tower)

        Check.One (config, Prop.forAll (Arb.fromGen towerGen) property)

    /// The load-bearing one: stepping the chain one `endfinally` at a time is equivalent to
    /// computing it in full up front. Were `nextFinallyToRun` to skip a handler, repeat one, or
    /// reverse the order, the reconstructed list would differ from the specification.
    [<Test>]
    let ``walking the chain one endfinally at a time reproduces it exactly`` () : unit =
        let property (tower : ExceptionOffset list) : unit =
            let regions = tower |> List.map ExceptionRegion.Finally
            let leaveSite = (innermost tower).TryOffset
            let target = outsideEverything tower

            let expected = ExceptionHandling.finallyBlocksBetween regions leaveSite target

            // `leave` enters the head; every subsequent handler is whatever the previous one's
            // `endfinally` derives.
            let rec walk (current : ExceptionOffset) (acc : ExceptionOffset list) : ExceptionOffset list =
                match ExceptionHandling.finallyBlocksAfter regions current target with
                | None -> failwith $"handler %O{current} was not found in its own chain"
                | Some [] -> List.rev (current :: acc)
                | Some (next :: _) -> walk next (current :: acc)

            walk (List.head expected) [] |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen towerGen) property)

    /// The chain must not depend on the order the regions happen to appear in the table.
    ///
    /// ECMA-335 II.25.4.6 does require a producer to list more deeply nested clauses first, so
    /// relying on table order would pass on real assemblies — right up until it did not. Sorting
    /// on `(-TryOffset, TryLength)` makes the order intrinsic to the regions, and this is what
    /// says so: with the old `-TryOffset`-only key, a shuffled tower whose levels share a start
    /// offset comes back in whatever order the shuffle produced.
    [<Test>]
    let ``the chain does not depend on the order of the region table`` () : unit =
        let property (tower : ExceptionOffset list, permuted : ExceptionOffset list) : unit =
            let leaveSite = (innermost tower).TryOffset
            let target = outsideEverything tower

            let ofOrder (rs : ExceptionOffset list) =
                ExceptionHandling.finallyBlocksBetween (rs |> List.map ExceptionRegion.Finally) leaveSite target

            ofOrder permuted |> shouldEqual (ofOrder tower)

        let shuffledGen =
            gen {
                let! tower = towerGen
                let! permuted = Gen.shuffle tower
                return tower, List.ofArray permuted
            }

        Check.One (config, Prop.forAll (Arb.fromGen shuffledGen) property)

    /// A `leave` whose target is still inside an enclosing `try` must not run that enclosing
    /// handler — the C# shape where a statement follows the loop. This is the boundary that
    /// stops the chain from over-running.
    [<Test>]
    let ``handlers whose try still contains the target are not run`` () : unit =
        let property (tower : ExceptionOffset list) : unit =
            match tower with
            | _ :: _ :: _ ->
                let outer = List.head tower
                let inner = innermost tower
                let regions = tower |> List.map ExceptionRegion.Finally

                // Land on the last byte of the outermost try: outside every inner region (they
                // shrink from at least one end, so they cannot reach it unless they share the
                // outer's end).
                let target = outer.TryOffset + outer.TryLength - 1

                let chain = ExceptionHandling.finallyBlocksBetween regions inner.TryOffset target

                chain |> List.contains outer |> shouldEqual false
            | _ ->
                // A one-deep tower has no enclosing region to exclude.
                ()

        Check.One (config, Prop.forAll (Arb.fromGen towerGen) property)

    [<Test>]
    let ``regions that start together are ordered by extent, shortest first`` () : unit =
        let outer =
            {
                TryOffset = 4
                TryLength = 20
                HandlerOffset = 40
                HandlerLength = 1
            }

        let inner =
            {
                TryOffset = 4
                TryLength = 6
                HandlerOffset = 50
                HandlerLength = 1
            }

        // Table deliberately outermost-first, i.e. the opposite of what a real producer emits.
        let regions = [ ExceptionRegion.Finally outer ; ExceptionRegion.Finally inner ]

        ExceptionHandling.finallyBlocksBetween regions 5 100
        |> shouldEqual [ inner ; outer ]

    [<Test>]
    let ``non-finally regions are not part of the chain`` () : unit =
        let fin =
            {
                TryOffset = 0
                TryLength = 10
                HandlerOffset = 20
                HandlerLength = 1
            }

        let fault =
            {
                TryOffset = 0
                TryLength = 10
                HandlerOffset = 30
                HandlerLength = 1
            }

        let regions = [ ExceptionRegion.Fault fault ; ExceptionRegion.Finally fin ]

        ExceptionHandling.finallyBlocksBetween regions 5 100 |> shouldEqual [ fin ]

    /// `finallyBlocksAfter` distinguishes "no handlers left" from "that handler is not in this
    /// chain at all"; `nextFinallyToRun` turns the latter into a loud failure rather than
    /// silently resuming at the leave target and skipping the rest of the chain.
    [<Test>]
    let ``a region outside the chain is reported as absent, not as an empty tail`` () : unit =
        let fin =
            {
                TryOffset = 0
                TryLength = 10
                HandlerOffset = 20
                HandlerLength = 1
            }

        let unrelated =
            {
                TryOffset = 200
                TryLength = 10
                HandlerOffset = 220
                HandlerLength = 1
            }

        let regions = [ ExceptionRegion.Finally fin ]

        ExceptionHandling.finallyBlocksAfter regions fin 100 |> shouldEqual (Some [])
        ExceptionHandling.finallyBlocksAfter regions unrelated 100 |> shouldEqual None
