namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `ClockJitterStrategy` lets a run advance the virtual clock onto a deadline a
/// thread is already parked on, so that a timeout fires while other threads
/// still had work left in the window. Without it the clock is
/// `StepCounter * InstructionCostTicks` exactly, and a guest's timeout arm is
/// only ever reached when its own arithmetic sends it there.
///
/// Most of this fixture tests `ClockJitter.chooseJump`, which is the whole
/// decision: it takes the tick, the clock, and the outstanding deadlines, and
/// says where the clock goes. Keeping it a pure function of those three is what
/// makes the strategy replayable, and it is why these are ordinary property
/// tests rather than assertions about a running guest. The end-to-end section
/// at the bottom covers the wiring the pure function cannot: that the driver
/// consults it, that a disabled run is unchanged, and that an enabled one
/// reaches a timeout arm the guest would otherwise never take.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClockJitter =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Fold an arbitrary int64 into `[0, bound]`. Deliberately not `abs`, which
    /// throws on `Int64.MinValue` — a value FsCheck does generate.
    let private intoRange (bound : int64) (seed : int64) : int64 =
        let modulus = bound + 1L
        ((seed % modulus) + modulus) % modulus

    /// An arbitrary clock reading paired with an arbitrary bag of deadlines,
    /// both folded into a range small enough that collisions between deadlines
    /// — and deadlines level with or behind the clock — occur often rather than
    /// being vanishingly rare. Those are the interesting inputs: a deadline
    /// behind the clock must not be jumped to, and two threads parked on one
    /// instant must not make that instant twice as likely as any other.
    let private clockAndDeadlines : Arbitrary<int64 * int64 list> =
        ArbMap.defaults
        |> ArbMap.arbitrary<int64 * int64 list>
        |> Arb.convert (fun (clock, deadlines) -> intoRange 100L clock, deadlines |> List.map (intoRange 200L)) id

    let private seeds : Arbitrary<uint64> = ArbMap.defaults |> ArbMap.arbitrary<uint64>

    // ------------------------------------------------------------------
    // chooseJump — the contract every strategy shares
    // ------------------------------------------------------------------

    [<Test>]
    let ``Disabled never jumps`` () : unit =
        // The default, and the reason every existing run is bit-for-bit
        // unchanged by this feature's arrival.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            [ 0L ; 1L ; 7L ; 1_000L ]
            |> List.forall (fun tick -> ClockJitter.chooseJump ClockJitterStrategy.Disabled tick clock deadlines = None)

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``a jump always lands on an outstanding deadline strictly ahead of the clock`` () : unit =
        // The property the caller relies on to hand the answer straight to
        // `withVirtualClockTicks`: that setter rejects a backwards move, so a
        // `chooseJump` that could return a stale deadline would turn a fuzzing
        // knob into a crash. "Ahead of the clock" also makes `Some` mean a
        // genuine advance rather than a no-op the caller must re-check.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            [ 0L .. 30L ]
            |> List.forall (fun tick ->
                match
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (7UL, 1.0, 0L)) tick clock deadlines
                with
                | None -> true
                | Some target -> target > clock && List.contains target deadlines
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``the decision is a pure function of tick, clock and deadlines`` () : unit =
        // Replay rests on this and nothing else: no mutable PRNG state means
        // two runs of one seed cannot diverge however they are interleaved with
        // other draws.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            [ 0L .. 20L ]
            |> List.forall (fun tick ->
                let strategy = ClockJitterStrategy.EagerDeadlines (99UL, 0.5, 0L)

                ClockJitter.chooseJump strategy tick clock deadlines = ClockJitter.chooseJump
                    strategy
                    tick
                    clock
                    deadlines
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``the answer depends only on the set of deadlines ahead of the clock`` () : unit =
        // The caller enumerates a `Map` of threads, so the order deadlines
        // arrive in is an implementation detail of thread-id allocation, and
        // duplicates appear whenever two threads park on the same instant.
        // Neither may reach the answer, or the jitter schedule would silently
        // depend on how many threads happened to share a deadline.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            let strategy = ClockJitterStrategy.EagerDeadlines (3UL, 0.75, 0L)

            let permuted = deadlines |> List.rev
            let duplicated = deadlines @ deadlines
            // Duplicating *one* deadline rather than all of them. Doubling the
            // whole list is uniform, so it leaves the draw's distribution
            // unchanged even if duplicates were never removed at all; only a
            // lopsided repeat distinguishes "deduplicated" from "not".
            let oneRepeated =
                match deadlines with
                | [] -> []
                | head :: rest -> head :: head :: rest

            // Deadlines at or behind the clock are about to fire anyway, so
            // adding more of them must not change where the clock jumps.
            let withStale = deadlines @ [ clock ; 0L ]

            [ 0L .. 30L ]
            |> List.forall (fun tick ->
                let expected = ClockJitter.chooseJump strategy tick clock deadlines

                ClockJitter.chooseJump strategy tick clock permuted = expected
                && ClockJitter.chooseJump strategy tick clock duplicated = expected
                && ClockJitter.chooseJump strategy tick clock oneRepeated = expected
                && ClockJitter.chooseJump strategy tick clock withStale = expected
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``probability zero never jumps and probability one always jumps`` () : unit =
        // The endpoints are what a caller reaches for to turn the dial fully
        // off (equivalent to `Disabled`, and asserted end-to-end below) or
        // fully on, so neither may be an off-by-one on the comparison.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            let anyAhead = deadlines |> List.exists (fun d -> d > clock)

            [ 0L .. 30L ]
            |> List.forall (fun tick ->
                let never =
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (11UL, 0.0, 0L)) tick clock deadlines

                let always =
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (11UL, 1.0, 0L)) tick clock deadlines

                never = None && (Option.isSome always = anyAhead)
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``no deadline ahead of the clock means no jump, whatever the seed`` () : unit =
        // There is nothing to jump *to*: jitter moves the clock onto waits that
        // exist, so a tick with none is one where the strategy has no opinion.
        // Asserted across seeds because a draw that ignored the candidate list
        // would pass for most single seeds.
        let property (seed : uint64) : bool =
            let clock = 500L

            [ [] ; [ 500L ] ; [ 0L ; 100L ; 499L ] ; [ 500L ; 500L ] ]
            |> List.forall (fun deadlines ->
                [ 0L .. 20L ]
                |> List.forall (fun tick ->
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (seed, 1.0, 0L)) tick clock deadlines = None
                )
            )

        Check.One (propertyConfig, Prop.forAll seeds property)

    // ------------------------------------------------------------------
    // EagerDeadlines — which deadline, and how often
    // ------------------------------------------------------------------

    [<Test>]
    let ``every outstanding deadline is reachable, not just the nearest`` () : unit =
        // The whole reason the candidate is drawn from *all* pending deadlines.
        // Jumping only ever to the nearest would explore a single ordering of
        // several outstanding timeouts; drawing from all of them lets one jump
        // expire a batch, which is where multi-timeout orderings come from.
        //
        // Deterministic rather than statistical: one seed, a fixed tick range,
        // and an exact set comparison, so this cannot flake.
        let candidates = [ 10L ; 20L ; 30L ; 40L ; 50L ]

        let chosen =
            [ 0L .. 999L ]
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (2024UL, 1.0, 0L)) tick 0L candidates
            )
            |> Set.ofList

        chosen |> shouldEqual (Set.ofList candidates)

    [<Test>]
    let ``threads sharing a deadline do not make that instant likelier`` () : unit =
        // How many threads happen to be parked on one instant is an accident of
        // the guest's structure, not a statement about which orderings are
        // worth exploring. Left in, it would bias the search towards whichever
        // deadline the most threads shared — so the candidate list is
        // deduplicated, and this is the assertion that keeps it that way.
        let ticks = [ 0L .. 999L ]

        let countsFor (deadlines : int64 list) : Map<int64, int> =
            ticks
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (31UL, 1.0, 0L)) tick 0L deadlines
            )
            |> List.countBy id
            |> Map.ofList

        // Three threads waiting on 10 and one on 20, against one each.
        countsFor [ 10L ; 10L ; 10L ; 20L ] |> shouldEqual (countsFor [ 10L ; 20L ])

    [<Test>]
    let ``the jump rate tracks the configured probability`` () : unit =
        // A probability that did not control the *rate* would still pass every
        // property above — `>= probability` inverted, say, or a draw that
        // ignored the seed — so pin the frequency itself. Deterministic given
        // the seed and tick range, so the tolerance is not a flake budget: it
        // is the sampling error this particular hash happens to have.
        let ticks = [ 0L .. 9_999L ]
        let candidates = [ 1_000L ]

        for probability in [ 0.1 ; 0.25 ; 0.5 ] do
            let jumps =
                ticks
                |> List.sumBy (fun tick ->
                    match
                        ClockJitter.chooseJump
                            (ClockJitterStrategy.EagerDeadlines (5UL, probability, 0L))
                            tick
                            0L
                            candidates
                    with
                    | Some _ -> 1
                    | None -> 0
                )

            let observed = float jumps / float ticks.Length

            abs (observed - probability) |> shouldBeSmallerThan 0.02

    [<Test>]
    let ``a different seed gives a different jump sequence`` () : unit =
        // Sweeping seeds is how a harness explores timing orderings, so two
        // seeds must not agree tick-for-tick. (A hash that dropped the seed
        // entirely would satisfy every other property in this fixture.)
        let candidates = [ 10L ; 20L ; 30L ]

        let sequenceFor (seed : uint64) : int64 option list =
            [ 0L .. 199L ]
            |> List.map (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (seed, 0.5, 0L)) tick 0L candidates
            )

        sequenceFor 1UL |> shouldNotEqual (sequenceFor 2UL)

    // ------------------------------------------------------------------
    // EagerDeadlines — how late the timeout fires
    // ------------------------------------------------------------------

    [<Test>]
    let ``a jump lands within the overshoot bound of some outstanding deadline`` () : unit =
        // The generalisation of "lands exactly on a deadline", which holds only
        // at a zero bound. Checked against the *input* deadlines rather than
        // against a deadline the implementation reports having picked: an
        // implementation that chose badly and then told us which bad choice it
        // made would satisfy the latter.
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            let maxOvershoot = 7L

            [ 0L .. 30L ]
            |> List.forall (fun tick ->
                match
                    ClockJitter.chooseJump
                        (ClockJitterStrategy.EagerDeadlines (5UL, 1.0, maxOvershoot))
                        tick
                        clock
                        deadlines
                with
                | None -> true
                | Some target ->
                    target > clock
                    && deadlines
                       |> List.exists (fun d -> d > clock && d <= target && target <= d + maxOvershoot)
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``a zero bound fires every timeout exactly on its deadline`` () : unit =
        // The setting that reproduces the behaviour before overshoot existed,
        // and the one the rest of this fixture is written against. Worth its own
        // assertion because "0 means 0" is an off-by-one away from "0 means 1".
        let property ((clock, deadlines) : int64 * int64 list) : bool =
            [ 0L .. 30L ]
            |> List.forall (fun tick ->
                match
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (5UL, 1.0, 0L)) tick clock deadlines
                with
                | None -> true
                | Some target -> List.contains target deadlines
            )

        Check.One (propertyConfig, Prop.forAll clockAndDeadlines property)

    [<Test>]
    let ``the overshoot spans its whole bound, endpoints included`` () : unit =
        // A bound is only useful if the draw reaches it. Both endpoints matter
        // and for different reasons: never reaching 0 would mean no jitter run
        // ever fires a timeout punctually, and never reaching the bound would
        // make the knob quietly weaker than it says — which is exactly the
        // failure that hides the elapsed-time bugs this parameter exists for.
        //
        // One deadline, so the target *is* the overshoot plus a constant.
        let bound = 4L

        let overshoots =
            [ 0L .. 999L ]
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (8UL, 1.0, bound)) tick 0L [ 100L ]
                |> Option.map (fun target -> target - 100L)
            )
            |> Set.ofList

        overshoots |> shouldEqual (Set.ofList [ 0L .. bound ])

    [<Test>]
    let ``a bound of one is not silently a bound of zero`` () : unit =
        // The `+ 1L` in the draw is what makes the bound inclusive. Without it,
        // `draw * 1.0` floors to zero for every tick and the smallest non-zero
        // bound a caller can ask for does nothing at all.
        let overshoots =
            [ 0L .. 99L ]
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (13UL, 1.0, 1L)) tick 0L [ 100L ]
                |> Option.map (fun target -> target - 100L)
            )
            |> Set.ofList

        overshoots |> shouldEqual (Set.ofList [ 0L ; 1L ])

    [<Test>]
    let ``the overshoot spans its bound at a low probability too`` () : unit =
        // The overshoot must be drawn independently of the coin that decides
        // whether to fire. Sharing entropy between them would bias it: a tick
        // only fires when its coin draw came in *below* the probability, so at
        // 0.01 every jump would carry a tiny overshoot and the bound would be
        // decorative — which is precisely the regime the feature is meant to be
        // used in. Contrast the 1.0 case above, where every draw fires and the
        // bias is invisible.
        let bound = 4L

        let overshoots =
            [ 0L .. 99_999L ]
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (8UL, 0.01, bound)) tick 0L [ 100L ]
                |> Option.map (fun target -> target - 100L)
            )
            |> Set.ofList

        overshoots |> shouldEqual (Set.ofList [ 0L .. bound ])

    [<Test>]
    let ``every deadline is reachable with every overshoot`` () : unit =
        // The two draws must be independent of *each other*, not merely each
        // uniform on its own. Sharing entropy between them ties the two
        // together — the nearest deadline would always come with the smallest
        // overshoot and the furthest with the largest — so half the
        // (deadline, lateness) grid becomes unreachable while both marginal
        // distributions still look perfectly correct.
        //
        // The candidates are spaced wider than the bound so the deadline is
        // recoverable from the target by rounding down.
        let candidates = [ 100L ; 200L ]
        let bound = 3L

        let observed =
            [ 0L .. 999L ]
            |> List.choose (fun tick ->
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (44UL, 1.0, bound)) tick 0L candidates
                |> Option.map (fun target ->
                    let deadline = candidates |> List.filter (fun d -> d <= target) |> List.max
                    deadline, target - deadline
                )
            )
            |> Set.ofList

        let expected =
            Set.ofList
                [
                    for d in candidates do
                        for o in 0L .. bound -> d, o
                ]

        observed |> shouldEqual expected

    [<Test>]
    let ``the overshoot does not disturb which deadline was chosen`` () : unit =
        // The two draws are independent, so raising the bound must not resample
        // the deadline: a shrinker that lowers the bound to find the smallest
        // overshoot that still reproduces a failure needs the rest of the jump
        // sequence to hold still while it does.
        let candidates = [ 10L ; 20L ; 30L ; 40L ]

        let deadlinesChosen (bound : int64) : int64 list =
            [ 0L .. 199L ]
            |> List.map (fun tick ->
                match
                    ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (21UL, 0.5, bound)) tick 0L candidates
                with
                | None -> -1L
                // Recovering the deadline by rounding down to a candidate is
                // sound here only because the candidates are further apart than
                // the bounds under test.
                | Some target -> candidates |> List.filter (fun d -> d <= target) |> List.max
            )

        let reference = deadlinesChosen 0L

        for bound in [ 1L ; 3L ; 5L ] do
            deadlinesChosen bound |> shouldEqual reference

    [<Test>]
    let ``a malformed overshoot bound is rejected`` () : unit =
        // Negative would move the clock backwards from the deadline, which the
        // clock writer would reject anyway — but as a confusing fault at some
        // later tick rather than as a statement about the configuration. Beyond
        // the clock's range is the typo case (ticks mistaken for milliseconds,
        // say), and it is also what keeps the inclusive `+ 1L` from overflowing.
        for bad in [ -1L ; System.Int64.MinValue ; System.Int64.MaxValue ] do
            let choose () =
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (1UL, 1.0, bad)) 0L 0L [ 10L ]
                |> ignore<int64 option>

            Assert.Throws<Exception> (TestDelegate choose) |> ignore<Exception>

            let install () =
                EmulatedKernel.initial
                |> EmulatedKernel.withClockJitter (ClockJitterStrategy.EagerDeadlines (1UL, 1.0, bad))
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate install) |> ignore<Exception>

        // One below is legal: the rejection is about the inclusive bound's
        // arithmetic, not about the value being implausibly large. Whether a
        // large bound runs the clock off its representable range is the clock
        // writer's question, and it faults there.
        EmulatedKernel.initial
        |> EmulatedKernel.withClockJitter (ClockJitterStrategy.EagerDeadlines (1UL, 1.0, System.Int64.MaxValue - 1L))
        |> ignore<EmulatedKernel>

    [<Test>]
    let ``a malformed probability is rejected`` () : unit =
        // A NaN probability silently never fires (every comparison against NaN
        // is false), so a host that computed one would get a run that looked
        // jittered and was not. Out-of-range values are the same mistake in a
        // form the type system also cannot catch.
        for bad in [ nan ; -0.000001 ; 1.000001 ; infinity ; -infinity ] do
            let choose () =
                ClockJitter.chooseJump (ClockJitterStrategy.EagerDeadlines (1UL, bad, 0L)) 0L 0L [ 10L ]
                |> ignore<int64 option>

            Assert.Throws<Exception> (TestDelegate choose) |> ignore<Exception>

            // Rejected at install time too, so a misconfigured host finds out
            // before any guest code runs rather than at whichever tick first
            // consults the strategy.
            let install () =
                EmulatedKernel.initial
                |> EmulatedKernel.withClockJitter (ClockJitterStrategy.EagerDeadlines (1UL, bad, 0L))
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate install) |> ignore<Exception>

    // ------------------------------------------------------------------
    // Scripted
    // ------------------------------------------------------------------

    [<Test>]
    let ``a script fires only at the ticks it names`` () : unit =
        let script = ClockJitterStrategy.Scripted [ 5L, 1_000L ; 9L, 2_000L ]

        ClockJitter.chooseJump script 4L 0L [ 50L ] |> shouldEqual None
        ClockJitter.chooseJump script 5L 0L [ 50L ] |> shouldEqual (Some 1_000L)
        ClockJitter.chooseJump script 6L 0L [ 50L ] |> shouldEqual None
        ClockJitter.chooseJump script 9L 0L [ 50L ] |> shouldEqual (Some 2_000L)

    [<Test>]
    let ``a scripted target need not be a deadline anything is waiting on`` () : unit =
        // Deliberate: it is what keeps shrinking well-behaved. Drop an early
        // jump from a recorded script and the later ones must still mean what
        // they meant, which they could not if each had to remain a live
        // deadline in a run whose earlier history just changed.
        ClockJitter.chooseJump (ClockJitterStrategy.Scripted [ 3L, 777L ]) 3L 0L []
        |> shouldEqual (Some 777L)

    [<Test>]
    let ``several jumps at one tick collapse to the furthest`` () : unit =
        // The clock is monotonic, so applying them in turn is observationally
        // the same as applying the largest — and taking the max means a script
        // means the same thing however its entries are ordered.
        let script = ClockJitterStrategy.Scripted [ 2L, 500L ; 2L, 900L ; 2L, 700L ]

        ClockJitter.chooseJump script 2L 0L [] |> shouldEqual (Some 900L)

        let reordered = ClockJitterStrategy.Scripted [ 2L, 900L ; 2L, 700L ; 2L, 500L ]

        ClockJitter.chooseJump reordered 2L 0L [] |> shouldEqual (Some 900L)

    [<Test>]
    let ``a script that has drifted behind the clock fails loudly`` () : unit =
        // Silently skipping would let a script go on "replaying" a run it no
        // longer describes, which is the failure mode that makes a recorded
        // repro untrustworthy. The same reasoning as a `SpuriousWakeupStrategy`
        // script naming a waiter that is no longer queued.
        for target in [ 400L ; 500L ] do
            let choose () =
                ClockJitter.chooseJump (ClockJitterStrategy.Scripted [ 1L, target ]) 1L 500L []
                |> ignore<int64 option>

            Assert.Throws<Exception> (TestDelegate choose) |> ignore<Exception>

        // One tick further on is fine: the check is against the clock, not
        // against whether the script looks tidy.
        ClockJitter.chooseJump (ClockJitterStrategy.Scripted [ 1L, 501L ]) 1L 500L []
        |> shouldEqual (Some 501L)

    [<Test>]
    let ``a stale entry is caught even when a live one shares its tick`` () : unit =
        // The entries at a tick collapse to the furthest, so checking only the
        // winner would let a drifted entry ride along beside a valid one --
        // and a script that has half fallen behind the run is exactly as
        // untrustworthy as one that has wholly fallen behind it. Asserted in
        // both orders, since the stale entry is the smaller and a check that
        // looked at only the first or only the last would pass one of them.
        for script in [ [ 1L, 500L ; 1L, 700L ] ; [ 1L, 700L ; 1L, 500L ] ] do
            let choose () =
                ClockJitter.chooseJump (ClockJitterStrategy.Scripted script) 1L 600L []
                |> ignore<int64 option>

            Assert.Throws<Exception> (TestDelegate choose) |> ignore<Exception>

    [<Test>]
    let ``an EagerDeadlines run is replayable as a script`` () : unit =
        // The recording half of the shrinking story: the jumps a seeded run
        // makes are exactly a `Scripted` program, so a harness can capture one
        // and then remove entries from it.
        let candidates = [ 100L ; 250L ; 400L ]
        let strategy = ClockJitterStrategy.EagerDeadlines (77UL, 0.4, 0L)
        let ticks = [ 0L .. 499L ]

        let recorded =
            ticks
            |> List.choose (fun tick ->
                ClockJitter.chooseJump strategy tick 0L candidates
                |> Option.map (fun target -> tick, target)
            )

        // The recording is not vacuous: a script of no jumps would replay
        // trivially and prove nothing.
        recorded |> List.isEmpty |> shouldEqual false

        let script = ClockJitterStrategy.Scripted recorded

        for tick in ticks do
            ClockJitter.chooseJump script tick 0L candidates
            |> shouldEqual (ClockJitter.chooseJump strategy tick 0L candidates)

    // ------------------------------------------------------------------
    // Configuration
    // ------------------------------------------------------------------

    [<Test>]
    let ``jitter is off by default and installed through KernelConfig`` () : unit =
        // It has to be `KernelConfig` rather than a record-copy onto
        // `PreparedProgram.State`: class initialisers run during `prepare`, and
        // a `.cctor` that waits with a timeout is exactly the shape this
        // strategy exists to test.
        KernelConfig.Default.ClockJitter |> shouldEqual ClockJitterStrategy.Disabled
        EmulatedKernel.initial.ClockJitter |> shouldEqual ClockJitterStrategy.Disabled

        let strategy = ClockJitterStrategy.EagerDeadlines (42UL, 0.25, 0L)

        let configured =
            EmulatedKernel.initial
            |> KernelConfig.applyTo
                { KernelConfig.Default with
                    ClockJitter = strategy
                }

        configured.ClockJitter |> shouldEqual strategy

    // ------------------------------------------------------------------
    // End-to-end: the driver consults the strategy
    // ------------------------------------------------------------------

    /// A guest whose answer depends on whether a timeout expires. The worker
    /// finishes after a few thousand interpreted instructions, so at the
    /// default rate of 100 ns each it is comfortably inside the 500 ms window
    /// and `Join` reports success. The only way to reach the sentinel is for
    /// the clock to arrive at the deadline early.
    let private joinTimeoutSource : string =
        """
using System.Threading;

public static class Entry
{
    public static int Main()
    {
        var worker = new Thread(() =>
        {
            for (int i = 0; i < 2000; i++)
            {
                Thread.SpinWait(1);
            }
        });

        worker.Start();
        bool finished = worker.Join(500);
        worker.Join();
        return finished ? 0 : 17;
    }
}
"""

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// Run `joinTimeoutSource` under the given jitter strategy, returning the
    /// exit code and the state the guest left behind.
    let private runWithJitter (image : byte[]) (strategy : ClockJitterStrategy) : int * IlMachineState =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                ClockJitter = strategy
                            }
                    }
            }

        match BoundedRun.run loggerFactory "ClockJitterJoin" (Some "ClockJitterJoin.cs") peImage hostConfig with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> code, state
            | stack -> failwith $"expected an int exit code on the stack, got %A{stack}"
        | other -> failwith $"guest did not exit normally: %O{other}"

    [<Test>]
    let ``a disabled run and a zero-probability run are the same run`` () : unit =
        // The no-regression claim. `EagerDeadlines` at probability zero still
        // walks the candidate list on every tick, so this also asserts that
        // merely *looking* at the deadlines perturbs nothing.
        let image = Roslyn.compile [ joinTimeoutSource ]

        let disabledCode, disabledState = runWithJitter image ClockJitterStrategy.Disabled

        let zeroCode, zeroState =
            runWithJitter image (ClockJitterStrategy.EagerDeadlines (12345UL, 0.0, 0L))

        disabledCode |> shouldEqual 0
        zeroCode |> shouldEqual 0

        // The clocks and the step counters agreeing is the strong form: the two
        // runs retired the same instructions at the same simulated instants.
        zeroState.Kernel.StepCounter |> shouldEqual disabledState.Kernel.StepCounter

        zeroState.Kernel.VirtualClockTicks
        |> shouldEqual disabledState.Kernel.VirtualClockTicks

    [<Test>]
    let ``jitter drives the guest onto a timeout it would never otherwise reach`` () : unit =
        // Without this the feature is untestable from outside: every property
        // above would hold of a `chooseJump` the driver never called.
        let image = Roslyn.compile [ joinTimeoutSource ]

        let unjittered, _ = runWithJitter image ClockJitterStrategy.Disabled
        unjittered |> shouldEqual 0

        let jittered, jitteredState =
            runWithJitter image (ClockJitterStrategy.EagerDeadlines (1UL, 1.0, 0L))

        // 17 is the guest's "Join(500) reported failure" sentinel.
        jittered |> shouldEqual 17

        // And the clock really did leap rather than the guest merely running
        // long enough: 500 ms is 5,000,000 ticks, which at one tick per retired
        // instruction the guest could not have reached by executing.
        jitteredState.Kernel.VirtualClockTicks |> shouldBeGreaterThan 5_000_000L
        jitteredState.Kernel.StepCounter |> shouldBeSmallerThan 5_000_000L

    [<Test>]
    let ``a script is keyed on the same tick numbering the wakeup strategies use`` () : unit =
        // The driver's preamble applies the two spurious-wakeup strategies at
        // the *pre*-increment `StepCounter` and then advances it, so jitter must
        // read the same value or "tick N" would name two different moments
        // depending on which fuzz dial a caller was scripting against.
        //
        // Tick zero is the discriminating case: it is the first preamble's
        // number under that convention and belongs to no tick at all under the
        // other, so a jump scheduled there fires only if the convention holds.
        let image = Roslyn.compile [ joinTimeoutSource ]

        let target = 6_000_000L

        let code, state = runWithJitter image (ClockJitterStrategy.Scripted [ 0L, target ])

        // The jump lands before the guest ever calls `Join`, so its 500 ms
        // window is measured from the new clock and still succeeds; the point
        // here is the clock, not the exit code.
        code |> shouldEqual 0
        state.Kernel.VirtualClockTicks |> shouldBeGreaterThan target

    [<Test>]
    let ``one jitter seed reproduces its run exactly`` () : unit =
        // Determinism under replay is the property the whole interpreter
        // exists to provide, and a fuzzing dial that broke it would be worse
        // than no dial at all.
        let image = Roslyn.compile [ joinTimeoutSource ]
        let strategy = ClockJitterStrategy.EagerDeadlines (98765UL, 0.5, 0L)

        let firstCode, firstState = runWithJitter image strategy
        let secondCode, secondState = runWithJitter image strategy

        secondCode |> shouldEqual firstCode
        secondState.Kernel.StepCounter |> shouldEqual firstState.Kernel.StepCounter

        secondState.Kernel.VirtualClockTicks
        |> shouldEqual firstState.Kernel.VirtualClockTicks
