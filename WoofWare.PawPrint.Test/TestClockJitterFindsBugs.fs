namespace WoofWare.PawPrint.Test

open System.Collections.Concurrent
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The claim `ClockJitterStrategy` exists to support, stated as a test:
/// there are bugs that exploring the *schedule* space cannot reach, because
/// what has to go wrong is the passage of time rather than the order of two
/// threads.
///
/// `JoinTimeoutIgnored.cs` is one. Its worker fills a shared buffer and `Main`
/// joins it with a generous timeout, throwing away the bool that says whether
/// the worker actually finished. Whenever the join *succeeds* the worker has
/// terminated, so the buffer is complete however the two threads interleaved —
/// which is why the PCT sweep below never finds anything, and why it is here:
/// without that half, "jitter found a bug" would not distinguish a bug jitter
/// was needed for from one any seed would have found.
///
/// This also pins what jitter can and cannot do. `EagerDeadlines` only ever
/// moves the clock onto a deadline that is already outstanding, so it makes a
/// timeout arm *reachable* — it cannot make more time elapse than a guest asked
/// for. Bugs of the second kind (a lease that expires mid-computation) need a
/// strategy that advances time with no deadline pending, which does not exist
/// yet.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClockJitterFindsBugs =

    /// The guest's "I read a half-built buffer" sentinel.
    let private sentinel : int = 42

    let private sourceName : string = "JoinTimeoutIgnored.cs"

    let private assy = typeof<RunResult>.Assembly

    let private dotnetRuntimes : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    let private image : byte[] =
        Roslyn.compile [ Assembly.getEmbeddedResourceAsString sourceName assy ]

    /// How a run ended, reduced to what this fixture distinguishes. Anything
    /// other than a normal exit is a broken scenario rather than a result, so
    /// the arms carry enough to say what went wrong.
    [<RequireQualifiedAccess>]
    type private Ending =
        | ExitCode of int
        | Deadlock of stuck : string
        | Other of string

    let private endingOfOutcome (outcome : RunOutcome) : Ending =
        match outcome with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> Ending.ExitCode code
            | stack -> Ending.Other $"non-int return: %A{stack}"
        | RunOutcome.GuestUnhandledException _ -> Ending.Other "unhandled guest exception"
        | RunOutcome.FailFast (_, _, message) -> Ending.Other $"FailFast: %A{message}"
        | RunOutcome.SignalTerminated (_, signal) -> Ending.Other $"signalled: %O{signal}"

    /// Run the guest once under a given jitter strategy and PCT seed, from
    /// scratch. Used for the jittered runs: the fork-prefix shortcut below
    /// shares a prefix across seeds, and a prefix computed under one jitter
    /// strategy is not shared with any other.
    let private runFromScratch (jitter : ClockJitterStrategy) (pctSeed : uint64 option) : Ending =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "jitter", sprintf "%O" jitter ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                PctSeed = pctSeed
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                ClockJitter = jitter
                            }
                    }
            }

        BoundedRun.run loggerFactory sourceName (Some sourceName) peImage hostConfig
        |> endingOfOutcome

    // ------------------------------------------------------------------
    // The negative half: no schedule finds it
    // ------------------------------------------------------------------

    /// Wide enough to be a real claim about the schedule space rather than a
    /// spot check. The sweep shares one fork prefix, so the cost is dominated
    /// by the per-seed suffix.
    let private pctSeeds : uint64 list = [ 0UL .. 511UL ]

    [<Test>]
    let ``no schedule reaches the bug without jitter`` () : unit =
        // The load-bearing half of this fixture. Whenever `Join(200)` returns
        // having *succeeded*, the worker has terminated and the buffer is
        // complete, so no interleaving of the two threads can expose a partial
        // one — and at the default pace of one 100 ns tick per retired
        // instruction, 200 ms is two million instructions, which the worker
        // does not come close to needing. So this is not "PCT got unlucky 512
        // times": there is no schedule to find.
        let _messages, prefixLoggerFactory = LoggerFactory.makeTest ()
        use _prefixLoggerFactoryResource = prefixLoggerFactory

        use peImage = new MemoryStream (image)

        let snapshot =
            match
                Program.runToFirstFork
                    prefixLoggerFactory
                    (Some sourceName)
                    peImage
                    (GuestConfig.Default dotnetRuntimes)
            with
            | Program.PrefixOutcome.ForkedAt snapshot -> snapshot
            | other ->
                failwith
                    $"%s{sourceName} was expected to reach a fork point (it starts a worker and joins it), but: %A{other}"

        let observed = ConcurrentBag<uint64 * Ending> ()

        let runSeed (seed : uint64) : unit =
            let _messages, loggerFactory =
                LoggerFactory.makeTestWithProperties [ "source_file", sourceName ; "pct_seed", string seed ]

            use _loggerFactoryResource = loggerFactory
            let logger = loggerFactory.CreateLogger "TestClockJitterFindsBugs"

            let rec loop (prepared : Program.PreparedProgram) : Ending =
                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed outcome -> endingOfOutcome outcome
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> Ending.Deadlock stuck
                | Program.ProgramStepOutcome.InstructionStepped (p, _, _, _) -> loop p
                | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p

            observed.Add (seed, loop (Program.resumeFork loggerFactory (Some seed) snapshot))

        pctSeeds |> Array.ofList |> Array.Parallel.iter runSeed

        let notClean =
            observed
            |> Seq.filter (fun (_, ending) -> ending <> Ending.ExitCode 0)
            |> Seq.toList

        if not (List.isEmpty notClean) then
            failwith
                $"%s{sourceName} was supposed to be unreachable by schedule exploration alone, but %d{List.length notClean} of %d{List.length pctSeeds} PCT seeds did not exit 0: %A{notClean |> List.truncate 5}. If this is a genuine interleaving, the guest no longer isolates the passage of time and the positive test below proves less than it claims."

        observed.Count |> shouldEqual (List.length pctSeeds)

    // ------------------------------------------------------------------
    // The positive half: jitter reaches it
    // ------------------------------------------------------------------

    [<Test>]
    let ``jitter reaches the bug`` () : unit =
        // At probability 1.0 the first tick that sees the join deadline
        // outstanding jumps straight to it, so the worker has barely started
        // and `Main` reads a buffer of zeros.
        runFromScratch (ClockJitterStrategy.EagerDeadlines (1UL, 1.0)) None
        |> shouldEqual (Ending.ExitCode sentinel)

    [<Test>]
    let ``a small jitter probability still finds the bug within a seed sweep`` () : unit =
        // Probability 1.0 is a demonstration, not a usable setting: it expires
        // every wait the instant it is posted, which explores one timing rather
        // than many. The realistic use is a low probability swept over seeds,
        // so assert that shape works too — otherwise the feature would be
        // pinned only at a setting nobody would run.
        let findings =
            [ 0UL .. 31UL ]
            |> List.map (fun seed -> seed, runFromScratch (ClockJitterStrategy.EagerDeadlines (seed, 0.01)) None)
            |> List.filter (fun (_, ending) -> ending = Ending.ExitCode sentinel)

        if List.isEmpty findings then
            failwith
                $"%s{sourceName}: no jitter seed in 0..31 at probability 0.01 reached the bug. The strategy is meant to be usable at a low probability, not only at 1.0."

    [<Test>]
    let ``an explicit script reaches the bug without any seed`` () : unit =
        // `Scripted` drives the same bug with no randomness anywhere, which is
        // the property a recorded repro needs: it survives without the seed
        // that produced it.
        //
        // A ladder of jumps rather than one, because a single entry would have
        // to name a tick after `Main` has called `Join` — and that tick is a
        // function of how many instructions startup happens to retire today, so
        // pinning it would make this test fail confusingly the first time
        // startup got cheaper. Each rung is further out than the last, so
        // whichever rung first lands after the join crosses its deadline;
        // targets must increase, since a target behind the clock is a drifted
        // script and fails loudly by design.
        let ladder =
            [ 1L .. 40L ]
            |> List.map (fun rung -> rung * 2_000L, rung * 400L * EmulatedKernel.ticksPerMillisecond)

        runFromScratch (ClockJitterStrategy.Scripted ladder) None
        |> shouldEqual (Ending.ExitCode sentinel)

        // Recording the jumps a *seeded* run made, so a shrinker could start
        // deleting them, needs the driver to report which jumps it took —
        // observability that does not exist yet. `chooseJump`'s own tests cover
        // the recording being faithful; this covers the script reaching the
        // guest at all.
        ()
