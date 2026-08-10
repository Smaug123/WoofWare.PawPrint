namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The end-to-end harness runs guests in-process, so a guest that never terminates does not
/// fail its test — it wedges the whole suite, and CI reports a timeout with nothing in it.
/// `BoundedRun` is the guard against that, and a guard nothing exercises is a guard you find
/// out about the day it does not work.
///
/// The bound is a step count rather than a wall clock, so these tests can assert on it exactly:
/// a livelocked guest fails at a *known* number of steps on every machine. That is the property
/// a timeout could not have given, and it is why the bound is shaped this way.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBoundedRun =

    let private assy = typeof<RunResult>.Assembly

    let private runSource (maxSteps : int64) (name : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)
        BoundedRun.runWith loggerFactory maxSteps name (Some name) peImage (HostConfig.Default dotnetRuntimes)

    /// The guard fires, and says enough to diagnose the guest rather than merely that it gave
    /// up: the budget it was given, and what each live thread was executing.
    [<Test>]
    let ``a guest that never terminates fails instead of running for ever`` () : unit =
        let source =
            """
class SpinsForEver
{
    static int Main(string[] args)
    {
        // No BCL involved: this is a bare IL loop, so what the harness is bounding is
        // unambiguously the interpreter stepping guest instructions.
        while (true) { }
    }
}
"""

        // Small enough to trip in well under a second, which is the point of the bound being
        // a parameter rather than a constant.
        let exn =
            Assert.Throws (fun () -> runSource 50_000L "SpinsForEver.cs" source |> ignore<RunOutcome>)

        let message = exn.Message

        message |> shouldContainText "SpinsForEver.cs"
        message |> shouldContainText "50000"
        message |> shouldContainText "did not terminate"
        // The thread summary is what makes the failure actionable.
        message |> shouldContainText "Main"

    /// A deadlocked guest is a different shape — the scheduler detects it and no step is
    /// possible — and must also be a test failure rather than an escaping host exception.
    [<Test>]
    let ``a deadlocked guest is reported, not raised out of the interpreter`` () : unit =
        let source =
            """
using System.Threading;

class Deadlocks
{
    static int Main(string[] args)
    {
        // Nothing will ever set this, and there is no other thread to do so.
        new ManualResetEventSlim(false).Wait();
        return 0;
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource BoundedRun.defaultMaxSteps "Deadlocks.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "Deadlocks.cs"
        exn.Message |> shouldContainText "deadlocked"

    /// The bound must not be reachable by ordinary work: a guest that terminates does so
    /// unaffected, and its outcome is exactly what an unbounded run would have produced.
    [<Test>]
    let ``a terminating guest is unaffected by the bound`` () : unit =
        let source =
            """
class Terminates
{
    static int Main(string[] args)
    {
        int total = 0;
        for (int i = 0; i < 1000; i++) { total += i; }
        return total == 499500 ? 0 : 1;
    }
}
"""

        match runSource BoundedRun.defaultMaxSteps "Terminates.cs" source with
        | RunOutcome.NormalExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
            | other -> failwith $"expected an int exit code, got %O{other}"
        | other -> failwith $"expected a normal exit, got %O{other}"

    let private spinsForEver =
        """
class SpinsForEver
{
    static int Main(string[] args)
    {
        while (true) { }
    }
}
"""

    let private budgetFailure (maxSteps : int64) : string =
        let exn =
            Assert.Throws (fun () -> runSource maxSteps "Determinism.cs" spinsForEver |> ignore<RunOutcome>)

        exn.Message

    /// The budget counts *steps*, not time, so the same guest gives up in the same machine
    /// state every run. Without that, the bound would be one more source of the flakiness this
    /// interpreter exists to remove — a busy CI machine could fail a guest that a quiet laptop
    /// passes, and the failure would not reproduce.
    ///
    /// This pairs with the test below, and neither is worth much alone. Equality here says the
    /// stopping point is reproducible; the test below says the message can actually tell
    /// stopping points apart, without which this one would hold for a wall-clock bound too.
    [<Test>]
    let ``the budget is deterministic: the same guest gives up in the same state every time`` () : unit =
        budgetFailure 50_000L |> shouldEqual (budgetFailure 50_000L)

    /// The diagnostic distinguishes *where* the guest was stopped, so the equality above is a
    /// claim about the machine state rather than about a message too coarse to disagree.
    ///
    /// One extra step of budget must move the stopping point, and does: the kernel's step
    /// counter differs, and for this guest — a bare loop of more than one instruction — so does
    /// the IL offset. A message reporting only the thread's status and method would be
    /// identical across both, and this is what would catch that.
    [<Test>]
    let ``the diagnostic distinguishes different stopping points`` () : unit =
        let atBudget = budgetFailure 50_000L
        let oneStepLater = budgetFailure 50_001L

        atBudget |> shouldNotEqual oneStepLater

        // Specifically: it is the stopping point that differs, not merely the budget echoed
        // back into the text.
        let stateOf (message : string) : string =
            match message.IndexOf "Threads: " with
            | -1 -> failwith $"diagnostic carried no thread summary, so it cannot locate the guest: %s{message}"
            | i -> message.Substring i

        stateOf atBudget |> shouldNotEqual (stateOf oneStepLater)

    /// The diagnostic must survive threads that have no active frame.
    ///
    /// `ThreadStatus.hasNoActiveFrame` names two such states, `NotStarted` and `Parked`, and
    /// `ThreadState.MethodState` throws on both — it resolves the active frame, and there
    /// isn't one. A summary that reached for the executing method unconditionally would
    /// therefore crash with "Frame ... is not live" *instead of* reporting the stuck guest,
    /// destroying the diagnostic exactly when it is needed. A guest holding a constructed but
    /// unstarted thread is an entirely ordinary way to be in that state.
    [<Test>]
    let ``the diagnostic survives threads with no active frame`` () : unit =
        let source =
            """
using System.Threading;

class SpinsWithUnstartedThread
{
    static int Main(string[] args)
    {
        // Constructed, never started: a machine thread in `NotStarted`, with no frame.
        Thread t = new Thread(() => { });
        while (true) { }
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource 200_000L "SpinsWithUnstartedThread.cs" source |> ignore<RunOutcome>)

        // The budget diagnostic, not a frame-resolution failure from building it.
        exn.Message |> shouldContainText "did not terminate"
        exn.Message |> shouldNotContainText "is not live"

    /// Guest code runs before `Main` — the entry type's static initialiser is pumped during
    /// startup — so startup can wedge in exactly the way `Main` can, and must be bounded in
    /// exactly the same way.
    ///
    /// This is the case the bound originally did not cover: startup ran to completion behind a
    /// single call, so a `.cctor` that never returned hung the suite with no diagnostic at all,
    /// which is precisely the failure mode `BoundedRun` exists to prevent.
    let private wedgesInStaticInitialiser =
        """
class WedgesInCctor
{
    static readonly int X;

    static WedgesInCctor()
    {
        // Runs during startup, before Main is ever installed.
        while (true) { }
    }

    static int Main(string[] args)
    {
        return X;
    }
}
"""

    [<Test>]
    let ``a guest that wedges in a static initialiser fails instead of running for ever`` () : unit =
        let exn =
            Assert.Throws (fun () ->
                runSource 50_000L "WedgesInCctor.cs" wedgesInStaticInitialiser
                |> ignore<RunOutcome>
            )

        let message = exn.Message

        message |> shouldContainText "WedgesInCctor.cs"
        message |> shouldContainText "50000"

        // Said to be *startup*, not Main. The distinction is the whole diagnostic: "your static
        // initialiser never returned" and "your Main never returned" are different bugs, and a
        // message that conflated them would send you looking in the wrong place.
        message |> shouldContainText "did not finish starting up"
        message |> shouldNotContainText "did not terminate"

        // And it locates the guest within startup rather than merely reporting that startup was
        // where we were: the thread summary names the initialiser itself.
        message |> shouldContainText ".cctor"

    /// Startup and `Main` share one budget: `Main` resumes the count startup left off at, rather
    /// than each phase getting its own allowance.
    ///
    /// The distinction is worth a test because the alternative silently doubles the worst-case
    /// bound, and because it is not observable from any single-phase guest — a guest that wedges
    /// in startup fails under either design. What separates them is a guest that *completes*:
    /// give it a budget a little short of its whole run, and only the shared design fails it.
    ///
    /// Calibrated against the guest's own measured cost rather than hardcoded numbers. The slack
    /// below must be smaller than startup, which is what makes the two designs disagree:
    ///
    ///   * shared:    the run needs `total` steps and is given `total - slack`, so it fails.
    ///   * per-phase: `Main` alone needs `total - startup` steps, and since `startup > slack`
    ///                that is *less* than `total - slack`, so it would pass.
    [<Test>]
    let ``startup and Main share one budget`` () : unit =
        let source =
            """
class CompletesButNotCheaply
{
    static int Main(string[] args)
    {
        int total = 0;
        for (int i = 0; i < 5000; i++) { total += i; }
        return total == 12497500 ? 0 : 1;
    }
}
"""

        // Measure the whole run, startup included: `StepCounter` is the kernel's own count of
        // retired steps, so this is the guest's cost rather than an assumption about it.
        let total =
            match runSource BoundedRun.defaultMaxSteps "CompletesButNotCheaply.cs" source with
            | RunOutcome.NormalExit (state, _) -> state.Kernel.StepCounter
            | other -> failwith $"expected a normal exit, got %O{other}"

        // The slack must be smaller than startup (~3,300 steps) for the two designs to disagree
        // at all; 1,000 leaves a wide margin either side of that.
        let slack = 1_000L

        // ...and the resulting budget must still be larger than startup, or the run would give
        // up before reaching `Main` and this would be testing the startup bound instead. Both
        // facts are asserted rather than assumed, so editing the guest above cannot quietly turn
        // this into a test of something else.
        (total - slack) |> shouldBeGreaterThan 5_000L

        let exn =
            Assert.Throws (fun () ->
                runSource (total - slack) "CompletesButNotCheaply.cs" source
                |> ignore<RunOutcome>
            )

        // It ran out in `Main`, not in startup — so startup did fit inside the budget, and what
        // exhausted it was the two phases' costs *added together*.
        exn.Message |> shouldContainText "did not terminate"
        exn.Message |> shouldNotContainText "did not finish starting up"

    /// A static initialiser can deadlock as easily as it can spin, and that reaches a different
    /// arm: the scheduler reports `Deadlocked` and no step is possible.
    ///
    /// `Program.prepare` raises on this itself, with a message that carries no thread summary
    /// and no guest identification — so driving startup step by step is also what makes this
    /// failure legible. The assertions below are on detail `prepare` does not produce.
    [<Test>]
    let ``a guest that deadlocks in a static initialiser is reported with the guest's state`` () : unit =
        let source =
            """
using System.Threading;

class DeadlocksInCctor
{
    static readonly int X;

    static DeadlocksInCctor()
    {
        // Nothing will ever set this, and there is no other thread to do so.
        new ManualResetEventSlim(false).Wait();
    }

    static int Main(string[] args)
    {
        return X;
    }
}
"""

        let exn =
            Assert.Throws (fun () ->
                runSource BoundedRun.defaultMaxSteps "DeadlocksInCctor.cs" source
                |> ignore<RunOutcome>
            )

        let message = exn.Message

        message |> shouldContainText "DeadlocksInCctor.cs"
        message |> shouldContainText "deadlocked"
        message |> shouldContainText "startup"
        // Detail `Program.prepare`'s own failure lacks: which threads, and what they were doing.
        message |> shouldContainText "Threads:"

    /// The budget is *exact*, in both phases: a guest given N steps retires N, not N+1.
    ///
    /// Observable because every `stepPrepared` call bumps the kernel's own step counter by one,
    /// and the diagnostic reports it — so "counter equals budget" says the harness counted every
    /// step it took. The off-by-one this guards against is not hypothetical: handing the step
    /// count across the startup-to-`Main` handoff unchanged drops the tick that retired the
    /// startup frame's final `ret`, and only the `Main`-phase half of this test can see it (no
    /// handoff happens when the guest never leaves startup).
    [<Test>]
    let ``the budget is exact: a guest given N steps retires N`` () : unit =
        let budget = 40_000L

        let mainPhase =
            Assert.Throws (fun () -> runSource budget "ExactMain.cs" spinsForEver |> ignore<RunOutcome>)

        mainPhase.Message |> shouldContainText $"kernel step counter %d{budget}"

        let startupPhase =
            Assert.Throws (fun () ->
                runSource budget "ExactStartup.cs" wedgesInStaticInitialiser
                |> ignore<RunOutcome>
            )

        startupPhase.Message |> shouldContainText $"kernel step counter %d{budget}"

    let private startupBudgetFailure (maxSteps : int64) : string =
        let exn =
            Assert.Throws (fun () ->
                runSource maxSteps "StartupDeterminism.cs" wedgesInStaticInitialiser
                |> ignore<RunOutcome>
            )

        exn.Message

    /// The startup diagnostic is subject to the same pair of claims as the `Main` one, and for
    /// the same reason: equality alone would hold for a message too coarse to distinguish
    /// anything, so it is only worth something alongside the discrimination test below.
    [<Test>]
    let ``the startup budget is deterministic: the same guest gives up in the same state every time`` () : unit =
        startupBudgetFailure 20_000L |> shouldEqual (startupBudgetFailure 20_000L)

    /// One extra step of startup budget must move the reported stopping point. Without this, the
    /// determinism test above would pass for a startup diagnostic that said nothing about where
    /// the guest was — which is exactly what a wall-clock bound would have given us.
    [<Test>]
    let ``the startup diagnostic distinguishes different stopping points`` () : unit =
        let atBudget = startupBudgetFailure 20_000L
        let oneStepLater = startupBudgetFailure 20_001L

        atBudget |> shouldNotEqual oneStepLater

        let stateOf (message : string) : string =
            match message.IndexOf "Threads: " with
            | -1 -> failwith $"diagnostic carried no thread summary, so it cannot locate the guest: %s{message}"
            | i -> message.Substring i

        stateOf atBudget |> shouldNotEqual (stateOf oneStepLater)
