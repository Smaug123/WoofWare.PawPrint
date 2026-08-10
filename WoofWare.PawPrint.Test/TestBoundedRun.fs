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
