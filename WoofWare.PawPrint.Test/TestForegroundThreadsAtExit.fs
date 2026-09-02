namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The process ends when the last foreground thread does, not when `Main` returns: after
/// `Main`, CoreCLR's `RunMainPost` blocks the entry thread in `WaitForOtherThreads` until
/// every other foreground thread has finished. These tests pin the two halves of that which
/// the differential cases in `sourcesPure` cannot: what a foreground worker does *after*
/// `Main` has returned, which the exit code cannot see, and what happens when such a worker
/// never finishes, which on real .NET is a hang and so has no oracle.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestForegroundThreadsAtExit =

    let private assy = typeof<RunResult>.Assembly

    let private runSource (name : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)
        BoundedRun.run loggerFactory name (Some name) peImage (HostConfig.Default dotnetRuntimes)

    let private stdoutOf (state : IlMachineState) : string =
        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
        |> Seq.toArray
        |> Encoding.UTF8.GetString

    let private exitCodeOf (state : IlMachineState) (thread : ThreadId) : int =
        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
        | [] -> failwith "expected Main to leave an int on the entry thread's eval stack, but it left nothing"
        | other :: _ -> failwith $"expected Main to leave an int on the entry thread's eval stack, got %O{other}"

    /// The guest from the issue: measured on real .NET 10, this prints `worker ran` and exits 3.
    [<Test>]
    let ``a foreground worker still runs after Main has returned, and Main's exit code survives it`` () : unit =
        let source =
            """
using System;
using System.Threading;

class WorkerAfterMain
{
    static int Main()
    {
        new Thread(() => Console.WriteLine("worker ran")).Start();
        return 3;
    }
}
"""

        match runSource "WorkerAfterMain.cs" source with
        | RunOutcome.NormalExit (state, entryThread) ->
            stdoutOf state |> shouldEqual "worker ran\n"
            exitCodeOf state entryThread |> shouldEqual 3

            // The worker really finished, rather than being left runnable at the end of the
            // run; and the entry thread is what carried the exit code, not the worker.
            let workers =
                state.ThreadState
                |> Map.toList
                |> List.filter (fun (id, ts) -> id <> entryThread && not (ThreadStatus.hasNoActiveFrame ts.Status))

            workers
            |> List.map (fun (_, ts) -> ts.Status)
            |> shouldEqual [ ThreadStatus.Terminated ]
        | other -> failwith $"expected a normal exit, got %O{other}"

    /// On real .NET this guest hangs: the entry thread waits for the worker, and the worker
    /// never finishes. The interpreter's answer to a hang is `Deadlocked`, which `BoundedRun`
    /// reports as a failure naming the threads; a `NormalExit` here would be the interpreter
    /// silently dropping a thread the real runtime waits for.
    [<Test>]
    let ``a foreground worker that never finishes keeps the process alive: reported as a deadlock`` () : unit =
        let source =
            """
using System.Threading;

class ForegroundWorkerStuck
{
    static int Main()
    {
        new Thread(() => Thread.Sleep(Timeout.Infinite)).Start();
        return 3;
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource "ForegroundWorkerStuck.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "ForegroundWorkerStuck.cs"
        exn.Message |> shouldContainText "deadlocked"
        // The thread summary says what the entry thread is doing, so a reader can tell this
        // deadlock (Main is done and is waiting on the worker) from one inside Main.
        exn.Message |> shouldContainText "WaitingForForegroundThreads"
        exn.Message |> shouldContainText "BlockedOnSleep"

    /// The entry thread is not dead after `Main` returns — it is waiting — so a `Join` on it
    /// does not return. With a foreground joiner that is a hang on real .NET (measured: still
    /// running after five minutes), and a deadlock here. `BackgroundWorkerJoinsMainThread.cs`
    /// in `sourcesPure` is the other half: a background joiner is simply outlived.
    [<Test>]
    let ``a foreground worker joining the entry thread after Main returns is a deadlock`` () : unit =
        let source =
            """
using System.Threading;

class ForegroundWorkerJoinsMain
{
    static int Main()
    {
        Thread main = Thread.CurrentThread;
        new Thread(() => main.Join()).Start();
        return 3;
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource "ForegroundWorkerJoinsMain.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "ForegroundWorkerJoinsMain.cs"
        exn.Message |> shouldContainText "deadlocked"
        exn.Message |> shouldContainText "WaitingForForegroundThreads"
        exn.Message |> shouldContainText "BlockedOnJoin"

    /// The waiting entry thread is background — `WaitForOtherThreads` makes it so — and a worker
    /// may set that back. Real .NET then counts the entry thread as a foreground thread again
    /// and never exits (measured: still running three seconds after the worker finished, with
    /// nothing left to run). Here that is a deadlock in which the only thread holding the
    /// process open is the entry thread itself.
    [<Test>]
    let ``a worker re-foregrounding the entry thread after Main returns makes the process wait for ever`` () : unit =
        let source =
            """
using System.Threading;

class ReforegroundsMain
{
    static int Main()
    {
        Thread main = Thread.CurrentThread;
        new Thread(() =>
        {
            Thread.Sleep(200);
            main.IsBackground = false;
        }).Start();
        return 3;
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource "ReforegroundsMain.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "ReforegroundsMain.cs"
        exn.Message |> shouldContainText "deadlocked"
        exn.Message |> shouldContainText "WaitingForForegroundThreads"
        // The worker itself finished; it is the entry thread that is being waited for.
        exn.Message |> shouldNotContainText "BlockedOnSleep"

    /// The other side of `MainBackgroundBeforeStartingForegroundWorker.cs`: with the worker
    /// started *before* `Main` goes background, there is never a moment with no foreground
    /// thread alive, so CoreCLR's termination event is never set and the process waits for the
    /// worker. On real .NET that is a hang (measured: still running after three seconds); here
    /// it is a deadlock. This is what keeps the latch honest — going background is not by
    /// itself a shutdown signal.
    [<Test>]
    let ``Main going background after a foreground worker exists does not abandon the worker`` () : unit =
        let source =
            """
using System.Threading;

class WorkerThenMainBackground
{
    static int Main()
    {
        new Thread(() => Thread.Sleep(Timeout.Infinite)).Start();
        Thread.CurrentThread.IsBackground = true;
        return 3;
    }
}
"""

        let exn =
            Assert.Throws (fun () -> runSource "WorkerThenMainBackground.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "WorkerThenMainBackground.cs"
        exn.Message |> shouldContainText "deadlocked"
        exn.Message |> shouldContainText "WaitingForForegroundThreads"
        exn.Message |> shouldContainText "BlockedOnSleep"

    /// A worker that is running when `Main` returns finishes on its own and the process then
    /// ends with `Main`'s exit code; nothing the worker did in the meantime is lost. The
    /// worker here does enough after `Main` returns for the scheduler to interleave the two,
    /// and its output is the record of what it did.
    [<Test>]
    let ``a foreground worker's output after Main returns is all delivered`` () : unit =
        let source =
            """
using System;
using System.Threading;

class WorkerPrintsAfterMain
{
    static int Main()
    {
        new Thread(() =>
        {
            for (int i = 0; i < 5; i++)
            {
                Console.WriteLine("line " + i);
            }
        }).Start();
        return 6;
    }
}
"""

        match runSource "WorkerPrintsAfterMain.cs" source with
        | RunOutcome.NormalExit (state, entryThread) ->
            stdoutOf state |> shouldEqual "line 0\nline 1\nline 2\nline 3\nline 4\n"
            exitCodeOf state entryThread |> shouldEqual 6
        | other -> failwith $"expected a normal exit, got %O{other}"
