namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PawPrint.Test.LinuxCoreLibFlavour

/// Several programs, each a process, on one machine (`MultiProgram`): their process IDs and
/// output are their own, the clock is the machine's, a deadlock is the machine's, one program's
/// end leaves the others running, the choice between them replays from its seed, and two of
/// them can connect to each other over loopback.
///
/// The guests write through a direct `SystemNative_Write` P/Invoke rather than `Console`, which
/// would take minutes of interpretation to set up.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it `Explicit`, and CI
// selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestSeveralPrograms =

    /// What every guest below shares: `Out.Write(long)` writes the eight little-endian bytes of
    /// its argument to standard output, and `Out.Text(string)` the string's ASCII bytes.
    let private prelude : string =
        """
using System;
using System.Runtime.InteropServices;

static class Out
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    public static unsafe void Write(long value)
    {
        byte* buffer = stackalloc byte[8];
        for (int i = 0; i < 8; i++)
        {
            buffer[i] = (byte)(value >> (8 * i));
        }
        SystemNative_Write((IntPtr)1, buffer, 8);
    }

    public static unsafe void Text(string text)
    {
        byte* buffer = stackalloc byte[text.Length];
        for (int i = 0; i < text.Length; i++)
        {
            buffer[i] = (byte)text[i];
        }
        SystemNative_Write((IntPtr)1, buffer, text.Length);
    }
}
"""

    /// The machine every test here runs on, unless it says otherwise: the default kernel's.
    let private machineConfig (choice : ProgramChoice) : MachineConfig =
        { fst (KernelConfig.split KernelConfig.Default) with
            ProgramChoice = choice
        }

    let private processConfig : ProcessConfig =
        snd (KernelConfig.split KernelConfig.Default)

    let private launchOn (runtimeDirs : ImmutableArray<string>) (name : string) (image : byte[]) : ProgramLaunch =
        {
            Image = new MemoryStream (image)
            OriginalPath = Some name
            DotnetRuntimeDirs = runtimeDirs
            Process = processConfig
            Argv = []
            AssemblyPath = None
            AppContext = AppContextProperties.empty
            PctSeed = None
        }

    let private launch (name : string) (image : byte[]) : ProgramLaunch =
        launchOn (FrameworkUnderTest.runtimeDirs ()) name image

    let private compile (source : string) : byte[] = Roslyn.compile [ prelude ; source ]

    /// One tick of a run: which program stepped, and what it did.
    type private Tick =
        {
            Program : ProcessId
            /// The global tick the step retired, from the program's kernel after the step.
            StepCounter : int64
            /// The thread that ran, for a tick that retired an instruction.
            Thread : ThreadId option
        }

    /// How a run of the driver ended, and every tick it took on the way.
    type private Run =
        {
            Ticks : Tick list
            /// Each program's end, in launch order, as `MultiStepOutcome.Finished` gave them; or
            /// where each program was stuck, if the run deadlocked.
            Result : Result<(ProcessId * RunEnd) list, (ProcessId * string) list>
            /// The order programs ended in, by `ProgramEvent.Ended`, the last program's end
            /// excluded.
            EndedInOrder : ProcessId list
        }

    /// Drive `driver` until it finishes or deadlocks, or until `maxTicks` ticks have passed.
    let private drive
        (loggerFactory : ILoggerFactory)
        (maxTicks : int)
        (driver : MultiProgram)
        : Run * MultiProgram option
        =
        let logger = loggerFactory.CreateLogger "TestSeveralPrograms"

        let rec go (driver : MultiProgram) (count : int) (ticks : Tick list) (ended : ProcessId list) =
            if count >= maxTicks then
                {
                    Ticks = List.rev ticks
                    Result = Ok []
                    EndedInOrder = List.rev ended
                },
                Some driver
            else

            match MultiProgram.step loggerFactory logger driver with
            | MultiStepOutcome.Stepped (next, pid, event) ->
                let thread, ended =
                    match event with
                    | ProgramEvent.InstructionStepped (thread, _, _) -> Some thread, ended
                    | ProgramEvent.WorkerTerminated thread -> Some thread, ended
                    | ProgramEvent.PhaseAdvanced -> None, ended
                    | ProgramEvent.Ended _ -> None, pid :: ended

                let tick =
                    {
                        Program = pid
                        StepCounter = next.Current.State.Kernel.StepCounter
                        Thread = thread
                    }

                go next (count + 1) (tick :: ticks) ended
            | MultiStepOutcome.Finished (ends, machine) ->
                SimulatedMachine.processIds machine |> shouldEqual Set.empty
                SimulatedMachine.checkInvariants machine |> shouldEqual []

                {
                    Ticks = List.rev ticks
                    Result = Ok ends
                    EndedInOrder = List.rev ended
                },
                None
            | MultiStepOutcome.Deadlocked (_, stuck) ->
                {
                    Ticks = List.rev ticks
                    Result = Error stuck
                    EndedInOrder = List.rev ended
                },
                None

        go driver 0 [] []

    let private finished (run : Run) : (ProcessId * RunEnd) list =
        match run.Result with
        | Ok ends -> ends
        | Error stuck -> failwith $"expected the programs to finish, but they deadlocked: %A{stuck}"

    let private outcomeOf (runEnd : RunEnd) : RunOutcome =
        match runEnd with
        | RunEnd.Ended outcome -> outcome

    let private stdoutOf (runEnd : RunEnd) : byte[] =
        (RunOutcome.state (outcomeOf runEnd)).Kernel.OutputLog
        |> OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput
        |> Seq.toArray

    let private exitCodeOf (runEnd : RunEnd) : int =
        match outcomeOf runEnd with
        | RunOutcome.NormalExit (state, _, _)
        | RunOutcome.ProcessExit (state, _, _) -> state.LatchedExitCode
        | other -> failwith $"expected the program to exit, got %O{other}"

    /// The `long`s a program wrote with `Out.Write`, in order.
    let private longsOf (runEnd : RunEnd) : int64 list =
        let bytes = stdoutOf runEnd

        if bytes.Length % 8 <> 0 then
            failwith $"expected whole longs on standard output, got %d{bytes.Length} bytes"

        [ for i in 0 .. bytes.Length / 8 - 1 -> BitConverter.ToInt64 (bytes, 8 * i) ]

    let private pidSource (label : string) : string =
        $$"""
public static class Program
{
    public static int Main()
    {
        Out.Text("{{label}}");
        Out.Write(System.Environment.ProcessId);
        return 0;
    }
}
"""

    [<Test>]
    let ``two guests on one machine report distinct process IDs and keep separate output logs`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let config = machineConfig ProgramChoice.RoundRobin

        let driver =
            MultiProgram.start
                loggerFactory
                config
                [
                    launch "First.cs" (compile (pidSource "first:"))
                    launch "Second.cs" (compile (pidSource "second:"))
                ]

        let run, _ = drive loggerFactory Int32.MaxValue driver

        match finished run with
        | [ (firstPid, first) ; (secondPid, second) ] ->
            // The first process has the configured ID, and the second the kernel's choice.
            firstPid |> shouldEqual config.ProcessId
            secondPid |> shouldNotEqual firstPid

            let expected (label : string) (pid : ProcessId) : byte[] =
                Array.append
                    (Text.Encoding.ASCII.GetBytes label)
                    (BitConverter.GetBytes (int64 (ProcessId.toInt32 pid)))

            stdoutOf first |> shouldEqual (expected "first:" firstPid)
            stdoutOf second |> shouldEqual (expected "second:" secondPid)
            exitCodeOf first |> shouldEqual 0
            exitCodeOf second |> shouldEqual 0
        | other -> failwith $"expected two programs' ends, got %A{other}"

        // The startups interleaved from the first tick.
        let programsByTick = run.Ticks |> List.map _.Program

        programsByTick
        |> List.truncate 2
        |> List.distinct
        |> List.length
        |> shouldEqual 2

    /// The sleeper measures, with the precision of `Stopwatch`, how long its `Thread.Sleep(50)`
    /// took; the spinner reads `Environment.TickCount64` for 100 ms and reports the largest gap
    /// between two consecutive readings.
    let private sleeperSource : string =
        """
using System.Diagnostics;
using System.Threading;

public static class Program
{
    public static int Main()
    {
        long started = Stopwatch.GetTimestamp();
        Thread.Sleep(50);
        Out.Write(Stopwatch.GetElapsedTime(started).Ticks);
        return 0;
    }
}
"""

    let private spinnerSource : string =
        """
public static class Program
{
    public static int Main()
    {
        long start = System.Environment.TickCount64;
        long last = start;
        long largestGap = 0;
        while (last - start < 100)
        {
            long now = System.Environment.TickCount64;
            if (now - last > largestGap)
            {
                largestGap = now - last;
            }
            last = now;
        }
        Out.Write(largestGap);
        return 0;
    }
}
"""

    [<Test>]
    let ``the clock does not jump to a sleeper's deadline while another program is runnable`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let driver =
            MultiProgram.start
                loggerFactory
                (machineConfig ProgramChoice.RoundRobin)
                [
                    launch "Sleeper.cs" (compile sleeperSource)
                    launch "Spinner.cs" (compile spinnerSource)
                ]

        let run, _ = drive loggerFactory Int32.MaxValue driver

        match finished run with
        | [ (sleeperPid, sleeper) ; (spinnerPid, spinner) ] ->
            exitCodeOf sleeper |> shouldEqual 0
            exitCodeOf spinner |> shouldEqual 0

            // The sleep took its 50 ms of machine time, and less than a millisecond more.
            match longsOf sleeper with
            | [ slept ] ->
                slept |> shouldBeGreaterThan ((TimeSpan.FromMilliseconds 50.0).Ticks - 1L)
                slept |> shouldBeSmallerThan (TimeSpan.FromMilliseconds 51.0).Ticks
            | other -> failwith $"expected the sleeper to report one duration, got %A{other}"

            // The spinner never saw the clock jump.
            longsOf spinner |> shouldEqual [ 1L ]

            // The sleeper woke only after 50 ms of the spinner's instructions had retired: its
            // longest silence was at least that many ticks, every one of them the spinner's.
            let longestSilence =
                run.Ticks
                |> List.filter (fun tick -> tick.Program = sleeperPid && tick.Thread.IsSome)
                |> List.pairwise
                |> List.maxBy (fun (before, after) -> after.StepCounter - before.StepCounter)

            let before, after = longestSilence

            (after.StepCounter - before.StepCounter)
            |> shouldBeGreaterThan ((TimeSpan.FromMilliseconds 50.0).Ticks - 1L)

            run.Ticks
            |> List.filter (fun tick ->
                tick.StepCounter > before.StepCounter
                && tick.StepCounter < after.StepCounter
                && tick.Thread.IsSome
            )
            |> List.forall (fun tick -> tick.Program = spinnerPid)
            |> shouldEqual true
        | other -> failwith $"expected two programs' ends, got %A{other}"

    let private sleepsForeverSource : string =
        """
public static class Program
{
    public static int Main()
    {
        System.Threading.Thread.Sleep(System.Threading.Timeout.Infinite);
        return 0;
    }
}
"""

    [<Test>]
    let ``both programs blocked forever is one deadlock naming both`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let driver =
            MultiProgram.start
                loggerFactory
                (machineConfig ProgramChoice.RoundRobin)
                [
                    launch "SleepsForeverA.cs" (compile sleepsForeverSource)
                    launch "SleepsForeverB.cs" (compile sleepsForeverSource)
                ]

        let launched = driver.Launched
        let run, _ = drive loggerFactory Int32.MaxValue driver

        match run.Result with
        | Ok ends -> failwith $"expected a deadlock, but the programs finished: %A{ends}"
        | Error stuck ->
            stuck |> List.map fst |> shouldEqual launched

            for _, threads in stuck do
                threads |> shouldContainText "BlockedOnSleep"

    let private exitsThreeSource : string =
        """
public static class Program
{
    public static int Main()
    {
        System.Environment.Exit(3);
        return 0;
    }
}
"""

    let private throwsSource : string =
        """
public static class Program
{
    public static int Main()
    {
        throw new System.InvalidOperationException("no");
    }
}
"""

    /// Outlives the other two: it sleeps long enough for them to end, and then reports that it
    /// is still running.
    let private outlivesSource : string =
        """
public static class Program
{
    public static int Main()
    {
        System.Threading.Thread.Sleep(200);
        Out.Text("still here");
        return 0;
    }
}
"""

    [<Test>]
    let ``one program's Environment.Exit, or its unhandled exception, leaves the others running`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let driver =
            MultiProgram.start
                loggerFactory
                (machineConfig ProgramChoice.RoundRobin)
                [
                    launch "ExitsThree.cs" (compile exitsThreeSource)
                    launch "Throws.cs" (compile throwsSource)
                    launch "Outlives.cs" (compile outlivesSource)
                ]

        let run, _ = drive loggerFactory Int32.MaxValue driver

        match finished run with
        | [ (exiterPid, exiter) ; (throwerPid, thrower) ; (_, survivor) ] ->
            run.EndedInOrder
            |> Set.ofList
            |> shouldEqual (Set.ofList [ exiterPid ; throwerPid ])

            match outcomeOf exiter with
            | RunOutcome.ProcessExit (state, _, ProcessTermination.Exited status) ->
                state.LatchedExitCode |> shouldEqual 3
                ExitStatus.waitpidExitCode status |> shouldEqual 3
            | other -> failwith $"expected Environment.Exit, got %O{other}"

            match outcomeOf thrower with
            | RunOutcome.GuestUnhandledException _ -> ()
            | other -> failwith $"expected an unhandled exception, got %O{other}"

            match outcomeOf survivor with
            | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 0
            | other -> failwith $"expected a normal exit, got %O{other}"

            stdoutOf survivor |> shouldEqual (Text.Encoding.ASCII.GetBytes "still here")
        | other -> failwith $"expected three programs' ends, got %A{other}"

    let private trivialSource : string =
        """
public static class Program
{
    public static int Main()
    {
        Out.Text("hi");
        return 3;
    }
}
"""

    [<Test>]
    let ``one launch through MultiProgram is the same run as Program.run`` () : unit =
        let image = compile trivialSource
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let expected =
            use stream = new MemoryStream (image)

            Program.run
                loggerFactory
                (Some "Trivial.cs")
                stream
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
            |> outcomeOf

        let actual =
            match
                MultiProgram.run
                    loggerFactory
                    (fst (KernelConfig.split KernelConfig.Default))
                    [ launch "Trivial.cs" image ]
            with
            | [ pid, runEnd ] ->
                pid |> shouldEqual KernelConfig.Default.ProcessId
                outcomeOf runEnd
            | other -> failwith $"expected one program's end, got %A{other}"

        match expected, actual with
        | RunOutcome.NormalExit (expected, _, expectedTermination), RunOutcome.NormalExit (actual, _, actualTermination) ->
            actualTermination |> shouldEqual expectedTermination
            actual.LatchedExitCode |> shouldEqual expected.LatchedExitCode

            // Entry by entry: an `ImmutableArray`'s equality is its array's identity.
            let entries (state : IlMachineState) : (FileDescriptorRole * byte list) list =
                state.Kernel.OutputLog
                |> Seq.map (fun entry -> entry.Role, Seq.toList entry.Bytes)
                |> Seq.toList

            entries actual |> shouldEqual (entries expected)
            entries actual |> shouldNotEqual []

            actual.Kernel.StepCounter |> shouldEqual expected.Kernel.StepCounter
            actual.Kernel.VirtualClockTicks |> shouldEqual expected.Kernel.VirtualClockTicks
        | expected, actual -> failwith $"expected two normal exits, got %O{expected} and %O{actual}"

    /// Never blocks: every tick it has a Runnable thread, so the choice between two of it is a
    /// choice at every tick.
    let private busySource : string =
        """
public static class Program
{
    public static int Main()
    {
        long acc = 0;
        for (int i = 0; i < 1000000; i++)
        {
            acc = (acc * 31 + i) & 0xFFFFFF;
        }
        return (int)(acc % 7);
    }
}
"""

    let private busyImage : Lazy<byte[]> = lazy (compile busySource)

    /// Which program ran at each of the first `ticks` ticks of two busy guests, under `choice`.
    let private interleaving (choice : ProgramChoice) (ticks : int) : ProcessId list =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let driver =
            MultiProgram.start
                loggerFactory
                (machineConfig choice)
                [ launch "BusyA.cs" busyImage.Value ; launch "BusyB.cs" busyImage.Value ]

        let run, _ = drive loggerFactory ticks driver
        run.Ticks |> List.map _.Program

    [<Test>]
    let ``the same program choice seed replays the same interleaving, and two seeds give two`` () : unit =
        let ticks = 2000

        let property (seed : uint64) (other : uint64) : bool =
            let first = interleaving (ProgramChoice.Seeded seed) ticks
            let again = interleaving (ProgramChoice.Seeded seed) ticks

            first = again
            && (seed = other || interleaving (ProgramChoice.Seeded other) ticks <> first)
            // Each program ran at about half of the ticks.
            && (first |> List.countBy id |> List.forall (fun (_, count) -> count > ticks / 3))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 4, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property)

    [<Test>]
    let ``round robin alternates between two busy programs`` () : unit =
        let ran = interleaving ProgramChoice.RoundRobin 200

        ran
        |> List.pairwise
        |> List.forall (fun (before, after) -> before <> after)
        |> shouldEqual true

    /// The port the listener guest binds, outside every flavour's ephemeral range.
    [<Literal>]
    let private ListenerPort = 5000

    let private listenerSource : string =
        $$"""
using System.Net;
using System.Net.Sockets;

public static class Program
{
    public static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, {{ListenerPort}}));
        listener.Listen(1);
        using var accepted = listener.Accept();
        var remote = (IPEndPoint)accepted.RemoteEndPoint;
        if (!remote.Address.Equals(IPAddress.Loopback)) return 1;
        if (remote.Port == 0 || remote.Port == {{ListenerPort}}) return 2;
        if (((IPEndPoint)accepted.LocalEndPoint).Port != {{ListenerPort}}) return 3;
        return 0;
    }
}
"""

    /// Connects to the listener, retrying while nothing listens on its port yet.
    let private connectorSource : string =
        $$"""
using System.Net;
using System.Net.Sockets;
using System.Threading;

public static class Program
{
    public static int Main()
    {
        var endpoint = new IPEndPoint(IPAddress.Loopback, {{ListenerPort}});
        for (int attempt = 0; attempt < 1000; attempt++)
        {
            using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            try
            {
                client.Connect(endpoint);
            }
            catch (SocketException e) when (e.SocketErrorCode == SocketError.ConnectionRefused)
            {
                Thread.Sleep(1);
                continue;
            }
            if (!client.Connected) return 1;
            // Not `RemoteEndPoint`, which a connected client reads with getpeername(2).
            if (((IPEndPoint)client.LocalEndPoint).Port == 0) return 2;
            return 0;
        }
        return 3;
    }
}
"""

    let private listenerAndConnector (runtimeDirs : ImmutableArray<string>) : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let ends =
            MultiProgram.run
                loggerFactory
                (machineConfig ProgramChoice.RoundRobin)
                [
                    launchOn runtimeDirs "Listener.cs" (compile listenerSource)
                    launchOn runtimeDirs "Connector.cs" (compile connectorSource)
                ]

        match ends with
        | [ (_, listener) ; (_, connector) ] ->
            match outcomeOf listener, outcomeOf connector with
            | RunOutcome.NormalExit (listener, _, _), RunOutcome.NormalExit (connector, _, _) ->
                (listener.LatchedExitCode, connector.LatchedExitCode) |> shouldEqual (0, 0)
            | listener, connector ->
                failwith $"expected both programs to exit normally, got %O{listener} and %O{connector}"
        | other -> failwith $"expected two programs' ends, got %A{other}"

    [<Test>]
    let ``a listener and a connector, as two processes on one machine, both exit 0`` () : unit =
        listenerAndConnector (FrameworkUnderTest.runtimeDirs ())

    [<Test>]
    let ``a listener and a connector on the linux-x64 CoreLib both exit 0`` () : unit =
        listenerAndConnector (runtimeDirsPreferringLinux (requireLinuxFramework ()))

    /// Reads its processor from the shim five times, writing each answer.
    let private processorSource : string =
        """
using System.Runtime.InteropServices;

public static class Program
{
    // `Thread.GetCurrentProcessorId` caches its answer for a number of calls, so ask the
    // shim directly, as the cache does when it refreshes.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SchedGetCpu")]
    static extern int SchedGetCpu();

    public static int Main()
    {
        for (int i = 0; i < 5; i++)
        {
            Out.Write(SchedGetCpu());
        }
        return 0;
    }
}
"""

    [<Test>]
    let ``two programs taking turns on one processor each read it as theirs`` () : unit =
        // The default machine has one processor, and round robin steps the two programs
        // alternately, so each step of one displaces the other's thread from processor 0. The
        // kernel answers `sched_getcpu` only for a thread it records as running, so every read
        // checks that the driver reported the reading thread after the other program's step.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let config = machineConfig ProgramChoice.RoundRobin
        config.ProcessorCount |> shouldEqual 1

        let driver =
            MultiProgram.start
                loggerFactory
                config
                [
                    launch "First.cs" (compile processorSource)
                    launch "Second.cs" (compile processorSource)
                ]

        let run, _ = drive loggerFactory Int32.MaxValue driver

        match finished run with
        | [ (_, first) ; (_, second) ] ->
            exitCodeOf first |> shouldEqual 0
            exitCodeOf second |> shouldEqual 0
            longsOf first |> shouldEqual [ 0L ; 0L ; 0L ; 0L ; 0L ]
            longsOf second |> shouldEqual [ 0L ; 0L ; 0L ; 0L ; 0L ]
        | other -> failwith $"expected two programs' ends, got %A{other}"

        // The two programs did take turns while both lived, so each read followed the other
        // program's step.
        let firstEnded = List.exactlyOne run.EndedInOrder
        let programs = run.Ticks |> List.map _.Program

        let bothLive =
            programs |> List.take (List.findIndexBack ((=) firstEnded) programs + 1)

        bothLive
        |> List.pairwise
        |> List.forall (fun (before, after) -> before <> after)
        |> shouldEqual true
