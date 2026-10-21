namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Security.Cryptography
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The driver's sweep for syscall wakes (`MultiProgram.advance`), over two programs that sleep in
/// the kernel on each other: a server and a client, which contend for an `flock` on one file and
/// exchange messages over a loopback connection, the client writing each message in pieces so
/// that the server's reads wake on part of it. A worker thread in each contends for the `flock`
/// too, so that the sweep wakes threads of the program it runs beside as well as of the other.
///
/// After every tick's preamble, the sweep has woken every thread it should: a sweep of the
/// machine as the preamble leaves it, asked about the threads each program's state shows asleep,
/// wakes nobody. And each program's kept syscall sleepers are the ones its thread states and the
/// kernel's parks show (`Scheduler.checkSyscallSleepers`). Each run's trace, every tick's
/// program, thread and step counter and the threads its preamble woke, is written to the test's
/// output as a digest, so that two builds can be compared run for run.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it `Explicit`, and CI
// selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestMultiProgramWakes =

    /// The port the server listens on, outside every flavour's ephemeral range.
    [<Literal>]
    let private ServerPort = 5001

    /// `server <rounds> <chunk> <piece> <spin>` or `client <rounds> <chunk> <piece> <spin>`. Each
    /// round, each program's main thread holds the exclusive `flock` on `lock` for `spin`
    /// iterations, as does a worker thread of each program through a description of its own; then
    /// the client writes `chunk` bytes to the server, `piece` bytes at a time with `spin` iterations
    /// between, and the server reads them all and writes them back, which the client reads. The
    /// client retries its connect until the server listens. The exit code is the index of the
    /// first check that failed; 0 means all passed.
    let private source : string =
        $$"""
using System;
using System.Runtime.InteropServices;
using System.Threading;

public static class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read")]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open")]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock")]
    static extern int FLock(IntPtr fd, int operation);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;
    const int PAL_SUCCESS = 0;

    const int LOCK_EX = 2;
    const int LOCK_UN = 8;

    const int V4Size = 16;
    const uint Loopback = 0x0100007F;

    static int sink;

    static void Spin(int count)
    {
        for (int i = 0; i < count; i++) sink += i;
    }

    static int Parse(string text)
    {
        int value = 0;
        for (int i = 0; i < text.Length; i++) value = value * 10 + (text[i] - '0');
        return value;
    }

    static unsafe IntPtr Make()
    {
        IntPtr fd;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &fd) != PAL_SUCCESS) return (IntPtr)(-1);
        return fd;
    }

    static unsafe bool Address(byte* blob, ushort port)
    {
        for (int i = 0; i < V4Size; i++) blob[i] = 0;

        return SetAddressFamily(blob, V4Size, AF_INET) == PAL_SUCCESS
               && SetPort(blob, V4Size, port) == PAL_SUCCESS
               && SetIPv4Address(blob, V4Size, Loopback) == PAL_SUCCESS;
    }

    static unsafe IntPtr OpenLock()
    {
        byte* path = stackalloc byte[5];
        path[0] = (byte)'l';
        path[1] = (byte)'o';
        path[2] = (byte)'c';
        path[3] = (byte)'k';
        path[4] = 0;
        return Open(path, 0, 0);
    }

    static bool Locked(IntPtr lockFd, int spin)
    {
        if (FLock(lockFd, LOCK_EX) != 0) return false;
        Spin(spin);
        return FLock(lockFd, LOCK_UN) == 0;
    }

    static unsafe bool ReadFully(IntPtr fd, byte* buffer, int count)
    {
        int got = 0;
        while (got < count)
        {
            int n = Read(fd, buffer + got, count - got);
            if (n <= 0) return false;
            got += n;
        }
        return true;
    }

    static unsafe bool WriteFully(IntPtr fd, byte* buffer, int count, int piece, int spin)
    {
        int sent = 0;
        while (sent < count)
        {
            int n = Write(fd, buffer + sent, Math.Min(piece, count - sent));
            if (n <= 0) return false;
            sent += n;
            Spin(spin);
        }
        return true;
    }

    static unsafe int Server(int rounds, int chunk, int piece, int spin)
    {
        byte* address = stackalloc byte[V4Size];
        IntPtr listener = Make();
        if (listener == (IntPtr)(-1)) return 10;
        if (!Address(address, {{ServerPort}})) return 11;
        if (Bind(listener, PT_TCP, address, V4Size) != PAL_SUCCESS) return 12;
        if (Listen(listener, 1) != PAL_SUCCESS) return 13;
        IntPtr lockFd = OpenLock();
        if (lockFd == (IntPtr)(-1)) return 14;

        byte* peer = stackalloc byte[V4Size];
        int peerLength = V4Size;
        IntPtr connection;
        if (Accept(listener, peer, &peerLength, &connection) != PAL_SUCCESS) return 15;

        byte* buffer = stackalloc byte[chunk];
        for (int round = 0; round < rounds; round++)
        {
            if (!Locked(lockFd, spin)) return 16;
            if (!ReadFully(connection, buffer, chunk)) return 17;
            for (int i = 0; i < chunk; i++)
            {
                if (buffer[i] != (byte)(round + i)) return 18;
            }
            if (!WriteFully(connection, buffer, chunk, chunk, 0)) return 19;
        }
        return 0;
    }

    static unsafe int Client(int rounds, int chunk, int piece, int spin)
    {
        byte* address = stackalloc byte[V4Size];
        if (!Address(address, {{ServerPort}})) return 20;
        IntPtr lockFd = OpenLock();
        if (lockFd == (IntPtr)(-1)) return 21;

        IntPtr connection = (IntPtr)(-1);
        for (int attempt = 0; attempt < 100000 && connection == (IntPtr)(-1); attempt++)
        {
            IntPtr socket = Make();
            if (socket == (IntPtr)(-1)) return 22;
            if (Connect(socket, address, V4Size) == PAL_SUCCESS)
            {
                connection = socket;
            }
            else
            {
                Close(socket);
                Spin(spin);
            }
        }
        if (connection == (IntPtr)(-1)) return 23;

        byte* buffer = stackalloc byte[chunk];
        for (int round = 0; round < rounds; round++)
        {
            if (!Locked(lockFd, spin)) return 24;
            for (int i = 0; i < chunk; i++) buffer[i] = (byte)(round + i);
            if (!WriteFully(connection, buffer, chunk, piece, spin)) return 25;
            for (int i = 0; i < chunk; i++) buffer[i] = 0;
            if (!ReadFully(connection, buffer, chunk)) return 26;
            for (int i = 0; i < chunk; i++)
            {
                if (buffer[i] != (byte)(round + i)) return 27;
            }
        }
        return 0;
    }

    static int workerFailed;

    static void Worker(object state)
    {
        int[] arguments = (int[])state;
        IntPtr lockFd = OpenLock();
        if (lockFd == (IntPtr)(-1))
        {
            workerFailed = 1;
            return;
        }
        for (int round = 0; round < arguments[0]; round++)
        {
            if (!Locked(lockFd, arguments[1]))
            {
                workerFailed = 2;
                return;
            }
        }
    }

    public static int Main(string[] args)
    {
        int rounds = Parse(args[1]);
        int chunk = Parse(args[2]);
        int piece = Parse(args[3]);
        int spin = Parse(args[4]);
        var worker = new Thread(Worker);
        worker.Start(new int[] { rounds, spin });
        int result = args[0][0] == 's' ? Server(rounds, chunk, piece, spin) : Client(rounds, chunk, piece, spin);
        worker.Join();
        if (result == 0 && workerFailed != 0) return 30 + workerFailed;
        return result;
    }
}
"""

    let private image : Lazy<byte[]> = lazy (Roslyn.compile [ source ])

    /// One run: how the driver chooses between the two programs, and what each program does.
    type private WakeCase =
        {
            Choice : ProgramChoice
            Rounds : int
            Chunk : int
            /// How many bytes the client writes at a time.
            Piece : int
            ServerSpin : int
            ClientSpin : int
        }

    let private caseGen : Gen<WakeCase> =
        gen {
            let! choice =
                Gen.frequency
                    [
                        1, Gen.constant ProgramChoice.RoundRobin
                        3, Gen.choose64 (0L, Int64.MaxValue) |> Gen.map (uint64 >> ProgramChoice.Seeded)
                    ]

            let! rounds = Gen.choose (1, 3)
            let! chunk = Gen.choose (1, 24)
            let! piece = Gen.choose (1, chunk)
            let! serverSpin = Gen.choose (0, 40)
            let! clientSpin = Gen.choose (0, 40)

            return
                {
                    Choice = choice
                    Rounds = rounds
                    Chunk = chunk
                    Piece = piece
                    ServerSpin = serverSpin
                    ClientSpin = clientSpin
                }
        }

    let private machineConfig (choice : ProgramChoice) : MachineConfig =
        let machine, _ =
            KernelConfig.split
                { KernelConfig.Default with
                    FileSystem =
                        Map.ofList
                            [
                                DirectoryEntryName.parseOrFail "TestMultiProgramWakes" "lock",
                                SeedEntry.file ImmutableArray.Empty
                            ]
                }

        { machine with
            ProgramChoice = choice
        }

    let private launch (case : WakeCase) (role : string) (spin : int) : ProgramLaunch =
        {
            Image = new MemoryStream (image.Force ())
            OriginalPath = Some $"%s{role}.cs"
            DotnetRuntimeDirs = FrameworkUnderTest.runtimeDirs ()
            Process = snd (KernelConfig.split KernelConfig.Default)
            Argv =
                [
                    role
                    string case.Rounds
                    string case.Chunk
                    string case.Piece
                    string spin
                ]
            AssemblyPath = None
            AppContext = AppContextProperties.empty
            PctSeed = None
        }

    /// Every live program of `driver`, with its process ID: the checked-out one first.
    let private programs (driver : MultiProgram) : (ProcessId * RunningProgram) list =
        let current = driver.Current

        (current.State.Kernel.ProcessId, current) :: Map.toList driver.Idle

    /// The threads of each program parked in `BlockedInSyscall`, as its state shows them.
    let private waiters (driver : MultiProgram) : Map<ProcessId, Set<ThreadId>> =
        programs driver
        |> List.map (fun (pid, program) -> pid, Set.ofList (Scheduler.syscallWaiters program.State))
        |> Map.ofList

    /// What a sweep of `driver`'s machine wakes, where some program has a thread parked in
    /// `BlockedInSyscall`: the question the driver's preamble asks.
    let private unwoken (driver : MultiProgram) : ((ProcessId * ThreadId) * Set<WakePrimitive>) list =
        let anyWaiter =
            programs driver
            |> List.exists (fun (_, program) -> not (List.isEmpty (Scheduler.syscallWaiters program.State)))

        if not anyWaiter then
            []
        else
            UnixWait.wake driver.Current.State.Kernel.System
            |> fst
            |> List.map (fun (caller, fired) -> (Caller.processId caller, Caller.thread caller), fired)

    /// How a run went: its trace, how many threads its preambles woke, how many of those were in a
    /// program not checked out at the time, and each program's exit code in launch order.
    type private WakeRun =
        {
            Trace : string list
            Woken : int
            WokenIdle : int
            ExitCodes : int list
        }

    let private run (loggerFactory : ILoggerFactory) (case : WakeCase) : WakeRun =
        let logger = loggerFactory.CreateLogger "TestMultiProgramWakes"

        let driver =
            MultiProgram.start
                loggerFactory
                (machineConfig case.Choice)
                [ launch case "server" case.ServerSpin ; launch case "client" case.ClientSpin ]

        let rec go
            (driver : MultiProgram)
            (ticks : int)
            (trace : string list)
            (woken : int)
            (wokenIdle : int)
            : WakeRun
            =
            if ticks > 1_000_000 then
                failwith $"the run of %A{case} took more than a million ticks"

            let before = waiters driver

            match MultiProgram.advance (snd (LoggerFactory.makeTest ())) driver with
            | Advanced.Settled (MultiStepOutcome.Deadlocked (_, stuck)) -> failwith $"%A{case} deadlocked: %A{stuck}"
            | Advanced.Settled other -> failwith $"%A{case}: a preamble ended a program: %A{other}"
            | Advanced.Decide (advanced, tick) ->

            match unwoken advanced with
            | [] -> ()
            | missed -> failwith $"%A{case}: after the preamble of tick %d{tick}, a sweep still wakes %A{missed}"

            for _, program in programs advanced do
                Scheduler.checkSyscallSleepers program.State

            let after = waiters advanced
            let currentPid = advanced.Current.State.Kernel.ProcessId

            let wokenNow =
                before
                |> Map.toList
                |> List.collect (fun (pid, threads) ->
                    let still = Map.tryFind pid after |> Option.defaultValue Set.empty

                    Set.difference threads still
                    |> Set.toList
                    |> List.map (fun thread -> pid, thread)
                )

            let wakeLine =
                wokenNow
                |> List.map (fun (pid, thread) -> $"%O{pid}/%O{thread}")
                |> String.concat ","

            let woken = woken + wokenNow.Length

            let wokenIdle =
                wokenIdle
                + (wokenNow |> List.filter (fun (pid, _) -> pid <> currentPid) |> List.length)

            match MultiProgram.decide loggerFactory logger advanced tick with
            | MultiStepOutcome.Stepped (next, pid, event) ->
                let what =
                    match event with
                    | ProgramEvent.InstructionStepped (thread, _, _) -> $"step %O{thread}"
                    | ProgramEvent.WorkerTerminated thread -> $"terminated %O{thread}"
                    | ProgramEvent.PhaseAdvanced -> "phase"
                    | ProgramEvent.Ended _ -> "ended"

                let line =
                    $"%d{tick} woke [%s{wakeLine}] %O{pid} %s{what} %d{next.Current.State.Kernel.StepCounter}"

                go next (ticks + 1) (line :: trace) woken wokenIdle
            | MultiStepOutcome.Finished (ends, machine) ->
                UnixSystem.checkInvariants machine |> shouldEqual []

                let exitCodes =
                    ends
                    |> List.map (fun (_, runEnd) ->
                        match runEnd with
                        | RunEnd.Ended (RunOutcome.NormalExit (state, _, _)) -> state.LatchedExitCode
                        | other -> failwith $"%A{case}: a program did not exit normally: %O{other}"
                    )

                {
                    Trace = List.rev ($"%d{tick} woke [%s{wakeLine}] finished" :: trace)
                    Woken = woken
                    WokenIdle = wokenIdle
                    ExitCodes = exitCodes
                }
            | MultiStepOutcome.Deadlocked (_, stuck) -> failwith $"%A{case} deadlocked: %A{stuck}"

        go driver 0 [] 0 0

    let private digest (trace : string list) : string =
        trace
        |> String.concat "\n"
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString

    [<Test>]
    let ``after every preamble no thread asleep in the kernel is left for a sweep to wake, and each program keeps the sleepers its threads show``
        ()
        : unit
        =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let cases = Gen.sampleWithSeed (Rnd 20261013UL) 10 10 caseGen |> List.ofArray

        let runs =
            cases
            |> List.map (fun case ->
                let run = run loggerFactory case

                TestContext.Out.WriteLine
                    $"%A{case}: %d{run.Trace.Length} ticks, %d{run.Woken} woken (%d{run.WokenIdle} idle), trace %s{digest run.Trace}"

                run.ExitCodes |> shouldEqual [ 0 ; 0 ]
                run
            )

        // The sweep woke threads in both the checked-out program and an idle one.
        runs |> List.sumBy _.WokenIdle |> shouldBeGreaterThan 0

        runs
        |> List.sumBy (fun run -> run.Woken - run.WokenIdle)
        |> shouldBeGreaterThan 0

    /// A program's signal poll wakes its dispatcher's read through `UnixWait.wake`, which wakes
    /// every sleeping thread on the machine that may go on, another program's among them. The
    /// kernel wakes a thread once, so that thread must be flipped back to `Runnable` even where
    /// the same poll then ends its own process: here A's dispatcher reads a SIGTERM nothing has
    /// registered for, re-raises it at its default, and A dies, while B's leader, whose
    /// `epoll_wait` has timed out, was woken by the same wake.
    [<Test>]
    let ``a thread of another program that a signal poll wakes is flipped, even when the poll ends its own process``
        ()
        : unit
        =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestMultiProgramWakes"

        let spinner =
            Roslyn.compile
                [
                    "public static class Program { public static int Main() { while (true) {} } }"
                ]

        let launch (name : string) : ProgramLaunch =
            {
                Image = new MemoryStream (spinner)
                OriginalPath = Some name
                DotnetRuntimeDirs = FrameworkUnderTest.runtimeDirs ()
                Process = snd (KernelConfig.split KernelConfig.Default)
                Argv = []
                AssemblyPath = None
                AppContext = AppContextProperties.empty
                PctSeed = None
            }

        let machine, _ = KernelConfig.split KernelConfig.Default

        let started =
            MultiProgram.start loggerFactory machine [ launch "a.cs" ; launch "b.cs" ]

        // Both programs into `Main`, so that A's death ends its run rather than its startup.
        let rec warm (driver : MultiProgram) (ticks : int) : MultiProgram =
            let inMain (program : RunningProgram) : bool =
                match program.EntryFrame with
                | EntryFrameKind.Main _ -> true
                | EntryFrameKind.StartupCall _ -> false

            if programs driver |> List.forall (snd >> inMain) then
                driver
            elif ticks > 1_000_000 then
                failwith "startup took more than a million ticks"
            else
                match MultiProgram.step loggerFactory logger driver with
                | MultiStepOutcome.Stepped (driver, _, _) -> warm driver (ticks + 1)
                | other -> failwith $"startup ended: %A{other}"

        let driver = warm started 0
        let a = driver.Current
        let pidB, b = driver.Idle |> Map.toList |> List.exactlyOne

        // A's dispatcher, asleep in its read of its signal pipe.
        let parkedA =
            match NativeSystemNative.initializeSignalHandling "test" a.State.Kernel.Leader a.State with
            | Ok state ->
                match SignalDispatch.pollMachine loggerFactory a.BaseClassTypes state with
                | SignalPoll.Continues state, [] -> state
                | _, woken -> failwith $"the first poll woke %A{woken}, or ended the process"
            | Error error -> failwith $"initialising signal handling failed with %O{error}"

        let pipe = (PosixSignalShim.signalPipe parkedA.Kernel.PosixSignalShim).Value
        let callerA = parkedA.Kernel.Caller parkedA.Kernel.Leader
        let callerB = b.State.Kernel.Caller b.State.Kernel.Leader

        // B's leader in an `epoll_wait` of 1 ms on a port nothing makes ready.
        let waiting =
            match UnixPoll.epollCreate1 callerB 0 parkedA.Kernel.System with
            | DescriptorAnswer.Returned port, system ->
                match UnixPoll.epollWait callerB port 1 UserBuffer.Mapped 1 system with
                | EpollWaitOutcome.WouldBlock _, system -> system
                | other -> failwith $"expected B's epoll_wait to park, got %A{other}"
            | DescriptorAnswer.Failed error, _ -> failwith $"epoll_create1 failed with %O{error}"

        // SIGTERM's number in A's pipe, as a native handler would have written it.
        let written =
            match UnixReadWrite.write callerA pipe.WriteEnd UserBuffer.Mapped 1UL waiting with
            | CallProgress.NeedsCopyIn paused ->
                match UnixReadWrite.writeWithBytes (ImmutableArray.Create 15uy) paused with
                | ProcessFate.Continues (WriteOutcome.Returns (WriteAnswer.Completed 1L), system), None -> system
                | other -> failwith $"writing the pipe answered %A{other}"
            | other -> failwith $"writing the pipe answered %A{other}"

        // The clock past B's deadline.
        let readyA =
            parkedA.MapKernel (EmulatedKernel.withUnix written)
            |> fun state ->
                state.MapKernel (EmulatedKernel.withVirtualClockTicks (state.Kernel.VirtualClockTicks + 10_000L))

        let readyB =
            b.State.MapKernel (EmulatedKernel.withUnix readyA.Kernel.System)
            |> Scheduler.parkInSyscall b.State.Kernel.Leader

        let staged =
            { driver with
                Current =
                    { a with
                        State = readyA
                    }
                Roster =
                    { driver.Roster with
                        Idle =
                            Map.add
                                pidB
                                { b with
                                    State = readyB
                                }
                                driver.Idle
                    }
            }

        match MultiProgram.step loggerFactory logger staged with
        | MultiStepOutcome.Stepped (next, ended, ProgramEvent.Ended _) ->
            ended |> shouldEqual a.State.Kernel.ProcessId
            let survivor = next.Current.State

            survivor.Kernel.ProcessId |> shouldEqual pidB

            match
                (UnixThreadState.park (EmulatedKernel.taskOf survivor.Kernel.Leader survivor.Kernel.Tasks)).Value.State
            with
            | ParkState.Woken _ -> ()
            | ParkState.Asleep -> failwith "expected the poll's wake to have woken B's leader"

            survivor.ThreadState.[survivor.Kernel.Leader].Status
            |> shouldEqual ThreadStatus.Runnable
        | other -> failwith $"expected A's poll to end A, got %A{other}"
