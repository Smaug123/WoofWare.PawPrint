namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The park in `SystemNative_WaitForSocketEvents` -- the row the wait exists for, and the one
/// no guest can report on, because a parked guest never returns an exit code.
///
/// `TestSocketEventsWaitReason` covers what `ThreadStatus.BlockedOnSocketEvents` is obliged to
/// answer, constructing the status directly. These tests are the other half: that the handler
/// *reaches* it, and reaches it re-entrantly.
///
/// What is deliberately not pinned here: that the handler leaves `*count` unwritten before
/// parking. Reading a guest local out of a parked frame means depending on Roslyn's slot
/// ordering, and the write would in any case be overwritten by the re-entry that a wake
/// performs -- so the claim is structural rather than observable. The eval-stack assertion
/// below covers the part of it that a re-entry really would corrupt.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventsWait =

    let private assy = typeof<RunResult>.Assembly

    /// Waits on a *duplicated* port descriptor, which is what makes the identity assertion
    /// below bite: `dup` gives fd 4 naming the same open file description as fd 3, so the
    /// status must carry description id 3 and not the descriptor the guest passed. Waiting on
    /// fd 3 directly could not tell the two apart, since a fresh registry hands out
    /// description id 3 to the first port and that is also its descriptor number.
    let private source : string =
        """
using System;
using System.Runtime.InteropServices;

class WaitsOnADuplicatedPort
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return 1;
        if ((long)port != 3) return 2;

        IntPtr alias = Dup(port);
        if ((long)alias != 4) return 3;

        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(alias, buffer, &count);
        return 4;
    }
}
"""

    /// Steps the guest until it deadlocks, returning the state at that point and the
    /// description the driver would report.
    ///
    /// Fails loudly on any other outcome, exit code 4 included: the guest's `return 4` is
    /// unreachable unless the wait returned, so a run that completes has not covered the park.
    let private runToDeadlock () : IlMachineState * string =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "WaitsOnADuplicatedPort.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestSocketEventsWait"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "WaitsOnADuplicatedPort.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        // A bound rather than an unbounded loop: a regression that failed to park would spin
        // here forever instead of failing.
        let maxSteps = 20_000_000L

        let rec loop (prepared : Program.PreparedProgram) (steps : int64) : IlMachineState * string =
            if steps > maxSteps then
                failwith $"guest did not deadlock within %d{maxSteps} steps"

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) -> prepared.State, stuck
            | Program.ProgramStepOutcome.Completed outcome ->
                failwith
                    $"guest exited instead of parking in WaitForSocketEvents, so this test covered nothing: %O{outcome}"
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> loop prepared (steps + 1L)
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _) -> loop prepared (steps + 1L)

        loop prepared 0L

    /// One run, shared by every assertion below: reaching the park costs a few million
    /// interpreted steps, and the state is immutable, so there is nothing to isolate.
    let private deadlock = lazy (runToDeadlock ())

    let private parkedThread (state : IlMachineState) : ThreadId * ThreadState =
        state.ThreadState
        |> Map.toList
        |> List.filter (fun (_, ts) ->
            match ts.Status with
            | ThreadStatus.BlockedOnSocketEvents _ -> true
            | _ -> false
        )
        |> function
            | [ one ] -> one
            | other -> failwith $"expected exactly one thread parked on socket events, got %d{List.length other}"

    /// The wait parks the caller rather than answering it, and the port it parks on is the open
    /// file *description* the descriptor names. Waiting through a `dup` is what separates the
    /// two: a handler that stored the descriptor number would say 4 here.
    [<Test>]
    let ``the wait parks the caller on the port's open file description`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        threadState.Status
        |> shouldEqual (ThreadStatus.BlockedOnSocketEvents (OpenFileDescriptionId 3L))

    /// Re-entrant parking, stated as the frame stack: the dispatcher leaves the native frame in
    /// place, so a wake re-enters the handler and it re-reads the call's own arguments. Kills
    /// the mutant that parks and then pops the frame (resume-style parking), under which the
    /// active frame would be the guest's `Main` and the wake would have to write the event
    /// buffer from some other thread's step.
    [<Test>]
    let ``the parked thread still carries the native frame`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        let active = threadState.MethodStates.[threadState.ActiveMethodState]
        active.ExecutingMethod.Name |> shouldEqual "WaitForSocketEvents"

        // And the guest frame that called it is still below, so the native frame was pushed on
        // top rather than replacing anything.
        threadState.MethodStates
        |> Map.exists (fun _ frame -> frame.ExecutingMethod.Name = "Main")
        |> shouldEqual true

    /// Nothing is pushed at park time. The handler returns a PAL error code, so an optimistic
    /// push would be the natural way to write it -- and would be wrong here, because re-entry
    /// runs the handler again and would push a second one.
    [<Test>]
    let ``the wait leaves no optimistic return value on the stack`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        let active = threadState.MethodStates.[threadState.ActiveMethodState]
        active.EvaluationStack.Values |> shouldEqual []

    /// The deadlock report has to locate the wait, or a wedged `SocketAsyncEngine` thread is
    /// indistinguishable from any other blocked thread. `TestSocketEventsWaitReason` pins the
    /// rendering against a hand-built position; this pins that a real run reaches it.
    [<Test>]
    let ``the deadlock report names the wait and the port`` () : unit =
        let _, stuck = deadlock.Force ()

        stuck |> shouldContainText "BlockedOnSocketEvents (OpenFileDescriptionId 3L)"
        stuck |> shouldContainText "WaitForSocketEvents"

    /// Two waiters parked on one port, and an entry thread that then makes the port
    /// deliverable. The sleep is what makes the order deterministic: while the entry
    /// thread sleeps, the two waiters are the only runnable threads, so both reach their
    /// parks before the connect.
    let private twoWaitersSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class TwoWaitersOnOnePort
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration")]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    static IntPtr Port;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(Port, buffer, &count);
    }

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return 1;
        Port = port;
        IntPtr listener;
        if (Socket(2, 1, 6, &listener) != 0) return 2;
        byte* addr = stackalloc byte[16];
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, 2);
        SetIPv4Address(addr, 16, 0x0100007F);
        SetPort(addr, 16, 0);
        if (Bind(listener, 6, addr, 16) != 0) return 3;
        if (Listen(listener, 8) != 0) return 4;
        int len = 16;
        if (GetSockName(listener, addr, &len) != 0) return 5;
        if (TryChange(port, listener, 0, 0x3, (IntPtr)1) != 0) return 6;

        new Thread(Waiter) { IsBackground = true }.Start();
        new Thread(Waiter) { IsBackground = true }.Start();
        Thread.Sleep(100);

        IntPtr client;
        if (Socket(2, 1, 6, &client) != 0) return 7;
        if (Connect(client, addr, 16) != 0) return 8;
        return 9;
    }
}
"""

    /// epoll parks `epoll_wait` callers exclusively, so one edge wakes one waiter, chosen
    /// by park order — state PawPrint does not record. The sweep must refuse rather than
    /// wake both and let the scheduler invent the winner; this pins the refusal, and
    /// kills the mutant that wakes every waiter on the port.
    [<Test>]
    let ``an edge arriving with two waiters parked on one port refuses`` () : unit =
        let image = Roslyn.compile [ twoWaitersSource ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "TwoWaitersOnOnePort.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let exc =
            Assert.Throws<GuestFailureException> (fun () ->
                BoundedRun.runWith
                    loggerFactory
                    BoundedRun.defaultMaxSteps
                    "TwoWaitersOnOnePort.cs"
                    (Some "TwoWaitersOnOnePort.cs")
                    peImage
                    (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exc.Message
        |> shouldContainText "are all parked in SystemNative_WaitForSocketEvents"

    /// A waiter parked on a port whose last descriptor the entry thread then closes.
    let private closesParkedPortSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class ClosesAParkedPort
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    static IntPtr Port;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(Port, buffer, &count);
    }

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return 1;
        Port = port;
        new Thread(Waiter) { IsBackground = true }.Start();
        Thread.Sleep(100);
        Close(port);
        return 2;
    }
}
"""

    /// A real `close(2)` does not end an in-flight `epoll_wait` — the syscall holds a
    /// file reference, so the port and its registrations stay alive for it and a later
    /// edge can still complete the wait. PawPrint's close sweeps the description away,
    /// which would strand the waiter in a sleep a real kernel can end, so the close
    /// refuses instead. A `dup` of the port would survive the close and needs no
    /// refusal; this is only about the last descriptor.
    [<Test>]
    let ``closing the last descriptor of a parked-on port refuses`` () : unit =
        let image = Roslyn.compile [ closesParkedPortSource ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "ClosesAParkedPort.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let exc =
            Assert.Throws<GuestFailureException> (fun () ->
                BoundedRun.runWith
                    loggerFactory
                    BoundedRun.defaultMaxSteps
                    "ClosesAParkedPort.cs"
                    (Some "ClosesAParkedPort.cs")
                    peImage
                    (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exc.Message |> shouldContainText "Implement port retention"

    /// A waiter parked on a registered-but-unready listener, and an entry thread whose
    /// 200 ms join resolves only through the jump-to-deadline fallback — which exists
    /// exactly while the waiter *stays* parked.
    let private quietWaiterSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class QuietParkedWaiter
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration")]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    static IntPtr Port;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(Port, buffer, &count);
    }

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return 1;
        Port = port;
        IntPtr listener;
        if (Socket(2, 1, 6, &listener) != 0) return 2;
        byte* addr = stackalloc byte[16];
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, 2);
        SetIPv4Address(addr, 16, 0x0100007F);
        SetPort(addr, 16, 0);
        if (Bind(listener, 6, addr, 16) != 0) return 3;
        if (Listen(listener, 8) != 0) return 4;
        if (TryChange(port, listener, 0, 0x3, (IntPtr)1) != 0) return 5;

        Thread waiter = new Thread(Waiter);
        waiter.IsBackground = true;
        waiter.Start();

        bool finished = waiter.Join(200);
        return finished ? 6 : 0;
    }
}
"""

    /// The park has to be *quiet*: with nothing deliverable on the port, the waiter must
    /// stay parked, because that is what lets the driver jump the clock straight to the
    /// entry thread's join deadline. A readiness sweep that wakes on anything less than a
    /// deliverable event puts the waiter through a Runnable/deliver-nothing/re-park cycle
    /// every tick, which suppresses the jump and grinds the 200 ms join out in interpreted
    /// steps — every wait in every guest slows by orders of magnitude, and no exit code
    /// changes. The step counter is the observer wall clocks cannot be (measured: the
    /// healthy run finishes at a kernel step counter of ~3.9k, where the cycling one
    /// must grind through the join's two million virtual-clock ticks step by step).
    [<Test>]
    let ``a parked waiter with nothing deliverable costs no steps`` () : unit =
        let image = Roslyn.compile [ quietWaiterSource ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "QuietParkedWaiter.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let outcome =
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                "QuietParkedWaiter.cs"
                (Some "QuietParkedWaiter.cs")
                peImage
                (HostConfig.Default dotnetRuntimes)

        match outcome with
        | RunOutcome.NormalExit (state, thread) ->
            (match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
             | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
             | other -> failwith $"expected an int exit code, got %O{other}")

            state.Kernel.StepCounter |> shouldBeSmallerThan 500_000L
        | other -> failwith $"expected a normal exit, got %O{other}"
