namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The park in `SystemNative_Accept`: a blocking listener with nothing queued sleeps in the
/// kernel's accept, and a connection finishes the call through the handler's re-entry.
///
/// `sourcesPure/SocketAcceptBlocking.cs` is the differential half, through the managed
/// `Socket.Accept`. These are the rows it cannot state: what the park records, which of two
/// accepters a connection wakes (a real runtime would race its threads into the kernel), and that
/// the call finishes on the length the shim copied before sleeping rather than on the guest's cell.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBlockingAccept =

    /// The P/Invokes and the listener setup every guest below shares: `Listener` is a blocking
    /// IPv4 listener on loopback, and `Address` its bound address.
    let private prelude : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

static class Sys
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    public static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Bind")]
    public static extern unsafe int Bind(IntPtr socket, int protocolType, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Listen")]
    public static extern int Listen(IntPtr socket, int backlog);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Connect")]
    public static extern unsafe int Connect(IntPtr socket, byte* socketAddress, int socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Accept")]
    public static extern unsafe int Accept(IntPtr socket, byte* socketAddress, int* socketAddressLen, IntPtr* acceptedSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSockName")]
    public static extern unsafe int GetSockName(IntPtr socket, byte* socketAddress, int* socketAddressLen);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    public static extern IntPtr Dup(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    public static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    public static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    public static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    public static IntPtr Listener;
    public static IntPtr Address = Marshal.AllocHGlobal(16);

    public static unsafe int Setup()
    {
        IntPtr listener;
        if (Socket(2, 1, 6, &listener) != 0) return 100;
        Listener = listener;
        byte* addr = (byte*)Address;
        for (int i = 0; i < 16; i++) addr[i] = 0;
        SetAddressFamily(addr, 16, 2);
        SetIPv4Address(addr, 16, 0x0100007F);
        SetPort(addr, 16, 0);
        if (Bind(listener, 6, addr, 16) != 0) return 101;
        if (Listen(listener, 8) != 0) return 102;
        int len = 16;
        if (GetSockName(listener, addr, &len) != 0) return 103;
        return 0;
    }

    public static unsafe int ConnectOnce()
    {
        IntPtr client;
        if (Socket(2, 1, 6, &client) != 0) return 104;
        if (Connect(client, (byte*)Address, 16) != 0) return 105;
        return 0;
    }
}
"""

    let private run (fileName : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ prelude ; source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", fileName ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (image)

        BoundedRun.runWith
            loggerFactory
            BoundedRun.defaultMaxSteps
            fileName
            (Some fileName)
            peImage
            (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        |> ExpectRun.ended

    let private exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, _, _)
        | RunOutcome.ProcessExit (state, _, _) -> state.LatchedExitCode
        | other -> failwith $"expected the guest to exit, got %O{other}"

    /// Accepts through a `dup` of the listener, and nothing ever connects. The listener is fd 3
    /// on description 3, the dup fd 4 on the same description, so the park records description
    /// 3 entered through descriptor 4.
    let private neverConnectedSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class NeverConnected
{
    static unsafe int Main()
    {
        int setup = Sys.Setup();
        if (setup != 0) return setup;
        if ((long)Sys.Listener != 3) return 1;
        IntPtr alias = Sys.Dup(Sys.Listener);
        if ((long)alias != 4) return 2;

        byte* peer = stackalloc byte[16];
        int len = 16;
        IntPtr accepted;
        Sys.Accept(alias, peer, &len, &accepted);
        return 3;
    }
}
"""

    /// Steps the guest until it deadlocks, returning the state then and the driver's description
    /// of it. Fails on any other outcome: `return 3` is unreachable unless the accept returned.
    let private runToDeadlock () : Program.PreparedProgram * string =
        let image = Roslyn.compile [ prelude ; neverConnectedSource ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "NeverConnected.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestBlockingAccept"

        use peImage = new MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some "NeverConnected.cs")
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        let maxSteps = 20_000_000L

        let rec loop (prepared : Program.PreparedProgram) (steps : int64) : Program.PreparedProgram * string =
            if steps > maxSteps then
                failwith $"guest did not deadlock within %d{maxSteps} steps"

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) -> prepared, stuck
            | Program.ProgramStepOutcome.Completed outcome ->
                failwith $"guest exited instead of parking in Accept, so this test covered nothing: %O{outcome}"
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> loop prepared (steps + 1L)
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _) -> loop prepared (steps + 1L)

        loop prepared 0L

    let private deadlock = lazy (runToDeadlock ())

    /// The park names the listener's open file description and the descriptor the call was made
    /// through, the destination the call was entered
    /// with, and the length the shim copied out of the guest's cell; the thread keeps the native
    /// frame, with nothing pushed on it, so that a wake re-enters the handler.
    [<Test>]
    let ``an accept with nothing queued parks the caller on the listener's description`` () : unit =
        let prepared, _ = deadlock.Force ()
        let state = prepared.State

        let thread, threadState =
            state.ThreadState
            |> Map.toList
            |> List.filter (fun (_, ts) -> ts.Status = ThreadStatus.BlockedInSyscall)
            |> function
                | [ one ] -> one
                | other -> failwith $"expected exactly one thread parked in a syscall, got %d{List.length other}"

        UnixTaskTable.parkedFor thread state.Kernel.Tasks
        |> shouldEqual (
            Some (
                ParkedSyscall.Accept
                    {
                        Listener = SleepTarget.Waiting (OpenFileDescriptionId 3L, 4)
                        Destination = UserBuffer.Mapped
                        DeclaredLength = 16u
                    }
            )
        )

        let active = threadState.MethodStates.[threadState.ActiveMethodState]
        active.ExecutingMethod.Name |> shouldEqual "Accept"
        active.EvaluationStack.Values |> shouldEqual []

    [<Test>]
    let ``the deadlock report names the accept and the listener`` () : unit =
        let _, stuck = deadlock.Force ()

        stuck
        |> shouldContainText "BlockedInSyscall for a connection on the listener of open file description 3"

    /// Two threads accept on one listener, the first parking 100ms before the second, and one
    /// connection arrives. Measured on both flavours (`blocking-accept.c`, section B1), it goes
    /// to the accepter that parked first. Exit code 11 is the first accepter alone; the second
    /// alone would give 12.
    let private twoAcceptersSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class TwoAccepters
{
    static int Returned;

    static unsafe void Accepter(object id)
    {
        byte* peer = stackalloc byte[16];
        int len = 16;
        IntPtr accepted;
        if (Sys.Accept(Sys.Listener, peer, &len, &accepted) == 0)
            Interlocked.Add(ref Returned, (int)id);
    }

    static int Main()
    {
        int setup = Sys.Setup();
        if (setup != 0) return setup;

        new Thread(Accepter) { IsBackground = true }.Start(1);
        Thread.Sleep(100);
        new Thread(Accepter) { IsBackground = true }.Start(2);
        Thread.Sleep(100);

        int connected = Sys.ConnectOnce();
        if (connected != 0) return connected;
        Thread.Sleep(100);
        return 10 + Volatile.Read(ref Returned);
    }
}
"""

    [<Test>]
    let ``a connection with two accepters parked wakes the one that parked first`` () : unit =
        run "TwoAccepters.cs" twoAcceptersSource |> exitCodeOf |> shouldEqual 11

    /// The guest rewrites its length cell from 16 to 4 while the accept sleeps. The shim copied
    /// the cell into its own `socklen_t` before calling `accept4` (pal_networking.c), so the call
    /// still writes all sixteen bytes of the peer address and reports 16. Exit code 0 is that;
    /// 1 is a finish that re-read the cell and wrote four bytes.
    let private rewrittenLengthSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class RewrittenLength
{
    static IntPtr Peer = Marshal.AllocHGlobal(16);
    static IntPtr Length = Marshal.AllocHGlobal(4);
    static int Result = -1;

    static unsafe void Accepter()
    {
        IntPtr accepted;
        Result = Sys.Accept(Sys.Listener, (byte*)Peer, (int*)Length, &accepted);
    }

    static unsafe int Main()
    {
        int setup = Sys.Setup();
        if (setup != 0) return setup;
        byte* peer = (byte*)Peer;
        for (int i = 0; i < 16; i++) peer[i] = 0xAA;
        *(int*)Length = 16;

        var accepter = new Thread(Accepter);
        accepter.Start();
        Thread.Sleep(100);
        *(int*)Length = 4;

        int connected = Sys.ConnectOnce();
        if (connected != 0) return connected;
        accepter.Join();

        if (Result != 0) return 2;
        if (*(int*)Length != 16) return 3;
        // `sin_zero`, the last eight bytes, is written as zeros.
        if (peer[15] != 0) return 1;
        return 0;
    }
}
"""

    [<Test>]
    let ``a woken accept writes the address through the length the shim copied before sleeping`` () : unit =
        run "RewrittenLength.cs" rewrittenLengthSource |> exitCodeOf |> shouldEqual 0

    /// A background thread sleeps in an accept whose `acceptedSocket` names no storage, and the
    /// entry thread exits without connecting. The shim stores through that pointer only once
    /// `accept4` returns, so a call that never returns never faults on it, and the process exits
    /// normally, as it does on a real runtime.
    let private unreadOutPointerSource : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class UnreadOutPointer
{
    static unsafe void Accepter()
    {
        byte* peer = stackalloc byte[16];
        int len = 16;
        Sys.Accept(Sys.Listener, peer, &len, (IntPtr*)123);
    }

    static int Main()
    {
        int setup = Sys.Setup();
        if (setup != 0) return setup;
        new Thread(Accepter) { IsBackground = true }.Start();
        Thread.Sleep(100);
        return 7;
    }
}
"""

    [<Test>]
    let ``a sleeping accept does not touch its out-pointer`` () : unit =
        run "UnreadOutPointer.cs" unreadOutPointerSource |> exitCodeOf |> shouldEqual 7
