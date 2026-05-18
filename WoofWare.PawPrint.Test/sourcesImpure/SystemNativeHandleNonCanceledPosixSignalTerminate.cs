using System.Runtime.InteropServices;

// Exercises the SystemNative_HandleNonCanceledPosixSignal PawPrint
// handler directly via a P/Invoke stub, with a signo whose POSIX
// default disposition is Terminate. Real .NET would only ever reach
// this entry point from PosixSignalRegistration's worker thread after
// all registered handlers ran without cancelling; PawPrint accepts the
// direct call too, since the BCL contract for the import is the same
// either way (signo previously cleared by GetPlatformSignalNumber).
//
// This is an *impure* test: PawPrint always uses the Linux signo table
// for determinism across hosts (so 15 means SIGTERM unambiguously
// here), whereas the real CLR uses the host's signo table — and even
// if 15 happens to be SIGTERM on the host, running this through the
// real CLR would actually terminate the NUnit test process with
// SIGTERM, which is obviously not what we want. The PawPrint runner
// surfaces signal termination as `RunOutcome.SignalTerminated` so the
// test harness can assert on the originating signal without taking
// down the host.
//
// `Main` returning is unreachable: the P/Invoke call surfaces as
// `ExecutionResult.SignalTerminated` and never returns control to the
// guest. The `return 99` is present only to satisfy the C# compiler;
// a regression that *did* return here would surface as a `NormalExit`
// with exit code 99 and the test would fail on the outcome shape.

class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceledPosixSignal(int signalCode);

    static int Main(string[] args)
    {
        HandleNonCanceledPosixSignal(15); // SIGTERM (Linux signo)
        return 99;
    }
}
