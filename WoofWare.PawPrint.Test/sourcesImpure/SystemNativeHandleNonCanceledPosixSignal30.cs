using System.Runtime.InteropServices;

// Calls SystemNative_HandleNonCanceledPosixSignal with signo 30, which
// terminates the process under both flavours but is a different signal on
// each: SIGUSR1 on Darwin, SIGPWR on Linux. `TestSignalTermination` runs this
// under each platform and asserts which `Signal` the outcome carries, and so
// which exit code the host would report.
//
// Impure for the same reasons as
// SystemNativeHandleNonCanceledPosixSignalTerminate.cs: the identity is a
// fact about the configured kernel, and on the real CLR this call would kill
// the test host.
//
// The `return 99` is unreachable when the arm works: the call surfaces as
// `ExecutionResult.SignalTerminated` and control never returns to the guest.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceledPosixSignal(int signalCode);

    static int Main(string[] args)
    {
        HandleNonCanceledPosixSignal(30);
        return 99;
    }
}
