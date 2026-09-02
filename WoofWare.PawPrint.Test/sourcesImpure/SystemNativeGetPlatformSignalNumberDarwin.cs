using System.Runtime.InteropServices;

// SystemNative_GetPlatformSignalNumber under the Darwin flavour: the rows
// where Darwin's <signal.h> disagrees with Linux's, and the ceiling.
//
// Impure because the answer is a fact about the *configured* kernel rather
// than the machine: this guest is registered under
// `SimulatedUnixPlatform.macOsArm64`, and the real CLR would answer from
// whatever host it happens to run on. The rows the two flavours agree on
// are in sourcesPure/SystemNativeGetPlatformSignalNumber.cs.
//
// Darwin's numbers here were measured with a C probe on Darwin 25.6.0
// (`SIGCHLD 20`, `SIGCONT 19`, `SIGTSTP 18`, `NSIG 32`, no `SIGRTMAX`).
// The Linux twin of this file states the other column.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPlatformSignalNumber")]
    static extern int GetPlatformSignalNumber(int posixSignal);

    static int Main(string[] args)
    {
        // The three enum members whose signo differs between the flavours.
        // Under Linux these would be 17, 18 and 20.
        if (GetPlatformSignalNumber(-5) != 20) return 1;  // SIGCHLD
        if (GetPlatformSignalNumber(-6) != 19) return 2;  // SIGCONT
        if (GetPlatformSignalNumber(-10) != 18) return 3; // SIGTSTP

        // A row that agrees, so a table swapped wholesale would still be
        // caught by the rows above rather than by a coincidence here.
        if (GetPlatformSignalNumber(-1) != 1) return 4;   // SIGHUP

        // The PAL's GetSignalMax() is NSIG on a platform without SIGRTMAX,
        // and Darwin's NSIG is 32. So 32 is echoed back even though Darwin
        // has no signal 32 (sigaction(2) refuses it; see the signal-handling
        // twin of this file), and 33 is the first refusal. Under Linux the
        // ceiling is SIGRTMAX = 64.
        if (GetPlatformSignalNumber(31) != 31) return 5;  // SIGUSR2 on Darwin
        if (GetPlatformSignalNumber(32) != 32) return 6;
        if (GetPlatformSignalNumber(33) != 0) return 7;
        if (GetPlatformSignalNumber(64) != 0) return 8;
        if (GetPlatformSignalNumber(65) != 0) return 9;

        return 0;
    }
}
