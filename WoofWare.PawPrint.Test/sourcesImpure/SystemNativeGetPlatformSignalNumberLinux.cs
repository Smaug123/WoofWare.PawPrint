using System.Runtime.InteropServices;

// SystemNative_GetPlatformSignalNumber under the Linux flavour: the rows
// where Linux's <signal.h> disagrees with Darwin's, and the ceiling. The
// Darwin twin of this file states the other column; the rows the two
// flavours agree on are in sourcesPure/SystemNativeGetPlatformSignalNumber.cs.
//
// Impure because the answer is a fact about the *configured* kernel rather
// than the machine this runs on. Linux's numbers were measured with a C
// probe on Linux 6.18.5 / glibc 2.41 (`SIGCHLD 17`, `SIGCONT 18`,
// `SIGTSTP 20`, `SIGRTMAX 64`).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPlatformSignalNumber")]
    static extern int GetPlatformSignalNumber(int posixSignal);

    static int Main(string[] args)
    {
        // The three enum members whose signo differs between the flavours.
        // Under Darwin these would be 20, 19 and 18.
        if (GetPlatformSignalNumber(-5) != 17) return 1;  // SIGCHLD
        if (GetPlatformSignalNumber(-6) != 18) return 2;  // SIGCONT
        if (GetPlatformSignalNumber(-10) != 20) return 3; // SIGTSTP

        if (GetPlatformSignalNumber(-1) != 1) return 4;   // SIGHUP

        // The PAL's GetSignalMax() is SIGRTMAX where it is defined, and
        // glibc defines it as 64. Everything in (0, 64] is echoed back,
        // including 32 and 33, which glibc reserves for itself and whose
        // sigaction(2) therefore fails later (see the signal-handling twin
        // of this file); 65 is the first refusal.
        if (GetPlatformSignalNumber(32) != 32) return 5;
        if (GetPlatformSignalNumber(33) != 33) return 6;
        if (GetPlatformSignalNumber(64) != 64) return 7;
        if (GetPlatformSignalNumber(65) != 0) return 8;

        return 0;
    }
}
