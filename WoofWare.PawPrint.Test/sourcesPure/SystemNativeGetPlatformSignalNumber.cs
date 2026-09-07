using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_GetPlatformSignalNumber PawPrint handler directly
// via a P/Invoke stub. The function is pure (no global state, no host signal
// handler installation), so calling it from a unit test is safe on both the
// real CLR and PawPrint — unlike Enable/DisablePosixSignalHandling, which
// install sigaction handlers in the host process.
//
// The test asserts only signal numbers where Linux and macOS agree, because
// the real CLR reports whichever native value the host header file defines
// while PawPrint reports the configured platform's, which here is the
// default Linux. The rows on which the two disagree are pinned per flavour by
// sourcesImpure/SystemNativeGetPlatformSignalNumber{Linux,Darwin}.cs.
//
// PosixSignal enum values (negative for cross-platform identities) from the
// managed BCL:
//   SIGHUP   = -1
//   SIGINT   = -2
//   SIGQUIT  = -3
//   SIGTERM  = -4
//   SIGCHLD  = -5  (signo differs: Linux 17, macOS 20 — NOT tested)
//   SIGCONT  = -6  (signo differs: Linux 18, macOS 19 — NOT tested)
//   SIGWINCH = -7  (signo agrees: 28)
//   SIGTTIN  = -8  (signo agrees: 21)
//   SIGTTOU  = -9  (signo agrees: 22)
//   SIGTSTP  = -10 (signo differs: Linux 20, macOS 18 — NOT tested)
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPlatformSignalNumber")]
    static extern int GetPlatformSignalNumber(int posixSignal);

    static int Main(string[] args)
    {
        // Cross-platform cases where Linux and macOS agree.
        if (GetPlatformSignalNumber(-1) != 1) return 1;  // SIGHUP
        if (GetPlatformSignalNumber(-2) != 2) return 2;  // SIGINT
        if (GetPlatformSignalNumber(-3) != 3) return 3;  // SIGQUIT
        if (GetPlatformSignalNumber(-4) != 15) return 4; // SIGTERM
        if (GetPlatformSignalNumber(-7) != 28) return 5; // SIGWINCH
        if (GetPlatformSignalNumber(-8) != 21) return 6; // SIGTTIN
        if (GetPlatformSignalNumber(-9) != 22) return 7; // SIGTTOU

        // Passing a positive signo that the runtime recognises must round-trip
        // back unchanged. SIGINT (2) is universally signo 2 on every modern
        // Unix, so this is safe on every supported host.
        if (GetPlatformSignalNumber(2) != 2) return 8;

        // Unmodelled-but-valid native signos must also round-trip — this is
        // what guests rely on when they cast a raw signo to PosixSignal and
        // hand it to PosixSignalRegistration.Create. SIGILL (4) and SIGSEGV
        // (11) agree on both Linux and macOS; neither is in PawPrint's
        // modelled set, but both sit well within GetSignalMax(), so the real
        // native code returns them unchanged and PawPrint must too (via
        // Signal.Other).
        if (GetPlatformSignalNumber(4) != 4) return 11;  // SIGILL
        if (GetPlatformSignalNumber(11) != 11) return 12; // SIGSEGV

        // Zero is the "unknown signal" sentinel: GetPlatformSignalNumber
        // returns 0 for any input it doesn't recognise, which the BCL then
        // promotes to ArgumentOutOfRangeException.
        if (GetPlatformSignalNumber(0) != 0) return 9;

        // A negative value outside the cross-platform enum range is unknown
        // and must produce 0. -100 is comfortably past the lowest defined
        // value (-10) and not a valid platform signo.
        if (GetPlatformSignalNumber(-100) != 0) return 10;

        return 0;
    }
}
