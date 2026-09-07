using System.Runtime.InteropServices;

// SystemNative_EnablePosixSignalHandling, _DisablePosixSignalHandling and
// _HandleNonCanceledPosixSignal under the Linux flavour, on the signos whose
// identity differs from Darwin's. The Darwin twin of this file states the
// other column.
//
// Impure twice over: the real CLR's answers would describe the host rather
// than the configured kernel, and on the real CLR these entry points install
// sigaction handlers in the test host's own process.
//
// Every expectation here was measured on Linux 6.18.5 / glibc 2.41 with a C
// probe: `sigaction(2)` refuses 9 (SIGKILL), 19 (SIGSTOP), and 32 and 33 —
// glibc's own SIGCANCEL and SIGSETXID, screened in its wrapper — with
// EINVAL, while 64 (SIGRTMAX) is accepted; and a process with the default
// disposition survives SIGCHLD (17) and SIGURG (23).
//
// The registration asserts on the terminal kernel state as well as on the
// exit code: which of the signals enabled below are still enabled after
// their non-cancelled handling ran.
class Program
{
    const int EINVAL = 22;

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_EnablePosixSignalHandling")]
    static extern int Enable(int signalCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_DisablePosixSignalHandling")]
    static extern void Disable(int signalCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceled(int signalCode);

    static int Main(string[] args)
    {
        // 19 is SIGSTOP here, so sigaction refuses it. Under Darwin 19 is
        // SIGCONT and enables fine.
        if (Enable(19) != 0) return 1;
        if (Marshal.GetLastSystemError() != EINVAL) return 2;

        // 17 is SIGCHLD here and enables. Under Darwin it is SIGSTOP.
        if (Enable(17) != 1) return 3;

        // glibc reserves 32 and 33 and refuses to install a handler for them,
        // even though the PAL's GetSignalMax() (SIGRTMAX = 64) admits them.
        if (Enable(32) != 0) return 4;
        if (Marshal.GetLastSystemError() != EINVAL) return 5;
        if (Enable(33) != 0) return 6;
        if (Marshal.GetLastSystemError() != EINVAL) return 7;

        // SIGRTMAX itself is an ordinary catchable signal.
        if (Enable(64) != 1) return 8;

        // Disabling a signal that could never have been enabled still has
        // the PAL restore its prior disposition, and sigaction(2) refuses
        // that with EINVAL, unchecked: for 9 (SIGKILL) and 19 (SIGSTOP) in
        // the kernel, and for 32 and 33 in glibc's wrapper. Reset errno
        // first so the value read back was written by the Disable.
        Marshal.SetLastSystemError(0);
        Disable(19);
        if (Marshal.GetLastSystemError() != EINVAL) return 10;
        Marshal.SetLastSystemError(0);
        Disable(9);
        if (Marshal.GetLastSystemError() != EINVAL) return 11;
        Marshal.SetLastSystemError(0);
        Disable(32);
        if (Marshal.GetLastSystemError() != EINVAL) return 12;
        Marshal.SetLastSystemError(0);
        Disable(33);
        if (Marshal.GetLastSystemError() != EINVAL) return 13;

        // Disabling a catchable signal restores its disposition without
        // incident, so errno is left as it was.
        Marshal.SetLastSystemError(0);
        Disable(64);
        if (Marshal.GetLastSystemError() != 0) return 14;

        // Kernel-default dispositions that do not end the process: 17 is
        // SIGCHLD and 23 is SIGURG, both discarded, and both named by the
        // PAL's switch, so their handlers stay installed and the enable
        // bits stay set.
        if (Enable(23) != 1) return 9;
        HandleNonCanceled(17);
        HandleNonCanceled(23);

        return 0;
    }
}
