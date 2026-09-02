using System.Runtime.InteropServices;

// SystemNative_EnablePosixSignalHandling, _DisablePosixSignalHandling and
// _HandleNonCanceledPosixSignal under the Darwin flavour, on the signos
// whose identity differs from Linux's.
//
// Impure twice over: the real CLR's answers would describe the host rather
// than the configured kernel, and on the real CLR these entry points install
// sigaction handlers in the test host's own process.
//
// Every expectation here was measured on Darwin 25.6.0 with a C probe:
// `sigaction(2)` refuses 9 (SIGKILL), 17 (SIGSTOP) and everything from 32 up
// with EINVAL; 19 is SIGCONT and 30 is SIGUSR1, both catchable; and a process
// with the default disposition survives SIGINFO (29), SIGIO (23) and
// SIGURG (16).
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
        // 17 is SIGSTOP here, so sigaction refuses it and the PAL reports the
        // failure through errno. Under Linux 17 is SIGCHLD and enables fine.
        if (Enable(17) != 0) return 1;
        if (Marshal.GetLastSystemError() != EINVAL) return 2;

        // 19 is SIGCONT here and enables. Under Linux it is SIGSTOP.
        if (Enable(19) != 1) return 3;

        // 32 passes GetPlatformSignalNumber (Darwin's NSIG is 32) but is not
        // a signal Darwin has, so sigaction refuses it.
        if (Enable(32) != 0) return 4;
        if (Marshal.GetLastSystemError() != EINVAL) return 5;

        // 30 is SIGUSR1 here (10 under Linux).
        if (Enable(30) != 1) return 6;

        // Disabling clears the enable bit for a signo that was enabled. For
        // 32, which never could have been, the PAL still calls sigaction(2)
        // to restore the prior disposition and does not check the result;
        // Darwin refuses it, so what the call leaves behind is EINVAL in
        // errno. Reset errno first so the value read back was written here
        // rather than left over from Enable(32) above.
        Marshal.SetLastSystemError(0);
        Disable(32);
        if (Marshal.GetLastSystemError() != EINVAL) return 10;
        Disable(19);
        Disable(30);

        // Kernel-default dispositions that do not end the process. 29 is
        // SIGINFO here and 23 is SIGIO, both discarded by default; under
        // Linux 29 is SIGIO, which terminates. Neither has an explicit arm
        // in the PAL's switch, so the PAL restores SIG_DFL and re-raises:
        // the process survives, but its native handler for that signo is
        // gone, which the registration observes as the enable bit having
        // been cleared. 16 is SIGURG, which the PAL's switch names, so its
        // handler stays installed. 32 is not a signal at all, so the PAL's
        // sigaction(2) and kill(2) both fail with EINVAL, unchecked, and the
        // process continues with that in errno.
        if (Enable(29) != 1) return 7;
        if (Enable(23) != 1) return 8;
        if (Enable(16) != 1) return 9;
        HandleNonCanceled(29);
        HandleNonCanceled(23);
        HandleNonCanceled(16);
        Marshal.SetLastSystemError(0);
        HandleNonCanceled(32);
        if (Marshal.GetLastSystemError() != EINVAL) return 11;

        return 0;
    }
}
