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
// with the default disposition survives SIGINFO (29) and SIGURG (16).
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

        // Disabling is a no-op on the enable bit for a signo that could never
        // have been enabled, and clears it for one that was.
        Disable(32);
        Disable(19);
        Disable(30);

        // Kernel-default dispositions that do not end the process. 29 is
        // SIGINFO here, whose default is to discard the signal; under Linux
        // 29 is SIGIO, which terminates. 16 is SIGURG (discarded) and 17 is
        // SIGSTOP (stops; the runtime cannot stop itself, so nothing
        // happens). 32 is not a signal at all, so the PAL's kill(2) fails
        // and the process continues.
        HandleNonCanceled(29);
        HandleNonCanceled(16);
        HandleNonCanceled(17);
        HandleNonCanceled(32);

        return 0;
    }
}
