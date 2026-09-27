using System.Runtime.InteropServices;

// System.Native's non-cancelled handling of a registered signal does nothing
// when the disposition its handler replaced was not the default: for SIGSEGV
// that is the runtime's own handler, which the shim's handler has already
// run, and for SIGPIPE it is SIG_IGN. Unregistering then puts that
// disposition back. Either way the process carries on.
//
// SIGSEGV is 11 and SIGPIPE 13 on both Linux and Darwin.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_EnablePosixSignalHandling")]
    static extern int Enable(int signalCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_DisablePosixSignalHandling")]
    static extern void Disable(int signalCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_HandleNonCanceledPosixSignal")]
    static extern void HandleNonCanceled(int signalCode);

    static int Main(string[] args)
    {
        // Registering anything initialises the shim's signal handling and
        // installs its managed callback, which the raw entry points assume.
        using (PosixSignalRegistration.Create(PosixSignal.SIGCONT, _ => { })) { }

        if (Enable(11) != 1) return 1;
        HandleNonCanceled(11);
        Disable(11);

        if (Enable(13) != 1) return 2;
        HandleNonCanceled(13);
        Disable(13);

        return 0;
    }
}
