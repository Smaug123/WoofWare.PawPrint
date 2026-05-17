using System.Runtime.InteropServices;

// Exercises the SystemNative_InitializeTerminalAndSignalHandling PawPrint
// handler directly via a P/Invoke stub, without depending on `Console`'s
// initialisation chain. The CLR runtime would dispatch this to the real
// libSystem.Native shim (which snapshots termios and installs the master
// SIGINT/SIGQUIT/SIGCHLD/SIGWINCH handler under a mutex); PawPrint models
// neither termios nor signals, so the handler returns success unconditionally
// (matching the WASI variant of the same native function).
//
// This test must pass on both the real runtime and PawPrint, so it asserts
// only the invariant that holds on every supported host: the call returns a
// non-zero value (truthy, marshalled as `bool true` at the BCL boundary).
// The specific contract — successive calls are idempotent — is documented
// in the native source but not separately observable here, since the only
// observable signal is the return value and it is success on the first call.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_InitializeTerminalAndSignalHandling")]
    static extern int SystemNative_InitializeTerminalAndSignalHandling();

    static int Main(string[] args)
    {
        int first = SystemNative_InitializeTerminalAndSignalHandling();
        if (first == 0) return 1;

        // A second call must also succeed: the native implementation is
        // documented as a one-shot mutex-guarded initialiser whose subsequent
        // calls short-circuit through a cached `initialized` flag. Both
        // branches surface as a non-zero return on every Unix host.
        int second = SystemNative_InitializeTerminalAndSignalHandling();
        if (second == 0) return 2;

        return 0;
    }
}
