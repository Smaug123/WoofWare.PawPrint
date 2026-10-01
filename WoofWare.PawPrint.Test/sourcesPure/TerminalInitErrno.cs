using System.Runtime.InteropServices;

// What System.Native's terminal initialisation leaves in errno. Its first step
// asks `tcgetattr(STDIN_FILENO)` whether standard input is a terminal; under
// the launch both runtimes give a guest here, standard input is a pipe, so the
// answer is ENOTTY, which nothing afterwards overwrites. A second call finds
// the work done and leaves errno alone.
//
// Measured on Darwin 27.0.0 and Linux 6.18.5 with .NET 10
// (docs/plans/2026-08-23-posix-kernel-extraction/terminal-init-errno.cs), and
// ENOTTY is 25 on both. Through an import without SetLastError, so that errno
// is read as the native call left it.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_InitializeTerminalAndSignalHandling")]
    static extern int InitializeTerminalAndSignalHandling();

    static int Main(string[] args)
    {
        Marshal.SetLastSystemError(4242);
        int first = InitializeTerminalAndSignalHandling();
        int firstErrno = Marshal.GetLastSystemError();

        if (first != 1) return 1;
        if (firstErrno != 25) return 2;

        Marshal.SetLastSystemError(4242);
        int second = InitializeTerminalAndSignalHandling();
        int secondErrno = Marshal.GetLastSystemError();

        if (second != 1) return 3;
        if (secondErrno != 4242) return 4;

        return 0;
    }
}
