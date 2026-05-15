using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Dup PawPrint handler directly via a P/Invoke
// stub, without depending on `SafeFileHandle` marshalling or `Console`'s
// initialisation chain. The CLR runtime would dispatch this to the real
// libSystem.Native shim; PawPrint intercepts the call and routes it through
// FileDescriptorRegistry.
//
// This test must pass on both the real runtime and PawPrint, so it asserts
// only invariants that hold on every Unix kernel:
//   * dup of an invalid fd returns -1
//   * dup of a live fd returns a non-negative fd in the "user" range (>= 3,
//     since 0/1/2 are reserved for stdin/stdout/stderr at exec time and dup
//     is required by POSIX to allocate the lowest free fd)
//   * two consecutive dups return distinct fds
// The specific fd numbers PawPrint allocates (3, 4, 5, ...) are validated
// separately in TestFileDescriptorRegistry, where determinism is in scope.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr SystemNative_Dup(IntPtr oldfd);

    static int Main(string[] args)
    {
        // -1 is never a live fd on any Unix; dup(2) returns -1 (EBADF).
        IntPtr bad = SystemNative_Dup((IntPtr)(-1));
        if ((long)bad != -1L) return 1;

        // stdin (fd 0) is guaranteed to be live at process start. dup must
        // return a fresh fd >= 3 (POSIX reserves 0/1/2 for std streams).
        IntPtr first = SystemNative_Dup((IntPtr)0);
        if ((long)first < 3L) return 2;

        // A second dup of stdin must allocate a different fd: dup(2) is
        // defined to return a fresh slot, never re-aliasing the source.
        IntPtr second = SystemNative_Dup((IntPtr)0);
        if ((long)second < 3L) return 3;
        if ((long)second == (long)first) return 4;

        return 0;
    }
}
