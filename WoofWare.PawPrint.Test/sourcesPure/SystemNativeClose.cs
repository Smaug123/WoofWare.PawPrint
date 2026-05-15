using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Close PawPrint handler directly via a P/Invoke
// stub, without depending on `SafeFileHandle` marshalling. The CLR runtime
// dispatches this to the real libSystem.Native shim; PawPrint intercepts the
// call and routes it through FileDescriptorRegistry.
//
// This test must pass on both the real runtime and PawPrint, so it asserts
// only invariants that hold on every Unix kernel:
//   * close of an invalid fd returns -1
//   * close of a freshly-duped fd returns 0
//   * a second close of the same fd returns -1 (the slot is gone)
//   * after close-then-dup, the freed fd is the one POSIX allocates next
//     (lowest non-negative integer not in use)
//
// The test deliberately never closes fd 0/1/2 — on the real CLR that would
// detach the test process from its stdin/stdout/stderr and corrupt the test
// runner's output. The registry unit tests in TestFileDescriptorRegistry
// cover close semantics over std-stream fds directly.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int SystemNative_Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr SystemNative_Dup(IntPtr oldfd);

    static int Main(string[] args)
    {
        // -1 is never a live fd on any Unix; close(2) returns -1 (EBADF).
        if (SystemNative_Close((IntPtr)(-1)) != -1) return 1;

        // dup stdin to a fresh fd; close must succeed and return 0.
        IntPtr duped = SystemNative_Dup((IntPtr)0);
        if ((long)duped < 3L) return 2;
        if (SystemNative_Close(duped) != 0) return 3;

        // A second close of the same fd hits the now-empty slot and must
        // return -1 (EBADF). This is the load-bearing case: closing the
        // table entry actually removed it, so the slot is gone.
        if (SystemNative_Close(duped) != -1) return 4;

        // POSIX requires dup to allocate the lowest non-negative fd not in
        // use, so after the close above the next dup of stdin must reuse
        // the fd we just freed. This exercises the gap-fill at the handler
        // boundary, not just the registry unit.
        IntPtr reused = SystemNative_Dup((IntPtr)0);
        if ((long)reused != (long)duped) return 5;

        // Tidy up so the test doesn't leave a dangling table entry behind.
        if (SystemNative_Close(reused) != 0) return 6;

        return 0;
    }
}
