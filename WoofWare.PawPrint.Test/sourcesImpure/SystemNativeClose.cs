using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Close PawPrint handler directly via a P/Invoke
// stub, without depending on `SafeFileHandle` marshalling. PawPrint intercepts
// the call and routes it through FileDescriptorRegistry.
//
// This is an *impure* test: it runs only inside PawPrint, never against the
// real CLR. The assertions below — particularly the lowest-free gap-fill and
// the double-close-returns-EBADF cases — only hold when the fd table is not
// being mutated concurrently by anything else in the host process. The real
// CLR test harness lives inside a multi-threaded NUnit process where parallel
// tests and runtime background threads (GC, finalizer, ThreadPool I/O) can
// open or close fds at arbitrary moments; on Linux CI this raced our close +
// dup window and flaked the test, and worse could let our double-close
// actually close another thread's fd if it had reused the freed slot.
// PawPrint's interpreter is single-threaded and deterministic, so the exact
// POSIX semantics are stable here.
//
// The assertions:
//   * close of an invalid fd returns -1
//   * close of a freshly-duped fd returns 0
//   * a second close of the same fd returns -1 (the slot is gone)
//   * after close-then-dup, the freed fd is the one allocated next
//     (lowest non-negative integer not in use)
//
// The test deliberately never closes fd 0/1/2 — on PawPrint that would remove
// the stdin/stdout/stderr entries from the simulated fd table.
// `TestFileDescriptorRegistry` covers close over std-stream fds directly,
// alongside property tests for the lowest-free invariant.
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
        // return -1 (EBADF): proof that the first close removed the table
        // entry rather than merely reporting success.
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
