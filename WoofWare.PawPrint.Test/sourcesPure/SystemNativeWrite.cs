using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Write PawPrint handler directly via a P/Invoke
// stub. The CLR runtime dispatches this to the real libSystem.Native shim;
// PawPrint intercepts the call and routes it through FileDescriptorRegistry.
//
// This test must pass on both the real runtime and PawPrint, so it asserts
// only invariants that hold on every Unix kernel AND that do not actually
// emit bytes to the test runner's real stdout/stderr (which would corrupt
// the test report). The cases here are therefore:
//   * write of a negative byteCount returns -1 (PawPrint sets ERANGE; the
//     real Common_Write does too)
//   * write to a non-existent fd returns -1 (EBADF)
//   * zero-byte write to stdout returns 0 and does not dereference the
//     pointer (so passing IntPtr.Zero is safe)
//
// The success path with non-zero byte count is exercised separately by an
// impure test that asserts on EmulatedKernel.OutputLog without ever going
// through the real kernel.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Main(string[] args)
    {
        byte b = 0;

        // bufferSize < 0: Common_Write sets errno = ERANGE and returns -1
        // before any dereference of `buffer`.
        if (SystemNative_Write((IntPtr)1, &b, -1) != -1) return 1;

        // fd that was never opened: write(2) returns -1 with errno = EBADF.
        // -1 is never a live fd on any Unix.
        if (SystemNative_Write((IntPtr)(-1), &b, 1) != -1) return 2;

        // Zero-byte write to stdout: no-op, returns 0, must not dereference
        // the buffer. Passing IntPtr.Zero deliberately to verify the
        // short-circuit (a real write(fd, NULL, 0) is well-defined on Linux
        // and Darwin and returns 0).
        if (SystemNative_Write((IntPtr)1, (byte*)0, 0) != 0) return 3;

        // Zero-byte write to stdout with a non-null, non-managed bit
        // pattern: same contract. write(2) does not dereference the
        // buffer when count is zero, so the bit pattern is irrelevant.
        // This guards against PawPrint regressing to eagerly decoding the
        // pointer argument (which would crash on the verbatim 123) before
        // checking bufferSize.
        if (SystemNative_Write((IntPtr)1, (byte*)123, 0) != 0) return 4;

        // Non-zero write to stdout with a NULL buffer: real write(2)
        // returns -1 with errno = EFAULT and does not perform any I/O
        // (POSIX guarantees the failure precedes any data emission, so
        // this is safe to run on the real CLR without polluting test
        // runner stdout). PawPrint must surface the same syscall
        // failure rather than crashing the interpreter on a direct
        // P/Invoke that bypasses the BCL's null guard in Stream.Write.
        if (SystemNative_Write((IntPtr)1, (byte*)0, 5) != -1) return 5;

        return 0;
    }
}
