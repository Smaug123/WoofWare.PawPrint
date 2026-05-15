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
// impure test that asserts on EmulatedKernel.StdoutAppended without ever
// going through the real kernel.
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

        return 0;
    }
}
