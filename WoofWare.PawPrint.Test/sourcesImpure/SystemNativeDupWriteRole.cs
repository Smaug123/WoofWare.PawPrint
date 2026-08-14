using System;
using System.Runtime.InteropServices;

// Byte-emitting counterpart to sourcesPure/SystemNativeDupWrite.cs: writes real
// payloads through descriptors produced by `dup(2)` and lets the harness assert
// that they landed under the right role in EmulatedKernel.OutputLog.
//
// This is *impure* (PawPrint-only) for two reasons. The assertion is about
// PawPrint's OutputLog, which has no real-runtime counterpart; and the writes
// actually emit bytes, which on the real runtime would go to the captured
// child stdout/stderr rather than anywhere this test could inspect.
//
// The guest half asserts only the syscall return values; the role routing is
// asserted by AssertTerminalState in TestImpureCases.fs.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr SystemNative_Dup(IntPtr oldfd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Main(string[] args)
    {
        // "ab" through a dup of stdout, "z" through a dup of stderr. Distinct
        // payloads so the harness can tell the two streams apart even if the
        // routing collapsed them onto one.
        byte[] outPayload = new byte[] { (byte)'a', (byte)'b' };
        byte[] errPayload = new byte[] { (byte)'z' };

        IntPtr dupedOut = SystemNative_Dup((IntPtr)1);
        if ((long)dupedOut < 3L) return 1;

        IntPtr dupedErr = SystemNative_Dup((IntPtr)2);
        if ((long)dupedErr < 3L) return 2;

        fixed (byte* p = outPayload)
        {
            if (SystemNative_Write(dupedOut, p, 2) != 2) return 3;
        }

        fixed (byte* p = errPayload)
        {
            if (SystemNative_Write(dupedErr, p, 1) != 1) return 4;
        }

        return 0;
    }
}
