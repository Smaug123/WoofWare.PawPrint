using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_Write success path under PawPrint. This is in
// `sourcesImpure` because the assertion is on PawPrint-internal state
// (`EmulatedKernel.StdoutAppended`) rather than on the guest's exit code
// alone; the test harness reads that buffer after the run terminates and
// matches against the expected byte sequence.
//
// The pure-source `SystemNativeWrite.cs` covers the error paths (negative
// size, bad fd, zero-size no-op) that can be asserted just on the return
// value, since those don't pollute the real test runner's stdout.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Main(string[] args)
    {
        // Write the literal three-byte sequence "hi\n" to fd 1 (stdout).
        // PawPrint must decode the byte* argument correctly and the handler
        // must return the number of bytes written (3); any deviation
        // surfaces as a non-zero exit code below, before the harness even
        // gets to inspect `StdoutAppended`.
        byte[] msg = new byte[] { 0x68, 0x69, 0x0A };

        fixed (byte* p = msg)
        {
            int written = SystemNative_Write((IntPtr)1, p, msg.Length);
            if (written != msg.Length) return 1;
        }

        return 0;
    }
}
