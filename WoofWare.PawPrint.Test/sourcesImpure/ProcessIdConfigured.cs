using System;
using System.Runtime.InteropServices;

// Reads the process id through Environment.ProcessId and writes it to stdout as
// four little-endian bytes, so that the F# registration (which configured it in
// the first place) can assert the exact value, as EffectiveUserIdConfigured.cs
// does for the uid.
//
// Through stdout rather than the exit code, which is only eight bits: a pid is
// an int32, and a handler that truncated it would be invisible to every
// registration whose pid fits in a byte.
//
// PawPrint-only: on a real runtime the answer is whatever pid the host handed
// out, which is exactly the host dependence the emulated kernel exists to
// remove. There is no oracle here.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPid")]
    static extern int GetPid();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Main(string[] args)
    {
        int pid = Environment.ProcessId;

        // CoreLib caches the first answer; the raw entry point asks again, so a
        // disagreement would mean the kernel's answer changed within the run.
        if (GetPid() != pid) return 1;

        // Shifted out by hand rather than via BitConverter, so that the byte
        // order is stated here and matched by an equally explicit expectation
        // on the F# side.
        byte[] observed = new byte[]
        {
            (byte)(pid & 0xFF),
            (byte)((pid >> 8) & 0xFF),
            (byte)((pid >> 16) & 0xFF),
            (byte)((pid >> 24) & 0xFF),
        };

        fixed (byte* p = observed)
        {
            if (Write((IntPtr)1, p, observed.Length) != observed.Length) return 2;
        }

        return 0;
    }
}
