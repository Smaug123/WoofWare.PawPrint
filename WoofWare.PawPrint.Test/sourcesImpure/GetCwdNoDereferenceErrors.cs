using System;
using System.Runtime.InteropServices;

// SystemNative_GetCwd must report its failures to the guest rather than abort
// the interpreter, for every buffer pointer that addresses nothing.
//
// Three of the calls below are decided *without* dereferencing the buffer at
// all, which is what the C does: the negative-size guard runs before getcwd(3)
// is called, and getcwd validates the size and compares it against the path
// length before writing a byte. The last one does get as far as writing, and
// faults — EFAULT — as SystemNative_Write already models for the same shape of
// pointer.
//
// Impure rather than pure for safety, not for determinism: the differential
// harness runs pure guests in-process on the real CLR, where the last call
// deliberately asks the kernel to write to an unmapped address. That is exactly
// the case a real kernel refuses with EFAULT, but it is not something to hand a
// process whose crash would take the test host with it. Under PawPrint no write
// can escape the simulated address space at all. The cross-runtime half of this
// entry point's contract lives in the pure sibling SystemNativeGetCwd.cs.
public class TestGetCwdNoDereferenceErrors
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCwd", SetLastError = true)]
    static extern unsafe byte* GetCwd(byte* buffer, int bufferSize);

    const int ERANGE = 34;
    const int EINVAL = 22;
    const int EFAULT = 14;

    // Not a valid address, and deliberately not one PawPrint could resolve to a
    // cell even in principle: a bare integer with no managed provenance.
    static unsafe byte* Bogus => (byte*)123;

    static unsafe int Main(string[] argv)
    {
        // Negative size: rejected by the shim's own guard, before getcwd.
        Marshal.SetLastSystemError(0);
        if (GetCwd(Bogus, -1) != null) return 1;
        if (Marshal.GetLastSystemError() != EINVAL) return 2;

        // Zero size with a non-null buffer: EINVAL from getcwd itself.
        Marshal.SetLastSystemError(0);
        if (GetCwd(Bogus, 0) != null) return 3;
        if (Marshal.GetLastSystemError() != EINVAL) return 4;

        // Too small for the path plus its NUL: ERANGE, again without writing.
        // 1 byte can never suffice — the shortest possible cwd is "/", which
        // needs 2 with its terminator.
        Marshal.SetLastSystemError(0);
        if (GetCwd(Bogus, 1) != null) return 5;
        if (Marshal.GetLastSystemError() != ERANGE) return 6;

        // Big enough that the size checks all pass, so the kernel really does
        // try to write — and faults, because the address is not mapped. Note
        // this is *after* the ERANGE case above: the size is checked before the
        // buffer is touched, so a too-small unmapped buffer is ERANGE, not
        // EFAULT. Both must be reported to the guest rather than aborting the
        // interpreter, exactly as SystemNative_Write already does.
        Marshal.SetLastSystemError(0);
        if (GetCwd(Bogus, 8192) != null) return 7;
        if (Marshal.GetLastSystemError() != EFAULT) return 8;

        return 0;
    }
}
