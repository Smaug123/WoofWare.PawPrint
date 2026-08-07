using System;
using System.Runtime.InteropServices;

// SystemNative_GetCwd must decide its error returns *without* dereferencing the
// caller's buffer, which is what the C does: the negative-size guard runs before
// getcwd(3) is called at all, and getcwd validates the size and compares it
// against the path length before writing a byte. So every call below hands it a
// pointer that addresses nothing, and must still get the documented errno back.
//
// Impure rather than pure for safety, not for determinism: the differential
// harness runs pure guests in-process on the real CLR, and while these exact
// calls do return errors there without touching the pointer, a regression that
// made one of them *write* would corrupt the test host rather than fail a test.
// Under PawPrint the pointer is never resolved to storage on these paths, which
// is precisely the property being pinned. The cross-runtime half of the
// contract lives in the pure sibling SystemNativeGetCwd.cs.
public class TestGetCwdNoDereferenceErrors
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCwd", SetLastError = true)]
    static extern unsafe byte* GetCwd(byte* buffer, int bufferSize);

    const int ERANGE = 34;
    const int EINVAL = 22;

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

        return 0;
    }
}
