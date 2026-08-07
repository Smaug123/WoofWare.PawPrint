using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_GetCwd PawPrint handler directly via a P/Invoke
// stub, mirroring the shape CoreLib's own [LibraryImport] generates
// (`(byte*, int) -> byte*`, SetLastError). The managed path that reaches it is
// covered by the sibling PathGetFullPathRelative.cs.
//
// This is a *pure* test, so it runs on the real CLR as well as under PawPrint.
// The directory therefore cannot be asserted exactly: on the host it is
// wherever the test runner happens to have been started, while PawPrint
// reports its deterministic KernelConfig.CurrentDirectory (the impure
// CurrentDirectoryConfigured.cs pins the *value*). What is asserted here is
// the contract that holds on both: the buffer-size trichotomy that CoreLib's
// growth loop depends on.
class Program
{
    // Errno is read back through Marshal.GetLastSystemError rather than
    // Marshal.GetLastPInvokeError, which is what CoreLib's own Interop.Sys.GetCwd
    // reads immediately after this call. PawPrint does not yet act on a P/Invoke's
    // SetLastError flag -- nothing copies the system error into the separate
    // last-P/Invoke-error slot -- so GetLastPInvokeError is not a seam the two
    // runtimes currently agree on. SetLastError is still declared, because on the
    // real runtime it is what makes the CLR preserve errno across the transition.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetCwd", SetLastError = true)]
    static extern unsafe byte* GetCwd(byte* buffer, int bufferSize);

    // Raw kernel errno values. A direct P/Invoke like this one skips CoreLib's
    // SystemNative_ConvertErrorPlatformToPal, so what Marshal.GetLastSystemError
    // reports is the raw value, not the Interop.Error PAL enum. Both Linux and
    // macOS define these identically.
    const int ERANGE = 34;
    const int EINVAL = 22;

    // Larger than any cwd a real test runner could plausibly have (Linux caps
    // paths at PATH_MAX = 4096), so the first call is guaranteed to fit.
    const int Big = 8192;

    static unsafe int StrLen(byte* p, int max)
    {
        for (int i = 0; i < max; i++)
        {
            if (p[i] == 0) return i;
        }

        return -1;
    }

    static unsafe int Main(string[] args)
    {
        byte* big = stackalloc byte[Big];

        // A buffer with ample room must succeed and hand back *the caller's own
        // pointer*, which is what getcwd(3) promises and what distinguishes a
        // faithful shim from one that allocates its own block.
        byte* result = GetCwd(big, Big);
        if (result == null) return 1;
        if (result != big) return 2;

        int length = StrLen(big, Big);
        if (length < 0) return 3;   // no NUL within the buffer
        if (length == 0) return 4;  // empty; getcwd never returns ""
        if (big[0] != (byte)'/') return 5;  // every cwd is absolute

        // A second call must observe the same directory: the cwd is fixed for
        // the lifetime of the process (neither runtime chdirs under us).
        byte* second = stackalloc byte[Big];
        if (GetCwd(second, Big) == null) return 6;
        if (StrLen(second, Big) != length) return 7;

        for (int i = 0; i < length; i++)
        {
            if (big[i] != second[i]) return 8;
        }

        // An exact fit is `length + 1`: the path plus its terminator. This is
        // the boundary CoreLib's growth loop turns on, so pin both sides of it.
        byte* exact = stackalloc byte[Big];
        if (GetCwd(exact, length + 1) == null) return 9;
        if (StrLen(exact, Big) != length) return 10;

        // One byte short must fail with ERANGE rather than truncating, because
        // ERANGE is precisely the signal Interop.Sys.GetCwd reads back to
        // decide to retry with a bigger buffer instead of throwing.
        byte* tooSmall = stackalloc byte[Big];
        if (GetCwd(tooSmall, length) != null) return 11;
        if (Marshal.GetLastSystemError() != ERANGE) return 12;

        // A zero-length buffer is EINVAL, not ERANGE: POSIX distinguishes
        // "you asked for nothing" from "your buffer is too small", and a guest
        // that conflated them would grow-and-retry forever.
        if (GetCwd(tooSmall, 0) != null) return 13;
        if (Marshal.GetLastSystemError() != EINVAL) return 14;

        // Deliberately no negative-bufferSize case: the native shim asserts
        // bufferSize >= 0 before returning EINVAL, so a checked build of
        // libSystem.Native would abort rather than return, and this test must
        // hold against whatever runtime the differential harness loads.

        return 0;
    }
}
