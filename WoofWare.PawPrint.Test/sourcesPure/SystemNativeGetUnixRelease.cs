using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_GetUnixRelease PawPrint handler directly via a
// P/Invoke stub, mirroring the shape CoreLib's own [LibraryImport] generates
// (`() -> byte*`, StringMarshalling.Utf8).
//
// A direct stub rather than Environment.OSVersion, because that property only
// reaches this entry point on a *Linux* CoreLib: the macOS CoreLib implements
// Environment.GetOSVersion via Interop.libobjc.GetOperatingSystemVersion and
// does not even declare Interop.Sys.GetUnixRelease. The native shim exports
// the symbol on every Unix (it lives in pal_runtimeinformation.c), so the
// P/Invoke below resolves on both.
//
// This is a *pure* test, so it runs on the real CLR as well as under PawPrint.
// The release string therefore cannot be asserted exactly: on the host it is
// whatever `uname -r` prints, while PawPrint reports its own deterministic
// SimulatedUnixPlatform value. What is asserted is the contract that holds on
// both — a freshly-allocated, caller-owned, NUL-terminated printable-ASCII C
// string containing a version number.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetUnixRelease")]
    static extern unsafe byte* GetUnixRelease();

    // utsname.release is at most _UTSNAME_LENGTH - 1 = 64 bytes on Linux and
    // _SYS_NAMELEN - 1 = 255 bytes on macOS. Scanning past the NUL never
    // happens (the loop stops there), so this bound only decides how long we
    // are willing to look before declaring the string unterminated.
    const int MaxRelease = 256;

    // Returns the index of the terminating NUL, or -1 if none was found
    // within MaxRelease bytes, or -2 if a byte outside printable ASCII
    // (0x20..0x7E) appeared first. The release string is reported to managed
    // code as single-byte characters, so anything else would not round-trip.
    static unsafe int ValidatedLength(byte* p)
    {
        for (int i = 0; i < MaxRelease; i++)
        {
            byte b = p[i];
            if (b == 0) return i;
            if (b < 0x20 || b > 0x7E) return -2;
        }

        return -1;
    }

    static unsafe bool ContainsDigit(byte* p, int length)
    {
        for (int i = 0; i < length; i++)
        {
            if (p[i] >= (byte)'0' && p[i] <= (byte)'9') return true;
        }

        return false;
    }

    static unsafe int Main(string[] args)
    {
        byte* first = GetUnixRelease();
        if (first == null) return 1;

        int firstLength = ValidatedLength(first);
        if (firstLength == -1) return 2;  // no NUL within MaxRelease bytes
        if (firstLength == -2) return 3;  // non-printable-ASCII byte
        if (firstLength == 0) return 4;   // empty; every Unix fills utsname.release

        // Every release string a real uname produces carries a kernel version
        // number, and Environment.GetOperatingSystem parses exactly that out
        // of it. A release with no digits at all would silently degrade
        // Environment.OSVersion to 0.0.0.0.
        if (!ContainsDigit(first, firstLength)) return 5;

        // The native function strdups its result, so each call must hand back
        // a distinct, caller-owned allocation. Deliberately taken *before*
        // freeing `first`: two simultaneously-live allocations can never share
        // an address, whereas a pointer compared after a free could legally
        // be recycled to the same address by any allocator.
        byte* second = GetUnixRelease();
        if (second == null) return 6;
        if (second == first) return 7;

        // Both calls must observe the same value: the platform identity is
        // fixed for the lifetime of the process.
        int secondLength = ValidatedLength(second);
        if (secondLength != firstLength) return 8;

        for (int i = 0; i < firstLength; i++)
        {
            if (first[i] != second[i]) return 9;
        }

        // The pointers are ours to release. This is what CoreLib's
        // Utf8StringMarshaller.Free does with the return value, and it is the
        // step that proves the handler returned a genuine native-heap block
        // base rather than a byref into some other kind of storage.
        NativeMemory.Free(first);
        NativeMemory.Free(second);

        // Allocating again after the frees must still work.
        byte* third = GetUnixRelease();
        if (third == null) return 10;
        if (ValidatedLength(third) != firstLength) return 11;
        NativeMemory.Free(third);

        return 0;
    }
}
