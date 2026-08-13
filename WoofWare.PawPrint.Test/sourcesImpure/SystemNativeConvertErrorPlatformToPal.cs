using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_ConvertErrorPlatformToPal handler, which turns a
// raw <errno.h> number into the platform-independent Interop.Error value that
// CoreLib actually switches on. Every BCL failure path goes through this:
// Interop.Sys.GetLastErrorInfo() constructs ErrorInfo(Marshal.GetLastPInvokeError()),
// whose constructor calls it.
//
// This is an *impure* test: it runs only inside PawPrint, never against the
// real CLR. It could not be a cross-runtime test even in principle. The PAL
// values are platform-independent, but the *mapping* is not: the real shim is
// compiled against one platform's <errno.h>, so on macOS raw 39 is
// EDESTADDRREQ while on Linux it is ENOTEMPTY. PawPrint deliberately models
// only the errnos that mean the same thing on every Unix it models, so an
// oracle comparison would be asserting a host-specific fact.
//
// Two declarations of the same entry point, deliberately: CoreLib declares the
// return as the `Interop.Error` enum — nested in a class in the *global*
// namespace — while a guest hand-rolling the P/Invoke would naturally write
// `int`. Both must reach the handler, so both are covered here. `Codes.Error`
// below is nested in the global namespace exactly as `Interop.Error` is, which
// is what makes this test able to exercise the enum arm at all: the real
// `Interop.Error` is internal to CoreLib and cannot be named from a guest.
class Program
{
    // Mirrors the shape of CoreLib's Interop.Error: a nested enum, global
    // namespace, int underlying type. Only the members this test needs.
    internal static class Codes
    {
        internal enum Error
        {
            SUCCESS = 0,
            EBADF = 0x10008,
            ENOENT = 0x1002D,
            ERANGE = 0x10047,
        }
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern Codes.Error ConvertToPalEnum(int platformErrno);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertToPalInt(int platformErrno);

    static int Main(string[] args)
    {
        // Raw 0 is "no error", and maps to Interop.Error.SUCCESS (also 0),
        // which is the value CoreLib's `ErrorInfo` treats as "call succeeded".
        if (ConvertToPalEnum(0) != Codes.Error.SUCCESS) return 1;

        // The three portable errnos PawPrint's own handlers already report.
        // These are deliberately spread across the errno range rather than
        // adjacent, so an off-by-one or truncated table shows up.
        if (ConvertToPalEnum(2) != Codes.Error.ENOENT) return 2;
        if (ConvertToPalEnum(9) != Codes.Error.EBADF) return 3;
        if (ConvertToPalEnum(34) != Codes.Error.ERANGE) return 4;

        // The same entry point declared with an `int` return must agree; this
        // covers the other arm of the handler's return-type match.
        if (ConvertToPalInt(2) != (int)Codes.Error.ENOENT) return 5;
        if (ConvertToPalInt(0) != 0) return 6;

        // PAL values are deliberately numbered outside the raw errno range so
        // the two cannot be confused. Guard that we are genuinely converting
        // rather than echoing the input back.
        if (ConvertToPalInt(9) == 9) return 7;

        return 0;
    }
}
