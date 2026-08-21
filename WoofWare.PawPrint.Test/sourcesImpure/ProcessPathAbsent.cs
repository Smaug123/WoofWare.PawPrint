using System;
using System.Runtime.InteropServices;

// A simulated process with no executable path: KernelConfig.ProcessPath = None,
// which is PawPrint's default.
//
// Impure because the *state* is PawPrint-only. A real Unix process reaches it
// only by having its executable unlinked from under it, which is how the errno
// asserted here was measured: on macOS arm64 and on Linux arm64, a guest that
// deletes its own executable before its first read sees a NULL return, errno 2
// (ENOENT), and a null Environment.ProcessPath. Both flavours agree, so the
// numbers below are measurements rather than choices.
public class TestProcessPathAbsent
{
    // Deliberately *unflagged*. With SetLastError = true the runtime's stub
    // zeroes errno before the call and captures it afterwards, so the errno read
    // below would see the stub's work rather than the handler's. Unflagged, the
    // slot is untouched by the marshalling layer and Marshal.GetLastSystemError
    // reads errno itself.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetProcessPath")]
    static extern unsafe byte* GetProcessPath();

    public static unsafe int Main(string[] argv)
    {
        // A sentinel, so that "the handler wrote ENOENT" is distinguishable from
        // "nothing wrote anything and the slot happened to be 2".
        Marshal.SetLastSystemError(4242);

        byte* raw = GetProcessPath();

        // Captured immediately: allocation or string formatting in between can
        // trash errno, and CoreCLR's own source warns about exactly that.
        int errno = Marshal.GetLastSystemError();

        if (raw != null) return 1;

        // ENOENT, which is 2 on both flavours. Not 4242, which would mean the
        // handler returned NULL without reporting a reason — something no
        // flavour of minipal_getexepath does.
        if (errno != 2) return 2;

        // A second call must answer the same way: the absence is a property of
        // the process, not a one-shot failure.
        Marshal.SetLastSystemError(4242);
        if (GetProcessPath() != null) return 3;
        if (Marshal.GetLastSystemError() != 2) return 4;

        // And the managed view. This must come last: Environment.ProcessPath
        // reaches the entry point through a SetLastError = true [LibraryImport],
        // whose stub zeroes and then rewrites the very slot asserted above.
        if (Environment.ProcessPath is not null) return 5;

        // Read twice. The first read stored `GetProcessPath() ?? ""` in CoreLib's
        // cache, so this one takes the cached-value branch and never reaches the
        // entry point: it pins that a cached absence still reports as null,
        // rather than surfacing the empty string the cache actually holds.
        if (Environment.ProcessPath is not null) return 6;

        return 0;
    }
}
