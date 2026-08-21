using System;
using System.IO;
using System.Runtime.InteropServices;

// The path to the executable that started the simulated process, as the guest
// sees it, plus the allocation contract of the entry point underneath.
//
// Impure rather than pure because the value asserted is PawPrint's own. The real
// runtime reports whatever launched the test host — the `dotnet` muxer for
// `dotnet guest.dll`, or the apphost for a published app, and never the guest's
// own `.dll` — so no differential oracle can pin it. Everything asserted below
// nevertheless holds on real .NET too, which is how this file's expected exit
// code was established rather than assumed.
//
// Deliberately hardcodes no path. The executable under test is whatever
// KernelConfig.ProcessPath was set to, and this program echoes it to stdout so
// that the F# registration (which chose it in the first place) can assert the
// exact bytes. That keeps one source of truth per case and lets one guest source
// cover every configuration TestImpureCases registers it under.
public class TestProcessPathConfigured
{
    // Deliberately raw and *unflagged*. Raw, because the allocation contract
    // below is invisible through Environment.ProcessPath, which caches its first
    // answer under an Interlocked.CompareExchange and so calls the entry point
    // exactly once per process. Unflagged, because a SetLastError = true import
    // makes the runtime stub zero errno before the call and capture it after,
    // which would overwrite the slot this file leaves alone.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetProcessPath")]
    static extern unsafe byte* GetProcessPath();

    // PATH_MAX is 4096 on Linux and 1024 on macOS; realpath cannot return more
    // than that, so scanning further only decides how long we are willing to
    // look before declaring the string unterminated. The loop stops at the NUL,
    // so this never reads past one.
    const int MaxPath = 4096;

    // Returns the index of the terminating NUL, or -1 if none was found within
    // MaxPath bytes.
    static unsafe int TerminatedLength(byte* p)
    {
        for (int i = 0; i < MaxPath; i++)
        {
            if (p[i] == 0) return i;
        }

        return -1;
    }

    public static unsafe int Main(string[] argv)
    {
        // The managed view first, because it is the thing a guest actually reads,
        // and because it is what gets echoed at the end.
        string managed = Environment.ProcessPath;

        if (managed is null) return 1;
        if (managed.Length == 0) return 2;
        if (!Path.IsPathRooted(managed)) return 3;

        // Cached by CoreLib, so this pins the caching rather than the handler —
        // but a handler that somehow answered differently per call would still
        // be caught by the byte-for-byte comparison of two raw calls below.
        if (Environment.ProcessPath != managed) return 4;

        // Now the entry point directly. Each call must hand back a distinct
        // caller-owned allocation, because every flavour of minipal_getexepath
        // ends in realpath(..., NULL) or strdup, all of which malloc.
        byte* first = GetProcessPath();
        if (first == null) return 5;

        int firstLength = TerminatedLength(first);
        if (firstLength == -1) return 6;   // no NUL within MaxPath bytes
        if (firstLength == 0) return 7;    // empty; realpath never returns ""
        if (first[0] != (byte)'/') return 8;

        // Taken *before* freeing `first`: two simultaneously-live allocations can
        // never share an address, whereas a pointer compared after a free could
        // legally be recycled to the same address by any allocator.
        byte* second = GetProcessPath();
        if (second == null) return 9;
        if (second == first) return 10;

        // Both calls must observe the same value: the executable path is fixed
        // for the lifetime of the process.
        if (TerminatedLength(second) != firstLength) return 11;

        for (int i = 0; i < firstLength; i++)
        {
            if (first[i] != second[i]) return 12;
        }

        // The pointers are ours to release. This is what the generated
        // Utf8StringMarshaller.Free does with the return value inside CoreLib's
        // own [LibraryImport] wrapper, and it is the step that proves the handler
        // returned a genuine native-heap block base rather than a byref into some
        // other kind of storage — SystemNative_Free refuses the latter.
        NativeMemory.Free(first);
        NativeMemory.Free(second);

        // Allocating again after the frees must still work.
        byte* third = GetProcessPath();
        if (third == null) return 13;
        if (TerminatedLength(third) != firstLength) return 14;
        NativeMemory.Free(third);

        // Echoed for the F# side to compare against the configured value. Only
        // reached once every property above has held, so a failure here is
        // unambiguously about the *value*.
        Console.Out.Write(managed);

        return 0;
    }
}
