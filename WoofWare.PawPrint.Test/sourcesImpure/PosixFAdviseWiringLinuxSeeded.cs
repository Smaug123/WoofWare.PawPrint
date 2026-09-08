using System;
using System.Runtime.InteropServices;

// `SystemNative_PosixFAdvise` on a **Linux**-configured kernel: the raw value it
// returns for each descriptor kind, each advice and each range.
//
// PawPrint-only, because the returned number is exactly what the two flavours
// disagree about. macOS's libc exports no `posix_fadvise` at all, so the shim
// compiles its body out and answers ENOTSUP for every input; a differential run
// would compare PawPrint's configured kernel against whichever kernel happened
// to run the oracle. `sourcesPure/PosixFAdviseSeeded.cs` carries what the two
// platforms do agree about, which is only that the BCL's own caller ignores the
// answer.
//
// This file and its Darwin twin exist as a **pair**, and neither alone is
// enough: `TestPosixFadvise` hands the library its platform explicitly and
// `TestFileAdvicePal` covers the advice screen on its own, so a handler that
// ignored `SimulatedUnixPlatform.providesPosixFadvise` and hardcoded either
// answer would satisfy all of those plus one of these two.
//
// Unlike its neighbours this entry point does not touch errno at all (the
// managed declaration says `SetLastError = false`): the error number *is* the
// return value, and 0 is success.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): `f`, holding the five bytes "hello".
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PosixFAdvise", SetLastError = false)]
    static extern int PosixFAdvise(IntPtr fd, long offset, long length, int advice);

    const int O_RDONLY = 0x0000;

    // What a Linux kernel answers. The Darwin twin differs from this file in
    // exactly these constants and its `KernelConfig`.
    const int Ok = 0;
    const int BadFd = 9;          // EBADF
    const int Invalid = 22;       // EINVAL
    const int IsPipe = 29;        // ESPIPE
    // The shim's advice screen runs before the syscall, so a malformed advice
    // beats a bad descriptor here even though the kernel would answer EBADF.
    const int BadFdBadAdvice = Invalid;
    // The length screen is the kernel's, and so runs after the descriptor
    // lookup.
    const int BadFdBadLength = BadFd;

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[32];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0x1B6 /* 0o666 */);
    }

    static int Check(int index, int actual, int expected)
    {
        if (actual == expected) return 0;
        Console.WriteLine($"check {index}: expected {expected}, got {actual}");
        return index;
    }

    static unsafe int Main(string[] args)
    {
        IntPtr fd = OpenPath("f", O_RDONLY);
        if (fd == new IntPtr(-1))
        {
            Console.WriteLine("could not open the seeded file");
            return 1;
        }

        int failure = 0;

        // Every advice value the shim accepts, on a regular file.
        for (int advice = 0; advice <= 5; advice++)
        {
            int result = Check(2 + advice, PosixFAdvise(fd, 0, 0, advice), Ok);
            if (result != 0 && failure == 0) failure = result;
        }

        IntPtr closed = OpenPath("f", O_RDONLY);
        Close(closed);

        (int Index, int Actual, int Expected)[] rows =
        {
            // A descriptor that was open and is not any more.
            (8, PosixFAdvise(closed, 0, 0, 0), BadFd),
            // Standard input, which this kernel models as one end of a pipe.
            (9, PosixFAdvise(IntPtr.Zero, 0, 0, 0), IsPipe),
            (10, PosixFAdvise(new IntPtr(1), 0, 0, 0), IsPipe),
            // Advice outside the six the shim knows.
            (11, PosixFAdvise(fd, 0, 0, 6), Invalid),
            (12, PosixFAdvise(fd, 0, 0, -1), Invalid),
            // A negative length, which the kernel screens.
            (13, PosixFAdvise(fd, 0, -1, 0), Invalid),
            (14, PosixFAdvise(fd, 0, long.MinValue, 0), Invalid),
            // The offset is never screened, however absurd.
            (15, PosixFAdvise(fd, -1, 0, 0), Ok),
            (16, PosixFAdvise(fd, long.MinValue, 0, 0), Ok),
            (17, PosixFAdvise(fd, long.MaxValue, long.MaxValue, 0), Ok),
            // Which screen wins when two arguments are wrong at once.
            (18, PosixFAdvise(closed, 0, 0, 6), BadFdBadAdvice),
            (19, PosixFAdvise(closed, 0, -1, 0), BadFdBadLength),
            (20, PosixFAdvise(IntPtr.Zero, 0, -1, 0), IsPipe),
            // A descriptor argument that is not a descriptor number at all. The
            // shim's advice screen runs before it touches the descriptor, so an
            // unknown advice answers EINVAL without the argument being read —
            // and a runtime handle is a value this interpreter tracks the
            // provenance of and cannot turn into an integer, so it can tell
            // "not read" from "read and found absent".
            (21, PosixFAdvise(typeof(int).TypeHandle.Value, 0, 0, 6), Invalid),
        };

        foreach (var row in rows)
        {
            int result = Check(row.Index, row.Actual, row.Expected);
            if (result != 0 && failure == 0) failure = result;
        }

        Close(fd);
        return failure;
    }
}
