using System;
using System.Runtime.InteropServices;

// `SystemNative_PosixFAdvise` on a **Darwin**-configured kernel, where there is
// no such call to make: measured, macOS 26.6's libc exports no `posix_fadvise`
// symbol, so the shim compiles its body out under `HAVE_POSIX_ADVISE` and
// answers ENOTSUP without looking at anything it was passed.
//
// So every row below is the same number, and that is the point: each one is an
// input its Linux twin answers *differently*, and a handler that screened the
// descriptor, the advice or the length before consulting the configured
// platform would fail one of them. See that twin for why the two are a pair.
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

    // Darwin numbers ENOTSUP 45, where Linux gives 95 to it and to EOPNOTSUPP
    // alike. Nothing here can produce Linux's number, since nothing here reaches
    // a kernel that has the call.
    const int NotSupported = 45;

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

        // Every advice value the Linux shim accepts, on a regular file: there it
        // is success, here there is no call to succeed.
        for (int advice = 0; advice <= 5; advice++)
        {
            int result = Check(2 + advice, PosixFAdvise(fd, 0, 0, advice), NotSupported);
            if (result != 0 && failure == 0) failure = result;
        }

        IntPtr closed = OpenPath("f", O_RDONLY);
        Close(closed);

        (int Index, int Actual, int Expected)[] rows =
        {
            // EBADF on Linux: the descriptor is not looked at here.
            (8, PosixFAdvise(closed, 0, 0, 0), NotSupported),
            // ESPIPE on Linux, this kernel modelling the standard streams as
            // pipe ends.
            (9, PosixFAdvise(IntPtr.Zero, 0, 0, 0), NotSupported),
            (10, PosixFAdvise(new IntPtr(1), 0, 0, 0), NotSupported),
            // EINVAL on Linux: the advice screen sits inside the compiled-out
            // body, so it is absent here too.
            (11, PosixFAdvise(fd, 0, 0, 6), NotSupported),
            (12, PosixFAdvise(fd, 0, 0, -1), NotSupported),
            // EINVAL on Linux.
            (13, PosixFAdvise(fd, 0, -1, 0), NotSupported),
            (14, PosixFAdvise(fd, 0, long.MinValue, 0), NotSupported),
            // Success on Linux.
            (15, PosixFAdvise(fd, -1, 0, 0), NotSupported),
            (16, PosixFAdvise(fd, long.MinValue, 0, 0), NotSupported),
            (17, PosixFAdvise(fd, long.MaxValue, long.MaxValue, 0), NotSupported),
            // Each of Linux's three orderings between two wrong arguments.
            (18, PosixFAdvise(closed, 0, 0, 6), NotSupported),
            (19, PosixFAdvise(closed, 0, -1, 0), NotSupported),
            (20, PosixFAdvise(IntPtr.Zero, 0, -1, 0), NotSupported),
            // A descriptor argument that is not a descriptor number at all, on
            // a platform whose shim has no body to read it with. A runtime
            // handle is a value this interpreter tracks the provenance of and
            // cannot turn into an integer, so these rows can tell "not read"
            // from "read and found absent" — with a valid advice as well as an
            // invalid one, since here not even the advice is screened.
            (21, PosixFAdvise(typeof(int).TypeHandle.Value, 0, 0, 6), NotSupported),
            (22, PosixFAdvise(typeof(int).TypeHandle.Value, 0, 0, 0), NotSupported),
            (23, PosixFAdvise(typeof(int).TypeHandle.Value, 0, -1, 2), NotSupported),
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
