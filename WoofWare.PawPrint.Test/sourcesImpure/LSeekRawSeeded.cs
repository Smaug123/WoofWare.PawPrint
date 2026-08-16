using System;
using System.Runtime.InteropServices;

// The rows of `SystemNative_LSeek`'s contract on which Linux and Darwin
// disagree. Everything portable is in `sourcesPure/ReadSeekSeeded.cs`, which is
// most of it — this file exists for two measured divergences:
//
//   1. **The order the checks run in.** Both platforms resolve the descriptor
//      first, and both do the offset arithmetic last, but they differ in the
//      middle: Linux validates `whence` before asking whether the object is
//      seekable, and Darwin the other way round. Measured:
//
//        input                        Linux    Darwin
//        pipe + whence 99             EINVAL   ESPIPE
//        pipe + whence 99 + overflow  EINVAL   ESPIPE
//
//      PawPrint simulates Linux.
//
//   2. **The overflow errno.** A computed offset that leaves `int64` is EINVAL
//      on Linux and EOVERFLOW (raw 84) on Darwin.
//
// A third divergence deliberately has *no* row here, because it is not one:
// `lseek(f, INT64_MAX, SEEK_SET)` succeeds on macOS and answers EINVAL on a
// Linux CI runner, which reads like a platform difference and is actually
// ext4's `s_maxbytes`. Measured, tmpfs — the honest analogue of PawPrint's
// in-memory filesystem — accepts the full `int64` range just as APFS does, so
// PawPrint accepts it too. Asserting it here would fail against the oracle on
// Linux for a reason having nothing to do with the kernel; `TestLSeek` pins it
// against the model instead.
//
// That same ceiling is why check 5 below is weaker evidence than it looks on a
// Linux host: real ext4 answers EINVAL there via its ceiling rather than via
// overflow. It still pins PawPrint against the platform whose errno it claims
// to report, which is what this file is for.
//
// errno is read via Marshal.GetLastSystemError rather than GetLastPInvokeError:
// with a raw DllImport there is no generated stub to copy one to the other.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Seed: f = "hello" (5 bytes).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int SEEK_SET = 0;
    const int SEEK_END = 2;

    // Linux numbering, which is what PawPrint reports.
    const int EINVAL = 22;

    static unsafe IntPtr OpenPath(string name)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }

    static bool Rejected(IntPtr fd, long offset, int whence, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        long r = LSeek(fd, offset, whence);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }

    static unsafe int Main(string[] args)
    {
        int check;

        IntPtr f = OpenPath("f");
        check = 1;
        if (f == new IntPtr(-1)) return check;

        // --- whence validity is decided before seekability ---

        // fd 0 is a pipe, so it is unseekable *and* the whence is nonsense.
        // Linux answers the whence; Darwin answers the seekability.
        check = 2;
        if (!Rejected(new IntPtr(0), 0, 99, EINVAL)) return check;
        check = 3;
        if (!Rejected(new IntPtr(1), 0, -1, EINVAL)) return check;
        // ...and it still wins when the offset would also have overflowed, so
        // this is about the whence rather than about which fault is "worse".
        check = 4;
        if (!Rejected(new IntPtr(0), long.MaxValue, 99, EINVAL)) return check;

        // --- the overflow errno ---

        // `f` is 5 bytes, so SEEK_END with INT64_MAX-4 lands one past
        // INT64_MAX. Linux EINVAL, Darwin EOVERFLOW.
        check = 5;
        if (!Rejected(f, long.MaxValue - 4, SEEK_END, EINVAL)) return check;
        check = 6;
        if (!Rejected(f, long.MaxValue, SEEK_END, EINVAL)) return check;
        // The failure left the offset alone, so the overflow was detected
        // rather than wrapped into a negative position.
        check = 7;
        if (LSeek(f, 0, SEEK_SET) != 0) return check;

        return 0;
    }
}
