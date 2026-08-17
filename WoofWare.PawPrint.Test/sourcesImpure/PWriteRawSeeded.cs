using System;
using System.IO;
using System.Runtime.InteropServices;

// The parts of the write path that a differential test cannot arbitrate. What is
// portable — the counts, the hole past the end, the untouched file after a
// zero-length write, EBADF for the wrong access mode, EINVAL for a negative
// offset, EISDIR for a directory, ESPIPE for a pipe — is in
// `sourcesPure/WriteSeeded.cs`. Three things are left, for three different
// reasons:
//
//   1. **fd 0 is where the platforms disagree.** PawPrint models the standard
//      streams as pipes, so stdin is a pipe's *read* end: neither seekable nor
//      open for writing. Measured, the two kernels break that tie differently,
//      and identically to how they break it for `pread`:
//
//        descriptor                        Linux    Darwin
//        pipe write end (unseekable)       ESPIPE   ESPIPE
//        pipe read end (also unwritable)   ESPIPE   EBADF
//        regular file O_RDONLY (seekable)  EBADF    EBADF
//
//      PawPrint simulates Linux. The third row is the control showing this is
//      about the tie rather than about writability generally, and it is portable,
//      so it lives in the pure half.
//
//   2. **Everything that turns on a file's mode depends on who is asking.** A
//      privileged process bypasses these rules entirely — measured, root opens a
//      mode-0000 file for writing — and this suite does not choose the uid its
//      oracle runs as, so a differential case would assert whatever privilege the
//      machine happened to give it. PawPrint's uid is configuration
//      (`KernelConfig.UserId`, 1000 by default), which is what makes these rows
//      statable at all. The facts were measured identically on macOS 26.6 and
//      Linux 6.18.5 at uid 1000, and every row inverts at uid 0:
//
//        mode   O_RDONLY  O_WRONLY  O_RDWR
//        0644   ok        ok        ok
//        0444   ok        EACCES    EACCES
//        0200   EACCES    ok        EACCES
//
//   3. **The timestamps need a deterministic clock.** A write moves mtime and
//      ctime and leaves atime alone (measured on both), but "moved" is only
//      sharply observable against a clock that advances by construction. On a
//      real filesystem a write can land inside the same granule as the one
//      before it, so the same assertion would be racy rather than wrong.
//
// errno is read via Marshal.GetLastSystemError rather than GetLastPInvokeError:
// with a raw DllImport there is no generated stub to copy one to the other.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello" (0644), ro = "hello" (0444),
// wo = "hello" (0200).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PWrite", SetLastError = true)]
    static extern unsafe int PWrite(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_RDWR = 0x0002;

    // Linux numbering, which is what PawPrint reports.
    const int EBADF = 9;
    const int EACCES = 13;
    const int EINVAL = 22;
    const int ESPIPE = 29;

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0);
    }

    static unsafe bool RejectedOpen(string name, int flags, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        IntPtr fd = OpenPath(name, flags);
        return fd == new IntPtr(-1) && Marshal.GetLastSystemError() == expectedErrno;
    }

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[8];
        buf[0] = (byte)'A';

        // --- 1: the descriptor the two platforms disagree about ---

        // stdin. Linux's answer, unseekability winning over unwritability.
        check = 1;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(0), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != ESPIPE) return check;

        // ...and it is still ESPIPE with nothing to transfer, so this is the
        // descriptor's kind rather than anything about the bytes.
        check = 2;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(0), buf, 0, 0) != -1 || Marshal.GetLastSystemError() != ESPIPE) return check;

        // A negative offset still beats it, as it beats everything else.
        check = 3;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(0), buf, 1, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // Plain `write` has no seekability requirement, so stdin's *access mode*
        // is the whole answer there — EBADF on both platforms, and the row that
        // shows the ESPIPE above is about `pwrite` specifically.
        check = 4;
        Marshal.SetLastSystemError(0);
        if (Write(new IntPtr(0), buf, 1) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // --- 2: the file's own permission bits ---

        // A file the process may read but not write.
        check = 5;
        if (OpenPath("ro", O_RDONLY) == new IntPtr(-1)) return check;
        check = 6;
        if (!RejectedOpen("ro", O_WRONLY, EACCES)) return check;
        check = 7;
        if (!RejectedOpen("ro", O_RDWR, EACCES)) return check;

        // ...and the converse, which is what shows the check consults the bit the
        // access mode needs rather than merely "is anything missing".
        check = 8;
        if (OpenPath("wo", O_WRONLY) == new IntPtr(-1)) return check;
        check = 9;
        if (!RejectedOpen("wo", O_RDONLY, EACCES)) return check;
        check = 10;
        if (!RejectedOpen("wo", O_RDWR, EACCES)) return check;

        // The control: a default-mode file grants all three.
        check = 11;
        if (OpenPath("f", O_RDONLY) == new IntPtr(-1)) return check;
        check = 12;
        if (OpenPath("f", O_WRONLY) == new IntPtr(-1)) return check;
        check = 13;
        if (OpenPath("f", O_RDWR) == new IntPtr(-1)) return check;

        // --- 3: the timestamps a write moves ---

        IntPtr w = OpenPath("f", O_WRONLY);
        check = 14;
        if (w == new IntPtr(-1)) return check;

        DateTime modifiedBefore = File.GetLastWriteTimeUtc("f");
        DateTime accessedBefore = File.GetLastAccessTimeUtc("f");
        DateTime directoryBefore = Directory.GetLastWriteTimeUtc(".");

        // A zero-length write is not a write: nothing moves, even though the call
        // succeeds.
        check = 15;
        if (PWrite(w, buf, 0, 0) != 0) return check;
        check = 16;
        if (File.GetLastWriteTimeUtc("f") != modifiedBefore) return check;

        check = 17;
        if (PWrite(w, buf, 1, 0) != 1) return check;

        // mtime moves forward. Strictly: the virtual clock advances as the
        // interpreter runs, so a stamp taken after the write cannot equal one
        // taken before it.
        check = 18;
        if (File.GetLastWriteTimeUtc("f") <= modifiedBefore) return check;

        // atime does not: nothing read the file. This is the row that fails if a
        // write is implemented by restamping every timestamp at once.
        check = 19;
        if (File.GetLastAccessTimeUtc("f") != accessedBefore) return check;

        // The file's *birth* time is deliberately not asserted here, and cannot
        // be: this kernel's flavour is Linux, whose `stat` does not report one
        // (`pal_io.c` zeroes it under `#else`), so CoreLib falls back to
        // `min(mtime, ctime)` — which a write moves by design. `File.GetCreationTimeUtc`
        // would therefore report the write's own timestamp on any correct
        // implementation. That birth time does not move is pinned against the
        // model instead, in `TestVirtualFileSystem`.

        // The directory holding it is untouched: the set of names it binds did not
        // change, so neither did its mtime. Captured before the write rather than
        // compared with itself, which would pass whatever the implementation did.
        check = 20;
        if (Directory.GetLastWriteTimeUtc(".") != directoryBefore) return check;

        // --- 4: the set-ID bits a content-changing write strips ---
        //
        // Measured non-root on macOS 26.6 and Linux 6.18.5, and as root on Linux:
        // an unprivileged write clears set-user-ID, and set-group-ID when the file
        // is group-executable, while root keeps both and the sticky bit survives
        // either way. Impure for the same reason as the rows above — root keeps
        // the bits, and the oracle's privilege is not this suite's to choose.
        check = 21;
        if (File.GetUnixFileMode("suid") != (UnixFileMode.SetUser
                                             | UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                             | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                             | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;

        IntPtr s = OpenPath("suid", O_WRONLY);
        check = 22;
        if (s == new IntPtr(-1)) return check;

        // A zero-length write is not a content change, so it strips nothing.
        check = 23;
        if (PWrite(s, buf, 0, 0) != 0) return check;
        check = 24;
        if ((File.GetUnixFileMode("suid") & UnixFileMode.SetUser) == 0) return check;

        check = 25;
        if (PWrite(s, buf, 1, 0) != 1) return check;
        check = 26;
        if ((File.GetUnixFileMode("suid") & UnixFileMode.SetUser) != 0) return check;

        // Only those bits: the ordinary permission triples come through unharmed.
        check = 27;
        if (File.GetUnixFileMode("suid") != (UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                                             | UnixFileMode.GroupRead | UnixFileMode.GroupExecute
                                             | UnixFileMode.OtherRead | UnixFileMode.OtherExecute)) return check;

        // The sticky bit is not a privilege bit and is left alone, which is what
        // shows the mask is the measured one rather than "clear the top three".
        IntPtr t = OpenPath("sticky", O_WRONLY);
        check = 28;
        if (t == new IntPtr(-1)) return check;
        check = 29;
        if (PWrite(t, buf, 1, 0) != 1) return check;
        check = 30;
        if ((File.GetUnixFileMode("sticky") & UnixFileMode.StickyBit) == 0) return check;

        // A `write` strips them exactly as a `pwrite` does — the rule is about the
        // content changing, not about which syscall changed it.
        IntPtr g = OpenPath("sgid", O_WRONLY);
        check = 31;
        if (g == new IntPtr(-1)) return check;
        check = 32;
        if ((File.GetUnixFileMode("sgid") & UnixFileMode.SetGroup) == 0) return check;
        check = 33;
        if (Write(g, buf, 1) != 1) return check;
        check = 34;
        if ((File.GetUnixFileMode("sgid") & UnixFileMode.SetGroup) != 0) return check;

        return 0;
    }
}
