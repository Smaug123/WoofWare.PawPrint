using System;
using System.IO;
using System.Runtime.InteropServices;

// `SystemNative_FTruncate` and `O_TRUNC`: a file's length changing, which is the
// second thing in PawPrint that modifies the filesystem (after `SystemNative_Write`).
//
// Pure, and that is a measured claim rather than an assumption. Every row below
// was measured identically on Linux 6.18.5 (as uid 0 *and* uid 1000) and macOS
// 26.6: the errno order, which descriptors refuse a truncation, the zero fill an
// extension leaves, and every `O_TRUNC` row that does not turn on a file's mode.
//
// What is *not* portable stays out of this file:
//  - the set-user-ID and set-group-ID bits, which Linux clears on a truncation
//    and macOS leaves alone. That lives in `sourcesImpure/TruncateWiring{Linux,
//    Darwin}Seeded.cs`, one per flavour, because it is also uid-dependent — a
//    privileged process strips nothing on either kernel, and this suite does not
//    choose the uid its oracle runs as.
//  - everything else that turns on a file's mode, for that same uid reason: the
//    EACCES `O_TRUNC` owes a file it may not write is in the impure half too.
//  - the timestamps a truncation moves, which need a deterministic clock to
//    state without racing a real filesystem's granularity.
//
// `O_APPEND` is deliberately absent: there is no `PAL_O_APPEND` (the PAL's flag
// set stops at `NOFOLLOW`, pal_io.h), CoreLib emulates append in managed code,
// and a guest passing the platform's own bit gets EINVAL from the unknown-flag
// check. Do not add a row for it.
//
// errno is read via Marshal.GetLastSystemError rather than GetLastPInvokeError:
// with a raw DllImport there is no generated stub to copy one to the other.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases): f = g = h = "hello" (5 bytes), d = a directory,
// lf -> f is a symlink.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FTruncate", SetLastError = true)]
    static extern int FTruncate(IntPtr fd, long length);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    // `Interop.Sys.OpenFlags`, the PAL's portable numbering rather than any
    // platform's <fcntl.h>.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_RDWR = 0x0002;
    const int O_CREAT = 0x0020;
    const int O_EXCL = 0x0040;
    const int O_TRUNC = 0x0080;
    const int O_NOFOLLOW = 0x0200;

    const int SEEK_SET = 0;
    const int SEEK_CUR = 1;

    // Linux numbering, which is what PawPrint reports; every errno asserted here
    // has the same number on macOS.
    const int EBADF = 9;
    const int EEXIST = 17;
    const int EISDIR = 21;
    const int EINVAL = 22;

    static unsafe IntPtr OpenPath(string name, int flags)
    {
        byte* path = stackalloc byte[32];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, flags, 0x1B6 /* 0o666 */);
    }

    static unsafe bool RejectedOpen(string name, int flags, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        IntPtr fd = OpenPath(name, flags);
        if (fd != new IntPtr(-1)) { Close(fd); return false; }
        return Marshal.GetLastSystemError() == expectedErrno;
    }

    /// Refused, without saying with what. For the one row whose errno is *not*
    /// portable: ELOOP is 40 on Linux and 62 on macOS, and `UnixError` refuses to
    /// pick a number for exactly that reason. The number PawPrint reports is
    /// pinned in the impure half, where the configured flavour decides it; here
    /// the claim is only that the open failed and left the file alone.
    static unsafe bool RefusedOpen(string name, int flags)
    {
        IntPtr fd = OpenPath(name, flags);
        if (fd != new IntPtr(-1)) { Close(fd); return false; }
        return true;
    }

    /// Whether the whole of `name` reads back as `expected`, and is exactly that
    /// long. Opens its own descriptor each time, so it cannot be fooled by an
    /// offset the caller happened to leave somewhere.
    static unsafe bool Holds(string name, byte[] expected)
    {
        IntPtr fd = OpenPath(name, O_RDONLY);
        if (fd == new IntPtr(-1)) return false;

        byte* got = stackalloc byte[64];
        int n = PRead(fd, got, 64, 0);
        Close(fd);

        if (n != expected.Length) return false;
        for (int i = 0; i < expected.Length; i++)
        {
            if (got[i] != expected[i]) return false;
        }
        return true;
    }

    static byte[] Bytes(string s)
    {
        byte[] result = new byte[s.Length];
        for (int i = 0; i < s.Length; i++) result[i] = (byte)s[i];
        return result;
    }

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[64];
        buf[0] = (byte)'Z';

        // --- ftruncate's refusals, in the order the kernel makes them ---

        IntPtr w = OpenPath("f", O_WRONLY);
        check = 1;
        if (w == new IntPtr(-1)) return check;

        // A negative length, which is checked before anything else.
        check = 2;
        Marshal.SetLastSystemError(0);
        if (FTruncate(w, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;
        check = 3;
        if (!Holds("f", Bytes("hello"))) return check;

        // An unknown descriptor.
        check = 4;
        Marshal.SetLastSystemError(0);
        if (FTruncate(new IntPtr(4242), 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // ...and the pair that shows the length really is validated *before* the
        // descriptor is looked up: the same bad fd with a negative length is
        // EINVAL, not EBADF.
        check = 5;
        Marshal.SetLastSystemError(0);
        if (FTruncate(new IntPtr(4242), -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // A descriptor not open for writing is EINVAL — `ftruncate` differs from
        // `write` here, which answers EBADF for the same shape.
        IntPtr r = OpenPath("f", O_RDONLY);
        check = 6;
        if (r == new IntPtr(-1)) return check;
        check = 7;
        Marshal.SetLastSystemError(0);
        if (FTruncate(r, 0) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // A directory, which can only ever be opened read-only, so it reaches the
        // same refusal. EINVAL and not EISDIR: that is path-based `truncate(2)`'s
        // answer, not `ftruncate(2)`'s.
        IntPtr d = OpenPath("d", O_RDONLY);
        check = 8;
        if (d == new IntPtr(-1)) return check;
        check = 9;
        Marshal.SetLastSystemError(0);
        if (FTruncate(d, 0) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // fd 1 is stdout, which both PawPrint and the oracle's launcher give the
        // process as a pipe: not a regular file, so EINVAL.
        check = 10;
        Marshal.SetLastSystemError(0);
        if (FTruncate(new IntPtr(1), 0) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        Close(r);
        Close(d);

        // --- ftruncate's effect ---

        // Shrinking discards the tail.
        check = 11;
        if (FTruncate(w, 3) != 0) return check;
        check = 12;
        if (!Holds("f", Bytes("hel"))) return check;

        // Extending zero-fills.
        check = 13;
        if (FTruncate(w, 6) != 0) return check;
        check = 14;
        if (!Holds("f", new byte[] { (byte)'h', (byte)'e', (byte)'l', 0, 0, 0 })) return check;

        // Truncating to the length the file already has is legal and changes no
        // bytes. (That it still moves the timestamps is asserted in the impure
        // half, which has a deterministic clock.)
        check = 15;
        if (FTruncate(w, 6) != 0) return check;
        check = 16;
        if (!Holds("f", new byte[] { (byte)'h', (byte)'e', (byte)'l', 0, 0, 0 })) return check;

        // Truncating to zero.
        check = 17;
        if (FTruncate(w, 0) != 0) return check;
        check = 18;
        if (!Holds("f", new byte[0])) return check;

        // --- the description's offset is not the file's length ---

        // `ftruncate` leaves the offset exactly where it was, even when it
        // truncates below it, and a write there then leaves a hole. No FileStream
        // row can pin this: the BCL tracks its position in managed code.
        IntPtr off = OpenPath("g", O_RDWR);
        check = 19;
        if (off == new IntPtr(-1)) return check;
        check = 20;
        if (LSeek(off, 5, SEEK_SET) != 5) return check;
        check = 21;
        if (FTruncate(off, 2) != 0) return check;
        check = 22;
        if (LSeek(off, 0, SEEK_CUR) != 5) return check;
        check = 23;
        if (Write(off, buf, 1) != 1) return check;
        check = 24;
        if (!Holds("g", new byte[] { (byte)'h', (byte)'e', 0, 0, 0, (byte)'Z' })) return check;
        Close(off);

        // --- O_TRUNC ---

        // It empties the file at open time, through a write-mode descriptor...
        check = 25;
        IntPtr t = OpenPath("h", O_WRONLY | O_TRUNC);
        if (t == new IntPtr(-1)) return check;
        check = 26;
        if (!Holds("h", new byte[0])) return check;
        Close(t);

        // ...and, measured on both kernels, through a *read-only* one too. What
        // O_TRUNC needs is the write permission bit, not a write access mode.
        using (FileStream refill = new FileStream("h", FileMode.Open, FileAccess.Write, FileShare.None))
        {
            refill.Write(Bytes("abcde"), 0, 5);
        }
        check = 27;
        IntPtr tr = OpenPath("h", O_RDONLY | O_TRUNC);
        if (tr == new IntPtr(-1)) return check;
        check = 28;
        if (!Holds("h", new byte[0])) return check;

        // ...but that descriptor is still read-only, so it cannot itself
        // truncate. This is the row that fails if O_TRUNC were implemented by
        // registering the descriptor as writable.
        check = 29;
        Marshal.SetLastSystemError(0);
        if (FTruncate(tr, 0) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;
        Close(tr);

        // A directory is EISDIR whatever the access mode — including read-only,
        // which without O_TRUNC opens perfectly well.
        check = 30;
        if (!RejectedOpen("d", O_RDONLY | O_TRUNC, EISDIR)) return check;
        check = 31;
        if (!RejectedOpen("d", O_WRONLY | O_TRUNC, EISDIR)) return check;
        check = 32;
        if (!RejectedOpen("d", O_CREAT | O_RDONLY | O_TRUNC, EISDIR)) return check;
        // ...the control: without O_TRUNC, a read-only open of a directory works.
        IntPtr plain = OpenPath("d", O_RDONLY);
        check = 33;
        if (plain == new IntPtr(-1)) return check;
        Close(plain);

        // O_TRUNC follows a symlink and empties its *target*.
        check = 34;
        IntPtr viaLink = OpenPath("lf", O_WRONLY | O_TRUNC);
        if (viaLink == new IntPtr(-1)) return check;
        check = 35;
        if (!Holds("f2", new byte[0])) return check;
        Close(viaLink);

        // --- a refused open truncates nothing ---

        // O_EXCL on an existing file is EEXIST, and the bytes are still there.
        // Without the read-back this row would pass against an implementation
        // that truncated before deciding.
        check = 36;
        if (!RejectedOpen("keep", O_WRONLY | O_CREAT | O_EXCL | O_TRUNC, EEXIST)) return check;
        check = 37;
        if (!Holds("keep", Bytes("hello"))) return check;

        // O_NOFOLLOW on a symlink refuses, and the target is untouched. (The
        // errno is ELOOP on both, but its *number* is not portable, so this row
        // asserts the refusal rather than the number.)
        check = 38;
        if (!RefusedOpen("lkeep", O_WRONLY | O_TRUNC | O_NOFOLLOW)) return check;
        check = 39;
        if (!Holds("keep", Bytes("hello"))) return check;

        // --- through the BCL ---

        // `FileMode.Truncate` on an existing file. With file locking enabled this
        // emits no O_TRUNC at all: it opens, takes LOCK_EX, and calls
        // `SystemNative_FTruncate(fd, 0)` from `SafeFileHandle.Init`.
        //
        // `FileShare.None` rather than the default, and not arbitrarily:
        // `CanLockTheFile` returns immediately for LOCK_EX, while for a LOCK_SH
        // taken with write access it consults `SystemNative_GetFileSystemType`,
        // which PawPrint has no handler for yet.
        //
        // The seeded file is non-empty on purpose. `SafeFileHandle.Init` *swallows*
        // EINVAL and EBADF from FTruncate ("a special file that can't be
        // truncated"), so a runtime that wrongly refused would raise nothing at
        // all and simply leave the old bytes — which only a read-back can see.
        using (new FileStream("bcl", FileMode.Truncate, FileAccess.Write, FileShare.None)) { }
        check = 40;
        if (File.ReadAllBytes("bcl").Length != 0) return check;

        // `FileMode.Create` on an existing file truncates it the same way.
        using (FileStream fs = new FileStream("bcl2", FileMode.Create, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("xy"), 0, 2);
        }
        check = 41;
        byte[] created = File.ReadAllBytes("bcl2");
        if (created.Length != 2 || created[0] != (byte)'x' || created[1] != (byte)'y') return check;

        // `FileStream.SetLength` reaches FTruncate through
        // `RandomAccess.SetFileLength`, in both directions.
        using (FileStream fs = new FileStream("bcl3", FileMode.Open, FileAccess.Write, FileShare.None))
        {
            fs.SetLength(2);
            check = 42;
            if (fs.Length != 2) return check;

            fs.SetLength(7);
            check = 43;
            if (fs.Length != 7) return check;
        }

        check = 44;
        byte[] grown = File.ReadAllBytes("bcl3");
        if (grown.Length != 7 || grown[0] != (byte)'h' || grown[1] != (byte)'e' || grown[2] != 0 || grown[6] != 0)
        {
            return check;
        }

        // Shrinking below the stream's own position drags the position down with
        // it — a BCL-level rule (`OSFileStreamStrategy.SetLengthCore`), and the
        // control showing the syscall's own offset behaviour above is the
        // kernel's rather than the BCL's.
        using (FileStream fs = new FileStream("bcl4", FileMode.Open, FileAccess.Write, FileShare.None))
        {
            fs.Write(Bytes("abcde"), 0, 5);
            check = 45;
            if (fs.Position != 5) return check;
            fs.SetLength(2);
            check = 46;
            if (fs.Position != 2) return check;
        }

        Close(w);
        return 0;
    }
}
