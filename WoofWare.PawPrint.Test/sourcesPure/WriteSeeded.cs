using System;
using System.IO;
using System.Runtime.InteropServices;

// `SystemNative_PWrite` and the writable half of `SystemNative_Write`: bytes
// actually changing in the filesystem, which is the first thing in PawPrint that
// modifies it at all.
//
// Pure, and that is a measured claim rather than an assumption. Every row below
// was measured identically on Linux 6.18.5 and macOS 26.6 — the counts, the hole
// a write past the end leaves, the timestamps a zero-length write does *not*
// move, EBADF for a descriptor whose access mode is wrong for the operation,
// EINVAL for a negative offset, EISDIR for a directory opened for writing, and
// ESPIPE for a pipe. What is *not* portable stays out of this file and lives in
// `sourcesImpure/PWriteRawSeeded.cs`: fd 0, where Linux lets unseekability win
// (ESPIPE) and Darwin lets unwritability win (EBADF); everything that turns on
// the *mode* of a file, because a privileged process bypasses those rules and
// this suite does not control whether its oracle runs as root; and the fact that
// a write moves mtime and ctime, which needs a deterministic clock to state
// without racing a real filesystem's granularity.
//
// Every buffer address used below is real, or `NULL`, or 8 — never `(byte*)-1`.
// Linux screens a buffer range that leaves the user address space *before* the
// file operation, so a top-of-address-space pointer faults there even at size 0
// where macOS returns 0; that divergence is pinned against the model in
// `TestUserBufferFault.fs` rather than through a guest whose oracle is whichever
// kernel ran it.
//
// Nothing here writes past offset 100, so nothing depends on a filesystem's own
// size ceiling.
//
// errno is read via Marshal.GetLastSystemError rather than GetLastPInvokeError:
// with a raw DllImport there is no generated stub to copy one to the other.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases): f = g = h = "hello" (5 bytes), d = a directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PWrite", SetLastError = true)]
    static extern unsafe int PWrite(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    // `Interop.Sys.OpenFlags`, the PAL's portable numbering rather than any
    // platform's <fcntl.h>.
    const int O_RDONLY = 0x0000;
    const int O_WRONLY = 0x0001;
    const int O_RDWR = 0x0002;

    const int SEEK_SET = 0;
    const int SEEK_CUR = 1;

    // Linux numbering, which is what PawPrint reports; every errno asserted here
    // has the same number on macOS.
    const int EBADF = 9;
    const int EACCES = 13;
    const int EISDIR = 21;
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

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[64];
        buf[0] = (byte)'A';
        buf[1] = (byte)'B';
        buf[2] = (byte)'C';
        buf[3] = (byte)'D';

        // Deliberately not a mapped address, and deliberately small: PawPrint's
        // simulated address space has nothing here, and every platform accepts
        // the *address* so the row is about the dereference rather than about
        // Linux's up-front screen.
        byte* bogus = (byte*)8;

        IntPtr w = OpenPath("f", O_WRONLY);
        check = 1;
        if (w == new IntPtr(-1)) return check;

        // --- the write itself ---

        // Overwriting inside the file does not truncate what follows.
        check = 2;
        if (PWrite(w, buf, 2, 0) != 2) return check;
        check = 3;
        if (!Holds("f", new byte[] { (byte)'A', (byte)'B', (byte)'l', (byte)'l', (byte)'o' })) return check;

        // Straddling the end: part overwrite, part extension.
        check = 4;
        if (PWrite(w, buf, 2, 4) != 2) return check;
        check = 5;
        if (!Holds("f", new byte[] { (byte)'A', (byte)'B', (byte)'l', (byte)'l', (byte)'A', (byte)'B' })) return check;

        // Starting past the end leaves a hole, and a hole reads as zeroes.
        check = 6;
        if (PWrite(w, buf, 1, 8) != 1) return check;
        check = 7;
        if (!Holds("f", new byte[]
            {
                (byte)'A', (byte)'B', (byte)'l', (byte)'l', (byte)'A', (byte)'B', 0, 0, (byte)'A',
            })) return check;

        // A zero-length write reports 0 and does not extend the file, however far
        // past the end it is aimed. The buffer is not touched, so an unmapped one
        // is fine.
        check = 8;
        if (PWrite(w, buf, 0, 100) != 0) return check;
        check = 9;
        if (PWrite(w, bogus, 0, 100) != 0) return check;
        check = 10;
        if (!Holds("f", new byte[]
            {
                (byte)'A', (byte)'B', (byte)'l', (byte)'l', (byte)'A', (byte)'B', 0, 0, (byte)'A',
            })) return check;

        // --- pwrite ignores the offset; write consumes it ---

        // Nine pwrites in and the description has not moved: that is the whole
        // difference between the two syscalls.
        check = 11;
        if (LSeek(w, 0, SEEK_CUR) != 0) return check;

        check = 12;
        if (Write(w, buf, 1) != 1) return check;
        check = 13;
        if (LSeek(w, 0, SEEK_CUR) != 1) return check;
        check = 14;
        if (Write(w, buf, 1) != 1) return check;
        check = 15;
        if (LSeek(w, 0, SEEK_CUR) != 2) return check;

        // ...and it landed where the offset said, not at 0 both times.
        check = 16;
        if (!Holds("f", new byte[]
            {
                (byte)'A', (byte)'A', (byte)'l', (byte)'l', (byte)'A', (byte)'B', 0, 0, (byte)'A',
            })) return check;

        // A write after an explicit seek starts there, and a seek past the end
        // makes the next write leave a hole.
        check = 17;
        if (LSeek(w, 12, SEEK_SET) != 12) return check;
        check = 18;
        if (Write(w, buf, 1) != 1) return check;
        check = 19;
        if (LSeek(w, 0, SEEK_CUR) != 13) return check;
        check = 20;
        if (!Holds("f", new byte[]
            {
                (byte)'A', (byte)'A', (byte)'l', (byte)'l', (byte)'A', (byte)'B', 0, 0, (byte)'A',
                0, 0, 0, (byte)'A',
            })) return check;

        // A zero-length `write` is a no-op that does not move the offset either.
        check = 21;
        if (Write(w, buf, 0) != 0) return check;
        check = 22;
        if (LSeek(w, 0, SEEK_CUR) != 13) return check;

        // --- the access mode ---

        // A descriptor open for writing is not open for reading, whatever the
        // file's own permissions say. This is `vfs_read`'s EBADF, and it beats
        // both the buffer address and the zero-size short-circuit.
        check = 23;
        Marshal.SetLastSystemError(0);
        if (Read(w, buf, 4) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 24;
        Marshal.SetLastSystemError(0);
        if (PRead(w, buf, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 25;
        Marshal.SetLastSystemError(0);
        if (Read(w, buf, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 26;
        Marshal.SetLastSystemError(0);
        if (PRead(w, bogus, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // ...and the converse, on a descriptor opened read-only.
        IntPtr r = OpenPath("f", O_RDONLY);
        check = 27;
        if (r == new IntPtr(-1)) return check;
        check = 28;
        Marshal.SetLastSystemError(0);
        if (PWrite(r, buf, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 29;
        Marshal.SetLastSystemError(0);
        if (Write(r, buf, 4) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 30;
        Marshal.SetLastSystemError(0);
        if (PWrite(r, buf, 0, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;
        check = 31;
        Marshal.SetLastSystemError(0);
        if (Write(r, buf, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // The refusal is the descriptor's, not the file's: `f` is perfectly
        // writable, as `w` is still demonstrating.
        check = 32;
        if (PWrite(w, buf, 1, 0) != 1) return check;

        // `O_RDWR` permits both through one descriptor.
        IntPtr rw = OpenPath("g", O_RDWR);
        check = 33;
        if (rw == new IntPtr(-1)) return check;
        check = 34;
        if (PWrite(rw, buf, 1, 0) != 1) return check;
        check = 35;
        if (PRead(rw, buf + 32, 5, 0) != 5) return check;
        check = 36;
        if (buf[32] != (byte)'A' || buf[33] != (byte)'e') return check;

        // --- errors ---

        // A negative offset, which `pwrite` validates before anything else.
        check = 37;
        Marshal.SetLastSystemError(0);
        if (PWrite(w, buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;
        // ...even when nothing would have been transferred.
        check = 38;
        Marshal.SetLastSystemError(0);
        if (PWrite(w, buf, 0, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        // An unmapped buffer is EFAULT rather than a crash — asserted through the
        // errno of a *successful-looking* call rather than by address arithmetic,
        // so `bogus` stays a small address every platform maps nothing at.
        check = 39;
        Marshal.SetLastSystemError(0);
        if (PWrite(w, bogus, 4, 0) != -1) return check;
        check = 40;
        Marshal.SetLastSystemError(0);
        if (Write(w, bogus, 4) != -1) return check;

        // A directory cannot be opened for writing at all, which is what CoreLib
        // relies on instead of checking the type itself.
        check = 41;
        if (!RejectedOpen("d", O_WRONLY, EISDIR)) return check;
        check = 42;
        if (!RejectedOpen("d", O_RDWR, EISDIR)) return check;
        // ...but opens fine for reading.
        IntPtr d = OpenPath("d", O_RDONLY);
        check = 43;
        if (d == new IntPtr(-1)) return check;

        // A pipe is not seekable, so `pwrite` to one is ESPIPE. fd 1 is stdout,
        // which both PawPrint and the oracle's launcher give the process as a
        // pipe's write end.
        check = 44;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(1), buf, 1, 0) != -1 || Marshal.GetLastSystemError() != ESPIPE) return check;

        // An unknown descriptor.
        check = 45;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(4242), buf, 4, 0) != -1 || Marshal.GetLastSystemError() != EBADF) return check;

        // ...and the pair that shows the offset really is validated *before* the
        // descriptor is even looked up: the same bad fd with a negative offset is
        // EINVAL, not EBADF. Unlike `pread`, where Darwin answers EBADF here, both
        // kernels agree — which is why this row can live in the pure half.
        check = 46;
        Marshal.SetLastSystemError(0);
        if (PWrite(new IntPtr(4242), buf, 4, -1) != -1 || Marshal.GetLastSystemError() != EINVAL) return check;

        Close(w);
        Close(r);
        Close(rw);
        Close(d);

        // --- through the BCL ---

        // `FileStream` with `FileAccess.Write` on an existing file is the whole
        // write path: `open(O_WRONLY)`, `flock`, then `RandomAccess.WriteAtOffset`,
        // which prefers `pwrite` for any handle supporting random access.
        // `FileMode.Open` on its own, so nothing here creates or truncates; those
        // paths are exercised by `CreateSeeded.cs` and `TruncateSeeded.cs`.
        //
        // `FileShare.None` rather than the default `FileShare.Read`, and not
        // arbitrarily: `SafeFileHandle.CanLockTheFile` returns immediately for
        // `LOCK_EX`, while for a `LOCK_SH` taken with write access it asks
        // `SystemNative_GetFileSystemType` first (locking is unsafe on NFS, CIFS
        // and SMB). PawPrint has no handler for that native yet, so the default
        // share mode reaches an unimplemented one. The write path itself is
        // identical either way — only which lock is taken differs.
        using (FileStream stream = new FileStream("h", FileMode.Open, FileAccess.Write, FileShare.None))
        {
            stream.Write(new byte[] { (byte)'X', (byte)'Y' }, 0, 2);
        }

        check = 47;
        byte[] read = File.ReadAllBytes("h");
        if (read.Length != 5) return check;
        check = 48;
        if (read[0] != (byte)'X' || read[1] != (byte)'Y' || read[2] != (byte)'l') return check;

        // The stream's own position advanced with the write, as a sequential
        // stream's does, even though the syscall underneath took an explicit
        // offset.
        using (FileStream stream = new FileStream("h", FileMode.Open, FileAccess.Write, FileShare.None))
        {
            stream.Write(new byte[] { (byte)'Z' }, 0, 1);
            check = 49;
            if (stream.Position != 1) return check;
            stream.Write(new byte[] { (byte)'W' }, 0, 1);
            check = 50;
            if (stream.Position != 2) return check;
        }

        check = 51;
        read = File.ReadAllBytes("h");
        if (read.Length != 5 || read[0] != (byte)'Z' || read[1] != (byte)'W' || read[2] != (byte)'l')
        {
            return check;
        }

        // A read-only stream cannot write, and the exception arrives before any
        // syscall — the control showing that check 23's EBADF is the kernel's
        // answer rather than the BCL's.
        check = 52;
        using (FileStream stream = new FileStream("h", FileMode.Open, FileAccess.Read))
        {
            if (stream.CanWrite) return check;
        }

        // The EACCES a write-mode open owes a read-only file is asserted in the
        // impure half, because root bypasses it and this suite does not choose the
        // uid its oracle runs as. This is the control: a default-mode file does
        // open for writing, whoever is asking.
        check = 53;
        if (OpenPath("f", O_WRONLY) == new IntPtr(-1)) return check;

        return 0;
    }
}
