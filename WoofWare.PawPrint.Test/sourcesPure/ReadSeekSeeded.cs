using System;
using System.Runtime.InteropServices;

// `SystemNative_Read` and `SystemNative_LSeek`: the open file description's
// *offset*, which is the one piece of state the two share and the reason they
// are one feature rather than two. `read` consumes from it and advances it;
// `lseek` moves it and reports it; `pread` (already implemented) deliberately
// ignores it, which is what makes it a separate syscall at all.
//
// Pure, and that is a measured claim rather than an assumption. Every `read(2)`
// row below was measured identically on Linux and macOS — the counts, the short
// read at EOF, EBADF, EISDIR, EFAULT, the untouched buffer when nothing moves,
// and the order the checks run in. `lseek(2)` diverges in three ways, and none
// of them appear here: they live in `sourcesImpure/LSeekRawSeeded.cs`.
//
// `read(2)` has exactly one divergence of its own, and this file stays clear of
// it rather than being lucky. Linux's `access_ok` rejects a buffer range that
// leaves the user address space before the file operation runs, so
// such an address faults even at EOF, even with size 0, and even on a directory
// — where macOS answers 0, 0 and EISDIR. Every address used below is `NULL` or
// 8, which every platform accepts, so every row here holds on both. Using
// `(byte*)-1` as the unmapped pointer, which is the natural thing to reach for,
// would have made half these rows platform-dependent: this suite's oracle is
// the host kernel, so such a row would answer 0 on a macOS dev box and EFAULT
// on a Linux CI runner. Those rows are pinned against the model instead, in
// `TestUserBufferFault.fs`.
//
// It is also filesystem-independent, which is a *separate* portability question
// this syscall pair makes real. A real `lseek` rejects an offset above the
// filesystem's `s_maxbytes`, and that ceiling is `0xffffffff000` on ext4 but the
// full `int64` range on tmpfs and APFS — so a guest asserting anything about a
// huge offset would pass on a macOS dev box and fail on a Linux CI runner for
// reasons having nothing to do with the kernel. Nothing here seeks past 1000.
//
// The rows about fds 0, 1 and 2 assert the *launch shape* rather than the
// kernel: that all three standard streams are pipes, stdin's writer already
// closed. That is what `RealRuntime` gives a guest — it redirects all three and
// closes the child's stdin — and it is what `FileDescriptorRegistry.initial`
// models, so the two agree by construction. Verified out of the harness on both
// platforms, where getting it wrong is easy: redirecting any of the three from
// `/dev/null` or a file makes it *seekable*, and checks 61-63 fail for a reason
// that has nothing to do with this handler.
//
// errno is read via Marshal.GetLastSystemError rather than GetLastPInvokeError:
// with a raw DllImport there is no generated stub to copy one to the other.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases): f = "hello" (5 bytes), d = a directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup", SetLastError = true)]
    static extern IntPtr Dup(IntPtr fd);

    const int SEEK_SET = 0;
    const int SEEK_CUR = 1;
    const int SEEK_END = 2;

    // Linux numbering, which is what PawPrint reports. All four are in the
    // portable 1-34 band inherited from V7, so they are the same on macOS.
    const int EBADF = 9;
    const int EFAULT = 14;
    const int EISDIR = 21;
    const int EINVAL = 22;
    const int ESPIPE = 29;

    static unsafe IntPtr OpenPath(string name)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }

    static unsafe bool ReadRejected(IntPtr fd, byte* buf, int size, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        int r = Read(fd, buf, size);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }

    static bool SeekRejected(IntPtr fd, long offset, int whence, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        long r = LSeek(fd, offset, whence);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }

    static unsafe bool Is(byte* buf, string expected)
    {
        for (int i = 0; i < expected.Length; i++) if (buf[i] != (byte)expected[i]) return false;
        return true;
    }

    static unsafe int Main(string[] args)
    {
        int check;
        byte* buf = stackalloc byte[64];
        // Deliberately not a mapped address: PawPrint's simulated address space
        // has nothing here, which is what makes the EFAULT rows reachable.
        byte* bogus = (byte*)8;

        IntPtr f = OpenPath("f");
        check = 1;
        if (f == new IntPtr(-1)) return check;
        IntPtr d = OpenPath("d");
        check = 2;
        if (d == new IntPtr(-1)) return check;

        // --- a fresh description starts at 0, and `read` consumes ---

        check = 3;
        if (LSeek(f, 0, SEEK_CUR) != 0) return check;
        check = 4;
        if (Read(f, buf, 2) != 2 || !Is(buf, "he")) return check;
        // The offset moved by what was read. This is the whole difference from
        // `pread`, which would still report 0 here.
        check = 5;
        if (LSeek(f, 0, SEEK_CUR) != 2) return check;
        check = 6;
        if (Read(f, buf, 64) != 3 || !Is(buf, "llo")) return check;
        check = 7;
        if (LSeek(f, 0, SEEK_CUR) != 5) return check;

        // At EOF: zero, and the offset stays put rather than being clamped or
        // advanced by the amount asked for.
        check = 8;
        if (Read(f, buf, 64) != 0) return check;
        check = 9;
        if (LSeek(f, 0, SEEK_CUR) != 5) return check;

        // --- the three whences ---

        check = 10;
        if (LSeek(f, 0, SEEK_SET) != 0) return check;
        check = 11;
        if (Read(f, buf, 5) != 5 || !Is(buf, "hello")) return check;
        check = 12;
        if (LSeek(f, 3, SEEK_SET) != 3) return check;
        check = 13;
        if (Read(f, buf, 64) != 2 || !Is(buf, "lo")) return check;
        check = 14;
        if (LSeek(f, 0, SEEK_END) != 5) return check;
        check = 15;
        if (LSeek(f, -2, SEEK_END) != 3) return check;
        check = 16;
        if (Read(f, buf, 64) != 2 || !Is(buf, "lo")) return check;
        // SEEK_CUR is relative, and accepts a negative offset as long as the
        // result is not negative. At 5 after the read above.
        check = 17;
        if (LSeek(f, -1, SEEK_CUR) != 4) return check;
        check = 18;
        if (LSeek(f, 1, SEEK_CUR) != 5) return check;

        // Past the end is legal — it is how sparse files are made — and reading
        // there transfers nothing without erroring or rewinding.
        check = 19;
        if (LSeek(f, 1000, SEEK_SET) != 1000) return check;
        check = 20;
        if (Read(f, buf, 64) != 0) return check;
        check = 21;
        if (LSeek(f, 0, SEEK_CUR) != 1000) return check;

        // A zero-length read succeeds and moves nothing.
        check = 22;
        if (LSeek(f, 2, SEEK_SET) != 2) return check;
        check = 23;
        if (Read(f, buf, 0) != 0) return check;
        check = 24;
        if (LSeek(f, 0, SEEK_CUR) != 2) return check;

        // --- the offset belongs to the *description*, not the descriptor ---

        // `dup` shares one description, so reading through the copy moves the
        // original's offset. A model storing the offset per *descriptor* would
        // pass every check above and fail here.
        check = 25;
        if (LSeek(f, 0, SEEK_SET) != 0) return check;
        IntPtr fDup = Dup(f);
        check = 26;
        if (fDup == new IntPtr(-1)) return check;
        check = 27;
        if (Read(fDup, buf, 2) != 2 || !Is(buf, "he")) return check;
        check = 28;
        if (LSeek(f, 0, SEEK_CUR) != 2) return check;
        // ...and seeking through either is seen by the other.
        check = 29;
        if (LSeek(f, 4, SEEK_SET) != 4) return check;
        check = 30;
        if (LSeek(fDup, 0, SEEK_CUR) != 4) return check;

        // A second `open` on the same path is a *separate* description, so it
        // starts at 0 while the first is at 4. The mirror image of the dup case:
        // a model sharing one offset per file would fail here.
        IntPtr f2 = OpenPath("f");
        check = 31;
        if (f2 == new IntPtr(-1)) return check;
        check = 32;
        if (LSeek(f2, 0, SEEK_CUR) != 0) return check;
        check = 33;
        if (LSeek(f, 0, SEEK_CUR) != 4) return check;
        check = 34;
        if (Read(f2, buf, 5) != 5 || !Is(buf, "hello")) return check;
        check = 35;
        if (LSeek(f, 0, SEEK_CUR) != 4) return check;

        // --- read: errors ---

        check = 36;
        if (LSeek(f, 0, SEEK_SET) != 0) return check;
        check = 37;
        if (!ReadRejected(new IntPtr(4242), buf, 5, EBADF)) return check;
        check = 38;
        if (!ReadRejected(d, buf, 5, EISDIR)) return check;
        // fds 1 and 2 are the write ends of pipes: not open for reading.
        check = 39;
        if (!ReadRejected(new IntPtr(1), buf, 5, EBADF)) return check;
        check = 40;
        if (!ReadRejected(new IntPtr(2), buf, 5, EBADF)) return check;
        check = 41;
        if (!ReadRejected(f, bogus, 5, EFAULT)) return check;
        // A fault transfers nothing, so the offset has not moved.
        check = 42;
        if (LSeek(f, 0, SEEK_CUR) != 0) return check;

        // `Common_Read` rejects a negative size itself, in C, *before* the fd is
        // resolved — so this beats an otherwise-diagnosable bad descriptor. Note
        // EINVAL: `Common_Write` answers ERANGE for the same mistake, and the
        // asymmetry is upstream's.
        check = 43;
        if (!ReadRejected(f, buf, -1, EINVAL)) return check;
        check = 44;
        if (!ReadRejected(new IntPtr(4242), buf, -1, EINVAL)) return check;
        check = 45;
        if (!ReadRejected(d, buf, -1, EINVAL)) return check;

        // --- read: the buffer is only touched when bytes actually move ---

        // A kernel faults in `copy_to_user`, so a call that transfers nothing
        // never looks at the buffer. Same rule `pread` obeys, and easy to break
        // by validating arguments up front.
        check = 46;
        if (LSeek(f, 5, SEEK_SET) != 5) return check;
        check = 47;
        if (Read(f, bogus, 5) != 0) return check;
        check = 48;
        if (LSeek(f, 0, SEEK_SET) != 0) return check;
        check = 49;
        if (Read(f, bogus, 0) != 0) return check;

        // ...but the descriptor and its kind are resolved *before* the buffer,
        // so an unreadable buffer on a bad or wrong-kind fd reports that instead.
        check = 50;
        if (!ReadRejected(new IntPtr(4242), bogus, 5, EBADF)) return check;
        check = 51;
        if (!ReadRejected(d, bogus, 5, EISDIR)) return check;
        check = 52;
        if (!ReadRejected(new IntPtr(1), bogus, 5, EBADF)) return check;

        // ...and a zero-size request does not short-circuit ahead of them either.
        check = 53;
        if (!ReadRejected(new IntPtr(4242), buf, 0, EBADF)) return check;
        check = 54;
        if (!ReadRejected(d, buf, 0, EISDIR)) return check;
        check = 55;
        if (!ReadRejected(new IntPtr(1), buf, 0, EBADF)) return check;

        // --- stdin is at end of file ---

        // fd 0 is the read end of a pipe whose write end the launcher closed, so
        // it is permanently at EOF rather than blocking. That is exactly the
        // shape `RealRuntime` starts a guest in — it redirects all three streams
        // and closes the child's stdin immediately — which is why this is
        // assertable against the oracle rather than merely plausible.
        check = 56;
        if (Read(new IntPtr(0), buf, 5) != 0) return check;
        // EOF beats an unreadable buffer, for the same copy_to_user reason.
        check = 57;
        if (Read(new IntPtr(0), bogus, 5) != 0) return check;
        check = 58;
        if (Read(new IntPtr(0), buf, 0) != 0) return check;
        // ...but not a negative size, which the C shim rejects before it looks
        // at anything.
        check = 59;
        if (!ReadRejected(new IntPtr(0), buf, -1, EINVAL)) return check;

        // --- lseek: errors ---

        check = 60;
        if (!SeekRejected(new IntPtr(4242), 0, SEEK_CUR, EBADF)) return check;
        // A pipe is not seekable, whichever end. This is the answer
        // `SafeFileHandle.CanSeek` reads back, so it is on the BCL's own path.
        check = 61;
        if (!SeekRejected(new IntPtr(0), 0, SEEK_CUR, ESPIPE)) return check;
        check = 62;
        if (!SeekRejected(new IntPtr(1), 0, SEEK_CUR, ESPIPE)) return check;
        check = 63;
        if (!SeekRejected(new IntPtr(2), 0, SEEK_CUR, ESPIPE)) return check;

        // A computation landing below zero is rejected rather than clamped...
        check = 64;
        if (LSeek(f, 2, SEEK_SET) != 2) return check;
        check = 65;
        if (!SeekRejected(f, -1, SEEK_SET, EINVAL)) return check;
        check = 66;
        if (!SeekRejected(f, -3, SEEK_CUR, EINVAL)) return check;
        check = 67;
        if (!SeekRejected(f, -6, SEEK_END, EINVAL)) return check;
        // ...and leaves the offset where it was.
        check = 68;
        if (LSeek(f, 0, SEEK_CUR) != 2) return check;
        // Landing exactly on zero is fine, so the rejection is about negativity
        // rather than about subtracting.
        check = 69;
        if (LSeek(f, -2, SEEK_CUR) != 0) return check;
        check = 70;
        if (LSeek(f, -5, SEEK_END) != 0) return check;

        check = 71;
        if (!SeekRejected(f, 0, 99, EINVAL)) return check;
        check = 72;
        if (!SeekRejected(f, 0, -1, EINVAL)) return check;

        // --- lseek: the check order, pinned by inputs the guards disagree on ---

        // The descriptor is resolved before anything else, on both platforms:
        // a bad fd beats a bad whence and beats a negative result.
        check = 73;
        if (!SeekRejected(new IntPtr(4242), 0, 99, EBADF)) return check;
        check = 74;
        if (!SeekRejected(new IntPtr(4242), -1, SEEK_SET, EBADF)) return check;

        // Seekability is decided before the offset arithmetic, on both: a
        // negative seek on a pipe is ESPIPE, not EINVAL. (Bad *whence* on a pipe
        // is where the platforms part company, so it is not here.)
        check = 75;
        if (!SeekRejected(new IntPtr(0), -1, SEEK_SET, ESPIPE)) return check;
        check = 76;
        if (!SeekRejected(new IntPtr(1), -1, SEEK_SET, ESPIPE)) return check;

        // --- a directory is seekable ---

        // SEEK_SET and SEEK_CUR on a directory are portable (measured identical
        // on tmpfs, ext4 and APFS), even though *reading* one is EISDIR. Only
        // SEEK_END is filesystem-dependent, and it is not asked here.
        check = 77;
        if (LSeek(d, 0, SEEK_CUR) != 0) return check;
        check = 78;
        if (LSeek(d, 3, SEEK_SET) != 3) return check;
        check = 79;
        if (LSeek(d, 0, SEEK_CUR) != 3) return check;
        check = 80;
        if (!SeekRejected(d, -1, SEEK_SET, EINVAL)) return check;
        check = 81;
        if (LSeek(d, 0, SEEK_SET) != 0) return check;

        return 0;
    }
}
