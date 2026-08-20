using System;
using System.Runtime.InteropServices;

// `SystemNative_PRead`'s contract at the syscall boundary, including the order
// in which it decides its errors.
//
// Impure for two reasons, neither of them "the platforms disagree about
// pread". On *single-fault* inputs Linux and macOS agree on every row — the
// count, the short read at EOF, EINVAL, EISDIR, EBADF, EFAULT, ESPIPE, and the
// fact that the offset is not consumed. What is not portable is:
//
//   1. **The order the checks run in**, which only shows up when two things are
//      wrong at once. Measured (scratchpad/preadpairs.c):
//
//        input                        Linux    Darwin
//        negative offset + bad fd     EINVAL   EBADF
//        negative offset + pipe       EINVAL   ESPIPE
//        negative offset + directory  EINVAL   EINVAL
//
//      Linux validates the offset before it even looks the descriptor up;
//      Darwin resolves the descriptor and its seekability first. PawPrint
//      simulates Linux.
//   2. **Several arms are about PawPrint's own simulated fd table and address
//      space** — fd 1 being a pipe, an unmapped buffer — which a real kernel
//      need not be asked about.
//
// errno is read via Marshal.GetLastSystemError, the slot the syscall itself
// writes, rather than the P/Invoke slot the SetLastError stub copies it into.
// The imports below declare that flag, so the two agree; what makes them agree
// is pinned by sourcesPure/PInvokeSetLastError.cs.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello" (5 bytes), d = a directory.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    // Linux numbering, which is what PawPrint reports.
    const int EBADF = 9;
    const int EISDIR = 21;
    const int EINVAL = 22;
    const int EFAULT = 14;
    const int ESPIPE = 29;

    static unsafe IntPtr OpenPath(string name)
    {
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }

    static unsafe bool Rejected(IntPtr fd, byte* buf, int size, long offset, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        int r = PRead(fd, buf, size, offset);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
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

        // The counts, and the bytes. Asserting only the count would let "right
        // length, wrong bytes" through, which is exactly what an offset bug
        // looks like.
        check = 3;
        if (PRead(f, buf, 5, 0) != 5) return check;
        check = 4;
        if (buf[0] != 'h' || buf[1] != 'e' || buf[2] != 'l' || buf[3] != 'l' || buf[4] != 'o') return check;

        // Asking for more than the file holds is a short read, not an error.
        check = 5;
        if (PRead(f, buf, 64, 0) != 5) return check;

        // A non-zero offset, and the bytes it should land on.
        check = 6;
        if (PRead(f, buf, 64, 3) != 2) return check;
        check = 7;
        if (buf[0] != 'l' || buf[1] != 'o') return check;

        // At and past the end: zero, not an error.
        check = 8;
        if (PRead(f, buf, 64, 5) != 0) return check;
        check = 9;
        if (PRead(f, buf, 64, 99) != 0) return check;

        // A zero-length request reads nothing and succeeds.
        check = 10;
        if (PRead(f, buf, 0, 0) != 0) return check;

        // `pread` does not consume: reading twice at the same offset gives the
        // same bytes, because the offset is an argument rather than descriptor
        // state. A model that stored an offset on the description and advanced
        // it would fail here.
        check = 11;
        if (PRead(f, buf, 2, 0) != 2 || buf[0] != 'h' || buf[1] != 'e') return check;
        check = 12;
        if (PRead(f, buf, 2, 0) != 2 || buf[0] != 'h' || buf[1] != 'e') return check;

        // --- single-fault errors ---

        check = 13;
        if (!Rejected(f, buf, 5, -1, EINVAL)) return check;
        check = 14;
        if (!Rejected(new IntPtr(4242), buf, 5, 0, EBADF)) return check;
        check = 15;
        if (!Rejected(d, buf, 5, 0, EISDIR)) return check;
        // fd 1 is a standard stream, which PawPrint models as a pipe, and a
        // pipe is not seekable.
        check = 16;
        if (!Rejected(new IntPtr(1), buf, 5, 0, ESPIPE)) return check;
        check = 17;
        if (!Rejected(f, bogus, 5, 0, EFAULT)) return check;

        // --- the buffer is only touched when there are bytes to move ---

        // A kernel faults in `copy_to_user`, so a call that transfers nothing
        // never looks at the buffer: an unreadable buffer at EOF is 0, not
        // EFAULT. Measured on both platforms. This is the row that catches an
        // implementation validating its arguments up front.
        check = 18;
        if (PRead(f, bogus, 5, 5) != 0) return check;
        check = 19;
        if (PRead(f, bogus, 5, 99) != 0) return check;
        // ...and likewise a zero-size request with an unreadable buffer.
        check = 20;
        if (PRead(f, bogus, 0, 0) != 0) return check;

        // --- the ordering, pinned by inputs on which the guards disagree ---

        // Negative offset beats a bad fd. This is the Linux order; Darwin
        // answers EBADF here, which is why this file is impure.
        check = 21;
        if (!Rejected(new IntPtr(4242), buf, 5, -1, EINVAL)) return check;
        // ...and beats an unseekable fd, where Darwin answers ESPIPE.
        check = 22;
        if (!Rejected(new IntPtr(1), buf, 5, -1, EINVAL)) return check;
        // ...and beats a directory, where the two platforms happen to agree.
        check = 23;
        if (!Rejected(d, buf, 5, -1, EINVAL)) return check;
        // ...and beats an unreadable buffer.
        check = 24;
        if (!Rejected(f, bogus, 5, -1, EINVAL)) return check;
        // ...and is checked even when nothing would have been transferred, so
        // it precedes the transfer-window shortcut too.
        check = 25;
        if (!Rejected(f, buf, 0, -1, EINVAL)) return check;

        // A zero-size request does *not* short-circuit ahead of the descriptor
        // checks: a bad fd with size 0 is still EBADF, and a directory with
        // size 0 is still EISDIR. So the fd and its kind are resolved before
        // the transfer window is computed.
        check = 26;
        if (!Rejected(new IntPtr(4242), buf, 0, 0, EBADF)) return check;
        check = 27;
        if (!Rejected(d, buf, 0, 0, EISDIR)) return check;
        check = 28;
        if (!Rejected(new IntPtr(1), buf, 0, 0, ESPIPE)) return check;

        // The fd's *kind* is likewise decided before the buffer is looked at:
        // an unreadable buffer on a directory is EISDIR, not EFAULT.
        check = 29;
        if (!Rejected(d, bogus, 5, 0, EISDIR)) return check;
        check = 30;
        if (!Rejected(new IntPtr(1), bogus, 5, 0, ESPIPE)) return check;
        check = 31;
        if (!Rejected(new IntPtr(4242), bogus, 5, 0, EBADF)) return check;

        // A directory is a directory whatever offset you ask for, so EISDIR is
        // not something the transfer window could have produced.
        check = 32;
        if (!Rejected(d, buf, 5, 99, EISDIR)) return check;

        return 0;
    }
}
