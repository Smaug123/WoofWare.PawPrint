using System;
using System.Runtime.InteropServices;

// The parts of SystemNative_FLock's contract the differential oracle cannot be
// asked about, because Linux and Darwin disagree about them and
// PawPrint simulates Linux (SimulatedUnixPlatform defaults to LinuxX64).
//
// Measured on both, with the same C program run natively on macOS and inside a
// Linux container (scratchpad/flockops.c):
//
//   operation      Linux    Darwin
//   0              EINVAL   EBADF
//   SH|EX (3)      EINVAL   0
//   NB alone (4)   EINVAL   EBADF
//   UN|SH (9)      EINVAL   0
//   16             EINVAL   EBADF
//   SH|16 (17)     EINVAL   0
//   flock on pipe  0        EOPNOTSUPP
//   EWOULDBLOCK    11       35
//
// So Linux's rule is: exactly one of SH/EX/UN, optionally with NB, and nothing
// else. Darwin accepts several of those outright and rejects the rest with a
// *different* errno. A pure differential case would therefore be asserting
// whichever machine happened to run it.
//
// errno is read via Marshal.GetLastSystemError, the slot the syscall itself
// writes, rather than the P/Invoke slot the SetLastError stub copies it into.
// The imports below declare that flag, so the two agree; what makes them agree
// is pinned by sourcesPure/PInvokeSetLastError.cs.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f, a regular file.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    const int LOCK_SH = 1;
    const int LOCK_EX = 2;
    const int LOCK_NB = 4;
    const int LOCK_UN = 8;

    // Linux numbering, which is what PawPrint reports.
    const int EBADF = 9;
    const int EINVAL = 22;
    const int EAGAIN = 11;

    static unsafe IntPtr OpenF()
    {
        byte* path = stackalloc byte[2];
        path[0] = (byte)'f';
        path[1] = 0;
        return Open(path, 0, 0);
    }

    // True if `operation` was rejected with exactly `expectedErrno`.
    static bool Rejected(IntPtr fd, int operation, int expectedErrno)
    {
        Marshal.SetLastSystemError(0);
        int result = FLock(fd, operation);
        return result == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }

    static bool Granted(IntPtr fd, int operation)
    {
        return FLock(fd, operation) == 0;
    }

    static int Main(string[] args)
    {
        int check = 0;

        IntPtr a = OpenF();
        check = 1;
        if (a == new IntPtr(-1)) return check;

        // Operation validation, Linux's rule. Each of these is a *different*
        // way of being malformed: no mode bit at all; two mode bits; a modifier
        // with no mode; an unknown bit alone; and an unknown bit alongside a
        // valid mode (which Darwin would accept, so it pins that PawPrint is
        // not merely masking the operation down to the bits it knows).
        check = 2;
        if (!Rejected(a, 0, EINVAL)) return check;
        check = 3;
        if (!Rejected(a, LOCK_SH | LOCK_EX, EINVAL)) return check;
        check = 4;
        if (!Rejected(a, LOCK_NB, EINVAL)) return check;
        check = 5;
        if (!Rejected(a, LOCK_UN | LOCK_SH, EINVAL)) return check;
        check = 6;
        if (!Rejected(a, 16, EINVAL)) return check;
        check = 7;
        if (!Rejected(a, LOCK_SH | 16, EINVAL)) return check;

        // ...and every well-formed operation is accepted, so the validation is
        // not "reject everything". LOCK_UN|LOCK_NB is legal and a no-op
        // modifier, which is the one combination that looks malformed but is
        // not.
        check = 8;
        if (!Granted(a, LOCK_SH)) return check;
        check = 9;
        if (!Granted(a, LOCK_EX)) return check;
        check = 10;
        if (!Granted(a, LOCK_SH | LOCK_NB)) return check;
        check = 11;
        if (!Granted(a, LOCK_EX | LOCK_NB)) return check;
        check = 12;
        if (!Granted(a, LOCK_UN)) return check;
        check = 13;
        if (!Granted(a, LOCK_UN | LOCK_NB)) return check;

        // Releasing a lock that was never taken is a success, not an error.
        check = 14;
        if (!Granted(a, LOCK_UN)) return check;

        // An unknown descriptor is EBADF...
        check = 15;
        if (!Rejected(new IntPtr(-1), LOCK_EX | LOCK_NB, EBADF)) return check;
        check = 16;
        if (!Rejected(new IntPtr(4242), LOCK_EX, EBADF)) return check;

        // ...but the *operation* is validated first, so a bad fd carrying a
        // malformed operation reports EINVAL rather than EBADF. Measured on
        // Linux (scratchpad/flockorder.c): flock(-1, 0) is EINVAL while
        // flock(-1, LOCK_EX|LOCK_NB) is EBADF. Without an input on which the
        // two checks disagree, swapping their order would be undetectable.
        check = 17;
        if (!Rejected(new IntPtr(-1), 0, EINVAL)) return check;
        check = 18;
        if (!Rejected(new IntPtr(-1), LOCK_SH | LOCK_EX, EINVAL)) return check;

        // Contention reports EWOULDBLOCK, whose *raw* number is the one errno
        // where Linux and Darwin are transposed (11 against 35). The managed
        // sibling can only see that this became an IOException; this pins the
        // number, and hence that PawPrint reports its simulated platform's
        // numbering rather than the host's.
        IntPtr b = OpenF();
        check = 19;
        if (b == new IntPtr(-1)) return check;
        check = 20;
        if (!Granted(a, LOCK_EX | LOCK_NB)) return check;
        check = 21;
        if (!Rejected(b, LOCK_SH | LOCK_NB, EAGAIN)) return check;
        check = 22;
        if (!Rejected(b, LOCK_EX | LOCK_NB, EAGAIN)) return check;

        // ...and a release through the holder makes it available again, so the
        // refusals above were about the lock rather than about `b`.
        check = 23;
        if (!Granted(a, LOCK_UN)) return check;
        check = 24;
        if (!Granted(b, LOCK_EX | LOCK_NB)) return check;

        // A standard stream is lockable. PawPrint models fds 0/1/2 as pipes,
        // and Linux permits flock on a pipe (Darwin answers EOPNOTSUPP). It can
        // never conflict, because each stream has exactly one open file
        // description.
        check = 25;
        if (!Granted(new IntPtr(1), LOCK_EX | LOCK_NB)) return check;
        check = 26;
        if (!Granted(new IntPtr(1), LOCK_UN)) return check;
        check = 27;
        if (!Granted(new IntPtr(0), LOCK_SH | LOCK_NB)) return check;

        // ...and a stream is validated like anything else.
        check = 28;
        if (!Rejected(new IntPtr(2), LOCK_SH | LOCK_EX, EINVAL)) return check;

        return 0;
    }
}
