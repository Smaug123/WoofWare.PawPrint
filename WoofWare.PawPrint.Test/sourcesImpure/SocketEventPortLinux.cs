using System;
using System.Runtime.InteropServices;

// A socket event port descriptor -- epoll on the Linux flavour -- and what the
// ordinary file operations answer for one.
//
// PawPrint-only, on two counts. The descriptor *numbers* are unpredictable under
// the oracle (its process holds the runtime's own descriptors, so the first port
// is not fd 3), which is the same reason OpenFdNumbering.cs gives. And every
// errno row below except pread/pwrite differs between Linux and Darwin, so a
// differential guest would have to agree with whichever kernel the test host
// happens to be -- macOS locally, Linux in CI. The Darwin column is asserted by
// the sibling SocketEventPortDarwin.cs, under the macOS preset.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup", SetLastError = true)]
    static extern IntPtr Dup(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int count, long offset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PWrite", SetLastError = true)]
    static extern unsafe int PWrite(IntPtr fd, byte* buffer, int count, long offset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetIsNonBlocking", SetLastError = true)]
    static extern unsafe int GetIsNonBlocking(IntPtr fd, int* isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_IsATty")]
    static extern int IsATty(IntPtr fd);

    // Interop.Error values, not raw errnos: these entry points return the PAL
    // enum directly rather than -1-and-errno.
    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;

    const int EBADF = 9;
    // Linux's numbering. EWOULDBLOCK is the same value as EAGAIN on every Unix,
    // but the *number* is not portable: 11 here, 35 on Darwin.
    const int EWOULDBLOCK = 11;
    const int EINVAL = 22;
    const int ESPIPE = 29;

    const int LOCK_SH = 1;
    const int LOCK_EX = 2;
    const int LOCK_NB = 4;
    const int LOCK_UN = 8;

    static unsafe long Create()
    {
        IntPtr port;
        int error = CreateSocketEventPort(&port);
        return error == PAL_SUCCESS ? (long)port : -error;
    }

    static unsafe int Main(string[] args)
    {
        int check = 0;
        byte* buf = stackalloc byte[8];

        // A port takes the lowest free descriptor, exactly as open(2) does.
        // stdin/stdout/stderr are 0..2, so the first port is 3.
        check = 1;
        long p = Create();
        if (p != 3) return check;

        // ---- read/write: refused for the kind of object, not the access mode.
        // Linux answers EINVAL (Darwin answers ENXIO). Note this is NOT EBADF:
        // an EBADF here would mean the description was created without write
        // permission, which would be the wrong model.
        check = 2;
        if (Read((IntPtr)p, buf, 8) != -1) return check;
        check = 3;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // Length is irrelevant -- unlike stdin, whose zero-length read is a
        // successful 0.
        check = 4;
        if (Read((IntPtr)p, buf, 0) != -1) return check;
        check = 5;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        check = 6;
        if (Write((IntPtr)p, buf, 8) != -1) return check;
        check = 7;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // The wrong-object-kind answer precedes the buffer screen: a bad
        // pointer does not turn this into EFAULT.
        check = 8;
        if (Read((IntPtr)p, (byte*)(-1), 8) != -1) return check;
        check = 9;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // ---- pread/pwrite: ESPIPE, and the one row where the platforms agree.
        check = 10;
        if (PRead((IntPtr)p, buf, 8, 0) != -1) return check;
        check = 11;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        check = 12;
        if (PWrite((IntPtr)p, buf, 8, 0) != -1) return check;
        check = 13;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        // Unseekability beats the zero-length shortcut and the buffer screen.
        check = 14;
        if (PRead((IntPtr)p, buf, 0, 0) != -1) return check;
        check = 15;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;
        check = 16;
        if (PRead((IntPtr)p, (byte*)(-1), 8, 0) != -1) return check;
        check = 17;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        // ---- lseek: noop_llseek. Succeeds, reports 0, and never consults the
        // offset. These are the rows an implementation that fell through to the
        // ordinary file arithmetic would fail while still passing a plain
        // SEEK_SET-0 test.
        check = 18;
        if (LSeek((IntPtr)p, 0, 0) != 0) return check;
        check = 19;
        if (LSeek((IntPtr)p, 7, 0) != 0) return check;
        check = 20;
        if (LSeek((IntPtr)p, -1, 0) != 0) return check;
        check = 21;
        if (LSeek((IntPtr)p, long.MaxValue, 0) != 0) return check;
        check = 22;
        if (LSeek((IntPtr)p, 0, 1) != 0) return check;
        check = 23;
        if (LSeek((IntPtr)p, 0, 2) != 0) return check;

        // Whence 3 and 4 are SEEK_DATA/SEEK_HOLE for a *file*, which PawPrint
        // refuses to answer. A port has no sparseness, so noop_llseek answers
        // them like any other whence the syscall accepts.
        check = 24;
        if (LSeek((IntPtr)p, 0, 3) != 0) return check;
        check = 25;
        if (LSeek((IntPtr)p, 0, 4) != 0) return check;

        // The syscall's own whence guard still applies above SEEK_MAX.
        check = 26;
        if (LSeek((IntPtr)p, 0, 5) != -1) return check;
        check = 27;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // ---- flock: permitted on Linux, for every operation.
        check = 28;
        if (FLock((IntPtr)p, LOCK_SH) != 0) return check;
        check = 29;
        if (FLock((IntPtr)p, LOCK_EX) != 0) return check;
        check = 30;
        if (FLock((IntPtr)p, LOCK_UN) != 0) return check;

        // Two *independently created* ports contend, because on Linux every
        // anon-inode file shares one inode: measured, epoll A, epoll B and an
        // eventfd all report st_ino 15, and LOCK_EX on the second returns
        // EWOULDBLOCK. This is the row that fails if each port is given its own
        // OpenFileObject identity -- PawPrint would then grant two exclusive
        // locks where Linux grants one.
        //
        // LOCK_NB throughout: a blocking acquisition against a held lock is a
        // refusal in PawPrint (it would need the scheduler to park the caller),
        // not an errno.
        check = 31;
        long second = Create();
        if (second != 4) return check;
        check = 32;
        if (FLock((IntPtr)p, LOCK_EX | LOCK_NB) != 0) return check;
        check = 33;
        if (FLock((IntPtr)second, LOCK_EX | LOCK_NB) != -1) return check;
        check = 34;
        if (Marshal.GetLastSystemError() != EWOULDBLOCK) return check;

        // Releasing the first hands the lock over, so the exclusion is a live
        // lock rather than a blanket refusal.
        check = 35;
        if (FLock((IntPtr)p, LOCK_UN) != 0) return check;
        check = 36;
        if (FLock((IntPtr)second, LOCK_EX | LOCK_NB) != 0) return check;
        check = 37;
        if (FLock((IntPtr)second, LOCK_UN) != 0) return check;
        check = 38;
        if (Close((IntPtr)second) != 0) return check;

        // ---- isatty: a port is not a terminal.
        check = 39;
        if (IsATty((IntPtr)p) != 0) return check;

        // ---- dup shares the description rather than copying it, so the second
        // descriptor names the same port and closing one leaves the other live.
        check = 40;
        long q = (long)Dup((IntPtr)p);
        if (q != 4) return check;
        check = 41;
        if (Close((IntPtr)p) != 0) return check;
        check = 42;
        if (LSeek((IntPtr)q, 0, 0) != 0) return check;

        // A second port is a *distinct* instance, and takes the freed number.
        check = 43;
        long r = Create();
        if (r != 3) return check;

        // ---- CloseSocketEventPort is close(2): it returns the PAL enum rather
        // than -1-and-errno, and it does not check that the descriptor is a
        // port.
        check = 44;
        Marshal.SetLastSystemError(0);
        if (CloseSocketEventPort((IntPtr)r) != PAL_SUCCESS) return check;
        // Unix convention: a successful call leaves errno alone.
        check = 45;
        if (Marshal.GetLastSystemError() != 0) return check;

        check = 46;
        if (CloseSocketEventPort((IntPtr)r) != PAL_EBADF) return check;
        // The PAL code is the return value, but close(2) still sets errno on the
        // way past, so a caller reading it sees EBADF rather than the stale 0
        // this check deliberately seeded above.
        check = 47;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        check = 48;
        if (CloseSocketEventPort((IntPtr)q) != PAL_SUCCESS) return check;

        // Both descriptors are gone, so the number is free again.
        check = 49;
        long s = Create();
        if (s != 3) return check;
        check = 50;
        if (Close((IntPtr)s) != 0) return check;

        // ---- a null out-pointer is EFAULT, and creates nothing: the next
        // successful create still gets fd 3.
        //
        // Only *null* takes this path. A non-null address naming no storage
        // passes the C wrapper's single screen, so the real code creates the
        // descriptor and then faults storing through it; PawPrint refuses loudly
        // there rather than inventing an EFAULT, which no exit-code guest can
        // assert.
        check = 51;
        if (CreateSocketEventPort(null) != PAL_EFAULT) return check;
        check = 52;
        long t = Create();
        if (t != 3) return check;

        // ---- fcntl(F_SETFL): on an epoll descriptor the call succeeds and
        // O_NONBLOCK round-trips, where Darwin's kqueue answers -1/ENOTTY (with
        // the bit toggled regardless -- the sibling file's rows). Measured on
        // 6.18.5.
        check = 53;
        int flag = 9;
        if (GetIsNonBlocking((IntPtr)t, &flag) != 0) return check;
        check = 54;
        if (flag != 0) return check;
        check = 55;
        if (SetIsNonBlocking((IntPtr)t, 1) != 0) return check;
        check = 56;
        if (GetIsNonBlocking((IntPtr)t, &flag) != 0) return check;
        check = 57;
        if (flag != 1) return check;
        check = 58;
        if (SetIsNonBlocking((IntPtr)t, 0) != 0) return check;
        check = 59;
        if (GetIsNonBlocking((IntPtr)t, &flag) != 0) return check;
        check = 60;
        if (flag != 0) return check;

        return 0;
    }
}
