using System;
using System.Runtime.InteropServices;

// The `SystemNative_Socket` rows that are true under the Linux flavour and not
// under Darwin, plus the two socket rows a differential guest cannot reach.
//
// Not differential, for three separate reasons, one per group below:
//
//  * `AF_UNIX` with `SOCK_SEQPACKET` or `SOCK_RAW` is a socket Linux makes and
//    Darwin refuses (measured: EPROTONOSUPPORT), so a differential guest would
//    be asserting whichever kernel the oracle ran on.
//  * `AF_INET6` succeeds on both, but only on a host whose kernel has IPv6
//    configured, which is a property of the machine rather than of the runtime.
//  * `flock` on a socket succeeds on Linux and is ENOTSUP on Darwin, and
//    `lseek`'s two screens are ordered differently on the two.
//
// The whole matrix of triples is checked against the measurement itself in
// TestSocketCreation.fs; what this file adds is that the handler really reaches
// it, and the two descriptor-level rows above.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int PAL_SUCCESS = 0;

    const int AF_UNIX = 1;
    const int AF_INET6 = 23;

    const int SOCK_STREAM = 1;
    const int SOCK_RAW = 3;
    const int SOCK_SEQPACKET = 5;

    const int PT_UNSPECIFIED = 0;
    const int PT_TCP = 6;

    const int LOCK_EX = 2;
    const int LOCK_NB = 4;

    // Linux numbering, which is what PawPrint reports under this flavour.
    const int EINVAL = 22;

    static unsafe IntPtr Create(int addressFamily, int socketType, int protocolType)
    {
        IntPtr created = (IntPtr)0x5EED;

        if (Socket(addressFamily, socketType, protocolType, &created) != PAL_SUCCESS)
        {
            return (IntPtr)(-1);
        }

        return created;
    }

    static unsafe int Main()
    {
        // The first descriptor a guest opens is 3: PawPrint's table starts with
        // the three standard streams and nothing else, and `socket(2)` takes the
        // lowest free number exactly as `open(2)` does.
        IntPtr seqPacket = Create(AF_UNIX, SOCK_SEQPACKET, PT_UNSPECIFIED);
        if ((long)seqPacket != 3)
            return 1;

        IntPtr raw = Create(AF_UNIX, SOCK_RAW, PT_UNSPECIFIED);
        if ((long)raw != 4)
            return 2;

        IntPtr v6 = Create(AF_INET6, SOCK_STREAM, PT_TCP);
        if ((long)v6 < 0)
            return 3;

        // Two sockets are two `flock` objects, so an exclusive lock on one leaves
        // the other free to take its own. Measured on Linux 6.18.5: each socket
        // gets its own `sockfs` inode, unlike two epoll ports, which share the
        // single `anon_inodefs` one and therefore exclude one another.
        Marshal.SetLastSystemError(0);
        if (FLock(seqPacket, LOCK_EX | LOCK_NB) != 0)
            return 4;

        Marshal.SetLastSystemError(0);
        if (FLock(raw, LOCK_EX | LOCK_NB) != 0)
            return 5;

        // A socket is unseekable, and Linux checks the whence *first*: an invalid
        // one is EINVAL rather than the ESPIPE every valid whence gets (which
        // SocketCreateScreens.cs asserts differentially). Darwin reverses this.
        Marshal.SetLastSystemError(0);
        if (LSeek(seqPacket, 0, 9) != -1 || Marshal.GetLastSystemError() != EINVAL)
            return 6;

        if (Close(seqPacket) != 0)
            return 7;

        if (Close(raw) != 0)
            return 8;

        if (Close(v6) != 0)
            return 9;

        return 0;
    }
}
