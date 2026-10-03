using System;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;

// FD_CLOEXEC, through the two entry points that carry it:
// `SystemNative_FcntlGetFD` (pal_io.c:616, `fcntl(F_GETFD)`) and
// `SystemNative_FcntlSetFD` (pal_io.c:609, `fcntl(F_SETFD,
// ConvertOpenFlags(flags))`), and where the descriptors the shim makes get it
// from. Differential: every row answers identically on Linux and macOS.
//
// Facts pinned:
//
//   * `SystemNative_Dup` is `F_DUPFD_CLOEXEC`, so its copy has FD_CLOEXEC
//     whatever its source had, and the source keeps its own;
//   * the flag is per descriptor: clearing it on the copy leaves a second copy
//     alone;
//   * `SystemNative_FcntlSetFD`'s argument goes through the *open* flags'
//     conversion, so PAL_O_WRONLY (1) becomes O_WRONLY (1), which the kernel
//     reads as FD_CLOEXEC, and PAL_O_CLOEXEC (0x10) becomes the platform's
//     O_CLOEXEC, whose bit 0 is clear, so it *clears* the flag;
//   * a socket from `SystemNative_Socket` has it (SOCK_CLOEXEC on Linux, a
//     `fcntl` after the call on Darwin), and so do one managed code makes and
//     one it accepts;
//     a pipe has it exactly when PAL_O_CLOEXEC was asked for;
//   * a descriptor that is not open answers -1 to both calls.
//
// Return codes only, as FcntlNonBlocking.cs does: the raw errno numbers differ
// between the platforms.
//
// The exit code names the first check that failed; 0 means all passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetFD")]
    static extern int GetFD(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetFD")]
    static extern int SetFD(IntPtr fd, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe")]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    // Interop.Sys.OpenFlags and PipeFlags.
    const int PAL_O_WRONLY = 0x0001;
    const int PAL_O_CLOEXEC = 0x0010;

    // Interop.Sys's AddressFamily, SocketType and ProtocolType.
    const int PAL_AF_INET = 2;
    const int PAL_SOCK_STREAM = 1;
    const int PAL_PT_TCP = 6;

    static unsafe int Main(string[] args)
    {
        // stdin is not close-on-exec: a process's standard streams survive the
        // exec that started it.
        if (GetFD((IntPtr)0) != 0) return 1;

        IntPtr copy = Dup((IntPtr)0);
        if ((long)copy < 0) return 2;
        if (GetFD(copy) != 1) return 3;
        // The source keeps its own.
        if (GetFD((IntPtr)0) != 0) return 4;

        IntPtr second = Dup(copy);
        if ((long)second < 0) return 5;
        if (GetFD(second) != 1) return 6;

        if (SetFD(copy, 0) != 0) return 7;
        if (GetFD(copy) != 0) return 8;
        // Per descriptor: the second copy is untouched.
        if (GetFD(second) != 1) return 9;

        // PAL_O_WRONLY converts to O_WRONLY, which is FD_CLOEXEC's bit.
        if (SetFD(copy, PAL_O_WRONLY) != 0) return 10;
        if (GetFD(copy) != 1) return 11;

        // PAL_O_CLOEXEC converts to the platform's O_CLOEXEC, which is not.
        if (SetFD(copy, PAL_O_CLOEXEC) != 0) return 12;
        if (GetFD(copy) != 0) return 13;

        if (Close(copy) != 0) return 14;
        if (Close(second) != 0) return 15;

        // Closed, and never open.
        if (GetFD(copy) != -1) return 16;
        if (SetFD(copy, 0) != -1) return 17;
        if (GetFD((IntPtr)(-1)) != -1) return 18;

        IntPtr socket;
        if (Socket(PAL_AF_INET, PAL_SOCK_STREAM, PAL_PT_TCP, &socket) != 0) return 19;
        if (GetFD(socket) != 1) return 20;
        if (Close(socket) != 0) return 21;

        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 22;
        if (GetFD((IntPtr)fds[0]) != 0 || GetFD((IntPtr)fds[1]) != 0) return 23;
        Close((IntPtr)fds[0]);
        Close((IntPtr)fds[1]);

        if (Pipe(fds, PAL_O_CLOEXEC) != 0) return 24;
        if (GetFD((IntPtr)fds[0]) != 1 || GetFD((IntPtr)fds[1]) != 1) return 25;
        Close((IntPtr)fds[0]);
        Close((IntPtr)fds[1]);

        using (var managed = new System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            if (GetFD(managed.Handle) != 1) return 26;
        }

        // An accepted socket: `accept4(SOCK_CLOEXEC)` on Linux, `accept` and a
        // `fcntl` on Darwin.
        using (var listener = new System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
        {
            listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
            listener.Listen(1);

            using (var client = new System.Net.Sockets.Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
            {
                client.Connect(listener.LocalEndPoint!);

                using (var accepted = listener.Accept())
                {
                    if (GetFD(accepted.Handle) != 1) return 27;
                }
            }
        }

        return 0;
    }
}
