using System;
using System.Runtime.InteropServices;

// The epoll-only rows of `SystemNative_TryChangeSocketEventRegistration`,
// under the Linux flavour: everything on which kqueue disagrees, so nothing
// here can live in the differential `SocketEventRegistration.cs`. Every
// expected value was measured with a C probe on Linux 6.18.5 (via
// `container`), not read off the kernel source.
//
// The facts pinned deliberately:
//
//   * a second ADD of the same target answers EEXIST (kqueue silently
//     replaces);
//   * the registration key is the (fd, open file description) *pair*: an ADD
//     through a `dup` of a registered target succeeds and creates a second
//     registration, so both DELs then succeed;
//   * a `dup` of the *port* shares the one interest table;
//   * a regular file cannot be registered: EPERM, for MOD and DEL as much as
//     ADD (the can-poll check beats the op's table check; kqueue registers
//     files happily);
//   * a non-epoll port answers EINVAL, for all three ops (kqueue folds this
//     into EBADF);
//   * the target-fd lookup beats the not-an-epoll check (dead target through a
//     socket "port" is EBADF, not EINVAL);
//   * the can-poll check beats the not-an-epoll check (file as port *and*
//     target is EPERM, not EINVAL) -- and a pollable target through a file
//     port is EINVAL, so the two rows disagree on purpose;
//   * the same-object EINVAL compares open file *descriptions*, not fd
//     numbers: a `dup` of the port as target is EINVAL;
//   * DEL of a dead target is EBADF (Darwin answers ENOENT, the row that
//     evicted this from the pure guest);
//   * a standard stream (a pipe here) registers fine.
//
// No row asserts that the wrapper's screens leave errno alone: through a
// `SetLastError = true` import that claim is false of real .NET, whose stub
// zeroes errno before every call (see GetFileSystemTypeLinux.cs's note; the
// stub itself is not yet modelled), so such a row would pin PawPrint to a
// divergence. Failure errnos are asserted, which the pre-call zero cannot
// disturb.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketEventRegistrationLinux
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration", SetLastError = true)]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open")]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EEXIST = 0x10014;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_EPERM = 0x10042;

    // Linux's numbering.
    const int EPERM = 1;
    const int ENOENT = 2;
    const int EBADF = 9;
    const int EEXIST = 17;
    const int EINVAL = 22;

    // PAL SocketEvents bits.
    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;
    const int SA_BAD = 0x20;

    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;

    // Interop.Sys.OpenFlags (PAL numbering).
    const int O_WRONLY = 0x0001;
    const int O_CREAT = 0x0020;

    static unsafe int Main(string[] args)
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;
        IntPtr s;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &s) != PAL_SUCCESS) return 2;

        // --- EEXIST, and its number ---
        if (TryChange(port, s, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_SUCCESS) return 3;
        if (TryChange(port, s, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EEXIST) return 4;
        if (Marshal.GetLastSystemError() != EEXIST) return 5;

        // --- the key is the (fd, description) pair ---
        IntPtr d = Dup(s);
        if (d == (IntPtr)(-1)) return 6;
        // Same description, different fd: a second, separate registration.
        if (TryChange(port, d, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_SUCCESS) return 7;
        if (TryChange(port, s, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_SUCCESS) return 8;
        if (TryChange(port, d, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_SUCCESS) return 9;
        if (TryChange(port, d, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_ENOENT) return 10;
        if (Marshal.GetLastSystemError() != ENOENT) return 11;
        if (Close(d) != 0) return 12;

        // --- a dup of the port shares the interest table ---
        IntPtr dp = Dup(port);
        if (dp == (IntPtr)(-1)) return 13;
        if (TryChange(dp, s, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_SUCCESS) return 14;
        if (TryChange(port, s, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EEXIST) return 15;
        if (TryChange(port, s, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_SUCCESS) return 16;

        // --- a regular file: EPERM from all three ops ---
        IntPtr f;
        unsafe
        {
            byte* path = stackalloc byte[8];
            path[0] = (byte)'r'; path[1] = (byte)'e'; path[2] = (byte)'g'; path[3] = 0;
            f = Open(path, O_WRONLY | O_CREAT, 0x1B6 /* 0o666 */);
        }
        if (f == (IntPtr)(-1)) return 17;
        if (TryChange(port, f, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EPERM) return 18;
        if (Marshal.GetLastSystemError() != EPERM) return 19;
        if (TryChange(port, f, SA_READ, SA_WRITE, IntPtr.Zero) != PAL_EPERM) return 20;
        if (TryChange(port, f, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_EPERM) return 21;

        // --- a non-epoll port: EINVAL from all three ops ---
        IntPtr t;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &t) != PAL_SUCCESS) return 22;
        if (TryChange(s, t, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 23;
        if (Marshal.GetLastSystemError() != EINVAL) return 24;
        if (TryChange(s, t, SA_READ, SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 25;
        if (TryChange(s, t, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_EINVAL) return 26;

        // --- orderings, each pinned by an input the two checks disagree on ---
        IntPtr dead;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &dead) != PAL_SUCCESS) return 27;
        if (Close(dead) != 0) return 28;
        // Target lookup beats not-an-epoll: EBADF, not EINVAL.
        if (TryChange(s, dead, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EBADF) return 29;
        if (Marshal.GetLastSystemError() != EBADF) return 30;
        // Can-poll beats not-an-epoll: file port + file target is EPERM...
        if (TryChange(f, f, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EPERM) return 31;
        // ...while file port + pollable target is EINVAL.
        if (TryChange(f, t, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 32;
        // Target-fd lookup beats the op's table check: DEL of a dead target is
        // EBADF, not ENOENT. (Darwin answers ENOENT here.)
        if (TryChange(port, dead, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_EBADF) return 33;

        // --- the same-object EINVAL compares descriptions, not fd numbers ---
        if (TryChange(port, port, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 34;
        if (TryChange(port, dp, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 35;
        if (Close(dp) != 0) return 36;

        // --- a standard stream (modelled as a pipe) registers fine ---
        if (TryChange(port, IntPtr.Zero, 0, SA_READ, IntPtr.Zero) != PAL_SUCCESS) return 37;
        if (TryChange(port, IntPtr.Zero, SA_READ, 0, IntPtr.Zero) != PAL_SUCCESS) return 38;

        if (Close(t) != 0) return 39;
        if (Close(f) != 0) return 40;
        if (Close(s) != 0) return 41;
        if (Close(port) != 0) return 42;

        return 0;
    }
}
