using System;
using System.Runtime.InteropServices;

// `SystemNative_TryChangeSocketEventRegistration` (pal_networking.c:3471)
// reached by hand-rolled P/Invoke. No modelled operation can make a registered
// descriptor ready -- `SystemNative_Connect` does not exist -- so nothing here
// waits for an event; every row is about the wrapper's screens and the
// registration table's own answers.
//
// Differential, so only rows on which the two real implementations (epoll on
// Linux, kqueue on macOS) agree through the PAL wrapper; the epoll-only rows
// (EEXIST, EPERM, EINVAL for a non-epoll port, the dup key-shape rows) live in
// `SocketEventRegistrationLinux.cs`, under PawPrint alone where the flavour is
// known. The MOD/DEL sequence below is chosen so that kqueue's per-filter state
// agrees with epoll's whole-entry state at every step: each MOD only ever
// *removes* a filter that the claimed `currentEvents` says is present.
//
// The facts pinned deliberately:
//
//   * bits outside READ|WRITE|READCLOSE|CLOSE|ERROR in either argument answer
//     EINVAL, ahead of the current==new short-circuit and of any look at
//     either descriptor;
//   * current == new answers SUCCESS having consulted nothing -- dead
//     descriptors and no-descriptor-at-all (a pointer as `port`) included;
//   * ADD, MOD and DEL of a live socket succeed, deriving the op from the
//     caller's claims;
//   * MOD or DEL of an unregistered target answers ENOENT;
//   * a dead port answers EBADF, and so does an ADD of a dead target.
//
// DEL of a dead target is deliberately absent: measured, Linux answers EBADF
// (the fd lookup beats the table check) where Darwin answers ENOENT (close
// already discarded the knotes, so the per-filter lookup fails first). That
// row lives in `SocketEventRegistrationLinux.cs`.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class SocketEventRegistration
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration")]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOENT = 0x1002D;

    // PAL SocketEvents bits.
    const int SA_READ = 0x01;
    const int SA_WRITE = 0x02;
    const int SA_BAD = 0x20; // first bit outside SupportedEvents

    // PAL numbering, which is not any platform's.
    const int AF_INET = 2;
    const int SOCK_STREAM = 1;
    const int PT_TCP = 6;

    // A descriptor number nothing here allocates, so a live answer through it
    // is proof the callee never looked it up.
    static readonly IntPtr NeverAllocated = (IntPtr)77;

    static unsafe int Main(string[] args)
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;

        IntPtr s;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &s) != PAL_SUCCESS) return 2;

        // --- the bits screen: EINVAL, consulting nothing ---
        if (TryChange(port, s, SA_BAD, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EINVAL) return 3;
        if (TryChange(port, s, 0, SA_BAD, IntPtr.Zero) != PAL_EINVAL) return 4;
        // Ahead of the current==new short-circuit: equal *invalid* masks are
        // still EINVAL, not SUCCESS.
        if (TryChange(port, s, SA_BAD, SA_BAD, IntPtr.Zero) != PAL_EINVAL) return 5;
        // Ahead of the descriptor lookup: dead fds, and a pointer as `port`.
        int scratch = 0;
        if (TryChange(NeverAllocated, NeverAllocated, SA_BAD, 0, IntPtr.Zero) != PAL_EINVAL) return 6;
        if (TryChange((IntPtr)(&scratch), s, SA_BAD, 0, IntPtr.Zero) != PAL_EINVAL) return 7;

        // --- the current==new short-circuit: SUCCESS, consulting nothing ---
        if (TryChange(port, s, SA_READ | SA_WRITE, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_SUCCESS) return 8;
        if (TryChange(port, s, 0, 0, IntPtr.Zero) != PAL_SUCCESS) return 9;
        if (TryChange(NeverAllocated, s, SA_READ, SA_READ, IntPtr.Zero) != PAL_SUCCESS) return 10;
        if (TryChange((IntPtr)(&scratch), s, SA_READ, SA_READ, IntPtr.Zero) != PAL_SUCCESS) return 11;

        // --- ADD, MOD, DEL of a live socket ---
        if (TryChange(port, s, 0, SA_READ | SA_WRITE, (IntPtr)0xABCD) != PAL_SUCCESS) return 12;
        // MOD down to READ alone: kqueue deletes the WRITE filter it holds.
        if (TryChange(port, s, SA_READ | SA_WRITE, SA_READ, IntPtr.Zero) != PAL_SUCCESS) return 13;
        // MOD across to WRITE alone: kqueue deletes READ and adds WRITE.
        if (TryChange(port, s, SA_READ, SA_WRITE, IntPtr.Zero) != PAL_SUCCESS) return 14;
        // DEL: kqueue deletes the WRITE filter, epoll drops the entry.
        if (TryChange(port, s, SA_WRITE, 0, IntPtr.Zero) != PAL_SUCCESS) return 15;

        // --- ENOENT: the table's own answer for an absent registration ---
        if (TryChange(port, s, SA_READ | SA_WRITE, 0, IntPtr.Zero) != PAL_ENOENT) return 16;
        IntPtr t;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &t) != PAL_SUCCESS) return 17;
        // A MOD whose only change *removes*: never registered, so ENOENT on
        // both sides (kqueue's first and only change is a failing delete).
        if (TryChange(port, t, SA_READ, 0, IntPtr.Zero) != PAL_ENOENT) return 18;

        // --- EBADF: descriptor lookups beat the op's table check ---
        IntPtr dead;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &dead) != PAL_SUCCESS) return 19;
        if (Close(dead) != 0) return 20;
        // Dead port, live target.
        if (TryChange(dead, t, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EBADF) return 21;
        // Live port, dead target, ADD.
        if (TryChange(port, dead, 0, SA_READ | SA_WRITE, IntPtr.Zero) != PAL_EBADF) return 22;

        // --- `data` is opaque: failures never read it, and DEL never stores
        //     it, so a pointer there must not disturb either answer ---
        if (TryChange(dead, t, 0, SA_READ | SA_WRITE, (IntPtr)(&scratch)) != PAL_EBADF) return 27;
        if (TryChange(port, t, 0, SA_READ | SA_WRITE, (IntPtr)55) != PAL_SUCCESS) return 28;
        if (TryChange(port, t, SA_READ | SA_WRITE, 0, (IntPtr)(&scratch)) != PAL_SUCCESS) return 29;

        if (Close(t) != 0) return 24;
        if (Close(s) != 0) return 25;
        if (Close(port) != 0) return 26;

        return 0;
    }
}
