using System;
using System.Runtime.InteropServices;

// `SystemNative_Poll`'s Linux-flavour rows, plus the multi-entry array path.
//
// `sourcesPure/SocketPoll.cs` carries everything the two kernels agree on,
// through the managed `Socket.Poll`. Everything here is either flavour-specific
// or unreachable from the managed surface:
//
//  * An idle TCP socket presents OUT|HUP on Linux and *nothing* on Darwin, so
//    the single most basic row of all is not portable.
//  * POLLERR/POLLHUP/POLLNVAL are output-only on Linux -- reported whether or
//    not they were asked for, and counted in the return value -- and are not on
//    Darwin: a poll with `events = 0` over a socket carrying HUP answers 1 here
//    and 0 there.
//  * `*triggered` and the multi-entry loop have no managed caller on a
//    macOS-flavour CoreLib at all: `SocketPal.Select` branches on
//    `SelectOverPollIsBroken` (`OperatingSystem.IsMacOS()`, which is
//    `#if TARGET_OSX`), so `Socket.Select` reaches `SystemNative_Select` there
//    and `SelectViaPoll` only on a Linux-flavour image. Calling the entry point
//    directly sidesteps that branch entirely.
//
// Every expectation is measured, not derived --
// docs/plans/2026-08-23-socket-poll, probes `pollmask.c`, `pollmulti.c` and
// `pollimmediate.c`, on Linux 6.18.5 and Darwin 25.6.0.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketPollLinux
{
    [StructLayout(LayoutKind.Sequential)]
    struct PollEvent
    {
        public int FileDescriptor;
        public short Events;
        public short TriggeredEvents;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Poll")]
    static extern unsafe int Poll(PollEvent* pollEvents, uint eventCount, int milliseconds, uint* triggered);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;

    const int PAL_AF_INET = 2;
    const int PAL_SOCK_STREAM = 1;
    const int PAL_PT_TCP = 6;

    const short POLLIN = 0x0001;
    const short POLLOUT = 0x0004;
    const short POLLHUP = 0x0010;
    const short POLLNVAL = 0x0020;

    // Well past anything the fd table hands out, and never opened.
    const int NeverOpened = 4096;

    static unsafe int Main()
    {
        IntPtr idle;
        if (Socket(PAL_AF_INET, PAL_SOCK_STREAM, PAL_PT_TCP, &idle) != PAL_SUCCESS) return 1;
        int idleFd = (int)idle;

        uint triggered;
        PollEvent one;

        // 2-3: the wrapper's own screens, answered in user space. The null check
        // does not consult `eventCount`, which is the one row where this entry
        // point and libc `poll(2)` disagree: `poll(NULL, 0, 0)` succeeds.
        triggered = 12345;
        if (Poll(null, 0, 0, &triggered) != PAL_EFAULT) return 2;
        // `*triggered` is left exactly as the caller set it: the C returns before
        // assigning it.
        if (triggered != 12345) return 3;

        one = new PollEvent { FileDescriptor = idleFd, Events = POLLIN };
        if (Poll(&one, 1, 0, null) != PAL_EFAULT) return 4;

        // 5: `milliseconds < -1`. -1 itself is legal (infinite).
        one = new PollEvent { FileDescriptor = idleFd, Events = POLLIN };
        if (Poll(&one, 1, -2, &triggered) != PAL_EINVAL) return 5;

        // 6-7: an idle TCP socket is write-ready *and* hung up on Linux. This is
        // the row that has no Darwin twin: there it reports nothing at all.
        one = new PollEvent { FileDescriptor = idleFd, Events = POLLIN | POLLOUT };
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return 6;
        if (one.TriggeredEvents != (POLLOUT | POLLHUP)) return 7;

        // 8-9: HUP is output-only. Asking for nothing still reports it, and it
        // still counts towards `*triggered`.
        one = new PollEvent { FileDescriptor = idleFd, Events = 0 };
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return 8;
        if (one.TriggeredEvents != POLLHUP || triggered != 1) return 9;

        // 10-11: request bits outside the six the PAL knows are dropped rather
        // than rejected -- `Common_ConvertPollEventsPalToPlatform` translates
        // exactly six, so the rest never reach the kernel. The answer is
        // therefore the same as asking for nothing.
        one = new PollEvent { FileDescriptor = idleFd, Events = unchecked((short)0x7FC0) };
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return 10;
        if (one.TriggeredEvents != POLLHUP) return 11;

        // 12-14: a negative descriptor is ignored -- no revents, and *not*
        // counted towards the return value. `TriggeredEvents` is nevertheless
        // overwritten, to zero: preloaded garbage must not survive, which is
        // what distinguishes "wrote 0" from "did not write". Measured with a
        // 0xff preload on both kernels (`pollmask.c`, last screen row).
        one = new PollEvent
        {
            FileDescriptor = -1,
            Events = POLLIN | POLLOUT,
            TriggeredEvents = unchecked((short)0x7FFF),
        };
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return 12;
        if (one.TriggeredEvents != 0) return 13;
        if (triggered != 0) return 14;

        // 15-16: an fd naming nothing answers POLLNVAL, which *is* counted.
        // Still not an error return.
        one = new PollEvent { FileDescriptor = NeverOpened, Events = POLLIN };
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return 15;
        if (one.TriggeredEvents != POLLNVAL || triggered != 1) return 16;

        // 17-22: the multi-entry array, and `*triggered` as the number of
        // entries carrying anything -- not `eventCount`, and not the number of
        // conditions. Three of these four are ready, so returning 4 (the count)
        // fails, and so does returning 5 (the conditions: OUT, HUP, NVAL, HUP).
        IntPtr idle2;
        if (Socket(PAL_AF_INET, PAL_SOCK_STREAM, PAL_PT_TCP, &idle2) != PAL_SUCCESS) return 17;

        PollEvent* many = stackalloc PollEvent[4];
        many[0] = new PollEvent { FileDescriptor = idleFd, Events = POLLIN | POLLOUT };
        many[1] = new PollEvent { FileDescriptor = -1, Events = POLLIN };
        many[2] = new PollEvent { FileDescriptor = NeverOpened, Events = POLLIN };
        // Asked only for IN, so only the unconditional HUP comes back -- a
        // different revents from entry 0 on an identically-conditioned socket,
        // which is what pins the per-entry mask rather than a shared one.
        many[3] = new PollEvent { FileDescriptor = (int)idle2, Events = POLLIN };
        if (Poll(many, 4, 0, &triggered) != PAL_SUCCESS) return 18;
        if (triggered != 3) return 19;
        if (many[0].TriggeredEvents != (POLLOUT | POLLHUP)) return 20;
        if (many[1].TriggeredEvents != 0) return 21;
        if (many[2].TriggeredEvents != POLLNVAL) return 22;
        if (many[3].TriggeredEvents != POLLHUP) return 23;

        // 24-27: THE READY PREDICATE. These are the only checks that run at a
        // *positive* timeout, and they are the point of the file rather than a
        // detail: PawPrint answers a poll at any timeout when something is
        // already ready, and refuses only when every entry masks to empty. A
        // ready-predicate computed as "level intersected with what was
        // requested" -- while still writing ERR/HUP into revents correctly --
        // passes every other check here, because every other ready row runs at
        // timeout 0, where the predicate is never consulted.
        //
        // Measured (`pollimmediate.c`): on Linux each of these returns in 0.0ms
        // at timeout 5000 and at timeout -1 alike.
        one = new PollEvent { FileDescriptor = idleFd, Events = POLLIN };
        if (Poll(&one, 1, 5000, &triggered) != PAL_SUCCESS) return 24;
        if (one.TriggeredEvents != POLLHUP || triggered != 1) return 25;

        one = new PollEvent { FileDescriptor = NeverOpened, Events = POLLIN };
        if (Poll(&one, 1, 5000, &triggered) != PAL_SUCCESS) return 26;
        if (one.TriggeredEvents != POLLNVAL || triggered != 1) return 27;

        if (Close(idle2) != PAL_SUCCESS) return 28;
        if (Close(idle) != PAL_SUCCESS) return 29;

        return 0;
    }
}
