using System;
using System.Runtime.InteropServices;

// The `SystemNative_WaitForSocketEvents` rows whose answer is the same on every
// kernel: the C wrapper's own argument screen, which answers before any syscall
// runs, and a descriptor that is not open at all.
//
// Differential, and only these rows can be. The wrapper (pal_networking.c:3492)
// is compiled once for every platform, and EBADF for a closed descriptor is the
// one syscall row epoll and kqueue agree on. What each flavour writes through
// `count` on that row does *not* agree -- 0 under epoll, -1 under kqueue -- so
// this file never reads `count` after a row that reached the syscall. Those
// columns are asserted against PawPrint's own platform presets by the siblings
// SocketEventsWaitLinux.cs and SocketEventsWaitDarwin.cs.
//
// Every row is reached with everything *else* valid -- a real port, a real
// buffer, a positive count -- so that an earlier guard cannot be what answered.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    // Interop.Error values, not raw errnos: this entry point returns the PAL enum
    // directly rather than -1-and-errno.
    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;

    // No descriptor this high is open in either runtime: PawPrint's table holds
    // the three standard streams plus whatever this guest opened, and a real
    // `dotnet` process holds a few dozen. Deliberately *not* a descriptor this
    // guest closed itself -- the host runtime's own threads could reopen that
    // number between the close and the wait, and a deterministic suite must not
    // contain that race.
    const int ClosedFd = 500;

    // Somewhere to point a function pointer at. Never called.
    static void Nothing()
    {
    }

    static unsafe int Main()
    {
        int check;

        IntPtr port;
        check = 1;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;

        // 32 bytes is one `struct kevent`, the larger of the two element sizes, so
        // a one-event buffer is in range on either kernel.
        byte* buffer = stackalloc byte[32];

        // ---- `buffer == NULL` is the wrapper's first screen, and it answers
        // without running a syscall -- so `count` is left exactly as set here.
        check = 2;
        int count = 1;
        if (WaitForSocketEvents(port, null, &count) != PAL_EFAULT) return check;
        check = 3;
        if (count != 1) return check;

        // ---- `count == NULL`. There is nothing to check afterwards, which is
        // the point: the wrapper cannot have dereferenced it.
        check = 4;
        if (WaitForSocketEvents(port, buffer, null) != PAL_EFAULT) return check;

        // ---- A negative `*count`. EFAULT, which is the wrapper's own answer and
        // neither kernel's: `epoll_wait` reports EINVAL for a non-positive
        // `maxevents`, and never sees this one.
        check = 5;
        count = -1;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_EFAULT) return check;
        check = 6;
        if (count != -1) return check;

        // ---- A descriptor that is not open. Past the wrapper, so this is the
        // syscall's own answer, and it is EBADF on both kernels.
        check = 7;
        count = 1;
        if (WaitForSocketEvents((IntPtr)ClosedFd, buffer, &count) != PAL_EBADF) return check;

        // ---- The wrapper's screens are *ahead* of the descriptor lookup, which
        // takes an input that would provoke both. A closed descriptor with a null
        // buffer, and a closed descriptor with a negative count, are EFAULT rather
        // than EBADF -- and that is flavour-free, since it is the wrapper's own
        // ordering rather than either kernel's.
        check = 8;
        count = 1;
        if (WaitForSocketEvents((IntPtr)ClosedFd, null, &count) != PAL_EFAULT) return check;
        check = 9;
        if (count != 1) return check;

        check = 10;
        count = -1;
        if (WaitForSocketEvents((IntPtr)ClosedFd, buffer, &count) != PAL_EFAULT) return check;
        check = 11;
        if (count != -1) return check;

        // ---- The screens are ahead of `ToFileDescriptor(port)` as well, and that
        // is observable with a `port` whose value is not a number: a function
        // pointer. The C never inspects the argument on this path, so the answer is
        // EFAULT and says nothing about any descriptor -- where an implementation
        // that decoded `port` up front would have to make something up for a value
        // that is not an fd.
        check = 12;
        count = 1;
        if (WaitForSocketEvents((IntPtr)(void*)(delegate*<void>)&Nothing, null, &count) != PAL_EFAULT) return check;
        check = 13;
        if (count != 1) return check;

        check = 14;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;

        return 0;
    }
}
