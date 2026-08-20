using System;
using System.Runtime.InteropServices;

// `SystemNative_WaitForSocketEvents` under the Linux flavour: `epoll_wait`'s four
// screens, the order they are applied in, and what each writes through `count`.
//
// PawPrint-only. Five of the eight rows of the entry point's contract differ
// between the two kernels, so a differential guest would have to agree with
// whichever kernel the test host happens to be -- macOS locally, Linux in CI. The
// Darwin column is asserted by the sibling SocketEventsWaitDarwin.cs, under the
// macOS preset; the rows both kernels agree on are differential and live in
// sourcesPure/SocketEventsWaitScreening.cs. The descriptor *numbers* asserted here
// are unpredictable under the oracle for the reason OpenFdNumbering.cs gives.
//
// Measured on Linux 6.18.5 aarch64 via the `container` CLI, not read off the
// source: the widely-reproduced `do_epoll_wait` listing checks `maxevents` and
// `access_ok` before `fdget`, which is stale. Current kernels resolve the
// descriptor first, so `epoll_wait(badfd, evs, 0, -1)` is EBADF and not EINVAL.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    // Deliberately *without* `SetLastError`. Several rows below assert that a
    // call left the previous errno standing, and that is a fact about the
    // native: through a flagged import the P/Invoke stub zeroes errno before the
    // call, so "left alone" and "cleared" would be the same observation. See
    // sourcesPure/PInvokeSetLastError.cs for the stub itself.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    // Interop.Error values: this entry point returns the PAL enum rather than
    // -1-and-errno.
    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;

    // Raw errnos, which the syscall still sets on its way past even though the
    // entry point's *return* is a PAL code. ESPIPE is the arranged prior value:
    // distinct from every errno any row below produces, so "left alone" and
    // "overwritten" are different numbers rather than coincidentally equal ones.
    const int EBADF = 9;
    const int EFAULT = 14;
    const int EINVAL = 22;
    const int ESPIPE = 29;

    // `EP_MAX_EVENTS` is `INT_MAX / sizeof(struct epoll_event)`, and that size is
    // 12 on x86-64 -- `EPOLL_PACKED` is `__attribute__((packed))` under
    // `#ifdef __x86_64__`, over `{ __poll_t events; __u64 data; }`. So the cap is
    // 178_956_970 for the linux-x64 platform PawPrint simulates. (It would be
    // 134_217_727 on aarch64, where the struct is unpacked at 16 bytes.)
    const int EpMaxEvents = 178956970;

    const int ClosedFd = 500;
    const int Stdout = 1;
    const int SeekSet = 0;

    // Arrange a known errno that no row under test produces: PawPrint models the
    // standard streams as pipes, and `lseek` on a pipe is ESPIPE.
    static bool ArrangeErrno()
    {
        return LSeek((IntPtr)0, 0, SeekSet) == -1 && Marshal.GetLastSystemError() == ESPIPE;
    }

    static unsafe int Main()
    {
        int check;
        int count;

        // A port takes the lowest free descriptor, so with stdin/stdout/stderr at
        // 0..2 the first one is fd 3.
        IntPtr port;
        check = 1;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;
        check = 2;
        if ((long)port != 3) return check;

        byte* buffer = stackalloc byte[32];
        // A user address the kernel refuses before doing anything: `access_ok`
        // rejects a range whose end passes `TASK_SIZE_MAX`, and this one is the top
        // of the address space.
        byte* unmappable = (byte*)-1;

        // ---- The wrapper's EFAULT rows run no syscall, so they leave errno alone.
        check = 3;
        if (!ArrangeErrno()) return check;
        check = 4;
        count = 1;
        if (WaitForSocketEvents(port, null, &count) != PAL_EFAULT) return check;
        check = 5;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;
        check = 6;
        if (count != 1) return check;

        // ---- A valid port with `*count == 0`. The wrapper rejects a *negative*
        // count itself, so zero is the only non-positive value that reaches
        // `epoll_wait`, whose `maxevents <= 0` screen answers EINVAL. On failure
        // the inner function writes `*count = 0` -- which is what a caller reads
        // back, and is Linux's sentinel where kqueue's is -1.
        check = 7;
        count = 0;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_EINVAL) return check;
        check = 8;
        if (count != 0) return check;
        // ...and, unlike the wrapper's rows, the syscall set errno on the way past.
        check = 9;
        if (Marshal.GetLastSystemError() != EINVAL) return check;

        // ---- A descriptor that is not open, with `*count == 0`. Both screens
        // would fire; EBADF is the answer, which pins that the descriptor is
        // resolved before `maxevents` is looked at.
        check = 10;
        count = 0;
        if (WaitForSocketEvents((IntPtr)ClosedFd, buffer, &count) != PAL_EBADF) return check;
        check = 11;
        if (count != 0) return check;
        check = 12;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        // ---- A live descriptor that is not a port. EINVAL: epoll has a
        // "wrong kind of object" answer, where kqueue folds that into EBADF.
        check = 13;
        count = 1;
        if (WaitForSocketEvents((IntPtr)Stdout, buffer, &count) != PAL_EINVAL) return check;
        check = 14;
        if (count != 0) return check;

        // ---- A live non-port descriptor *and* an unmappable buffer. EFAULT, which
        // pins that the buffer is screened before the object's kind is asked about.
        check = 15;
        count = 1;
        if (WaitForSocketEvents((IntPtr)Stdout, unmappable, &count) != PAL_EFAULT) return check;
        check = 16;
        if (count != 0) return check;
        check = 17;
        if (Marshal.GetLastSystemError() != EFAULT) return check;

        // ---- A valid port with an unmappable buffer. Not a mappedness check: on
        // 64-bit Linux `access_ok` only rejects ranges reaching into the kernel
        // half, so a merely-unmapped userspace address passes it and the wait then
        // blocks, faulting at delivery. This address is refused because it is above
        // the limit, not because nothing is mapped there.
        check = 18;
        count = 1;
        if (WaitForSocketEvents(port, unmappable, &count) != PAL_EFAULT) return check;
        check = 19;
        if (count != 0) return check;

        // ---- One past `EP_MAX_EVENTS`, with the unmappable buffer still in place.
        // EINVAL rather than EFAULT, which pins that `maxevents` is screened before
        // the buffer -- and that ordering is not cosmetic: the buffer's extent is
        // `maxevents * sizeof(struct epoll_event)` bytes, which for this count
        // overflows `int32`, so the cap is what makes the multiplication safe.
        check = 20;
        count = EpMaxEvents + 1;
        if (WaitForSocketEvents(port, unmappable, &count) != PAL_EINVAL) return check;
        check = 21;
        if (count != 0) return check;

        // ---- `EP_MAX_EVENTS` exactly, same buffer. EFAULT, so the cap is
        // inclusive: an off-by-one in the constant would answer EINVAL here, and
        // the row above is what stops that being satisfiable by widening the cap.
        check = 22;
        count = EpMaxEvents;
        if (WaitForSocketEvents(port, unmappable, &count) != PAL_EFAULT) return check;
        check = 23;
        if (count != 0) return check;

        // ---- A null `count` also leaves errno alone, checked from a prior value
        // the previous row did not already produce.
        check = 24;
        if (!ArrangeErrno()) return check;
        check = 25;
        if (WaitForSocketEvents(port, buffer, null) != PAL_EFAULT) return check;
        check = 26;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        check = 27;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;

        return 0;
    }
}
