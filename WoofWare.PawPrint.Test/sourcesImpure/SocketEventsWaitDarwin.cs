using System;
using System.Runtime.InteropServices;

// `SystemNative_WaitForSocketEvents` under the Darwin flavour: the rows where
// `kevent` answers differently from `epoll_wait`, which is most of them. The Linux
// column is in the sibling SocketEventsWaitLinux.cs, and the rows both kernels
// agree on are differential and live in
// sourcesPure/SocketEventsWaitScreening.cs. Configured as macOS for the same
// reason SocketEventPortDarwin.cs is.
//
// Three divergences, all measured on Darwin 25.6.0 arm64:
//
//   * the error sentinel is `*count = -1`, where epoll writes 0;
//   * `*count == 0` on a valid port *succeeds immediately* rather than answering
//     EINVAL -- `kevent` returns 0, and the zero-event assertion that would catch
//     it is compiled out of the shipped release build;
//   * a live descriptor that is not a port is EBADF, not EINVAL: kqueue has no
//     "wrong kind of object" answer.
//
// Darwin also screens no buffer up front, so the one row this file cannot assert
// is a valid port with a positive count and an unmappable buffer: on Linux that is
// EFAULT, and here it blocks forever.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents", SetLastError = true)]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;

    const int EBADF = 9;
    const int ESPIPE = 29;

    const int ClosedFd = 500;
    const int Stdout = 1;
    const int SeekSet = 0;

    // A known errno no row under test produces. `lseek` on a pipe is ESPIPE on
    // Darwin as on Linux, and PawPrint models the standard streams as pipes.
    static bool ArrangeErrno()
    {
        return LSeek((IntPtr)0, 0, SeekSet) == -1 && Marshal.GetLastSystemError() == ESPIPE;
    }

    static unsafe int Main()
    {
        int check;
        int count;

        IntPtr port;
        check = 1;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;
        check = 2;
        if ((long)port != 3) return check;

        byte* buffer = stackalloc byte[32];
        byte* unmappable = (byte*)-1;

        // ---- The wrapper is flavour-free, and its rows still run no syscall here.
        check = 3;
        if (!ArrangeErrno()) return check;
        check = 4;
        count = 1;
        if (WaitForSocketEvents(port, null, &count) != PAL_EFAULT) return check;
        check = 5;
        if (count != 1) return check;
        check = 6;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        // ---- A valid port with `*count == 0`. `kevent` returns 0 immediately, so
        // this is the one input on which the two flavours disagree about whether
        // the call blocks at all: EINVAL under epoll, success here. The wait
        // succeeded, so errno is untouched.
        check = 7;
        count = 0;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return check;
        check = 8;
        if (count != 0) return check;
        check = 9;
        if (Marshal.GetLastSystemError() != ESPIPE) return check;

        // ---- The same, with an unmappable buffer. Still success: Darwin screens no
        // buffer up front, so a call that copies nothing never looks at it.
        check = 10;
        count = 0;
        if (WaitForSocketEvents(port, unmappable, &count) != PAL_SUCCESS) return check;
        check = 11;
        if (count != 0) return check;

        // ---- A descriptor that is not open. EBADF, as on Linux -- but the sentinel
        // written through `count` is -1, not 0.
        check = 12;
        count = 1;
        if (WaitForSocketEvents((IntPtr)ClosedFd, buffer, &count) != PAL_EBADF) return check;
        check = 13;
        if (count != -1) return check;
        check = 14;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        // ---- Not open, and `*count == 0`. EBADF rather than the success two rows
        // up, which pins that `kevent` resolves the descriptor *before* its
        // `nevents == 0` early return -- and so that Darwin's success row is
        // reachable only with a real port.
        check = 15;
        count = 0;
        if (WaitForSocketEvents((IntPtr)ClosedFd, buffer, &count) != PAL_EBADF) return check;
        check = 16;
        if (count != -1) return check;

        // ---- A live descriptor that is not a port: EBADF, where epoll says EINVAL.
        check = 17;
        count = 1;
        if (WaitForSocketEvents((IntPtr)Stdout, buffer, &count) != PAL_EBADF) return check;
        check = 18;
        if (count != -1) return check;

        // ---- The same, with `*count == 0`, which is the input that separates the
        // kind check from the early return: both would fire, and EBADF is the
        // answer, so Darwin's immediate success needs a real port and not merely a
        // live descriptor.
        check = 19;
        count = 0;
        if (WaitForSocketEvents((IntPtr)Stdout, buffer, &count) != PAL_EBADF) return check;
        check = 20;
        if (count != -1) return check;

        // ---- A negative `*count` is still the wrapper's EFAULT, and still leaves
        // `count` alone -- asserted here as well as on Linux because the sentinel
        // write is what a flavour-branching handler could get wrong on this row.
        check = 21;
        count = -1;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_EFAULT) return check;
        check = 22;
        if (count != -1) return check;

        check = 23;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;

        return 0;
    }
}
