using System;
using System.Runtime.InteropServices;
using System.Threading;

// The socket event port of a Darwin process, which is a kqueue, driven through
// the System.Native shim the way SocketAsyncEngine drives it: kqueue() to make
// it, kevent() with no changes and a null timeout to wait on it, and close() to
// end it. Configured as macOS, and compared with real .NET on a macOS host.
//
// What it pins, each measured on Darwin 27.0.0 by kqueue-kevent.c:
//
//   * a wait for zero events returns at once with none (*count = 0, success),
//     even with an unmappable buffer, since an eventlist is read only to copy
//     events out;
//   * a wait through a descriptor that is not a kqueue is EBADF, *count = -1;
//   * closing a descriptor no waiter entered through wakes nobody;
//   * closing the descriptor a waiter entered through ends its wait with EBADF
//     (*count = -1), even though a dup keeps the kqueue open; and every later
//     wait through that dup is EBADF at once, while a wait for zero events
//     still returns none;
//   * a registration change that touches neither SA_READ nor SA_WRITE is a
//     kevent with no changes, which succeeds, and one through a closed kqueue
//     is EBADF.
//
// No descriptor number is asserted, so the real runtime's own descriptors do
// not matter.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseSocketEventPort")]
    static extern int CloseSocketEventPort(IntPtr port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup", SetLastError = true)]
    static extern IntPtr Dup(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents", SetLastError = true)]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_TryChangeSocketEventRegistration", SetLastError = true)]
    static extern int TryChange(IntPtr port, IntPtr socket, int currentEvents, int newEvents, IntPtr data);

    const int PAL_SUCCESS = 0;
    const int PAL_EBADF = 0x10008;

    const int EBADF = 9;

    const int SA_READ = 0x01;
    const int SA_ERROR = 0x10;

    // One kqueue event is 32 bytes on 64-bit Darwin.
    const int EventSize = 32;

    static IntPtr WaitedThrough;
    static volatile int WaiterResult = int.MinValue;
    static volatile int WaiterCount = int.MinValue;
    static volatile int WaiterErrno = int.MinValue;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[EventSize];
        int count = 1;
        int result = WaitForSocketEvents(WaitedThrough, buffer, &count);
        WaiterErrno = Marshal.GetLastPInvokeError();
        WaiterCount = count;
        WaiterResult = result;
    }

    static unsafe int Main()
    {
        int check;
        int count;
        byte* buffer = stackalloc byte[EventSize];

        IntPtr port;
        check = 1;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;

        // ---- Zero events: none, at once, whatever the buffer.
        check = 2;
        count = 0;
        if (WaitForSocketEvents(port, buffer, &count) != PAL_SUCCESS) return check;
        check = 3;
        if (count != 0) return check;
        check = 4;
        count = 0;
        if (WaitForSocketEvents(port, (byte*)8, &count) != PAL_SUCCESS) return check;
        check = 5;
        if (count != 0) return check;

        // ---- Not a kqueue: standard output.
        check = 6;
        count = 1;
        if (WaitForSocketEvents((IntPtr)1, buffer, &count) != PAL_EBADF) return check;
        check = 7;
        if (count != -1) return check;
        check = 8;
        if (Marshal.GetLastPInvokeError() != EBADF) return check;

        // ---- A registration change of neither SA_READ nor SA_WRITE is a kevent
        // with no changes, which succeeds.
        check = 9;
        if (TryChange(port, (IntPtr)1, 0, SA_ERROR, IntPtr.Zero) != PAL_SUCCESS) return check;

        // ---- Two dups: one to close under the waiter, one to outlive the drain.
        IntPtr bystander = Dup(port);
        check = 10;
        if ((long)bystander < 0) return check;
        IntPtr survivor = Dup(port);
        check = 11;
        if ((long)survivor < 0) return check;

        WaitedThrough = port;
        Thread waiter = new Thread(Waiter);
        waiter.IsBackground = true;
        waiter.Start();

        // Under PawPrint the join's deadline is the virtual clock's, so it costs no
        // wall time and the waiter is parked when it expires. On the real runtime a
        // waiter that has not yet reached kevent is caught by the next check anyway:
        // the close below would then leave it waiting on a live kqueue for ever.
        check = 12;
        if (waiter.Join(200)) return check;

        // ---- Closing a descriptor the waiter did not enter through wakes nobody.
        check = 13;
        if (CloseSocketEventPort(bystander) != PAL_SUCCESS) return check;
        check = 14;
        if (waiter.Join(200)) return check;

        // ---- Closing the one it did ends its wait with EBADF.
        check = 15;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;
        check = 16;
        if (!waiter.Join(5000)) return check;
        check = 17;
        if (WaiterResult != PAL_EBADF) return check;
        check = 18;
        if (WaiterCount != -1) return check;
        check = 19;
        if (WaiterErrno != EBADF) return check;

        // ---- The kqueue stays drained: a wait through the survivor is EBADF at
        // once, and a wait for zero events still returns none.
        check = 20;
        count = 1;
        if (WaitForSocketEvents(survivor, buffer, &count) != PAL_EBADF) return check;
        check = 21;
        if (count != -1) return check;
        check = 22;
        count = 0;
        if (WaitForSocketEvents(survivor, buffer, &count) != PAL_SUCCESS) return check;
        check = 23;
        if (count != 0) return check;

        // ---- A registration change through a closed kqueue is EBADF, ahead of
        // anything about the change.
        check = 24;
        if (CloseSocketEventPort(survivor) != PAL_SUCCESS) return check;
        check = 25;
        if (TryChange(survivor, (IntPtr)1, 0, SA_READ, IntPtr.Zero) != PAL_EBADF) return check;
        check = 26;
        if (Marshal.GetLastPInvokeError() != EBADF) return check;

        return 0;
    }
}
