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
// Whether a waiter has reached kevent cannot be seen from outside it, so the
// drain is checked in trials: one in which the waiter was not yet asleep when
// its descriptor closed drains nothing, which the survivor's wait detects by
// sleeping, and is retried. Under PawPrint the waiter is parked before the
// close, so the first trial is conclusive.
//
// The exit code is the index of the first check that failed; 0 means all
// passed, and 100 that no trial was conclusive.
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

    // One wait for one event through `Through`, made on a thread of its own.
    sealed class Wait
    {
        public IntPtr Through;
        public volatile int Result = int.MinValue;
        public volatile int Count = int.MinValue;
        public volatile int Errno = int.MinValue;

        public unsafe void Run()
        {
            byte* buffer = stackalloc byte[EventSize];
            int count = 1;
            int result = WaitForSocketEvents(Through, buffer, &count);
            Errno = Marshal.GetLastPInvokeError();
            Count = count;
            Result = result;
        }

        public static Thread Start(Wait wait)
        {
            Thread thread = new Thread(wait.Run);
            thread.IsBackground = true;
            thread.Start();
            return thread;
        }
    }

    // How many trials of the drain may find that the waiter was not yet asleep
    // in kevent when its descriptor closed, before the run gives up.
    const int Trials = 5;

    // The exit code when every trial was inconclusive: no close drained a
    // kqueue under a sleeper, so the rows that need one were never checked.
    const int AllTrialsInconclusive = 100;

    const int Inconclusive = -1;

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

        check = 10;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;

        for (int trial = 0; trial < Trials; trial++)
        {
            int outcome = DrainTrial();
            if (outcome != Inconclusive) return outcome;
        }

        return AllTrialsInconclusive;
    }

    // Closes a kqueue's descriptor under a thread waiting through it, and checks
    // the drain. Answers 0 when every check passed, the failing check's number,
    // or Inconclusive when the waiter had not reached kevent by the close, so
    // that the close drained nothing.
    static unsafe int DrainTrial()
    {
        int check;
        int count;
        byte* buffer = stackalloc byte[EventSize];

        IntPtr port;
        check = 11;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return check;

        // ---- Two dups: one to close under the waiter, one to outlive the drain.
        IntPtr bystander = Dup(port);
        check = 12;
        if ((long)bystander < 0) return check;
        IntPtr survivor = Dup(port);
        check = 13;
        if ((long)survivor < 0) return check;

        Wait waited = new Wait { Through = port };
        Thread waiter = Wait.Start(waited);

        // Under PawPrint the join's deadline is the virtual clock's, so it costs no
        // wall time and the waiter is parked when it expires. On the real runtime
        // the waiter may not have reached kevent yet, which the survivor's wait
        // below detects.
        check = 14;
        if (waiter.Join(200)) return check;

        // ---- Closing a descriptor the waiter did not enter through wakes nobody.
        check = 15;
        if (CloseSocketEventPort(bystander) != PAL_SUCCESS) return check;
        check = 16;
        if (waiter.Join(200)) return check;

        // ---- Closing the one it did ends its wait with EBADF. So does a waiter
        // that reaches kevent only after the close, through a closed descriptor.
        check = 17;
        if (CloseSocketEventPort(port) != PAL_SUCCESS) return check;
        check = 18;
        if (!waiter.Join(5000)) return check;
        check = 19;
        if (waited.Result != PAL_EBADF) return check;
        check = 20;
        if (waited.Count != -1) return check;
        check = 21;
        if (waited.Errno != EBADF) return check;

        // ---- The kqueue stays drained: a wait through the survivor is EBADF at
        // once. One that sleeps instead means the close drained nothing, because
        // the waiter was not yet asleep in kevent; closing the descriptor the
        // survivor's wait entered through then drains the kqueue and ends it.
        Wait probed = new Wait { Through = survivor };
        Thread prober = Wait.Start(probed);

        if (!prober.Join(2000))
        {
            check = 22;
            if (CloseSocketEventPort(survivor) != PAL_SUCCESS) return check;
            check = 23;
            if (!prober.Join(5000)) return check;
            return Inconclusive;
        }

        check = 24;
        if (probed.Result != PAL_EBADF) return check;
        check = 25;
        if (probed.Count != -1) return check;

        // ---- A wait for zero events on the drained kqueue still returns none.
        check = 26;
        count = 0;
        if (WaitForSocketEvents(survivor, buffer, &count) != PAL_SUCCESS) return check;
        check = 27;
        if (count != 0) return check;

        // ---- A registration change through a closed kqueue is EBADF, ahead of
        // anything about the change.
        check = 28;
        if (CloseSocketEventPort(survivor) != PAL_SUCCESS) return check;
        check = 29;
        if (TryChange(survivor, (IntPtr)1, 0, SA_READ, IntPtr.Zero) != PAL_EBADF) return check;
        check = 30;
        if (Marshal.GetLastPInvokeError() != EBADF) return check;

        return 0;
    }
}
