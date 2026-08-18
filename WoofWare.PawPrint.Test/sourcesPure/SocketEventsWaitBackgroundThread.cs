using System;
using System.Runtime.InteropServices;
using System.Threading;

// A background thread parked forever in `SystemNative_WaitForSocketEvents`, and
// an entry thread that exits anyway.
//
// Differential, which is what makes it worth having. With no descriptor ever
// registered with the port, upstream's own comment says the wait blocks until one
// is added *and* an event occurs on it -- so the real runtime's thread never
// returns either, and a background thread does not hold the process open. Both
// runtimes therefore exit 0 with the waiter still blocked, which is exactly the
// shape of `SocketAsyncEngine`'s engine thread in a process that never opens a
// socket.
//
// The join is what stops this passing vacuously. A handler that answered an error
// instead of parking would let the waiter run on to set `Returned` and finish, and
// the exit code reports both of those separately.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    const int PAL_SUCCESS = 0;

    static IntPtr Port;

    // Written by the waiter after the wait returns; read by the entry thread once
    // the join has expired. Volatile because the two threads are the only
    // synchronisation there is -- the join deliberately does not succeed, so it
    // establishes no ordering.
    static volatile int Returned;

    static unsafe void Waiter()
    {
        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(Port, buffer, &count);
        Returned = 1;
    }

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != PAL_SUCCESS) return 1;
        Port = port;

        Thread waiter = new Thread(Waiter);
        waiter.IsBackground = true;
        waiter.Start();

        // Expected false: the waiter is still blocked when the deadline expires.
        // Under PawPrint the deadline is virtual-clock time, so it costs no wall
        // clock and cannot race -- the waiter is parked, nothing else is runnable,
        // and the driver jumps the clock straight to it. On the real runtime 200 ms
        // is far more than a wait that wrongly failed to block would need to
        // return, and a waiter that has not yet *reached* `epoll_wait` answers
        // false here too, so a slow host cannot make this pass wrongly.
        bool finished = waiter.Join(200);

        return (finished ? 2 : 0) + (Returned != 0 ? 4 : 0);
    }
}
