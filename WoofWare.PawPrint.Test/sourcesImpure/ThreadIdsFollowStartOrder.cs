using System;
using System.Runtime.InteropServices;
using System.Threading;

// A thread's OS thread id is minted when the thread is started, not when its
// Thread object is constructed: CoreCLR's constructor creates no OS thread
// (SetupUnstartedThread), and Start is where it calls pthread_create. So a
// thread constructed and never started takes no id, and two threads started in
// the reverse of their construction order take ids in start order.
//
// Reads the raw Linux gettid(2) through libc, and writes the main thread's id
// and then each worker's, in start order, to stdout as little-endian int32s, so
// that the F# registration can assert the exact values: on a quiet Linux, ids
// count up from the pid. Real .NET agrees on the order
// (docs/plans/2026-08-23-posix-kernel-extraction/thread-start-order.cs), but its
// own runtime threads take ids too, so there is no oracle for the values.
class Program
{
    [DllImport("libc", EntryPoint = "gettid")]
    static extern int GetTid();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Main(string[] args)
    {
        int leader = GetTid();

        // A thread-group leader's tid is its pid.
        if (leader != Environment.ProcessId) return 1;

        Thread neverStarted = new Thread(() => { });

        int constructedFirstId = 0;
        int constructedSecondId = 0;
        Thread constructedFirst = new Thread(() => { constructedFirstId = GetTid(); });
        Thread constructedSecond = new Thread(() => { constructedSecondId = GetTid(); });

        constructedSecond.Start();
        constructedSecond.Join();
        constructedFirst.Start();
        constructedFirst.Join();

        if (constructedFirstId == 0 || constructedSecondId == 0) return 2;

        int[] ids = new int[] { leader, constructedSecondId, constructedFirstId };
        byte[] observed = new byte[4 * ids.Length];

        for (int i = 0; i < ids.Length; i++)
        {
            observed[4 * i] = (byte)(ids[i] & 0xFF);
            observed[4 * i + 1] = (byte)((ids[i] >> 8) & 0xFF);
            observed[4 * i + 2] = (byte)((ids[i] >> 16) & 0xFF);
            observed[4 * i + 3] = (byte)((ids[i] >> 24) & 0xFF);
        }

        fixed (byte* p = observed)
        {
            if (Write((IntPtr)1, p, observed.Length) != observed.Length) return 3;
        }

        GC.KeepAlive(neverStarted);
        return 0;
    }
}
