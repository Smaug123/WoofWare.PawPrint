using System;
using System.Runtime.InteropServices;
using System.Threading;

// A thread other than the main thread raises SIGTERM with libc's raise(3),
// with nothing registered for it: the signal is the raising thread's own,
// and its default kills the whole process.
//
// SIGTERM is 15 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Main(string[] args)
    {
        int result = 99;
        var worker = new Thread(() => result = Raise(15));
        worker.Start();
        worker.Join();

        // Unreachable if the default runs.
        Thread.Sleep(TimeSpan.FromSeconds(30));
        return result == 0 ? 3 : 1;
    }
}
