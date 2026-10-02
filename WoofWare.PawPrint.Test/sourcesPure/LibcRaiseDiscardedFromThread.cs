using System;
using System.Runtime.InteropServices;
using System.Threading;

// A thread other than the main thread raises, with libc's raise(3), signals
// the process discards: SIGWINCH, by default, and SIGPIPE, which the runtime
// ignores from startup. Each is the raising thread's own, and is discarded as
// it is raised.
//
// SIGWINCH is 28 and SIGPIPE 13 on both Linux and Darwin.
class Program
{
    [DllImport("libc", EntryPoint = "raise", SetLastError = true)]
    static extern int Raise(int sig);

    static int Main(string[] args)
    {
        int result = 99;
        var worker = new Thread(() =>
        {
            if (Raise(28) != 0) { result = 1; return; }
            if (Raise(13) != 0) { result = 2; return; }
            result = 0;
        });
        worker.Start();
        worker.Join();
        return result;
    }
}
