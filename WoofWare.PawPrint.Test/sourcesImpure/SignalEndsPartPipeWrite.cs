using System;
using System.Runtime.InteropServices;
using System.Threading;

// A signal interrupts the main thread's blocking write of more than a pipe
// holds, after part of it has gone in.
//
// The main thread writes 200000 bytes into an empty pipe that nobody reads:
// the kernel puts in the 65536 bytes the pipe holds and puts the thread to
// sleep for the rest. A signal delivered to it then ends the call with the
// count it had put in, under SA_RESTART (which System.Native installs its
// handler with) as without it: measured on Linux and Darwin alike. A count is
// a success, so `Common_Write` returns it rather than calling again, and errno
// keeps the 0 the P/Invoke stub cleared it to.
//
// A thread sends SIGTERM every 100 ms until the write has returned, so the
// main thread is signalled while it sleeps, however late it started writing.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int SIGTERM = 15;
    const int Large = 200000;

    static int Returned;

    static void SendUntilReturned()
    {
        int pid = Environment.ProcessId;
        for (int i = 0; i < 50 && Volatile.Read(ref Returned) == 0; i++)
        {
            Thread.Sleep(100);
            if (Volatile.Read(ref Returned) != 0) break;
            Kill(pid, SIGTERM);
        }
    }

    static unsafe int Main()
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr writeEnd = (IntPtr)fds[1];

        // Kept for the life of the process, so that a signal the sender sends
        // after the write has ended is handled rather than taking SIGTERM's
        // default.
        var registration = PosixSignalRegistration.Create(PosixSignal.SIGTERM, context => context.Cancel = true);

        Thread sender = new Thread(SendUntilReturned);
        sender.Start();

        byte[] source = new byte[Large];
        int rv;
        fixed (byte* start = source)
        {
            rv = Write(writeEnd, start, Large);
        }
        int lastError = Marshal.GetLastPInvokeError();
        Volatile.Write(ref Returned, 1);

        sender.Join();
        GC.KeepAlive(registration);
        GC.KeepAlive(fds[0]);

        if (rv != 65536) return 2;
        if (lastError != 0) return 3;
        return 0;
    }
}
