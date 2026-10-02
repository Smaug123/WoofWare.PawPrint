using System;
using System.Runtime.InteropServices;
using System.Threading;

// A signal interrupts the main thread's blocking read of an empty pipe.
//
// The kernel delivers a signal sent to the process to its main thread, which
// here is asleep in SystemNative_Read. System.Native installs its handler with
// SA_RESTART, under which both kernels restart a pipe read rather than failing
// it with EINTR, so the call goes on sleeping and returns the byte that ends
// the wait, and errno keeps the 0 the P/Invoke stub cleared it to: a restart
// leaves no trace. (Without SA_RESTART the read would fail with EINTR, which
// `Common_Read` would call again, leaving EINTR in errno.)
//
// The byte comes from the signal handler itself: a thread sends SIGTERM every
// 100 ms, and the third handler run writes to the pipe. So by the time there
// is a byte, the main thread has been signalled while reading, however late it
// started reading.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe", SetLastError = true)]
    static extern unsafe int Pipe(int* pipeFds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int SIGTERM = 15;

    static IntPtr WriteEnd;
    static int Handled;

    // Run by the runtime's signal-handling thread, for each SIGTERM.
    static unsafe void OnSigTerm(PosixSignalContext context)
    {
        context.Cancel = true;
        if (Interlocked.Increment(ref Handled) == 3)
        {
            byte x = (byte)'x';
            Write(WriteEnd, &x, 1);
        }
    }

    static void SendUntilHandled()
    {
        int pid = Environment.ProcessId;
        for (int i = 0; i < 50 && Volatile.Read(ref Handled) < 3; i++)
        {
            Thread.Sleep(100);
            if (Volatile.Read(ref Handled) >= 3) break;
            Kill(pid, SIGTERM);
        }
    }

    static unsafe int Main()
    {
        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr readEnd = (IntPtr)fds[0];
        WriteEnd = (IntPtr)fds[1];

        // Kept for the life of the process, so that a signal the sender sends
        // after the read has ended is handled too, rather than taking SIGTERM's
        // default.
        var registration = PosixSignalRegistration.Create(PosixSignal.SIGTERM, OnSigTerm);

        Thread sender = new Thread(SendUntilHandled);
        sender.Start();

        byte* buffer = stackalloc byte[8];
        int rv = Read(readEnd, buffer, 8);
        int lastError = Marshal.GetLastPInvokeError();

        sender.Join();
        GC.KeepAlive(registration);

        if (rv != 1) return 2;
        if (buffer[0] != (byte)'x') return 3;
        if (lastError != 0) return 4;
        return 0;
    }
}
