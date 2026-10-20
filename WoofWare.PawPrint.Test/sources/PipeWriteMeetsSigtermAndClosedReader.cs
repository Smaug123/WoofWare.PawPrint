using System;
using System.Runtime.InteropServices;
using System.Threading;

// The main thread writes more into a pipe than it holds, so its write sleeps.
// A second thread sends the process a SIGTERM, which a PosixSignalRegistration
// handler catches and cancels, closes the pipe's only read end, and exits. The
// write then ends with EPIPE, raising SIGPIPE, which the runtime ignores, or
// with the bytes it moved before the signal; and the handler runs.
//
// Whichever comes first, the writer is the only thread left that could make a
// call, and at the end of its write it owes the SIGTERM's delivery, so nothing
// may ask the kernel what SIGPIPE's disposition is by making a call.
unsafe class Program
{
    const int Length = 200000;

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Pipe")]
    static extern int Pipe(int* fds, int flags);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern int Write(IntPtr fd, byte* bytes, int length);

    [DllImport("libc", EntryPoint = "kill")]
    static extern int Kill(int pid, int signo);

    static int Main(string[] args)
    {
        using var handled = new ManualResetEventSlim(false);

        using var registration = PosixSignalRegistration.Create(
            PosixSignal.SIGTERM,
            context =>
            {
                context.Cancel = true;
                handled.Set();
            });

        int* fds = stackalloc int[2];
        if (Pipe(fds, 0) != 0) return 1;
        IntPtr reader = (IntPtr)fds[0];
        IntPtr writer = (IntPtr)fds[1];

        int killed = -1;
        int closed = -1;
        var other = new Thread(() =>
        {
            Thread.Sleep(50);
            // SIGTERM is 15 on both Linux and Darwin.
            killed = Kill(Environment.ProcessId, 15);
            closed = Close(reader);
        });
        other.Start();

        byte[] bytes = new byte[Length];
        int written;
        fixed (byte* p = bytes)
        {
            written = Write(writer, p, bytes.Length);
        }

        other.Join();
        if (killed != 0) return 2;
        if (closed != 0) return 3;
        if (written != -1 && (written <= 0 || written >= Length)) return 4;
        if (!handled.Wait(TimeSpan.FromSeconds(30))) return 5;
        if (Close(writer) != 0) return 6;
        return 0;
    }
}
