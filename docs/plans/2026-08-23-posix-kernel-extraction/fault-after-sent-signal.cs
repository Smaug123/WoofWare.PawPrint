// Measures whether a hardware fault in managed code still becomes a managed
// exception after the process has sent itself one of the fault signals, whose
// runtime handler then restores the default (see
// WoofWare.PawPrint/Native/StartupSignalDispositions.fs).
//
// Build as a console app (net10.0) and run it with a signal number (0 sends
// nothing) and an action: "nre" reads a field through null in a method the JIT
// does not inline, and "div" divides by a zero the JIT cannot see. It prints
// what it caught and exits 42, or dies of the fault. Linux: run the same build
// in the mcr.microsoft.com/dotnet/runtime:10.0 image, natively and with
// `container run --arch amd64`.
//
// Measured 2026-10-01 on .NET 10.0.7 / Darwin 27.0.0 arm64 and .NET 10.0.12 /
// Linux 6.18.5 aarch64 and Rosetta x86-64. Darwin caught both after each of
// its fault signals. Linux died of SIGSEGV (139) at "nre" after SIGSEGV, on both CPUs;
// x86-64 died of SIGFPE (136) at "div" after SIGFPE, where arm64's JIT checks
// the divisor itself and still caught DivideByZeroException. After SIGILL,
// SIGABRT or Linux's SIGBUS, every run caught. The rows are transcribed in
// WoofWare.PawPrint/Native/NativeLibc.fs, which refuses the sends that would
// change what a later fault does.
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

class Holder { public int Field; }

class Program
{
    [DllImport("libc", SetLastError = true)]
    static extern int kill(int pid, int sig);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int ReadField(Holder h) => h.Field;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Divide(int a, int b) => a / b;

    static int Main(string[] args)
    {
        int sig = int.Parse(args[0]);
        if (sig != 0 && kill(Environment.ProcessId, sig) != 0) return 1;
        Console.WriteLine($"sent {sig}, survived"); Console.Out.Flush();
        try
        {
            switch (args[1])
            {
                case "nre": ReadField(null); break;
                case "div": Divide(1, int.Parse("0")); break;
            }
        }
        catch (Exception e)
        {
            Console.WriteLine("caught " + e.GetType().Name); Console.Out.Flush();
            return 42;
        }
        return 43;
    }
}
