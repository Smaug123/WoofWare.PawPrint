// Reads, in a real .NET 10 process on Linux, every signal's disposition with
// the raw rt_sigaction(2) syscall, which unlike glibc's sigaction does not
// refuse glibc's reserved 32 and 33, and names the library each installed
// handler lives in (dladdr). Also prints /proc/self/status's signal masks.
//
// Build as a console app (net10.0, AllowUnsafeBlocks) and run with no
// arguments, e.g. in the mcr.microsoft.com/dotnet/sdk:10.0 image with
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1. The syscall number is arm64's 134
// or x86-64's 13.
//
// Measured 2026-09-26 on .NET 10.0.11 / Linux 6.18.5 aarch64 (glibc 2.41):
// 33's handler is in libc.so.6 (SA_SIGINFO | SA_RESTART | SA_RESTORER);
// 4 5 6 7 8 11 15 34 in libcoreclr.so; 13 is SIG_IGN; 2 3 18 in
// libSystem.Native.so, because writing to the console initialised the shim's
// terminal and signal handling before the reading. 32 is SIG_DFL. Every
// thread's mask is empty. Transcribed in
// WoofWare.PawPrint/Native/StartupSignalDispositions.fs.
using System;
using System.IO;
using System.Runtime.InteropServices;

unsafe class Program
{
    [DllImport("libc", SetLastError = true)]
    static extern long syscall(long number, long a, long b, long c, long d);

    [StructLayout(LayoutKind.Sequential)]
    struct DlInfo { public IntPtr fname; public IntPtr fbase; public IntPtr sname; public IntPtr saddr; }

    [DllImport("libc")]
    static extern int dladdr(IntPtr addr, DlInfo* info);

    static void Report(int sig)
    {
        // struct kernel_sigaction on arm64/x86-64: handler, flags, restorer, mask(8 bytes)
        byte* buf = stackalloc byte[64];
        for (int i = 0; i < 64; i++) buf[i] = 0;
        long nr = RuntimeInformation.ProcessArchitecture == Architecture.Arm64 ? 134 : 13;
        long r = syscall(nr, sig, 0, (long)buf, 8);
        long h = *(long*)buf;
        ulong flags = *(ulong*)(buf + 8);
        string where = "";
        if (h > 1)
        {
            DlInfo info;
            if (dladdr((IntPtr)h, &info) != 0)
                where = $" in {Marshal.PtrToStringAnsi(info.fname)} ({(info.sname == IntPtr.Zero ? "?" : Marshal.PtrToStringAnsi(info.sname))})";
        }
        Console.WriteLine($"rt_sigaction({sig}) = {r}: handler=0x{h:x} flags=0x{flags:x}{where}");
    }

    static int Main(string[] args)
    {
        foreach (var l in File.ReadAllLines("/proc/self/status"))
            if (l.StartsWith("Sig") || l.StartsWith("Shd")) Console.WriteLine(l);
        foreach (var d in Directory.GetDirectories("/proc/self/task"))
            foreach (var l in File.ReadAllLines(Path.Combine(d, "status")))
                if (l.StartsWith("SigBlk") || l.StartsWith("Name")) Console.WriteLine(Path.GetFileName(d) + " " + l);
        for (int s = 1; s <= 64; s++) Report(s);
        return 0;
    }
}
