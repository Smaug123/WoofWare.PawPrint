// Measures, in a real .NET 10 process, which signals the runtime has already
// caught or ignored when Main starts, and which self-sent signals the process
// survives with nothing registered through PosixSignalRegistration.
//
// Build as a console app (net10.0, AllowUnsafeBlocks) and run it with no
// arguments to print every signal whose disposition is not SIG_DFL, at Main
// and after PosixSignalRegistration has initialised the runtime's signal
// shim; or with a signal number, to send that signal to itself and exit 42
// if it survives. Linux: run the same build in a Debian trixie container with
// a .NET 10 runtime from dotnet-install.sh, with
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1.
//
// Measured 2026-09-24 on .NET 10.0.7 / Darwin 25.6.0 arm64 and .NET 10.0.12 /
// Linux 6.18.5 aarch64 (glibc 2.41). The survivors are transcribed in
// WoofWare.PawPrint/Native/StartupSignalDispositions.fs, and
// WoofWare.PawPrint.Test/TestStartupSignalDispositions.fs repeats the
// survival half against the host's runtime.
//
// At Main, Darwin: 2 3 4 6 8 10 11 15 30 caught, 13 ignored (9 and 17 cannot
// be read). Linux: 2 3 4 5 6 7 8 11 15 34 caught, 13 ignored (32 and 33
// cannot be read: glibc refuses them). After the shim initialises, SIGCONT is
// caught too (19 on Darwin, 18 on Linux).
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

unsafe class Program
{
    [DllImport("libc", SetLastError = true)]
    static extern int sigaction(int sig, void* act, void* oact);

    [DllImport("libc", SetLastError = true)]
    static extern int kill(int pid, int sig);

    static string Read(int sig)
    {
        byte* buf = stackalloc byte[1024];
        for (int i = 0; i < 1024; i++) buf[i] = 0;
        if (sigaction(sig, null, buf) != 0) return "ERR" + Marshal.GetLastPInvokeError();
        long h = *(long*)buf;
        return h == 0 ? "DFL" : h == 1 ? "IGN" : "HANDLER";
    }

    static List<string> Snapshot(int max)
    {
        var l = new List<string>();
        for (int s = 1; s <= max; s++)
        {
            var d = Read(s);
            if (d != "DFL") l.Add($"{s}:{d}");
        }
        return l;
    }

    static int Main(string[] args)
    {
        int max = OperatingSystem.IsLinux() ? 64 : 31;
        var atStart = Snapshot(max);
        if (args.Length == 0)
        {
            Console.WriteLine("at Main:         " + string.Join(" ", atStart));
            using (PosixSignalRegistration.Create(PosixSignal.SIGTERM, _ => { })) { }
            Console.WriteLine("after shim init: " + string.Join(" ", Snapshot(max)));
            return 0;
        }
        int sig = int.Parse(args[0]);
        bool register = args.Length > 1 && args[1] == "register";
        PosixSignalRegistration reg = null;
        if (register) reg = PosixSignalRegistration.Create((PosixSignal)sig, ctx => { Console.WriteLine("handler ran for " + ctx.Signal); });
        int r = kill(Environment.ProcessId, sig);
        System.Threading.Thread.Sleep(500);
        Console.WriteLine($"kill({sig}) returned {r}, process survived");
        GC.KeepAlive(reg);
        return 42;
    }
}
