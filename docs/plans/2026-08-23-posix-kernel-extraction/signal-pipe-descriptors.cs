// Which descriptors System.Native's signal handling takes in a real .NET 10
// process, and what they are: every descriptor open at Main, then registering
// the first PosixSignalRegistration (which initialises the shim's signal
// handling), then every descriptor again, with each new one's type (fstat),
// FD_CLOEXEC, access mode and O_NONBLOCK. An open(/dev/null) before and after
// shows where the next descriptor lands. Run with the argument "console" to
// write to Console.Out first. Prints only at the end, since writing to a
// terminal initialises signal handling itself.
//
// Build as a console app (net10.0, AllowUnsafeBlocks, InvariantGlobalization)
// and run with no arguments: natively on Darwin, and on Linux in the
// mcr.microsoft.com/dotnet/sdk:10.0 image under Apple's `container`.
//
// Measured 2026-09-27, on Darwin 27.0.0 arm64 (.NET 10.0.7) and Linux 6.18.5
// arm64 (Ubuntu 24.04 image, .NET 10.0.11), with stdout a pipe:
//   * the registration took exactly two new descriptors, the two lowest free
//     (26 and 27 on both, 29 and 30 in the "console" run), a pipe whose read
//     end is the lower: O_RDONLY then O_WRONLY, both FD_CLOEXEC, neither
//     O_NONBLOCK;
//   * the next open() landed two higher than the one before the registration
//     (26, then 28);
//   * writing "" to a redirected Console.Out did not initialise signal
//     handling: the registration still made the pipe afterwards.
// Transcribed in WoofWare.PawPrint/Native/NativeSystemNative.fs
// (`initializeSignalHandling`), and checked differentially by
// WoofWare.PawPrint.Test/sourcesPure/PosixSignalPipeTakesTwoDescriptors.cs.
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;

unsafe class Program
{
    [DllImport("libc", SetLastError = true)] static extern int fcntl(int fd, int cmd);
    [DllImport("libc", SetLastError = true)] static extern int open(byte* path, int flags);
    [DllImport("libc", SetLastError = true)] static extern int close(int fd);
    [DllImport("libc", SetLastError = true)] static extern int kill(int pid, int sig);
    [DllImport("libc", SetLastError = true)] static extern int fstat(int fd, byte* buf);

    const int F_GETFD = 1, F_GETFL = 3;

    static readonly bool Linux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux);

    static List<int> OpenFds()
    {
        var r = new List<int>();
        for (int fd = 0; fd < 1024; fd++) if (fcntl(fd, F_GETFD) >= 0) r.Add(fd);
        return r;
    }

    static int OpenDevNull()
    {
        byte* p = stackalloc byte[16];
        var s = Encoding.ASCII.GetBytes("/dev/null");
        for (int i = 0; i < s.Length; i++) p[i] = s[i];
        p[s.Length] = 0;
        return open(p, 0);
    }

    static string Describe(int fd)
    {
        int fdFlags = fcntl(fd, F_GETFD);
        int flFlags = fcntl(fd, F_GETFL);
        int nonBlock = Linux ? 0x800 : 0x4;
        byte* st = stackalloc byte[256];
        fstat(fd, st);
        // st_mode: Linux x86-64 offset 24 (u32), Linux aarch64 offset 16 (u32); Darwin offset 4 (u16)
        uint mode = Linux
            ? (RuntimeInformation.ProcessArchitecture == Architecture.Arm64 ? *(uint*)(st + 16) : *(uint*)(st + 24))
            : *(ushort*)(st + 4);
        string kind = (mode & 0xF000) switch { 0x1000 => "fifo", 0x2000 => "chr", 0x8000 => "reg", 0xC000 => "sock", 0x4000 => "dir", _ => $"mode 0x{mode:x}" };
        string link = "";
        if (Linux) { try { link = " -> " + System.IO.File.ResolveLinkTarget($"/proc/self/fd/{fd}", false)?.LinkTarget; } catch { } }
        return $"fd {fd}: {kind}, FD_CLOEXEC={(fdFlags & 1)}, accmode={flFlags & 3}, O_NONBLOCK={((flFlags & nonBlock) != 0 ? 1 : 0)}{link}";
    }

    static int Main(string[] args)
    {
        var log = new List<string>();
        bool consoleFirst = args.Length > 0 && args[0] == "console";
        if (consoleFirst) { Console.Out.Write(""); Console.Out.Flush(); log.Add("wrote \"\" to Console.Out first"); }

        var before = OpenFds();
        log.Add("open at start: " + string.Join(",", before));
        foreach (var fd in before) if (fd > 2) log.Add("  " + Describe(fd));
        int a = OpenDevNull();
        log.Add($"open(/dev/null) before registration: {a}");
        close(a);

        using var reg = PosixSignalRegistration.Create(PosixSignal.SIGWINCH, c => { });
        var after = OpenFds();
        log.Add("open after registration: " + string.Join(",", after));
        foreach (var fd in after) if (!before.Contains(fd)) log.Add("  new " + Describe(fd));
        int b = OpenDevNull();
        log.Add($"open(/dev/null) after registration: {b}");
        close(b);

        Console.WriteLine($"{RuntimeInformation.OSDescription} {RuntimeInformation.ProcessArchitecture} .NET {Environment.Version}");
        foreach (var l in log) Console.WriteLine(l);
        return 0;
    }
}
