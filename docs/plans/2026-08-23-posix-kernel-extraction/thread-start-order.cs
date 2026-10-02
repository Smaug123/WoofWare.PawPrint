// Whether a .NET thread's OS thread id follows the order its Thread was
// *started* in, rather than the order its Thread object was constructed in.
// CoreCLR's ThreadNative_Initialize (the Thread constructor) only calls
// SetupUnstartedThread, "there is no physical thread to match, yet"; the
// pthread_create is ThreadNative_Start's CreateNewThread (comsynchronizable.cpp,
// threads.cpp). Each round constructs A then B, starts and joins B, then
// starts and joins A, and records both ids. It also checks the leader's id
// against the pid, and on Linux that libc's gettid agrees with System.Native's
// id.
//
// Build as a net10.0 console app with AllowUnsafeBlocks; run as `dotnet probe.dll`.
//
// Measured 2026-10-01, 1000 rounds each: .NET 10.0.7 on Darwin 27.0.0 arm64, and
// .NET 10.0.11 on Linux 6.18.5 aarch64 and x86-64 under Rosetta
// (mcr.microsoft.com/dotnet/sdk:10.0):
//
//   Darwin  pid=25727 leader=16698297; B=16698309 A=16698310, ...
//   Linux   pid=1 leader=1 gettid=1;   B=9 A=10, B=11 A=12, ...   (both CPUs)
//   all     startOrder=1000 constructOrder=0 gettidDisagree=0
//   gap(A-B) min=1 max=1 on Linux; min=1 max=1, then max=2 on a second run, on
//   Darwin, whose counter other processes take ids from too
//
// So the thread started first takes the lower id, every round, on both kernels.
// that libc gettid agrees with System.Native's id.
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetUInt64OSThreadId")]
    static extern ulong GetUInt64OSThreadId();

    [DllImport("libc", EntryPoint = "gettid")]
    static extern int GetTid();

    static void Main(string[] args)
    {
        int rounds = 1000;
        bool linux = OperatingSystem.IsLinux();
        Console.WriteLine($"os={RuntimeInformation.OSDescription} arch={RuntimeInformation.ProcessArchitecture} runtime={RuntimeInformation.FrameworkDescription}");
        Console.WriteLine($"pid={Environment.ProcessId} leader={GetUInt64OSThreadId()}" + (linux ? $" gettid={GetTid()}" : ""));
        int startOrder = 0, constructOrder = 0, gettidDisagree = 0;
        long minGap = long.MaxValue, maxGap = long.MinValue;
        for (int i = 0; i < rounds; i++)
        {
            ulong a = 0, b = 0; int ga = 0, gb = 0;
            var ta = new Thread(() => { a = GetUInt64OSThreadId(); if (linux) ga = GetTid(); });
            var tb = new Thread(() => { b = GetUInt64OSThreadId(); if (linux) gb = GetTid(); });
            tb.Start(); tb.Join();
            ta.Start(); ta.Join();
            if (b < a) startOrder++; else constructOrder++;
            long gap = (long)a - (long)b;
            minGap = Math.Min(minGap, gap); maxGap = Math.Max(maxGap, gap);
            if (linux && ((ulong)ga != a || (ulong)gb != b)) gettidDisagree++;
            if (i < 3) Console.WriteLine($"round {i}: startedFirst(B)={b} startedSecond(A)={a}");
        }
        Console.WriteLine($"rounds={rounds} startOrder={startOrder} constructOrder={constructOrder} gap(A-B) min={minGap} max={maxGap} gettidDisagree={gettidDisagree}");
    }
}
