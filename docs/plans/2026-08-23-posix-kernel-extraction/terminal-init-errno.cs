// What errno System.Native's terminal initialisation leaves behind, on its first
// call and on a later one, with standard input a pipe (or /dev/null).
//
// Build as a net10.0 console app; run as `: | dotnet probe.dll flag out.txt`,
// `: | dotnet probe.dll noflag out.txt`, and `dotnet probe.dll flag out.txt </dev/null`.
// Nothing may initialise the console before the first call, so the result goes
// to a file.
//
// Measured 2026-10-01, .NET 10.0.7 on Darwin 27.0.0 arm64 and .NET 10.0.12 on
// Linux 6.18.5 aarch64 (mcr.microsoft.com/dotnet/runtime:10.0):
//
//   stdin       Darwin                         Linux
//   pipe        ret=1 err=25 ret2=1 err2=4242   ret=1 err=25 ret2=1 err2=4242
//   /dev/null   ret=1 err=19                    ret=1 err=25
//
// identically through a SetLastError import and without one: the first call
// leaves tcgetattr(0)'s ENOTTY (Darwin's ENODEV for /dev/null) in errno, and a
// second leaves errno alone.
using System;
using System.IO;
using System.Runtime.InteropServices;

static class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_InitializeTerminalAndSignalHandling", SetLastError = true)]
    static extern int Init();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_InitializeTerminalAndSignalHandling")]
    static extern int InitNoFlag();

    static int Main(string[] args)
    {
        // Measure before anything initialises the console.
        Marshal.SetLastSystemError(4242);
        int r;
        int errnoAfter;
        if (args.Length > 0 && args[0] == "noflag")
        {
            r = InitNoFlag();
            errnoAfter = Marshal.GetLastSystemError();
        }
        else
        {
            r = Init();
            errnoAfter = Marshal.GetLastPInvokeError();
        }
        // A second call: already initialised.
        Marshal.SetLastSystemError(4242);
        int r2 = InitNoFlag();
        int errnoAfter2 = Marshal.GetLastSystemError();
        File.WriteAllText(args.Length > 1 ? args[1] : "out.txt", $"ret={r} err={errnoAfter} ret2={r2} err2={errnoAfter2}\n");
        return 0;
    }
}
