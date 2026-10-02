// Whether asking System.Native for random bytes takes a descriptor in a real
// .NET 10 process, as minipal's Linux path (`open("/dev/urandom")`, kept open in
// a static) would: open(/dev/null) is called, closed, and called again around
// the first call to each of System.Native's two random entry points, so a
// descriptor taken between two opens shows as the second landing higher. With
// the argument "nonsecure-first" the non-secure entry point (behind
// Random.Shared) is called first; otherwise the secure one (behind
// Guid.NewGuid). Both are in CoreLib, so no assembly load takes a descriptor
// in between.
//
// Run with `dotnet run minipal-random-descriptors.cs [nonsecure-first]`:
// natively on Darwin, and on Linux in the mcr.microsoft.com/dotnet/sdk:10.0
// image under Apple's `container`.
//
// Measured 2026-10-02, on Darwin 27.0.0 arm64 and Linux 6.18.5 arm64 (Ubuntu
// 24.04 image, .NET 10.0.11), results in minipal-random-descriptors.txt:
//   * Linux: whichever entry point is called first takes one descriptor (the
//     next open lands one higher) and the second takes none, so they share it;
//     `strace -e openat` shows it is /dev/urandom, O_RDONLY|O_CLOEXEC, and that
//     CoreCLR opened a /dev/urandom of its own before loading
//     libSystem.Native.so. The shipped libSystem.Native.so and libcoreclr.so
//     name /dev/urandom and lrand48 but neither arc4random_buf nor getrandom.
//   * Darwin: neither entry point takes a descriptor.
// Read in WoofWare.PawPrint/Native/MinipalRandom.fs.
using System;
using System.IO;
using System.Runtime.CompilerServices;

static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static long NextFd()
    {
        using var h = File.OpenHandle("/dev/null");
        return (long)h.DangerousGetHandle();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Secure() => Guid.NewGuid();

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void NonSecure() => Random.Shared.Next();

    static void Main(string[] args)
    {
        bool nonSecureFirst = args.Length > 0 && args[0] == "nonsecure-first";
        long a = NextFd();
        long a2 = NextFd();
        if (nonSecureFirst) NonSecure(); else Secure();
        long b = NextFd();
        if (nonSecureFirst) Secure(); else NonSecure();
        long c = NextFd();
        Console.WriteLine($"{(nonSecureFirst ? "nonsecure" : "secure")} first: before={a},{a2} afterFirst={b} afterSecond={c}");
    }
}
