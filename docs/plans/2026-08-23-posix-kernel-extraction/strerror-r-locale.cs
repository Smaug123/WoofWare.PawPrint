// Does a .NET process's strerror text follow LANG / LC_ALL? glibc translates
// strerror's text through gettext, but only in a locale a setlocale() call
// selected; a process that never calls it stays in "C".
//
// Built as a net10.0 console app and run as `dotnet strerror-r-locale.dll`:
//   Linux: in mcr.microsoft.com/dotnet/runtime:10.0 (Ubuntu noble, glibc 2.39,
//     .NET 10.0.12, aarch64) with `language-pack-de` installed, under
//     LANG=de_DE.UTF-8 and then LC_ALL=de_DE.UTF-8.
//   Darwin 27.0 arm64, .NET 10.0.7, under LANG=fr_FR.UTF-8 LC_ALL=fr_FR.UTF-8.
//
// Positive control, in the same Linux container under LANG=de_DE.UTF-8: a C
// program printing strerror(2) before and after setlocale(LC_ALL, "") printed
// "No such file or directory", then "Datei oder Verzeichnis nicht gefunden",
// so the German catalogue was installed and reachable.
//
// Measured 2026-10-02: every row was the untranslated text on both, under
// every setting ("Success", "Operation not permitted", "No such file or
// directory", "Permission denied", "Is a directory", "Unknown error 4096",
// "Unknown error -1" on Linux; "Undefined error: 0", ..., "Unknown error:
// 4096", "Unknown error: -1" on Darwin). So the runtime never selects the
// environment's locale, and SystemNative_StrErrorR's text depends on the
// errno and the C library alone.
using System;
using System.Runtime.InteropServices;

class Program
{
    static void Main()
    {
        Console.WriteLine($"LANG={Environment.GetEnvironmentVariable("LANG")} LC_ALL={Environment.GetEnvironmentVariable("LC_ALL")}");
        foreach (var n in new[] { 0, 1, 2, 13, 21, 4096, -1 })
            Console.WriteLine($"{n}\t{Marshal.GetPInvokeErrorMessage(n)}");
    }
}
