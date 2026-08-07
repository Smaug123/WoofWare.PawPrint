using System;
using System.IO;

// The simulated process's current working directory, as the guest sees it.
//
// Impure rather than pure because the value asserted is PawPrint's own: the
// real runtime reports wherever the test host happens to have been started,
// so no differential oracle can pin it. The cross-runtime half of the
// contract — the buffer-size trichotomy of SystemNative_GetCwd itself — lives
// in the pure sibling SystemNativeGetCwd.cs.
//
// Deliberately hardcodes no path. The directory under test is whatever
// KernelConfig.CurrentDirectory was set to, and this program echoes it to
// stdout so that the F# registration (which built it in the first place) can
// assert the exact bytes. That keeps a single source of truth for each case,
// and lets one guest source cover every configuration TestImpureCases
// registers it under: the root default, an ordinary path, a path whose UTF-8
// encoding overflows CoreLib's 256-byte stackalloc, and a multi-byte path
// whose *character* count would not.
public class TestCurrentDirectoryConfigured
{
    public static int Main(string[] argv)
    {
        string cwd = Environment.CurrentDirectory;

        if (string.IsNullOrEmpty(cwd)) return 1;
        if (!Path.IsPathRooted(cwd)) return 2;

        // Everything below is asserted *relative to* the directory just read,
        // so it holds for every configuration this file is registered under.
        // The root is the one path that already ends in a separator.
        string prefix = cwd == "/" ? "/" : cwd + "/";

        // The plain case: a relative path is resolved against the cwd.
        if (Path.GetFullPath("a/b") != prefix + "a/b") return 3;

        // Relative segments are collapsed *after* the join, which is the whole
        // reason GetFullPath needs the cwd rather than just prefixing it.
        if (Path.GetFullPath("./a/../b") != prefix + "b") return 4;

        // "." resolves to the cwd itself, with no trailing separator left
        // behind (and no separator *removed* when the cwd is the root).
        if (Path.GetFullPath(".") != cwd) return 5;

        // An already-rooted path must ignore the cwd entirely.
        if (Path.GetFullPath("/x/y") != "/x/y") return 6;

        // Reading it twice must give the same answer: PawPrint models no
        // chdir(2), so the cwd is fixed for the lifetime of the process.
        if (Environment.CurrentDirectory != cwd) return 7;

        // Echoed for the F# side to compare against the configured value. Only
        // reached once every relative-resolution property above has held, so a
        // failure here is unambiguously about the *value*.
        Console.Out.Write(cwd);

        return 0;
    }
}
