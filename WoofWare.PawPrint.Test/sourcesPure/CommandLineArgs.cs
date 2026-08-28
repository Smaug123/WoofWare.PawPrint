using System;

// `Environment.GetCommandLineArgs()` is not backed by a native on Unix: CoreLib's
// `GetCommandLineArgsNative()` is `return Array.Empty<string>()`, reached only by a library
// hosted from native code. The real answer comes from the static `Environment.s_commandLineArgs`,
// which the VM fills by calling the *managed*
// `Environment.InitializeCommandLineArgs(char* exePath, int argc, char** argv)` during startup
// (`CorHost2::ExecuteAssembly` -> `SetCommandLineArgs`). That single call builds both this array
// and the array `Main` receives — it returns the latter — which is why the two always agree.
//
// Everything asserted here is a fact about that relationship rather than about any particular
// path, so it holds for `dotnet Guest.dll` and for PawPrint alike. The *value* of element 0 is
// deliberately not asserted: it is whatever assembly path the host supplied, which differs
// between the two runtimes and is not a cross-runtime fact.
class CommandLineArgs
{
    static int Main(string[] args)
    {
        string[] a = Environment.GetCommandLineArgs();

        if (a == null)
        {
            return 1;
        }

        // Element 0 is the program itself; the rest are exactly `Main`'s arguments. This is the
        // assertion that fails against a runtime which never populated `s_commandLineArgs`: the
        // empty-array fallback makes the length 0 rather than `args.Length + 1`.
        if (a.Length != args.Length + 1)
        {
            return 2;
        }

        for (int i = 0; i < args.Length; i++)
        {
            if (a[i + 1] != args[i])
            {
                return 3;
            }
        }

        if (string.IsNullOrEmpty(a[0]))
        {
            return 4;
        }

        // `GetCommandLineArgs` hands out a clone, so no caller can reach the runtime's own copy.
        string[] b = Environment.GetCommandLineArgs();

        if (ReferenceEquals(a, b))
        {
            return 5;
        }

        if (b.Length != a.Length)
        {
            return 6;
        }

        // The clone is shallow, so the element references are shared rather than re-created.
        for (int i = 0; i < a.Length; i++)
        {
            if (!ReferenceEquals(a[i], b[i]))
            {
                return 7;
            }
        }

        // Storing through the returned array must not be visible to the next caller, which is
        // the property the cloning exists for.
        a[0] = "clobbered";

        string[] c = Environment.GetCommandLineArgs();

        if (ReferenceEquals(c[0], a[0]))
        {
            return 8;
        }

        return 0;
    }
}
