using System;
using System.Reflection;

// `Assembly.GetExecutingAssembly()` walks the stack for the assembly of whoever called it, via the
// `AssemblyNative_GetExecutingAssembly` QCall and a `StackCrawlMark` local declared in
// `Assembly.GetExecutingAssembly()` itself.
//
// A single-assembly guest cannot tell "the caller's assembly" apart from "the entry assembly" —
// `TestCrossAssemblyExecutingAssembly.fs` is where that distinction is pinned. What this file
// pins is object identity: the runtime hands out one `Assembly` instance per assembly, so all
// three routes to it must produce the very same object rather than three equal ones.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly executing = Assembly.GetExecutingAssembly ();

        if (executing == null)
            return 1;

        if (!ReferenceEquals (executing, typeof (Program).Assembly))
            return 2;

        if (!ReferenceEquals (executing, Assembly.GetEntryAssembly ()))
            return 3;

        // Calling it twice must not mint a second object.
        if (!ReferenceEquals (executing, Assembly.GetExecutingAssembly ()))
            return 4;

        return 0;
    }
}
