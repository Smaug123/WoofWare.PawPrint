using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;

// A `[UnmanagedCallersOnly]` method as a *thread's* entry point.
//
// `ThreadStart` is `void()`, which is a valid signature for such a method — nothing here is
// hand-authored metadata — so this is an arrival a plain C# guest can write, and it is not one that
// goes through any call instruction. The runtime starts the thread by entering the delegate's
// target directly, which in PawPrint means a worker frame built in `Thread.StartInternal` rather
// than a call passing through `callMethodWithCommitment`.
//
// Measured: real .NET aborts with the same fatal error as every other managed entry, and `WORKER
// RAN` never prints.
//
// The bind and the marker both go to stderr so the ordering is legible in the failure report: a run
// that never prints `bound Worker` died before reaching the interesting part.

public static class Program
{
    [UnmanagedCallersOnly]
    public static void Worker ()
    {
        Console.Error.WriteLine ("WORKER RAN");
        Console.Error.Flush ();
    }

    public static int Main ()
    {
        ThreadStart start =
            (ThreadStart) typeof (Program).GetMethod ("Worker").CreateDelegate (typeof (ThreadStart));

        Console.Error.WriteLine ("bound " + start.Method.Name);
        Console.Error.Flush ();

        Thread thread = new Thread (start);
        thread.Start ();
        thread.Join ();

        return 0;
    }
}
