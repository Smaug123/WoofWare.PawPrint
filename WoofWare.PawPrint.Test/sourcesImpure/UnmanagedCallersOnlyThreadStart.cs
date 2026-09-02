using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;

// A `[UnmanagedCallersOnly]` method as a *thread's* entry point.
//
// `ThreadStart` is `void()`, which is a valid signature for such a method — nothing here is
// hand-authored metadata — so this is an arrival a plain C# guest can write. The new thread does
// not enter the delegate's target directly: its bottom frame is `Thread.StartCallback`, whose
// `StartHelper.RunWorker` invokes the delegate. Under PawPrint that invoke is an ordinary
// `callvirt` through `callMethodWithCommitment`, so the refusal is applied on the worker, at the
// delegate call, which is also where CoreCLR's target prologue raises it.
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
