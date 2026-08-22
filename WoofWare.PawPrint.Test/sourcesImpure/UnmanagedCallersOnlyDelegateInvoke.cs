using System;
using System.Reflection;
using System.Runtime.InteropServices;

// Invoking a delegate bound to a `[UnmanagedCallersOnly]` method. Real .NET binds it — `BindToMethod`
// carries no such check — and then dies uncatchably on the call, because the target's prologue is a
// reverse-P/Invoke transition that asserts preemptive GC mode.
//
// There is no exit code to agree on: the process is expected never to return. `Main` returns an int
// only so that a PawPrint that wrongly continued reports *how* it continued rather than falling off
// the end. See `TestUnmanagedCallersOnlyEntry.fs`, and the control in
// `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` for the routes that must keep working.

public static class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        Func<int, int> bound =
            (Func<int, int>) typeof (Program).GetMethod ("Doubler").CreateDelegate (typeof (Func<int, int>));

        // Uncatchable, so the `try` is not protection — it is here to say so: a runtime that let
        // the guest catch this would be diverging just as much as one that returned 42.
        try
        {
            return bound (21);
        }
        catch (Exception)
        {
            return 1;
        }
    }
}
