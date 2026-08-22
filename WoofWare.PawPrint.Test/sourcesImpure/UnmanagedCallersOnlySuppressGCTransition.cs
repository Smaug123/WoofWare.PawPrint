using System;
using System.Runtime.InteropServices;

// `delegate* unmanaged[SuppressGCTransition]<...>` — an *unmanaged* call site that is nonetheless a
// fatal way into a `[UnmanagedCallersOnly]` method.
//
// The suppression is what makes it fatal: the caller skips the GC transition and so is still in
// cooperative mode when the callee's reverse-P/Invoke prologue runs, which is exactly the state
// that prologue refuses. Measured — real .NET prints the same "Invalid Program: attempted to call a
// UnmanagedCallersOnly method from managed code." here as it does for a delegate or reflection.
//
// This is the pair that shows a gate keyed on the calling *convention* is not enough. The header is
// `Unmanaged` (0x09) for both this call site and the legal one in
// `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs`; the whole difference is a `modopt` on the
// return type, `09 01 08 08` against `09 01 20 49 08 08`. So the question the gate has to ask is
// "does the thread leave cooperative mode", not "is this convention unmanaged".
//
// The plain call happens first and must succeed, so a run that dies before printing `plain: 6` died
// for the wrong reason.

public static unsafe class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        delegate* unmanaged<int, int> plain = &Doubler;
        Console.Error.WriteLine ("plain: " + plain (3));
        Console.Error.Flush ();

        // C# will not convert between function-pointer types of differing convention directly, so
        // the address launders through `nint`. The target is the same method either way.
        nint raw = (nint) plain;

        delegate* unmanaged[SuppressGCTransition]<int, int> suppressed =
            (delegate* unmanaged[SuppressGCTransition]<int, int>) raw;

        try
        {
            return suppressed (21);
        }
        catch (Exception)
        {
            return 1;
        }
    }
}
