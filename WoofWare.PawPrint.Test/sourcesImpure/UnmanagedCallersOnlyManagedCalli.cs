using System;
using System.Runtime.InteropServices;

// The address of a `[UnmanagedCallersOnly]` method, laundered through `nint` and called back
// through a *managed* function pointer. The target is unchanged — it is the call site that differs
// from the legal route in `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs`, whose signature
// header carries `SignatureCallingConvention.Unmanaged` where this one carries `Default`.
//
// So this is the pair that shows the gate keys on the call site's convention rather than on the
// target alone: the same method, reached by `calli` twice, legal once and fatal once.

public static unsafe class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        delegate* unmanaged<int, int> unmanaged = &Doubler;

        // The launder through `nint` is what C# requires to spell this: it will not convert
        // between function-pointer types of differing calling convention directly.
        delegate*<int, int> managed = (delegate*<int, int>) (nint) unmanaged;

        try
        {
            return managed (21);
        }
        catch (Exception)
        {
            return 1;
        }
    }
}
