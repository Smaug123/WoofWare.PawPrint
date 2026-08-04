// `MemoryMarshal.GetArrayDataReference<T>(null)` throws `NullReferenceException`. This is
// documented on the method (`<exception cref="NullReferenceException">`) and the JIT emits an
// explicit null check for it rather than relying on a fault, so it is a guaranteed throw
// rather than an accident of the expansion.
//
// It is a static method, so `callvirt`'s own null check never sees the argument — the
// intrinsic has to raise this itself.

using System;
using System.Runtime.InteropServices;

public class Program
{
    // Opaque so the null cannot be constant-folded into the call.
    private static int[] NullArray()
    {
        return null;
    }

    private static int[] RealArray()
    {
        return new int[] { 11, 22, 33 };
    }

    public static int Main(string[] args)
    {
        try
        {
            ref int r = ref MemoryMarshal.GetArrayDataReference(NullArray());
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        // A non-null array must still produce a reference to element 0.
        ref int first = ref MemoryMarshal.GetArrayDataReference(RealArray());
        if (first != 11)
        {
            return 2;
        }

        return 0;
    }
}
