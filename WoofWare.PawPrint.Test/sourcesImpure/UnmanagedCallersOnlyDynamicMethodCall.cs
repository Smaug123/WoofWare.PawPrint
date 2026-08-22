using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

// The plain `call` opcode naming a `[UnmanagedCallersOnly]` method.
//
// C# will not emit that: a direct call is CS8901 and a method-group conversion is CS8902, so the
// only route to it is IL the guest writes itself. `DynamicMethod` is the cheapest one — Reflection.Emit
// applies no such rule — and the resulting body is an ordinary `call`, which is how a hand-written
// IL assembly or a future emitted invoke stub would reach the same method.
//
// Without this guest, `call` and `callvirt` are the two managed call sites the gate covers that no
// test reaches: mutation-testing the others to `Unmanaged` kills a guest apiece, but mutating
// `call` did not. (`callvirt` remains out of reach for a different and permanent reason:
// `[UnmanagedCallersOnly]` requires a static method and `callvirt` requires an instance one, so no
// valid IL puts the two together.)
//
// Needs `System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported`, which the fixture
// seeds; PawPrint's default is false.

public static class Program
{
    [UnmanagedCallersOnly]
    public static int Doubler (int x)
    {
        return x * 2;
    }

    public static int Main ()
    {
        DynamicMethod dm =
            new DynamicMethod ("CallDoubler", typeof (int), new[] { typeof (int) }, typeof (Program).Module);

        ILGenerator il = dm.GetILGenerator ();
        il.Emit (OpCodes.Ldarg_0);
        il.Emit (OpCodes.Call, typeof (Program).GetMethod ("Doubler"));
        il.Emit (OpCodes.Ret);

        Func<int, int> call = (Func<int, int>) dm.CreateDelegate (typeof (Func<int, int>));

        try
        {
            return call (21);
        }
        catch (Exception)
        {
            return 1;
        }
    }
}
