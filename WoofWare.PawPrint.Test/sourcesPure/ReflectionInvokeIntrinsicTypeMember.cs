using System;
using System.Reflection;

// `MethodBase.Invoke` on a static member of `Int128`, a type carrying a type-level `[Intrinsic]`.
// Reflection enters the method through its entry point, so on real .NET it runs
// `Int128.IsNegative`'s IL. Static, so the receiver needs no unboxing stub.

class Program
{
    static int Main(string[] args)
    {
        MethodInfo isNegative = typeof(Int128).GetMethod("IsNegative", new[] { typeof(Int128) });

        if (!(bool)isNegative.Invoke(null, new object[] { new Int128(ulong.MaxValue, ulong.MaxValue) }))
        {
            return 1;
        }

        if ((bool)isNegative.Invoke(null, new object[] { new Int128(0, 1) }))
        {
            return 2;
        }

        return 0;
    }
}
