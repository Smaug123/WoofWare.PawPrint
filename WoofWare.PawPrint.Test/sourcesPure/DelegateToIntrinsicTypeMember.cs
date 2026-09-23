using System;
using System.Reflection;

// Delegates whose target is a member of `Int128`, a type carrying a type-level `[Intrinsic]`, and
// is named as such: a method group over a static member (`ldftn Int128::IsNegative`) and a
// reflection-bound delegate over `Int128::GetHashCode`. Unlike `LdvirtftnIntrinsicDeclaringType.cs`,
// no declaration on another type is involved anywhere, so nothing here can be satisfied by
// remembering what a call site named: a delegate enters its target through the target's entry
// point, and on real .NET that runs the target's IL.
//
// Neither member is called directly: PawPrint gates a direct call to an unreviewed member of an
// `[Intrinsic]` type, which is the asymmetry this file exists to pin.

class Program
{
    static int Main(string[] args)
    {
        Int128 minusOne = new Int128(ulong.MaxValue, ulong.MaxValue);
        Int128 one = new Int128(0, 1);

        Func<Int128, bool> isNegative = Int128.IsNegative;
        if (!isNegative(minusOne))
        {
            return 1;
        }

        if (isNegative(one))
        {
            return 2;
        }

        object boxed = one;
        MethodInfo getHashCode = typeof(Int128).GetMethod("GetHashCode", Type.EmptyTypes);
        Func<int> viaReflection = getHashCode.CreateDelegate<Func<int>>(boxed);
        if (viaReflection() != boxed.GetHashCode())
        {
            return 3;
        }

        return 0;
    }
}
