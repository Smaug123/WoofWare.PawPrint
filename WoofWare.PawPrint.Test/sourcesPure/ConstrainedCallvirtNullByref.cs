// `constrained. !!T callvirt` whose receiver byref is null.
//
// Inside a generic method taking `ref T`, Roslyn calls a method on `x` as
// `ldarg.0; constrained. !!T; callvirt`, passing the byref itself as the receiver. When that byref
// is null (here from `Unsafe.NullRef`, as it would be from `CollectionsMarshal.GetValueRefOrNullRef`
// on a missing key), what happens depends on which ECMA III.2.1 case the prefix takes:
//
// * a reference-type (or array) T dereferences the byref to get the object: the load faults, so
//   `NullReferenceException`;
// * a value-type T calling a method it inherits from Object/ValueType/Enum boxes `*x`: the copy into
//   the box faults, so `NullReferenceException`;
// * a value-type T with its own implementation is called directly with the null byref as `this`, so
//   the exception comes (or not) from whatever the callee does with `this`.
//
// Measured on .NET 10 under default tiering, full opts (`DOTNET_TieredCompilation=0`) and min opts
// (`DOTNET_JITMinOpts=1`): every row below agrees across the three. `x.GetType()` on a value-type T
// is deliberately absent, because it does *not* agree: the optimising JIT folds `box; GetType` into
// the type's handle and returns `typeof(T)` without touching `x`, while the unoptimised JIT boxes
// and faults.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    struct NoOverride
    {
        public int X;
    }

    // Overrides without reading `this`, so a null receiver goes unnoticed on real .NET.
    struct IgnoresThis
    {
        public int X;

        public override int GetHashCode()
        {
            return 7;
        }

        public override string ToString()
        {
            return "ignored";
        }
    }

    enum Colour
    {
        Red = 1,
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Hash<T>(ref T x)
    {
        return x.GetHashCode();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static string Str<T>(ref T x)
    {
        return x.ToString();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool EqualsNull<T>(ref T x)
    {
        return x.Equals(null);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Type TypeOf<T>(ref T x)
    {
        return x.GetType();
    }

    static bool HashThrowsNre<T>()
    {
        try
        {
            Hash(ref Unsafe.NullRef<T>());
            return false;
        }
        catch (NullReferenceException)
        {
            return true;
        }
    }

    static bool StrThrowsNre<T>()
    {
        try
        {
            Str(ref Unsafe.NullRef<T>());
            return false;
        }
        catch (NullReferenceException)
        {
            return true;
        }
    }

    static bool EqualsThrowsNre<T>()
    {
        try
        {
            EqualsNull(ref Unsafe.NullRef<T>());
            return false;
        }
        catch (NullReferenceException)
        {
            return true;
        }
    }

    public static int Main(string[] args)
    {
        // Case 1: reference-type T, and an array T, which takes the same path by another route.
        if (!HashThrowsNre<string>())
        {
            return 1;
        }

        if (!HashThrowsNre<int[]>())
        {
            return 2;
        }

        if (!StrThrowsNre<string>())
        {
            return 3;
        }

        try
        {
            TypeOf(ref Unsafe.NullRef<string>());
            return 4;
        }
        catch (NullReferenceException)
        {
        }

        // Case 3: value-type T calling what it inherits, so `*x` is boxed.
        if (!HashThrowsNre<NoOverride>())
        {
            return 5;
        }

        if (!StrThrowsNre<NoOverride>())
        {
            return 6;
        }

        if (!EqualsThrowsNre<NoOverride>())
        {
            return 7;
        }

        if (!HashThrowsNre<Colour>())
        {
            return 8;
        }

        // Case 2: the value type's own implementation runs with a null `this`. `Int32.GetHashCode`
        // reads `this`, so it faults inside the callee...
        if (!HashThrowsNre<int>())
        {
            return 9;
        }

        // ... and `IgnoresThis` never reads it, so there is nothing to fault on.
        if (Hash(ref Unsafe.NullRef<IgnoresThis>()) != 7)
        {
            return 10;
        }

        if (Str(ref Unsafe.NullRef<IgnoresThis>()) != "ignored")
        {
            return 11;
        }

        // A non-null receiver must still reach the method, through each case.
        string s = "abc";
        if (Str(ref s) != "abc")
        {
            return 12;
        }

        NoOverride n = new NoOverride { X = 5 };
        if (Hash(ref n) != n.GetHashCode())
        {
            return 13;
        }

        return 0;
    }
}
