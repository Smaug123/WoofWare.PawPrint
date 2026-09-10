using System;
using System.Reflection;

// FieldInfo.GetValue on the static fields of a closed generic type. Statics are per
// instantiation, so the two instantiations below must answer independently.
//
// Enumerating the fields of a closed generic type calls
// `RuntimeFieldHandle.GetStaticFieldForGenericType` for each static field
// (RuntimeType.CoreCLR.cs:916), which is how CoreCLR swaps the canonical FieldDesc for the exact
// instantiation's. That InternalCall is its own primitive, separate from the GetValue QCall.

class Gen<T>
{
    public T Value;
    public static int Count;
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static int Main()
    {
        Gen<int>.Count = 21;
        Check((int)typeof(Gen<int>).GetField("Count").GetValue(null) == 21);
        Check((int)typeof(Gen<string>).GetField("Count").GetValue(null) == 0);

        // Once a closed generic type has a static field, its instance fields are enumerated by
        // the same loop, so they are reachable only when the static fixup is.
        Check((int)typeof(Gen<int>).GetField("Value").GetValue(new Gen<int> { Value = 8 }) == 8);

        Gen<string>.Count = 5;
        Check((int)typeof(Gen<string>).GetField("Count").GetValue(null) == 5);
        Check((int)typeof(Gen<int>).GetField("Count").GetValue(null) == 21);

        return firstFailure;
    }
}
