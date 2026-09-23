using System;
using System.Reflection;

// Static fields of a closed generic type reached through a *derived* type's reflection. Each type
// in the hierarchy has its fields enumerated separately, and a static field inherited from a
// closed generic base is swapped for the base instantiation's own
// (`RuntimeFieldHandle.GetStaticFieldForGenericType`), so the value read must be the base
// instantiation's storage whichever type the lookup started from.

class Base<T>
{
    public static int Shared;
    public static T Typed;
}

class DerivedFromClosed : Base<int>
{
}

class DerivedGeneric<U> : Base<U>
{
    public static int Own;
}

struct GenStruct<T>
{
    public static long Count;
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

    const BindingFlags Flattened = BindingFlags.Public | BindingFlags.Static | BindingFlags.FlattenHierarchy;

    static int Main()
    {
        Base<int>.Shared = 3;
        Base<string>.Shared = 4;
        Base<int>.Typed = 17;
        Base<string>.Typed = "hello";
        DerivedGeneric<string>.Own = 6;
        GenStruct<int>.Count = 100;
        GenStruct<byte>.Count = 200;

        Check((int)typeof(DerivedFromClosed).GetField("Shared", Flattened).GetValue(null) == 3);
        Check((int)typeof(DerivedFromClosed).GetField("Typed", Flattened).GetValue(null) == 17);
        Check((int)typeof(DerivedGeneric<string>).GetField("Shared", Flattened).GetValue(null) == 4);
        Check((string)typeof(DerivedGeneric<string>).GetField("Typed", Flattened).GetValue(null) == "hello");
        Check((int)typeof(DerivedGeneric<string>).GetField("Own").GetValue(null) == 6);
        Check((int)typeof(DerivedGeneric<int>).GetField("Own").GetValue(null) == 0);

        // The field's declaring type is the base instantiation, not the type the lookup started from.
        Check(typeof(DerivedFromClosed).GetField("Shared", Flattened).DeclaringType == typeof(Base<int>));
        Check(typeof(DerivedGeneric<string>).GetField("Shared", Flattened).DeclaringType == typeof(Base<string>));

        Check((long)typeof(GenStruct<int>).GetField("Count").GetValue(null) == 100);
        Check((long)typeof(GenStruct<byte>).GetField("Count").GetValue(null) == 200);
        Check((long)typeof(GenStruct<char>).GetField("Count").GetValue(null) == 0);

        // Enumerating every static field goes through the same fixup for each.
        FieldInfo[] statics = typeof(Base<string>).GetFields(BindingFlags.Public | BindingFlags.Static);
        Check(statics.Length == 2);
        int seen = 0;
        foreach (FieldInfo f in statics)
        {
            if (f.Name == "Shared" && (int)f.GetValue(null) == 4)
            {
                seen = seen + 1;
            }
            if (f.Name == "Typed" && (string)f.GetValue(null) == "hello")
            {
                seen = seen + 1;
            }
        }
        Check(seen == 2);

        return firstFailure;
    }
}
