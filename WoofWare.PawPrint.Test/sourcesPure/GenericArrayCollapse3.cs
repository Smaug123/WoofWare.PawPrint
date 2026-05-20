// Issue 3: resolveBaseTypeInfo TypeSpec path collapses array types.
//
// When walking the type hierarchy (e.g., for isinst/castclass), the
// interpreter calls resolveBaseConcreteType, which calls resolveBaseTypeInfo.
// For a base type encoded as a TypeSpec (generic base with array arg),
// resolveBaseTypeInfo calls resolveTypeFromSpecConcrete + typeInfoToTypeDefn,
// collapsing int[] to System.Array.
//
// This produces the wrong concrete type handle for the base class,
// causing type compatibility checks to fail: the runtime thinks the
// base is GenericBase<System.Array> when it should be GenericBase<int[]>.
//
// This test verifies that a derived class is correctly recognized as
// assignable to its generic base class parameterized with an array type.

public class GenBase<T>
{
    public T Data;

    public GenBase() {}

    public void Set(T value) { Data = value; }
    public T Get() { return Data; }
}

public class ArrayDerived : GenBase<string[]>
{
    public ArrayDerived() {}
}

public class DoubleDerived : ArrayDerived
{
}

public class GenericArrayCollapse3
{
    static bool IsGenBaseOfStringArray(object obj)
    {
        return obj is GenBase<string[]>;
    }

    public static int Main(string[] args)
    {
        // Direct derived: ArrayDerived extends GenBase<string[]>
        var ad = new ArrayDerived();
        ad.Set(new string[] { "hello", "world" });

        object boxed = ad;

        // isinst GenBase<string[]> must succeed
        if (!IsGenBaseOfStringArray(boxed)) return 1;

        // castclass should also work
        GenBase<string[]> asBase = (GenBase<string[]>)boxed;
        string[] data = asBase.Get();
        if (data[0] != "hello") return 2;
        if (data[1] != "world") return 3;

        // Two levels deep: DoubleDerived extends ArrayDerived extends GenBase<string[]>
        var dd = new DoubleDerived();
        dd.Set(new string[] { "deep" });

        if (!IsGenBaseOfStringArray(dd)) return 4;

        GenBase<string[]> ddBase = (GenBase<string[]>)dd;
        if (ddBase.Get()[0] != "deep") return 5;

        return 0;
    }
}
