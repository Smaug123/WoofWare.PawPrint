// Issue 2: resolveTypeFromSpecConcrete round-trip collapses array types.
//
// This test exercises the resolveMethodMemberRef path, where a method
// is called on a generic type parameterized with an array type.
// The parent TypeSpec (e.g. GenericHelper<int[]>) is resolved via
// resolveTypeFromSpec, which calls resolveTypeFromDefn. The
// GenericInstantiation case resolves the int[] arg and round-trips
// through TypeInfo, collapsing it to System.Array.
//
// The method resolution still works (it's name-based), but we verify
// correctness by checking the returned value's type identity via isinst,
// which exposes the collapsed type through the concretization chain.

public class Wrapper<T>
{
    public T Item;

    public Wrapper(T item)
    {
        Item = item;
    }
}

public class GenericArrayCollapse2
{
    // Generic method: the stelem inside MakeArray uses a TypeSpec for T.
    // When T = Wrapper<int[]>, the TypeSpec is GenericInstantiation(Wrapper, [int[]]).
    // resolveTypeFromSpecConcrete collapses int[] to System.Array.
    static T[] MakeArray<T>(T a, T b)
    {
        T[] arr = new T[2];
        arr[0] = a;
        arr[1] = b;
        return arr;
    }

    public static int Main(string[] args)
    {
        var w1 = new Wrapper<int[]>(new int[] { 10 });
        var w2 = new Wrapper<int[]>(new int[] { 20 });

        // T = Wrapper<int[]>
        Wrapper<int[]>[] result = MakeArray<Wrapper<int[]>>(w1, w2);

        if (result.Length != 2) return 1;
        if (result[0].Item[0] != 10) return 2;
        if (result[1].Item[0] != 20) return 3;

        // Type identity check: is result[0] a Wrapper<int[]>?
        // If the type was collapsed, this might fail.
        object boxed = result[0];
        if (!(boxed is Wrapper<int[]>)) return 4;

        return 0;
    }
}
