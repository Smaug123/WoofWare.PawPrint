// The token forms `ldelem <T>`, `stelem <T>` and `ldelema <T>` raise the same two faults as the
// element-typed `ldelem.*`/`stelem.*` opcodes: `NullReferenceException` when the array is null,
// and `IndexOutOfRangeException` when the index lies outside it (ECMA-335 III.4.8, III.4.9,
// III.4.26). The null check comes first, so a null array with an out-of-range index raises
// `NullReferenceException`.
//
// Roslyn reaches the token forms from a generic `T[]` (`ldelem !!T`, a TypeSpec token) and from
// an array of a user-defined struct (`ldelem Pair`, a TypeDef token); both spellings are here.

using System;

public struct Pair
{
    public int A;
    public int B;
}

public class Program
{
    private static T Load<T>(T[] arr, int i)
    {
        return arr[i];
    }

    private static void Store<T>(T[] arr, int i, T v)
    {
        arr[i] = v;
    }

    private static ref T Address<T>(T[] arr, int i)
    {
        return ref arr[i];
    }

    private static int LoadNullArray()
    {
        try
        {
            int x = Load<int>(null, 0);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            string x = Load<string>(null, 0);
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        // The null check comes before the bounds check.
        try
        {
            int x = Load<int>(null, 5);
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }

    private static int LoadOutOfRange()
    {
        int[] a = { 10, 20, 30 };
        string[] s = { "a", "b", "c" };

        try
        {
            int x = Load(a, 3);
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int x = Load(a, -1);
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            string x = Load(s, 3);
            return 3;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int x = Load(new int[0], 0);
            return 4;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (Load(a, 2) != 30 || Load(s, 1) != "b")
        {
            return 5;
        }

        return 0;
    }

    private static int StoreNullArray()
    {
        try
        {
            Store<int>(null, 0, 1);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Store<string>(null, 0, "z");
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Store<int>(null, 5, 1);
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }

    private static int StoreOutOfRange()
    {
        int[] a = { 10, 20, 30 };

        try
        {
            Store(a, 3, 99);
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            Store(a, -1, 99);
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 3;
        }

        Store(a, 1, 21);
        if (a[1] != 21)
        {
            return 4;
        }

        return 0;
    }

    private static int AddressNullArray()
    {
        try
        {
            ref int x = ref Address<int>(null, 0);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            ref string x = ref Address<string>(null, 5);
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }

    private static int AddressOutOfRange()
    {
        int[] a = { 10, 20, 30 };

        try
        {
            ref int x = ref Address(a, 3);
            return 1;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            ref int x = ref Address(a, -1);
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (a[0] != 10 || a[1] != 20 || a[2] != 30)
        {
            return 3;
        }

        Address(a, 2) = 31;
        if (a[2] != 31)
        {
            return 4;
        }

        return 0;
    }

    // A user-defined struct element puts a TypeDef token on the opcodes rather than a TypeSpec.
    private static Pair LoadPair(Pair[] arr, int i)
    {
        return arr[i];
    }

    private static void StorePair(Pair[] arr, int i, Pair p)
    {
        arr[i] = p;
    }

    private static int LoadPairField(Pair[] arr, int i)
    {
        return arr[i].A;
    }

    private static int StructElements()
    {
        Pair[] ps = new Pair[2];
        ps[0].A = 1;
        ps[1].A = 2;

        try
        {
            Pair p = LoadPair(null, 0);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Pair p = LoadPair(ps, 2);
            return 2;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            StorePair(null, 0, new Pair());
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            StorePair(ps, -1, new Pair());
            return 4;
        }
        catch (IndexOutOfRangeException)
        {
        }

        try
        {
            int x = LoadPairField(null, 0);
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            int x = LoadPairField(ps, 2);
            return 6;
        }
        catch (IndexOutOfRangeException)
        {
        }

        if (ps[0].A != 1 || ps[1].A != 2 || LoadPair(ps, 1).A != 2 || LoadPairField(ps, 0) != 1)
        {
            return 7;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int result;

        result = LoadNullArray();
        if (result != 0)
        {
            return 10 + result;
        }

        result = LoadOutOfRange();
        if (result != 0)
        {
            return 20 + result;
        }

        result = StoreNullArray();
        if (result != 0)
        {
            return 30 + result;
        }

        result = StoreOutOfRange();
        if (result != 0)
        {
            return 40 + result;
        }

        result = AddressNullArray();
        if (result != 0)
        {
            return 50 + result;
        }

        result = AddressOutOfRange();
        if (result != 0)
        {
            return 60 + result;
        }

        result = StructElements();
        if (result != 0)
        {
            return 70 + result;
        }

        return 0;
    }
}
