using System;

class Program
{
    static int TestLength()
    {
        int[] values = null;

        try
        {
            return values.Length;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestLoad()
    {
        int[] values = null;

        try
        {
            return values[0];
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestStore()
    {
        int[] values = null;

        try
        {
            values[0] = 1;
            return 1;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestAddress()
    {
        int[] values = null;

        try
        {
            ref int value = ref values[0];
            value = 1;
            return 1;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static T GenericLoad<T>(T[] values)
    {
        return values[0];
    }

    static void GenericStore<T>(T[] values, T value)
    {
        values[0] = value;
    }

    static int TestGenericLoad()
    {
        string[] values = null;

        try
        {
            return GenericLoad(values) == null ? 2 : 3;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestGenericStore()
    {
        string[] values = null;

        try
        {
            GenericStore(values, "x");
            return 1;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int Main(string[] args)
    {
        return
            TestLength()
            + TestLoad()
            + TestStore()
            + TestAddress()
            + TestGenericLoad()
            + TestGenericStore();
    }
}
