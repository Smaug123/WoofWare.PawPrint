using System;

class Program
{
    private int _field;

    static int TestNullFieldLoad()
    {
        Program p = null;
        try
        {
            int x = p._field;
            return 1; // should not reach
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestNullFieldStore()
    {
        Program p = null;
        try
        {
            p._field = 42;
            return 1; // should not reach
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestNullVirtcall()
    {
        object o = null;
        try
        {
            o.GetHashCode();
            return 1; // should not reach
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int TestThrowNull()
    {
        try
        {
            throw null;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    static int Main(string[] args)
    {
        int result = 0;
        result += TestNullFieldLoad();
        result += TestNullFieldStore();
        result += TestNullVirtcall();
        result += TestThrowNull();
        return result;
    }
}
