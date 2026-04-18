using System;

class Program
{
    static int SingleFinallyThenCatch()
    {
        int x = 0;

        try
        {
            try
            {
                throw new Exception();
            }
            finally
            {
                x += 10;
            }
        }
        catch
        {
            x += 100;
        }

        return x;
    }

    static int NestedFinallyThenCatch()
    {
        int x = 0;

        try
        {
            try
            {
                try
                {
                    throw new Exception();
                }
                finally
                {
                    x += 10;
                }
            }
            finally
            {
                x += 100;
            }
        }
        catch
        {
            x += 1000;
        }

        return x;
    }

    static int Main(string[] args)
    {
        if (SingleFinallyThenCatch() != 110)
        {
            return 1;
        }

        if (NestedFinallyThenCatch() != 1110)
        {
            return 2;
        }

        return 0;
    }
}
