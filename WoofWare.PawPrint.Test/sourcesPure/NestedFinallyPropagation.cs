using System;

class OuterFinallyException : Exception
{
}

class InnerFinallyException : Exception
{
}

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

    static int FinallyExceptionReplacesOriginal()
    {
        int x = 0;

        try
        {
            try
            {
                x += 1;
                throw new OuterFinallyException();
            }
            finally
            {
                x += 10;
                throw new InnerFinallyException();
            }
        }
        catch (InnerFinallyException)
        {
            x += 100;
        }
        catch (OuterFinallyException)
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

        if (FinallyExceptionReplacesOriginal() != 111)
        {
            return 3;
        }

        return 0;
    }
}
