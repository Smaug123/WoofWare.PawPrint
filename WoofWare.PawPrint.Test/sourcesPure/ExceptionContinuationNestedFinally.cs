using System;

class OuterContinuationException : Exception
{
}

class InnerContinuationException : Exception
{
}

class Program
{
    static int state;

    static bool Filter()
    {
        state += 10;
        return true;
    }

    static int NestedCatchInsidePropagatingFinally()
    {
        state = 0;

        try
        {
            try
            {
                state += 1;
                throw new OuterContinuationException();
            }
            finally
            {
                state += 10;

                try
                {
                    state += 100;
                    throw new InnerContinuationException();
                }
                catch (InnerContinuationException ex) when (ex != null && Filter())
                {
                    state += 1000;
                }

                state += 10000;
            }
        }
        catch (OuterContinuationException)
        {
            state += 100000;
        }

        return state;
    }

    static int Main(string[] args)
    {
        return NestedCatchInsidePropagatingFinally() == 111121 ? 0 : 1;
    }
}
