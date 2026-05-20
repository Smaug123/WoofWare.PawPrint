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

    static void ThrowInnerContinuationException()
    {
        state += 100;
        throw new InnerContinuationException();
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

    static int PlainCatchInsidePropagatingFinally()
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
                catch (InnerContinuationException ex)
                {
                    if (ex == null)
                    {
                        state += 1000000;
                    }
                    else
                    {
                        state += 1000;
                    }
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

    static int CalleeThrowCatchInsidePropagatingFinally()
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
                    ThrowInnerContinuationException();
                }
                catch (InnerContinuationException ex)
                {
                    if (ex == null)
                    {
                        state += 1000000;
                    }
                    else
                    {
                        state += 1000;
                    }
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
        if (NestedCatchInsidePropagatingFinally() != 111121)
        {
            return 1;
        }

        if (PlainCatchInsidePropagatingFinally() != 111111)
        {
            return 2;
        }

        if (CalleeThrowCatchInsidePropagatingFinally() != 111111)
        {
            return 3;
        }

        return 0;
    }
}
