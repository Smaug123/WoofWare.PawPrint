using System;

class FirstFilterException : Exception
{
}

class SecondFilterException : Exception
{
}

class Program
{
    static int state;

    static bool Filter(bool result, int add)
    {
        state += add;
        return result;
    }

    static int FalseFilterFallsThroughToTypedCatch()
    {
        state = 0;

        try
        {
            state += 1;
            throw new FirstFilterException();
        }
        catch (FirstFilterException ex) when (ex != null && Filter(false, 100))
        {
            state += 1000;
        }
        catch (FirstFilterException ex)
        {
            if (ex == null)
            {
                return -1;
            }

            state += 10;
        }

        return state;
    }

    static int SecondFilterRunsAfterFirstDeclines()
    {
        state = 0;

        try
        {
            state += 1;
            throw new SecondFilterException();
        }
        catch (SecondFilterException ex) when (ex != null && Filter(false, 10))
        {
            state += 1000;
        }
        catch (SecondFilterException ex) when (ex != null && Filter(true, 100))
        {
            state += 100;
        }
        catch
        {
            state += 10000;
        }

        return state;
    }

    static int Main(string[] args)
    {
        if (FalseFilterFallsThroughToTypedCatch() != 111)
        {
            return 1;
        }

        if (SecondFilterRunsAfterFirstDeclines() != 211)
        {
            return 2;
        }

        return 0;
    }
}
