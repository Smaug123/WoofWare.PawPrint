using System;

class GenericBaseException<T> : Exception
{
}

class GenericCatcher<T>
{
    public static int CatchIt()
    {
        try
        {
            throw new GenericBaseException<T>();
        }
        catch (GenericBaseException<T>)
        {
            return 0;
        }
    }
}

class Program
{
    static int Main(string[] argv)
    {
        return GenericCatcher<int>.CatchIt();
    }
}
