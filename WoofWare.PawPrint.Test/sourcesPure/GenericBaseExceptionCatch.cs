using System;

class GenericBase<T> : Exception
{
}

class Derived : GenericBase<int>
{
}

class Program
{
    static int Main(string[] argv)
    {
        try
        {
            throw new Derived();
        }
        catch (GenericBase<int>)
        {
            return 0;
        }
    }
}
