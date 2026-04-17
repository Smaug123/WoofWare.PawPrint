using System;

class MyBaseException : Exception
{
}

class MyDerivedException : MyBaseException
{
}

class UnrelatedExn : Exception
{
}

class Program
{
    static int Main(string[] argv)
    {
        // Test 1: Catch base type when throwing derived
        try
        {
            throw new MyDerivedException();
        }
        catch (MyBaseException)
        {
            // Correctly caught by base type
        }

        // Test 2: Catch specific type should NOT match unrelated type
        try
        {
            try
            {
                throw new MyDerivedException();
            }
            catch (UnrelatedExn)
            {
                // This should not be reached
                return 1;
            }
        }
        catch (MyBaseException)
        {
            // The MyDerivedException should fall through to here
        }

        return 0;
    }
}
