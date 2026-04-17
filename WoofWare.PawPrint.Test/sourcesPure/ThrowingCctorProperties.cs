using System;

class ThrowingStaticInit2
{
    public static int Value = 42;

    static ThrowingStaticInit2()
    {
        throw new InvalidOperationException("cctor failed");
    }
}

class Program
{
    static int Main(string[] args)
    {
        try
        {
            int v = ThrowingStaticInit2.Value;
            return 1;
        }
        catch (TypeInitializationException ex)
        {
            // Check TypeName property: should be the full name of the type whose .cctor failed.
            if (ex.TypeName != "ThrowingStaticInit2")
            {
                return 10;
            }

            // Check Message: CLR default is
            // "The type initializer for 'ThrowingStaticInit2' threw an exception."
            if (ex.Message == null)
            {
                return 20;
            }

            // Check HResult: COR_E_TYPEINITIALIZATION = 0x80131534 = -2146233036
            if (ex.HResult != unchecked((int)0x80131534))
            {
                return 30;
            }

            // Check InnerException is present and is the original InvalidOperationException.
            if (ex.InnerException == null)
            {
                return 40;
            }

            if (!(ex.InnerException is InvalidOperationException))
            {
                return 41;
            }

            return 0;
        }
    }
}
