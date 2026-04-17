using System;

class ThrowingStaticInit
{
    public static int Value = 42;

    static ThrowingStaticInit()
    {
        throw new InvalidOperationException("cctor failed");
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result = 0;
        try
        {
            int v = ThrowingStaticInit.Value;
            // Should not reach here
            return 1;
        }
        catch (TypeInitializationException)
        {
            // Per CLR spec, a throwing .cctor is surfaced as TypeInitializationException.
            result = 100;
        }
        catch (InvalidOperationException)
        {
            // The raw InvalidOperationException must NOT be caught here;
            // the CLR wraps it in TypeInitializationException.
            return 2;
        }

        return result == 100 ? 0 : result;
    }
}
