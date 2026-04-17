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
        // First access: triggers .cctor, which throws. The CLR wraps the exception
        // in TypeInitializationException.
        try
        {
            int v = ThrowingStaticInit.Value;
            // Should not reach here
            return 1;
        }
        catch (TypeInitializationException)
        {
            // Good: first access throws TypeInitializationException.
        }
        catch (InvalidOperationException)
        {
            // The raw InvalidOperationException must NOT be caught here;
            // the CLR wraps it in TypeInitializationException.
            return 2;
        }

        // Second access: the .cctor is NOT re-run, but subsequent access to a type
        // whose .cctor previously failed must also throw TypeInitializationException
        // (ECMA-335 §II.10.5.3.3).
        try
        {
            int v = ThrowingStaticInit.Value;
            // Should not reach here
            return 3;
        }
        catch (TypeInitializationException)
        {
            // Good: second access also throws TypeInitializationException.
        }
        catch (InvalidOperationException)
        {
            return 4;
        }

        return 0;
    }
}
