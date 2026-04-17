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
        catch (Exception)
        {
            // The .cctor threw, so we should land here.
            result = 100;
        }

        return result == 100 ? 0 : result;
    }
}
