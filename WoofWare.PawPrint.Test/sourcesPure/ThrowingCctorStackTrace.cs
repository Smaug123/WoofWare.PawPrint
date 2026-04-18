using System;

class ThrowingStaticInit3
{
    public static int Value = 42;

    static ThrowingStaticInit3()
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
            int v = ThrowingStaticInit3.Value;
            return 1;
        }
        catch (TypeInitializationException ex)
        {
            // The TIE's own StackTrace should mention the caller (Program.Main),
            // not the .cctor internals.
            string outerTrace = ex.StackTrace;
            if (outerTrace == null)
            {
                return 10;
            }

            if (!outerTrace.Contains("Program.Main"))
            {
                return 11;
            }

            // The inner exception's StackTrace should mention the .cctor.
            if (ex.InnerException == null)
            {
                return 20;
            }

            string innerTrace = ex.InnerException.StackTrace;
            if (innerTrace == null)
            {
                return 21;
            }

            if (!innerTrace.Contains(".cctor"))
            {
                return 22;
            }

            return 0;
        }
    }
}
