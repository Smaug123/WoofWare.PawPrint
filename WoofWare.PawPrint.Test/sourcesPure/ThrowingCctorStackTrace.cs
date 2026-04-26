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
    static bool ContainsSubstring(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return true;
            }
        }

        return false;
    }

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

            if (!ContainsSubstring(outerTrace, "Program.Main"))
            {
                return 11;
            }

            if (ContainsSubstring(outerTrace, ".cctor"))
            {
                return 12;
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

            if (!ContainsSubstring(innerTrace, ".cctor"))
            {
                return 22;
            }

            return 0;
        }
    }
}
