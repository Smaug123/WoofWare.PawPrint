using System;

class CaughtExceptionStackTrace
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

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static int Main(string[] args)
    {
        try
        {
            Thrower();
            return 1;
        }
        catch (InvalidOperationException ex)
        {
            string trace = ex.StackTrace;
            if (trace == null)
            {
                return 10;
            }

            if (!ContainsSubstring(trace, "CaughtExceptionStackTrace.Thrower"))
            {
                return 11;
            }

            if (!ContainsSubstring(trace, "CaughtExceptionStackTrace.Main"))
            {
                return 12;
            }

            return 0;
        }
    }
}
