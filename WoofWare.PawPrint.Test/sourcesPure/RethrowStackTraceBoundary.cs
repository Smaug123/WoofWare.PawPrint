using System;

class RethrowStackTraceBoundary
{
    static string observedBeforeRethrow = "";

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

    static void Thrower(string value)
    {
        throw new InvalidOperationException(value);
    }

    static void ObserveAndRethrow(string value)
    {
        try
        {
            Thrower(value);
        }
        catch (InvalidOperationException ex)
        {
            // Force stack trace materialisation before the same exception is rethrown.
            observedBeforeRethrow = ex.StackTrace;
            throw;
        }
    }

    static void WrapAfterRethrow(string value)
    {
        try
        {
            ObserveAndRethrow(value);
        }
        catch (InvalidOperationException ex)
        {
            throw new ApplicationException("wrapped", ex);
        }
    }

    static int Main(string[] args)
    {
        try
        {
            WrapAfterRethrow("boom");
        }
        catch (ApplicationException ex)
        {
            string observedTrace = observedBeforeRethrow;
            if (observedTrace == null)
            {
                return 10;
            }

            if (!ContainsSubstring(observedTrace, "RethrowStackTraceBoundary.Thrower(String value)"))
            {
                return 11;
            }

            string rendered = ex.ToString();

            // CLR ToString renders the wrapped inner exception with this boundary; PawPrint's
            // current projection is a flatter, parameterless stack trace string.
            if (!ContainsSubstring(rendered, " ---> System.InvalidOperationException: boom"))
            {
                return 20;
            }

            if (!ContainsSubstring(rendered, "--- End of inner exception stack trace ---"))
            {
                return 21;
            }

            if (!ContainsSubstring(rendered, "RethrowStackTraceBoundary.Thrower(String value)"))
            {
                return 22;
            }

            if (!ContainsSubstring(rendered, "RethrowStackTraceBoundary.ObserveAndRethrow(String value)"))
            {
                return 23;
            }

            if (!ContainsSubstring(rendered, "RethrowStackTraceBoundary.WrapAfterRethrow(String value)"))
            {
                return 24;
            }

            if (!ContainsSubstring(rendered, "RethrowStackTraceBoundary.Main(String[] args)"))
            {
                return 25;
            }

            if (ContainsSubstring(rendered, "RethrowStackTraceBoundary.Thrower()"))
            {
                return 26;
            }

            return 0;
        }

        return 1;
    }
}
