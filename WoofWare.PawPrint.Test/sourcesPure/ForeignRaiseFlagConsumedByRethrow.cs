using System;
using System.Reflection;

// The foreign-raise flag belongs to the thread's *next raise*, not specifically to its next
// `throw`. CoreCLR reads and resets it in `StackTraceInfo::AppendElement` (excep.cpp:3016-3017),
// which runs for the first frame appended by whatever raise follows — including a `rethrow`,
// whose own helper `IL_Rethrow` never sets the flag.
//
// `Exception.PrepareForForeignExceptionRaise` is a private static, so only reflection can set the
// flag without also throwing in the same breath, which is what makes this observable at all;
// `ExceptionDispatchInfo.Throw()` always pairs the two. The consequences are two, and both are
// asserted:
//
//   * the rethrow marks the last frame the exception already carried, so its trace gains exactly
//     one boundary — even though a plain rethrow never produces one;
//   * having been consumed, the flag is gone, so an ordinary `throw` afterwards produces none.
//
// A runtime that consumed the flag only at `throw` would fail the first (no boundary where real
// .NET has one) and then fail the second too, spending the leaked flag on the later throw.
//
// Measured on .NET 10 before being written: 1 boundary then 0.
class ForeignRaiseFlagConsumedByRethrow
{
    const string Boundary = "--- End of stack trace from previous location ---";

    static int IndexOfSubstring(string haystack, string needle, int startAt)
    {
        for (int i = startAt; i <= haystack.Length - needle.Length; i++)
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
                return i;
            }
        }

        return -1;
    }

    static int CountSubstring(string haystack, string needle)
    {
        int count = 0;
        int at = 0;

        while (true)
        {
            int found = IndexOfSubstring(haystack, needle, at);

            if (found < 0)
            {
                return count;
            }

            count++;
            at = found + needle.Length;
        }
    }

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static void CatchAndRethrow(MethodInfo prepare)
    {
        try
        {
            Thrower();
        }
        catch (InvalidOperationException)
        {
            prepare.Invoke(null, null);
            throw;
        }
    }

    static int Main(string[] args)
    {
        MethodInfo prepare = typeof(Exception).GetMethod(
            "PrepareForForeignExceptionRaise",
            BindingFlags.NonPublic | BindingFlags.Static);

        if (prepare == null)
        {
            return 1;
        }

        Exception rethrown;

        try
        {
            CatchAndRethrow(prepare);
            return 2;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        string afterRethrow = rethrown.StackTrace;

        if (afterRethrow == null)
        {
            return 3;
        }

        if (CountSubstring(afterRethrow, Boundary) != 1)
        {
            return 4;
        }

        // The marked frame is the one the exception was already carrying — `CatchAndRethrow`, the
        // method the rethrow stands in — so the original throwing method sits above the boundary.
        if (IndexOfSubstring(afterRethrow, "Thrower", 0) > IndexOfSubstring(afterRethrow, Boundary, 0))
        {
            return 5;
        }

        string afterPlainThrow;

        try
        {
            throw rethrown;
        }
        catch (InvalidOperationException ex)
        {
            afterPlainThrow = ex.StackTrace;
        }

        if (afterPlainThrow == null)
        {
            return 6;
        }

        if (CountSubstring(afterPlainThrow, Boundary) != 0)
        {
            return 7;
        }

        return 0;
    }
}
