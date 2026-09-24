using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// Every access to a type whose initialiser has failed rethrows the one cached
// TypeInitializationException, and CoreCLR clears all three of that instance's trace fields
// (`ExceptionObject::ClearStackTraceForThrow`: `_remoteStackTraceString`, `_stackTrace` and
// `_stackTraceString`) before each rethrow (`MethodTable::DoRunClassInitThrowing`). So each catch
// sees a trace naming only the access that raised it this time, whatever an earlier raise, or a
// reflective write, left in any of the three fields.

static class Boom
{
    public static int Value;

    static Boom()
    {
        throw new InvalidOperationException("boom");
    }
}

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static bool ContainsSubstring(string haystack, string needle)
    {
        if (haystack == null)
        {
            return false;
        }

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

    [MethodImpl(MethodImplOptions.NoInlining)]
    static TypeInitializationException FirstAccess()
    {
        try
        {
            Boom.Value = 1;
        }
        catch (TypeInitializationException e)
        {
            return e;
        }

        return null;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static TypeInitializationException SecondAccess()
    {
        try
        {
            Boom.Value = 2;
        }
        catch (TypeInitializationException e)
        {
            return e;
        }

        return null;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static TypeInitializationException ThirdAccess()
    {
        try
        {
            Boom.Value = 3;
        }
        catch (TypeInitializationException e)
        {
            return e;
        }

        return null;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static TypeInitializationException FourthAccess()
    {
        try
        {
            Boom.Value = 4;
        }
        catch (TypeInitializationException e)
        {
            return e;
        }

        return null;
    }

    static int Main()
    {
        FieldInfo remote =
            typeof(Exception).GetField("_remoteStackTraceString", BindingFlags.NonPublic | BindingFlags.Instance);
        FieldInfo stackTraceString =
            typeof(Exception).GetField("_stackTraceString", BindingFlags.NonPublic | BindingFlags.Instance);

        TypeInitializationException first = FirstAccess();
        Check(first != null);
        Check(first != null && ContainsSubstring(first.StackTrace, "FirstAccess"));

        // The trace of an ordinary earlier raise is replaced, not extended.
        TypeInitializationException second = SecondAccess();
        Check(second != null && ReferenceEquals(first, second));
        Check(second != null && ContainsSubstring(second.StackTrace, "SecondAccess"));
        Check(second != null && !ContainsSubstring(second.StackTrace, "FirstAccess"));

        // A remote trace is dropped too.
        remote.SetValue(second, "SENTINEL_REMOTE");
        Check(ContainsSubstring(second.StackTrace, "SENTINEL_REMOTE"));
        TypeInitializationException third = ThirdAccess();
        Check(third != null && ReferenceEquals(first, third));
        Check(third != null && remote.GetValue(third) == null);
        Check(third != null && !ContainsSubstring(third.StackTrace, "SENTINEL_REMOTE"));
        Check(third != null && ContainsSubstring(third.StackTrace, "ThirdAccess"));

        // As is a materialised trace string, which `StackTrace` would otherwise prefer to the frames.
        // PawPrint writes `_stackTraceString` itself on every raise, so under PawPrint this round
        // holds whether or not the rethrow clears it; it pins the real runtime's answer.
        stackTraceString.SetValue(third, "SENTINEL_STACK_TRACE_STRING");
        Check(ContainsSubstring(third.StackTrace, "SENTINEL_STACK_TRACE_STRING"));
        TypeInitializationException fourth = FourthAccess();
        Check(fourth != null && ReferenceEquals(first, fourth));
        Check(fourth != null && !ContainsSubstring(fourth.StackTrace, "SENTINEL_STACK_TRACE_STRING"));
        Check(fourth != null && ContainsSubstring(fourth.StackTrace, "FourthAccess"));
        Check(fourth != null && !ContainsSubstring(fourth.StackTrace, "ThirdAccess"));

        return firstFailure;
    }
}
