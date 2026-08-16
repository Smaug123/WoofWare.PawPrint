using System;

class FilterEscape : Exception
{
}

// An exception that escapes an exception *filter* is discarded: the CLR treats it as the
// filter returning false, and handler search resumes for the original exception. But the
// escaping exception did propagate, so it carries a stack trace, and a guest holding
// the object can read it afterwards.
//
// Measured on .NET 10 before being written. Both escape routes are covered because PawPrint
// reaches the filter boundary by two different code paths:
//
//   * a throw *inside the filter body*, which never leaves the filter's frame, so dispatch
//     starts and ends in that one frame. Real .NET reports one frame, `RunBody`.
//   * a throw inside a method the filter *called*, which unwinds into the filter's frame.
//     Real .NET reports `Callee`, `FilterViaCallee`, `RunViaCallee` — up to and including
//     the frame that hosts the filter, where dispatch stops.
//
// The objects are pre-allocated statics rather than caught and stashed, so nothing else ever
// records a trace for them: whatever they carry at the end came from the escaping raise
// alone.
class FilterEscapeExceptionHasTrace
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

    static readonly FilterEscape BodyBoom = new FilterEscape();
    static readonly FilterEscape CalleeBoom = new FilterEscape();

    static int declined;

    static void Callee()
    {
        throw CalleeBoom;
    }

    static bool FilterViaCallee()
    {
        Callee();
        return false;
    }

    static void RunBody()
    {
        try
        {
            throw new InvalidOperationException("outer-body");
        }
        // A `throw` *expression*: the raise begins in this frame, with the filter body as its
        // search PC, and no call ever leaves the frame.
        catch (InvalidOperationException) when (BodyBoom == null ? true : throw BodyBoom)
        {
            declined += 1000;
        }
        catch (InvalidOperationException)
        {
            declined += 1;
        }
    }

    static void RunViaCallee()
    {
        try
        {
            throw new InvalidOperationException("outer-callee");
        }
        catch (InvalidOperationException) when (FilterViaCallee())
        {
            declined += 1000;
        }
        catch (InvalidOperationException)
        {
            declined += 10;
        }
    }

    static int Main(string[] args)
    {
        RunBody();
        RunViaCallee();

        // Guards the premise: an escaping exception must make the filter decline, not accept,
        // and must not itself escape the method.
        if (declined != 11)
        {
            return 1;
        }

        string body = BodyBoom.StackTrace;

        if (body == null)
        {
            return 2;
        }

        if (!ContainsSubstring(body, "FilterEscapeExceptionHasTrace.RunBody"))
        {
            return 3;
        }

        string callee = CalleeBoom.StackTrace;

        if (callee == null)
        {
            return 4;
        }

        if (!ContainsSubstring(callee, "FilterEscapeExceptionHasTrace.Callee"))
        {
            return 5;
        }

        if (!ContainsSubstring(callee, "FilterEscapeExceptionHasTrace.FilterViaCallee"))
        {
            return 6;
        }

        // The frame hosting the filter is where dispatch stops, and it is in the trace.
        if (!ContainsSubstring(callee, "FilterEscapeExceptionHasTrace.RunViaCallee"))
        {
            return 7;
        }

        return 0;
    }
}
