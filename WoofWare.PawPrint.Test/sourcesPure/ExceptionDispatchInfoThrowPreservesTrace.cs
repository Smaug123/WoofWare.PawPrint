using System;
using System.Runtime.ExceptionServices;

// Stack-trace *content* across an `ExceptionDispatchInfo` capture/rethrow round trip.
//
// `ExceptionDispatchInfoThrow.cs` covers the round trip's identity and type behaviour without
// touching the trace; this file covers what the trace says. `RestoreDispatchState` puts the
// captured `_stackTrace` back and calls `Exception.PrepareForForeignExceptionRaise`, which sets a
// one-shot per-thread flag; the ensuing throw keeps the restored frames instead of starting over,
// and marks the last of them as coming from a previous throw. That mark renders as
// "--- End of stack trace from previous location ---".
//
// Trace text is not comparable across runtimes — real .NET appends " in file:line" to each frame
// and hides `[StackTraceHidden]` frames, PawPrint does neither — so everything below is a
// substring or a count of substrings, never an equality. All of it was measured on .NET 10 before
// being written down.
//
// The counts are the point. "Contains the annotation" would pass on an implementation that emitted
// one for every restored frame, or one for a capture of a never-thrown exception; requiring exactly
// one per hop and none for case 3 pins placement, not just presence.
class ExceptionDispatchInfoThrowPreservesTrace
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

    static void OriginalThrower()
    {
        throw new InvalidOperationException("boom");
    }

    // One capture/rethrow hop: the original frames survive, followed by exactly one boundary,
    // followed by the frames from the rethrow.
    static int SingleHop()
    {
        Exception captured;

        try
        {
            OriginalThrower();
            return 10;
        }
        catch (InvalidOperationException ex)
        {
            captured = ex;
        }

        ExceptionDispatchInfo edi = ExceptionDispatchInfo.Capture(captured);

        string trace;

        try
        {
            edi.Throw();
            return 11;
        }
        catch (InvalidOperationException ex)
        {
            trace = ex.StackTrace;
        }

        if (trace == null)
        {
            return 12;
        }

        int thrower = IndexOfSubstring(trace, "OriginalThrower", 0);

        if (thrower < 0)
        {
            return 13;
        }

        if (CountSubstring(trace, Boundary) != 1)
        {
            return 14;
        }

        int boundary = IndexOfSubstring(trace, Boundary, 0);

        // The frame that originally threw must sit *above* the boundary: the restored frames come
        // first, then the annotation, then the rethrow site.
        if (boundary < thrower)
        {
            return 15;
        }

        // ...and the rethrowing frame must appear *below* it too. `SingleHop` occurs on both sides
        // — it is both the frame that called `OriginalThrower` and the frame that called
        // `Throw()` — so this is a second occurrence after the annotation, not the first one.
        // Together the two checks pin the boundary between the two halves rather than merely
        // somewhere in the string.
        if (IndexOfSubstring(trace, "SingleHop", boundary) < 0)
        {
            return 16;
        }

        return 0;
    }

    // Two hops: each `Throw()` contributes its own boundary, and the earlier one is not lost.
    static int DoubleHop()
    {
        Exception captured;

        try
        {
            OriginalThrower();
            return 20;
        }
        catch (InvalidOperationException ex)
        {
            captured = ex;
        }

        Exception once;

        try
        {
            ExceptionDispatchInfo.Capture(captured).Throw();
            return 21;
        }
        catch (InvalidOperationException ex)
        {
            once = ex;
        }

        string trace;

        try
        {
            ExceptionDispatchInfo.Capture(once).Throw();
            return 22;
        }
        catch (InvalidOperationException ex)
        {
            trace = ex.StackTrace;
        }

        if (trace == null)
        {
            return 23;
        }

        if (CountSubstring(trace, Boundary) != 2)
        {
            return 24;
        }

        if (IndexOfSubstring(trace, "OriginalThrower", 0) < 0)
        {
            return 25;
        }

        return 0;
    }

    // Capturing an exception that has never been thrown is legal, and its dispatch state has no
    // frames at all. CoreCLR guards the mark with "numCurrentFrames > 0" (excep.cpp), so the
    // rethrow gets a trace with no boundary in it.
    static int NeverThrown()
    {
        ExceptionDispatchInfo edi = ExceptionDispatchInfo.Capture(new InvalidOperationException("unthrown"));

        string trace;

        try
        {
            edi.Throw();
            return 30;
        }
        catch (InvalidOperationException ex)
        {
            trace = ex.StackTrace;
        }

        if (trace == null)
        {
            return 31;
        }

        if (CountSubstring(trace, Boundary) != 0)
        {
            return 32;
        }

        return 0;
    }

    // The foreign-raise flag is consumed by the throw that follows it and by no later one. An
    // ordinary `throw ex` of the *same* already-thrown object, straight after a rethrow through an
    // `ExceptionDispatchInfo`, must therefore start a fresh trace with no boundary in it — real
    // .NET clears `_stackTrace` at such a throw, and PawPrint reaches the same rendered answer by
    // not reading the token at all.
    static int FlagIsOneShot()
    {
        Exception captured;

        try
        {
            OriginalThrower();
            return 40;
        }
        catch (InvalidOperationException ex)
        {
            captured = ex;
        }

        Exception rethrown;

        try
        {
            ExceptionDispatchInfo.Capture(captured).Throw();
            return 41;
        }
        catch (InvalidOperationException ex)
        {
            rethrown = ex;
        }

        string trace;

        try
        {
            throw rethrown;
        }
        catch (InvalidOperationException ex)
        {
            trace = ex.StackTrace;
        }

        if (trace == null)
        {
            return 42;
        }

        if (CountSubstring(trace, Boundary) != 0)
        {
            return 43;
        }

        return 0;
    }

    static int Main(string[] args)
    {
        int result = SingleHop();

        if (result != 0)
        {
            return result;
        }

        result = DoubleHop();

        if (result != 0)
        {
            return result;
        }

        result = NeverThrown();

        if (result != 0)
        {
            return result;
        }

        return FlagIsOneShot();
    }
}
