using System;
using System.Runtime.ExceptionServices;

// `ExceptionDispatchInfo.Capture` reaches the QCall `ExceptionNative_GetFrozenStackTrace`
// (`Exception.CaptureDispatchState`, Exception.CoreCLR.cs:229-237) unconditionally. This is
// the minimal guest that reaches it: no Task, no thread pool, no scheduler.
//
// Everything asserted here is a fact that holds on both runtimes. Exact stack-trace *strings*
// are deliberately not compared: with debug symbols the real runtime appends `in File:line` to
// every frame and PawPrint does not (see Roslyn.DebugSymbols), so this uses substring checks
// in the style of CaughtExceptionStackTrace.cs.
//
// `ExceptionDispatchInfo.SetCurrentStackTrace` would be the sharpest probe of the captured
// state — it tests `_stackTrace != null` from managed code — but it is not reachable under
// PawPrint yet: `CanSetRemoteStackTrace` calls the `IsImmutableAgileException` InternalCall,
// and its success path builds a `new StackTrace(...)`, which needs the
// `StackTrace_GetStackFramesInternal` QCall. Both are unimplemented. The PawPrint-side
// contract is pinned instead by `sourcesImpure/ExceptionDispatchInfoCaptureState.cs`.
class ExceptionDispatchInfoCapture
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

    // Hand-rolled rather than `string.Equals(a, b, StringComparison.Ordinal)`, which PawPrint
    // services as an unimplemented JIT intrinsic. Same reason CaughtExceptionStackTrace.cs
    // hand-rolls its substring search.
    static bool SameString(string a, string b)
    {
        if (a.Length != b.Length)
        {
            return false;
        }

        for (int i = 0; i < a.Length; i++)
        {
            if (a[i] != b[i])
            {
                return false;
            }
        }

        return true;
    }

    static void Thrower()
    {
        throw new InvalidOperationException("boom");
    }

    static int Main(string[] args)
    {
        Exception caught;

        try
        {
            Thrower();
            return 1;
        }
        catch (InvalidOperationException ex)
        {
            caught = ex;
        }

        string beforeCapture = caught.StackTrace;

        if (beforeCapture == null)
        {
            return 2;
        }

        ExceptionDispatchInfo edi = ExceptionDispatchInfo.Capture(caught);

        // Capture must not replace the exception object.
        if (!ReferenceEquals(edi.SourceException, caught))
        {
            return 3;
        }

        // Capture freezes the trace; it must not clear or rewrite the source exception's own.
        string afterCapture = caught.StackTrace;

        if (afterCapture == null)
        {
            return 4;
        }

        if (!ContainsSubstring(afterCapture, "ExceptionDispatchInfoCapture.Thrower"))
        {
            return 5;
        }

        if (!SameString(beforeCapture, afterCapture))
        {
            return 6;
        }

        // Capturing an exception that has never been thrown is legal, and must leave it
        // looking unthrown. An implementation that fabricated a non-null frozen trace here,
        // or that refused a null one, would diverge.
        var neverThrown = new InvalidOperationException("unthrown");
        ExceptionDispatchInfo freshEdi = ExceptionDispatchInfo.Capture(neverThrown);

        if (!ReferenceEquals(freshEdi.SourceException, neverThrown))
        {
            return 7;
        }

        if (neverThrown.StackTrace != null)
        {
            return 8;
        }

        return 0;
    }
}
