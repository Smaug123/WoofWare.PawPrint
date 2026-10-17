using System;
using System.Runtime.CompilerServices;
using System.Runtime.ExceptionServices;

// A filter that raises the object it was offered again, from an `ExceptionDispatchInfo` capture of
// an earlier raise, changes the trace the interrupted raise continues: CoreCLR reads the exception's
// `_stackTrace` afresh at every frame it appends, so the interrupted raise carries on from the
// restored trace, and the frames of its own `throw` are gone.

public static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void First() { throw new InvalidOperationException("first"); }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Second(Exception e) { throw e; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool SpliceInFilter(ExceptionDispatchInfo edi, bool accept)
    {
        try { edi.Throw(); } catch { }
        return accept;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Exception Run(bool accept)
    {
        Exception raised = null;
        ExceptionDispatchInfo edi = null;
        try { First(); }
        catch (Exception e) { raised = e; edi = ExceptionDispatchInfo.Capture(e); }

        try
        {
            try { Second(raised); }
            catch (Exception e) when (SpliceInFilter(edi, accept)) { return e; }
        }
        catch (Exception e) { return e; }

        return null;
    }

    static bool Contains(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j]) { matches = false; break; }
            }
            if (matches) return true;
        }
        return false;
    }

    public static int Main(string[] args)
    {
        // Rejected: the raise searches on past the filter from the restored trace.
        string rejected = Run(false).StackTrace;
        if (rejected == null) return 1;
        if (!Contains(rejected, "First")) return 2;
        if (Contains(rejected, "Second")) return 3;

        // Accepted: the handler sees the trace the object holds after the filter.
        string accepted = Run(true).StackTrace;
        if (accepted == null) return 4;
        if (!Contains(accepted, "First")) return 5;
        if (Contains(accepted, "Second")) return 6;

        return 0;
    }
}
