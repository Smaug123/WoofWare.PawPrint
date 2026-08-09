using System;
using System.Runtime.ExceptionServices;

// The `IsImmutableAgileException` InternalCall has no direct managed surface — it is a private
// static on `Exception` — so this exercises the three places CoreLib consults it, which is the
// only way a guest can observe it at all:
//
//   * `Exception.Data` (`CreateDataContainer`, Exception.CoreCLR.cs:47-53): a preallocated
//     exception gets an `EmptyReadOnlyDictionaryInternal`, everything else a mutable
//     `ListDictionaryInternal`. So a working `Data` round-trip is the observable.
//   * `CanSetRemoteStackTrace` (Exception.CoreCLR.cs:242-260), reached here through
//     `ExceptionDispatchInfo.SetRemoteStackTrace`, which is the one caller that does *not* also
//     need the `StackTrace` class — `SetCurrentStackTrace` builds a `new StackTrace(...)` and so
//     stays out of reach until `StackTrace_GetStackFramesInternal` lands.
//
// The preallocated set is exactly `OutOfMemoryException`, `StackOverflowException` and
// `ExecutionEngineException` (clrex.cpp:433), and only the runtime's own startup singletons are
// in it — a guest cannot construct one, and `new OutOfMemoryException()` is an ordinary object.
// So every case below is one where both runtimes must answer "not preallocated", and the
// assertions are cross-runtime facts rather than PawPrint-specific ones.
class ImmutableAgileExceptionEffects
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
        // Data must be the mutable container, not the empty read-only one.
        var withData = new InvalidOperationException("data");
        withData.Data["k"] = "v";

        if (!(withData.Data["k"] is string s) || s != "v")
        {
            return 2;
        }

        if (withData.Data.Count != 1)
        {
            return 3;
        }

        // An OutOfMemoryException the *guest* constructed is not the runtime's preallocated
        // singleton, so it too gets a mutable Data. This is the case that separates an identity
        // test from a type test: an implementation that answered "is this one of the three
        // preallocated *types*" rather than "is this one of the three preallocated *objects*"
        // passes everything above and fails here. Both wrong implementations were run against
        // this file and both fail it.
        var guestOom = new OutOfMemoryException("not the preallocated one");
        guestOom.Data["k"] = "v";

        if (guestOom.Data.Count != 1)
        {
            return 4;
        }

        // A never-thrown exception may have a remote trace stored into it.
        var fresh = new InvalidOperationException("fresh");
        ExceptionDispatchInfo.SetRemoteStackTrace(fresh, "MY REMOTE TRACE");

        string freshTrace = fresh.StackTrace;

        if (freshTrace == null)
        {
            return 5;
        }

        if (!ContainsSubstring(freshTrace, "MY REMOTE TRACE"))
        {
            return 6;
        }

        // Doing it twice is not allowed: the first call left a remote trace behind.
        try
        {
            ExceptionDispatchInfo.SetRemoteStackTrace(fresh, "AGAIN");
            return 7;
        }
        catch (InvalidOperationException)
        {
        }

        // Nor is it allowed on an exception that has already been thrown.
        Exception caught;

        try
        {
            Thrower();
            return 8;
        }
        catch (InvalidOperationException ex)
        {
            caught = ex;
        }

        try
        {
            ExceptionDispatchInfo.SetRemoteStackTrace(caught, "SHOULD NOT APPLY");
            return 9;
        }
        catch (InvalidOperationException)
        {
        }

        return 0;
    }
}
