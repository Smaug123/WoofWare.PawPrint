using System;
using System.Reflection;

// The stack trace of a `TypeInitializationException` raised under `Activator.CreateInstance<T>()`.
//
// This route used to produce a TIE with a null `StackTrace`, because both of PawPrint's exception
// wraps fired on one frame: `Activator.CreateInstance<T>()` set `WrapExceptionInTargetInvocation`
// on the very frame that carried `WasInitialisingType`, so the freshly synthesised TIE was
// re-wrapped in a `TargetInvocationException` before a single frame could be appended to it.
//
// Initialising T in its constructor's own prologue separates them: `WasInitialisingType` belongs
// to the `.cctor` frame and `WrapExceptionInTargetInvocation` to the constructor frame beneath it,
// so the TIE is raised at one boundary and wrapped at the next, gaining the constructor frame in
// between. That frame is what this file asserts on.
//
// Asserts a substring rather than mere non-nullness, so an implementation that gave the TIE an
// empty or placeholder trace would still fail. The sibling `ActivatorCctorThrowsInnerStackTrace.cs`
// asserts the weaker property that both wrappers carry a readable, non-empty trace.
class ActivatorCctorTypeInitializationTrace
{
    class Boom
    {
        static Boom()
        {
            throw new InvalidOperationException("cctor boom");
        }

        public Boom() { }
    }

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

    static T Make<T>() where T : new() => new T();

    static int Main(string[] args)
    {
        try
        {
            Make<Boom>();
            return 1;
        }
        catch (TargetInvocationException tie)
        {
            Exception inner = tie.InnerException;

            if (!(inner is TypeInitializationException))
            {
                return 2;
            }

            string trace = inner.StackTrace;

            if (trace == null)
            {
                return 3;
            }

            // Real .NET reports the failing type's constructor frame here.
            if (!ContainsSubstring(trace, "Boom"))
            {
                return 4;
            }

            return 0;
        }
    }
}
