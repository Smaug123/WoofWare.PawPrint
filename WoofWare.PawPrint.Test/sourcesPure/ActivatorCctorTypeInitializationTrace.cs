using System;
using System.Reflection;

// The stack trace of a `TypeInitializationException` raised under `Activator.CreateInstance<T>()`.
//
// A cctor failure reached by plain `newobj` gives its TIE a trace in PawPrint, because the TIE
// propagates through frames and accumulates them. This route does not: `Activator.CreateInstance<T>()`
// sets `WrapExceptionInTargetInvocation` on the same frame that carries `WasInitialisingType`, so
// the freshly synthesised TIE is re-wrapped in a `TargetInvocationException` before a single frame
// is appended to it, and surfaces as `InnerException` with a null `StackTrace`.
//
// Asserts a substring rather than mere non-nullness, so an implementation that gave the TIE an
// empty or placeholder trace would still fail. The sibling `ActivatorCctorThrowsInnerStackTrace.cs`
// covers what does hold today — that the read is reachable and does not throw — and passes.
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
