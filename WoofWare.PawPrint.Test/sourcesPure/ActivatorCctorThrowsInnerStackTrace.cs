using System;
using System.Reflection;

// A `.cctor` that throws underneath `Activator.CreateInstance<T>()` is the one path that hits
// *both* of PawPrint's exception wraps on a single frame: `WasInitialisingType` replaces the
// exception with a synthesised `TypeInitializationException`, and `WrapExceptionInTargetInvocation`
// then wraps that in a `TargetInvocationException`. The synthesised TIE is handed to the second
// wrap with an empty frame list, before any propagation frame has been appended to it.
//
// That makes it the regression test for the frozen-stack-trace token: a token minted over an
// empty frame list would leave the TIE with a non-null `_stackTrace` and a null
// `_stackTraceString`, which sends `Exception.StackTrace` past its short-circuit into
// `GetStackTrace()` and crashes at the unimplemented `StackTrace_GetStackFramesInternal` QCall.
//
// Deliberately asserts only that the trace is *readable*, not what it contains. Real .NET
// populates it (`at Boom..ctor()` / `at System.RuntimeType.CreateInstanceOfT()`) and PawPrint
// leaves it null, because PawPrint does not give its synthesised wrappers propagation frames —
// issue #865. Both runtimes must nonetheless get here and answer without throwing.
class ActivatorCctorThrowsInnerStackTrace
{
    class Boom
    {
        static Boom()
        {
            throw new InvalidOperationException("cctor boom");
        }

        public Boom() { }
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

            if (inner == null)
            {
                return 2;
            }

            if (!(inner is TypeInitializationException))
            {
                return 3;
            }

            // The crash under test happens here, on the read itself.
            string trace = inner.StackTrace;

            // Null is PawPrint's answer and non-null is the real runtime's; either is fine, and
            // reading `Length` on the non-null branch keeps the read from being optimised away.
            if (trace != null && trace.Length == 0)
            {
                return 4;
            }

            if (!(inner.InnerException is InvalidOperationException))
            {
                return 5;
            }

            return 0;
        }
    }
}
