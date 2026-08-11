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
// Both wrappers must carry a trace, and this asserts that of each in turn: the outer
// `TargetInvocationException` and the `TypeInitializationException` it holds. Real .NET gives the
// inner one `at Boom..ctor()` / `at System.RuntimeType.CreateInstanceOfT()`, and PawPrint — which
// inlines the `Activator` intrinsic and so has neither of those frames — gives it the call site
// that stands in for them. What the frames *say* therefore differs between the runtimes and is
// not asserted; that a synthesised wrapper is raised from somewhere, and can say where, holds on
// both.
//
// The named frame is `ActivatorCctorTypeInitializationTrace.cs`'s parked subject: real .NET's
// first frame is the constructor whose prologue triggered the initialisation, which PawPrint
// never pushes because it runs `loadClass` before the callee frame exists.
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

    class BoomCaughtInPlace
    {
        static BoomCaughtInPlace()
        {
            throw new InvalidOperationException("cctor boom, caught in place");
        }

        public BoomCaughtInPlace() { }
    }

    static T Make<T>() where T : new() => new T();

    // Handles the wrap in the very frame the wrap is raised from, so no frame boundary is
    // crossed afterwards and the wrapper's trace is exactly the frame it was seeded with. Under
    // `Make` + a handler in `Main` the wrapper picks up `Main` from ordinary propagation, which
    // hides whether it was seeded at all.
    static string MakeAndCatchHere<T>() where T : new()
    {
        try
        {
            T ignored = new T();
            return null;
        }
        catch (TargetInvocationException tie)
        {
            return tie.StackTrace;
        }
    }

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

            // A wrapper that was raised but given no frame reads back as null; one given an empty
            // frame list reads back as "". Both are wrong, and they fail differently in PawPrint,
            // so both are rejected here.
            if (trace == null || trace.Length == 0)
            {
                return 4;
            }

            if (!(inner.InnerException is InvalidOperationException))
            {
                return 5;
            }

            // The outer wrapper is raised at the same boundary and must equally have a frame.
            // Here it would have one either way, because it crosses back into `Main` before being
            // caught; `MakeAndCatchHere` below is the shape that can tell the difference.
            string outerTrace = tie.StackTrace;

            if (outerTrace == null || outerTrace.Length == 0)
            {
                return 6;
            }

            string caughtInPlace = MakeAndCatchHere<BoomCaughtInPlace>();

            if (caughtInPlace == null || caughtInPlace.Length == 0)
            {
                return 7;
            }

            return 0;
        }
    }
}
