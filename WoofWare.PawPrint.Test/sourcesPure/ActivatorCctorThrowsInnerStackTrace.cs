using System;
using System.Reflection;

// A `.cctor` that throws underneath `Activator.CreateInstance<T>()` hits *both* of PawPrint's
// exception wraps on one unwind: `WasInitialisingType` replaces the exception with a synthesised
// `TypeInitializationException` as the `.cctor` frame is left, and
// `WrapExceptionInTargetInvocation` wraps that in a `TargetInvocationException` as T's constructor
// frame beneath it is left in turn.
//
// That makes it the regression test for the frozen-stack-trace token: a token minted over an
// empty frame list would leave a wrapper with a non-null `_stackTrace` and a null
// `_stackTraceString`, which sends `Exception.StackTrace` past its short-circuit into
// `GetStackTrace()` and crashes at the unimplemented `StackTrace_GetStackFramesInternal` QCall.
// Both wraps used to fire on the *same* frame, because T was initialised before its constructor
// frame existed, and the inner one was then handed to the outer with no frame appended at all.
//
// Both wrappers must carry a trace, and this asserts that of each in turn: the outer
// `TargetInvocationException` and the `TypeInitializationException` it holds. Real .NET gives the
// inner one `at Boom..ctor()` / `at System.RuntimeType.CreateInstanceOfT()`; PawPrint inlines the
// `Activator` intrinsic and so has no `CreateInstanceOfT` frame of its own. What the frames *say*
// therefore still differs between the runtimes and is not asserted here —
// `ActivatorCctorTypeInitializationTrace.cs` is where the constructor frame is asserted by name.
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
