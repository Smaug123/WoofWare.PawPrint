using System;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.CompilerServices;

// Captures taken from inside shared generic code, `Holder<string>`, along each of CoreLib's own
// routes to the frame fill: `new StackFrame(int)` (`StackFrame.BuildStackFrame`), and
// `new StackTrace(Exception)` (`StackTrace.CaptureStackTrace` with an exception).
// `StackTraceGenericDeclaringFrame.cs` covers `new StackTrace()`.
//
// CoreCLR fills each frame with the canonical method that ran, here declared by
// `Holder<System.__Canon>`, and CoreLib reads it back only through `StackFrameHelper.GetMethodBase`,
// which reduces it to its typical definition. So the frame reports `Holder<T>.Capture`, exactly
// the method reflection over `typeof(Holder<>)` hands out.
class StackFrameSharedGenericFrame
{
    class Holder<T>
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        internal static StackFrame Capture()
        {
            return new StackFrame(0);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        internal static void Throw()
        {
            throw new InvalidOperationException();
        }
    }

    // Exit code is the index of the first failing check, so a failure names itself.
    static int Main(string[] args)
    {
        MethodInfo capture = typeof(Holder<>).GetMethod("Capture", BindingFlags.Static | BindingFlags.NonPublic);

        if (!ReferenceEquals(Holder<string>.Capture().GetMethod(), capture))
        {
            return 1;
        }

        Exception caught = null;

        try
        {
            Holder<string>.Throw();
        }
        catch (InvalidOperationException e)
        {
            caught = e;
        }

        MethodInfo throwMethod = typeof(Holder<>).GetMethod("Throw", BindingFlags.Static | BindingFlags.NonPublic);
        StackTrace fromException = new StackTrace(caught);

        if (fromException.FrameCount < 1)
        {
            return 2;
        }

        if (!ReferenceEquals(fromException.GetFrame(0).GetMethod(), throwMethod))
        {
            return 3;
        }

        return 0;
    }
}
