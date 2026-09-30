using System;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.CompilerServices;

public class Holder<T>
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static bool Capture(IntPtr expected) => Program.SomeFrameIs(expected);

    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static bool CaptureGeneric<U>(IntPtr expected) => Program.SomeFrameIs(expected);
}

public static class Plain
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    internal static bool CaptureGeneric<U>(IntPtr expected) => Program.SomeFrameIs(expected);
}

public static class Program
{
    const BindingFlags NonPublicStatic = BindingFlags.NonPublic | BindingFlags.Static;

    // `StackTrace.GetStackFramesInternal`, invoked by private reflection so that the frames' method
    // handles are read straight out of `StackFrameHelper.rgMethodHandle` rather than through
    // `StackFrameHelper.GetMethodBase`, which reduces each to its typical definition. CoreCLR reports
    // each frame's method with its own instantiation stripped and its declaring type as the code
    // that ran: the canonical form, which for `Holder<string>` would be `Holder<System.__Canon>`.
    // Every frame here runs code compiled for its declaring type alone, so the handle is exactly the
    // one reflection hands out for that type's method (measured on CoreCLR).
    internal static bool SomeFrameIs(IntPtr expected)
    {
        Assembly corelib = typeof(object).Assembly;
        Type helperType = corelib.GetType("System.Diagnostics.StackFrameHelper", true);
        MethodInfo capture = typeof(StackTrace).GetMethod(
            "GetStackFramesInternal", NonPublicStatic, null, new[] { helperType, typeof(bool), typeof(Exception) }, null);

        object helper = Activator.CreateInstance(helperType, nonPublic: true);
        capture.Invoke(null, new object[] { helper, false, null });
        IntPtr[] handles = (IntPtr[])helperType
            .GetField("rgMethodHandle", BindingFlags.NonPublic | BindingFlags.Instance)
            .GetValue(helper);

        foreach (IntPtr handle in handles)
        {
            if (handle == expected)
                return true;
        }

        return false;
    }

    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        if (!Holder<int>.Capture(typeof(Holder<int>).GetMethod("Capture", NonPublicStatic).MethodHandle.Value))
            return 1;

        // The method's own instantiation is stripped, even over a shareable argument, and the
        // declaring type is kept.
        if (!Holder<int>.CaptureGeneric<string>(
                typeof(Holder<int>).GetMethod("CaptureGeneric", NonPublicStatic).MethodHandle.Value))
            return 2;

        if (!Plain.CaptureGeneric<string>(typeof(Plain).GetMethod("CaptureGeneric", NonPublicStatic).MethodHandle.Value))
            return 3;

        return 0;
    }
}
