using System;
using System.Runtime.CompilerServices;

// An accessor's target can be a method the runtime services inline rather than by entering a frame
// -- `Object.GetType` and `Object.GetHashCode` are both JIT intrinsics. Such a target completes
// without a frame ever being pushed, so it is what moves the caller's program counter past the call
// site; the accessor's frame has no IL body and no program counter to move, which is why
// `Intrinsics.call` is told whether to move it.
//
// Nothing about the accessor is unusual here: `GetType` is a perfectly ordinary public method to
// name, and this is the shape that reaches the inline path.
public class TestUnsafeAccessorIntrinsicTarget
{
    private class Subject
    {
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetType")]
    private static extern Type GetTypeOf(object o);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetHashCode")]
    private static extern int HashOf(object o);

    private static int Run()
    {
        if (GetTypeOf("hi") != typeof(string)) return 1;

        Subject s = new Subject();
        if (GetTypeOf(s) != typeof(Subject)) return 2;

        // The accessor is reusable, so the frame really was returned rather than left behind.
        if (GetTypeOf(s) != typeof(Subject)) return 3;

        // A second intrinsic, whose result is a value rather than a reference.
        if (HashOf(s) != s.GetHashCode()) return 4;

        try
        {
            GetTypeOf(null);
            return 5;
        }
        catch (NullReferenceException) { }

        return 0;
    }

    public static int Main() => Run();
}
