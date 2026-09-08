using System;
using System.Runtime.CompilerServices;

// A value type's accessor takes its receiver by ref, and a `ref` can be null too --
// `Unsafe.NullRef<S>()` is how a guest produces one. The synthesised `callvirt` and `ldflda` treat
// it exactly as they treat a null object reference.
//
// Measured on real .NET 10: every one of these raises `NullReferenceException` from the accessor,
// including the field at a non-zero offset whose address is only taken and never read. A null
// *object* reference is the other half of the same check and is covered by
// `sourcesPure/UnsafeAccessorFailures.cs` and `sourcesPure/UnsafeAccessorVirtualTarget.cs`.
public class TestUnsafeAccessorNullByrefReceiver
{
    private struct Holder
    {
        public int First;

        public int Second;

        private int Read() => First + 1;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Read")]
    private static extern int Read(ref Holder h);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "First")]
    private static extern ref int First(ref Holder h);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "Second")]
    private static extern ref int Second(ref Holder h);

    private static int Check<TExpected>(int code, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected)
        {
            return 0;
        }
    }

    private static int Run()
    {
        // The ordinary receiver still works, so the check is about the null and not about byrefs.
        Holder h = default;
        h.First = 4;
        h.Second = 9;

        if (Read(ref h) != 5) return 1;
        if (First(ref h) != 4) return 2;
        if (Second(ref h) != 9) return 3;

        int r;

        r = Check<NullReferenceException>(4, () => Read(ref Unsafe.NullRef<Holder>()));
        if (r != 0) return r;

        // The address is taken and never read, and the accessor still faults.
        r = Check<NullReferenceException>(5, () =>
        {
            ref int unused = ref First(ref Unsafe.NullRef<Holder>());
        });
        if (r != 0) return r;

        // ... at a non-zero offset too, where `null + offset` would not be null.
        r = Check<NullReferenceException>(6, () =>
        {
            ref int unused = ref Second(ref Unsafe.NullRef<Holder>());
        });
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
