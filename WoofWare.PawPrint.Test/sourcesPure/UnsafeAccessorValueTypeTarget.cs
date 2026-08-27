using System.Runtime.CompilerServices;

// An instance member of a *value type* must be reached through a byref: CoreCLR refuses a
// by-value first argument as `BadImageFormatException` (vm/unsafeaccessors.cpp:1111 and :1134),
// because the accessor would otherwise operate on a copy. This checks the accepted shape actually
// mutates the caller's struct rather than a copy, for both the method and the field kind.
public class TestUnsafeAccessorValueTypeTarget
{
    private struct Counter
    {
        private int _count;
        private long _tag;

        private Counter(int count, long tag)
        {
            _count = count;
            _tag = tag;
        }

        private int Bump(int by)
        {
            _count += by;
            return _count;
        }

        private static int Scale = 2;

        private static int Scaled(int x) => x * Scale;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Counter NewCounter(int count, long tag);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Bump")]
    private static extern int Bump(ref Counter c, int by);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_count")]
    private static extern ref int Count(ref Counter c);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_tag")]
    private static extern ref long Tag(ref Counter c);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "Scale")]
    private static extern ref int Scale(Counter c);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Scaled")]
    private static extern int Scaled(Counter c, int x);

    private static int Run()
    {
        // A value type's constructor through the Constructor kind: the accessor returns the value.
        Counter c = NewCounter(5, 900L);
        if (Count(ref c) != 5) return 1;
        if (Tag(ref c) != 900L) return 2;

        // The instance method sees, and mutates, the caller's own storage.
        if (Bump(ref c, 3) != 8) return 3;
        if (Count(ref c) != 8) return 4;

        // Writing through the field byref is likewise visible to the target's own code.
        Count(ref c) = 40;
        if (Bump(ref c, 2) != 42) return 5;

        // The other field is untouched by all of that.
        if (Tag(ref c) != 900L) return 6;

        // A copy is a separate value: mutating it must not reach the original.
        Counter copy = c;
        Count(ref copy) = -1;
        if (Count(ref c) != 42) return 7;
        if (Count(ref copy) != -1) return 8;

        // Static members of a value type take the first argument by value, since there is no
        // receiver for it to be.
        if (Scaled(default, 6) != 12) return 9;
        Scale(default) = 10;
        if (Scaled(default, 6) != 60) return 10;

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();
}
