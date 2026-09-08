using System;
using System.Runtime.CompilerServices;

// The target type is read from the *outermost* element of the first parameter's signature, and only
// a bare generic parameter there is refused: `ValidateTargetType` peeks the element type, so a
// `ref T` is a BYREF and passes, and the byref is stripped after instantiation.
//
// Measured on real .NET 10: `ref int X<T>(ref T target)` reaches a struct `T`'s field, while
// `void Call<T>(T t, int x)` over a bare `T` raises `BadImageFormatException`
// (`sourcesPure/UnsafeAccessorFailures.cs` pins that half).
//
// An array target is the other shape this position can take, and is parked --
// `sourcesPure/UnsafeAccessorArrayConstructor.cs`.
public class TestUnsafeAccessorByrefGenericTarget
{
    private struct Boxy
    {
        private int x;

        public int Peek() => x;
    }

    private struct Other
    {
        private int x;

        public int Peek() => x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int X<T>(ref T target);

    private static int Run()
    {
        Boxy b = default;

        if (X<Boxy>(ref b) != 0) return 1;

        X<Boxy>(ref b) = 7;
        if (b.Peek() != 7) return 2;

        // The same accessor at a different instantiation reaches the other struct's own field.
        Other o = default;
        X<Other>(ref o) = 11;
        if (o.Peek() != 11) return 3;
        if (b.Peek() != 7) return 4;

        return 0;
    }

    public static int Main() => Run();
}
