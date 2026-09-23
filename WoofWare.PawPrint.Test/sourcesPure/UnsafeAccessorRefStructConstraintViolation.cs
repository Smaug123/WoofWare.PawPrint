using System;
using System.Runtime.CompilerServices;

// An accessor whose own type parameter says `allows ref struct`, over a target parameter that does
// not, instantiated with a byref-like type. The *absence* of `allows ref struct` (`gpAllowByRefLike`)
// is itself a constraint, so the target refuses an argument the accessor accepts. CoreCLR raises it
// while instantiating the target for the stub (`MethodDesc::SatisfiesMethodConstraints`, genmeth.cpp).
// Measured on real .NET 10: `VerificationException` with the `COR_E_VERIFICATION` HResult and the
// message `IDS_EE_METHOD_CONSTRAINTS_VIOLATION` formats, which names the *target's* type parameter
// rather than the accessor's. `sourcesPure/UnsafeAccessorAllowsRefStruct.cs` holds the
// instantiations that bind.
public class TestUnsafeAccessorRefStructConstraintViolation
{
    private class Target
    {
        private static U Id<U>(U u) => u;

        private static int Two<U1, U2>(U1 a, U2 b) => 5;

        private static int Both<U1, U2>(U1 a, U2 b) => 6;
    }

    private interface IStaticAbstract
    {
        static abstract W Id<W>(W w);
    }

    private ref struct GenericRef<T>
    {
        public T Value;
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Id")]
    private static extern V Id<V>(Target t, V v)
        where V : allows ref struct;

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Two")]
    private static extern int Two<V1, V2>(Target t, V1 a, V2 b)
        where V2 : allows ref struct;

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Both")]
    private static extern int Both<V1, V2>(Target t, V1 a, V2 b)
        where V1 : allows ref struct
        where V2 : allows ref struct;

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Id")]
    private static extern V AbstractId<V>(IStaticAbstract t, V v)
        where V : allows ref struct;

    private static int Check(int code, string expectedMessage, Action a)
    {
        try
        {
            a();
            return code;
        }
        catch (System.Security.VerificationException e)
        {
            if (e.HResult != unchecked((int) 0x8013150D)) return code + 1;
            if (e.Message != expectedMessage) return code + 2;
        }

        return 0;
    }

    private static int Run()
    {
        int r;

        r = Check(
            10,
            "Method TestUnsafeAccessorRefStructConstraintViolation+Target.Id: type argument 'System.Span`1[System.Int32]' violates the constraint of type parameter 'U'.",
            () =>
            {
                Span<int> span = stackalloc int[1];
                span[0] = 7;
                Id<Span<int>>(null, span);
            });
        if (r != 0) return r;

        r = Check(
            20,
            "Method TestUnsafeAccessorRefStructConstraintViolation+Target.Id: type argument 'TestUnsafeAccessorRefStructConstraintViolation+GenericRef`1[System.String]' violates the constraint of type parameter 'U'.",
            () => Id<GenericRef<string>>(null, default));
        if (r != 0) return r;

        // Only the second parameter is violated, and the first is instantiated with a reference
        // type, so the accessor is compiled for a shared instantiation; the violation is still
        // reported, naming the parameter it concerns.
        r = Check(
            30,
            "Method TestUnsafeAccessorRefStructConstraintViolation+Target.Two: type argument 'System.Span`1[System.Int32]' violates the constraint of type parameter 'U2'.",
            () =>
            {
                Span<int> span = stackalloc int[1];
                Two<string, Span<int>>(null, "x", span);
            });
        if (r != 0) return r;

        // Both parameters are violated; the first in declaration order is the one reported.
        r = Check(
            40,
            "Method TestUnsafeAccessorRefStructConstraintViolation+Target.Both: type argument 'System.Span`1[System.Int32]' violates the constraint of type parameter 'U1'.",
            () =>
            {
                Span<int> first = stackalloc int[1];
                ReadOnlySpan<char> second = "x";
                Both<Span<int>, ReadOnlySpan<char>>(null, first, second);
            });
        if (r != 0) return r;

        // A `static abstract` target is also refused as a `call` to an abstract method
        // (`BadImageFormatException`), but only when the stub's IL is compiled; instantiating the
        // target comes first.
        r = Check(
            50,
            "Method TestUnsafeAccessorRefStructConstraintViolation+IStaticAbstract.Id: type argument 'System.ReadOnlySpan`1[System.Byte]' violates the constraint of type parameter 'W'.",
            () => AbstractId<ReadOnlySpan<byte>>(null, default));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
