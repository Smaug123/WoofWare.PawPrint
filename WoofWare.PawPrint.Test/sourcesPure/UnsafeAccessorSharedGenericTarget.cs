using System;
using System.Runtime.CompilerServices;

// A generic method over a reference type is compiled once, for `System.__Canon`, so a `ref T`
// target position resolves against that shared instantiation rather than the exact class. `__Canon`
// declares no fields and no generic methods, so the lookup finds nothing however real the named
// member is on the actual argument -- the accessor is not a way to reach a class's private member
// generically.
//
// Which argument is shared is `ClassLoader::CanonicalizeGenericArg`'s decision: every reference
// type (a class, an interface, `string`, an array) becomes `__Canon`, and a value type does not, not
// even a generic struct instantiated over a reference type -- that becomes `GS<__Canon>`, which is
// still `GS`1` with the fields `GS`1` declares. Measured on real .NET 10.
public class TestUnsafeAccessorSharedGenericTarget
{
    private interface IMarker
    {
    }

    private class RefType : IMarker
    {
        private int x;

        private static int sx;

        public int Peek() => x;

        private static int StaticPeek() => sx;
    }

    private struct ValType
    {
        private int x;

        public int Peek() => x;
    }

    private struct GenericStruct<T>
    {
        private int x;

        public int Peek() => x;
    }

    private class Box<T>
    {
        private int x;

        public int Peek() => x;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int Field<T>(ref T t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Peek")]
    private static extern int Peek<T>(ref T t);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "sx")]
    private static extern ref int StaticField<T>(ref T t);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "StaticPeek")]
    private static extern int StaticPeek<T>(ref T t);

    // The type argument is shared, but the target is `Box<__Canon>`, which is `Box`1` and declares
    // `x`: sharing only hides members when the *whole* target is the type parameter.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int BoxField<T>(Box<T> b);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "NoSuch")]
    private static extern ref int BoxNoSuch<T>(Box<T> b);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
    private static extern ref int DirectlyOnArray(int[] a);

    private const int MissingMethod = unchecked((int) 0x80131513);
    private const int MissingField = unchecked((int) 0x80131511);

    private static int Check<TExpected>(int code, string expectedMessage, int expectedHResult, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected e)
        {
            if (e.Message != expectedMessage) return code + 100;
            if (e.HResult != expectedHResult) return code + 200;
            return 0;
        }
    }

    private static int Run()
    {
        int r;

        // Each value-type instantiation is its own type, so the field is reached.
        ValType v = default;
        Field<ValType>(ref v) = 4;
        if (v.Peek() != 4) return 1;

        // A generic struct over a reference type is shared as `GenericStruct<__Canon>`, which still
        // declares `x`.
        GenericStruct<string> g = default;
        Field<GenericStruct<string>>(ref g) = 5;
        if (g.Peek() != 5) return 2;

        r = Check<MissingFieldException>(
            10,
            "Field not found: 'System.__Canon.x'.",
            MissingField,
            () =>
            {
                RefType rt = new RefType();
                Field<RefType>(ref rt) = 4;
            });
        if (r != 0) return r;

        // The method kind shares the target position, so it shares the answer.
        r = Check<MissingMethodException>(
            20,
            "Method not found: 'System.__Canon.Peek'.",
            MissingMethod,
            () =>
            {
                RefType rt = new RefType();
                Peek<RefType>(ref rt);
            });
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            30,
            "Field not found: 'System.__Canon.sx'.",
            MissingField,
            () =>
            {
                RefType rt = null;
                StaticField<RefType>(ref rt);
            });
        if (r != 0) return r;

        r = Check<MissingMethodException>(
            40,
            "Method not found: 'System.__Canon.StaticPeek'.",
            MissingMethod,
            () =>
            {
                RefType rt = null;
                StaticPeek<RefType>(ref rt);
            });
        if (r != 0) return r;

        // Every kind of reference type is shared alike.
        r = Check<MissingFieldException>(
            50,
            "Field not found: 'System.__Canon.x'.",
            MissingField,
            () =>
            {
                IMarker m = new RefType();
                Field<IMarker>(ref m);
            });
        if (r != 0) return r;

        r = Check<MissingFieldException>(
            60,
            "Field not found: 'System.__Canon.x'.",
            MissingField,
            () =>
            {
                string s = "s";
                Field<string>(ref s);
            });
        if (r != 0) return r;

        // An array argument is a reference type too, so it canonicalises like any other -- and the
        // reported name is what says so: the *same* accessor named directly on `int[]` reports
        // `System.Int32[].x` instead, because nothing is shared there.
        r = Check<MissingFieldException>(
            70,
            "Field not found: 'System.__Canon.x'.",
            MissingField,
            () =>
            {
                int[] a = new int[1];
                Field<int[]>(ref a);
            });
        if (r != 0) return r;

        r = Check<MissingFieldException>(80, "Field not found: 'System.Int32[].x'.", MissingField, () => DirectlyOnArray(new int[1]));
        if (r != 0) return r;

        // A shared type argument inside the target leaves the target's own definition in place.
        Box<string> box = new Box<string>();
        BoxField<string>(box) = 6;
        if (box.Peek() != 6) return 90;

        r = Check<MissingFieldException>(100, "Field not found: 'Box`1.NoSuch'.", MissingField, () => BoxNoSuch<string>(new Box<string>()));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
