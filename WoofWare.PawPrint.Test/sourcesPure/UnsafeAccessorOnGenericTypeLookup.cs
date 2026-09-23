using System;
using System.Runtime.CompilerServices;

// What an accessor declared on a generic type binds, beyond the documented shape that
// `sourcesPure/UnsafeAccessorOnGenericType.cs` covers. CoreCLR compares the two signature blobs with
// no substitution on either side (vm/unsafeaccessors.cpp:401 and :408), so the accessor type's `!i`
// equals the target's `!i` by index alone -- whatever either is instantiated with -- and equals
// nothing else, the accessor's own `!!i` included. Then `VerifyDeclarationSatisfiesTargetConstraints`
// asks, of a *method* of a generic type only, that the accessor's type have as many type parameters
// as the target's. Every outcome here is measured on real .NET 10.
public class TestUnsafeAccessorOnGenericTypeLookup
{
    private class Boxed<T>
    {
        private T _typed;
        private int _plain = 17;

        private int GetPlain() => _plain;

        private int Both<V>(T t) => 1;

        private int TakesType<V>(T t) => 2;

        private int TakesMethod<V>(V v) => 3;
    }

    private class Plain
    {
        private X Echo<X>(X x) => x;

        private int TakesInt(int x) => x + 1;
    }

    private class Holder
    {
    }

    private struct S
    {
        private int x;
    }

    private static class A<T>
    {
        // `!0` against `!0` and `!!0` against `!!0`.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Both")]
        public static extern int Both<U>(Boxed<T> b, T t);

        // The accessor's `!!0` against the target's `!0`.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "TakesType")]
        public static extern int MethodAgainstType<U>(Boxed<T> b, U u);

        // The accessor's `!0` against the target's `!!0`.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "TakesMethod")]
        public static extern int TypeAgainstMethod<U>(Boxed<T> b, T t);

        // A non-generic target type: nothing to check the accessor type's parameter against, and
        // the accessor's `!0` is not the target method's `!!0`.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "TakesInt")]
        public static extern int TakesInt(Plain p, int x);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Echo")]
        public static extern T Echo(Plain p, T t);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuch")]
        public static extern int NoSuchMethod(Boxed<T> b);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "NoSuch")]
        public static extern ref int NoSuchField(Boxed<T> b);

        // A target type instantiated with something other than the accessor's own variable: the
        // signature mentions neither type's variable, and the parameter counts agree.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetPlain")]
        public static extern int GetPlainOfInt(Boxed<int> b);

        // `ref T` in the target position, which a reference-type `T` reads as `System.__Canon`.
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")]
        public static extern ref int ThroughRef(ref T t);

        // `__Canon` has no constructor to bind, even for a declaration that matches one.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = ".ctor")]
        public static extern void CanonConstructor(ref T t);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "NoSuch")]
        public static extern ref int ArrayField(T[] a);
    }

    private static class Two<T, U>
    {
        // A method of a one-parameter type from a two-parameter one: the lookup succeeds and the
        // constraint check refuses the count.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetPlain")]
        public static extern int GetPlain(Boxed<T> b);

        // A field is never constraint-checked.
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_plain")]
        public static extern ref int Plain(Boxed<T> b);

        // `!1` against the field's `!0`.
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_typed")]
        public static extern ref U TypedSecond(Boxed<T> b);

        // `!0` against the field's `!0`, though the target type is instantiated with `U`: bound by
        // position alone, and sound only because `T` and `U` are the same type below.
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_typed")]
        public static extern ref T TypedFirst(Boxed<U> b);
    }

    private static class NonGeneric
    {
        // The accessor's `!!0` against the target's `!0`, with no generic type on the accessor side.
        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "GetPlain")]
        public static extern int GenericMethodAgainstType<T>(Boxed<T> b);
    }

    private static int ExpectMissingMethod(int code, string message, Func<int> f)
    {
        try
        {
            f();
            return code;
        }
        catch (MissingMethodException e)
        {
            if (e.Message != message) return code + 1;
        }

        return 0;
    }

    private static int ExpectMissingField(int code, string message, Func<int> f)
    {
        try
        {
            f();
            return code;
        }
        catch (MissingFieldException e)
        {
            if (e.Message != message) return code + 1;
        }

        return 0;
    }

    private static int Run()
    {
        Boxed<int> ints = new Boxed<int>();
        Boxed<string> strings = new Boxed<string>();
        int r;

        if (A<int>.Both<string>(ints, 3) != 1) return 1;

        r = ExpectMissingMethod(10, "Method not found: 'Boxed`1.TakesType'.", () => A<int>.MethodAgainstType<string>(ints, "u"));
        if (r != 0) return r;

        r = ExpectMissingMethod(20, "Method not found: 'Boxed`1.TakesMethod'.", () => A<int>.TypeAgainstMethod<string>(ints, 3));
        if (r != 0) return r;

        if (A<int>.TakesInt(new Plain(), 3) != 4) return 30;

        r = ExpectMissingMethod(40, "Method not found: 'Plain.Echo'.", () => A<int>.Echo(new Plain(), 3));
        if (r != 0) return r;

        // The name reported is the definition's, whichever instantiation was searched.
        r = ExpectMissingMethod(50, "Method not found: 'Boxed`1.NoSuch'.", () => A<int>.NoSuchMethod(ints));
        if (r != 0) return r;
        r = ExpectMissingMethod(52, "Method not found: 'Boxed`1.NoSuch'.", () => A<string>.NoSuchMethod(strings));
        if (r != 0) return r;
        r = ExpectMissingField(54, "Field not found: 'Boxed`1.NoSuch'.", () => A<int>.NoSuchField(ints));
        if (r != 0) return r;
        r = ExpectMissingField(56, "Field not found: 'Boxed`1.NoSuch'.", () => A<string>.NoSuchField(strings));
        if (r != 0) return r;

        if (A<string>.GetPlainOfInt(ints) != 17) return 60;

        try
        {
            Two<int, string>.GetPlain(ints);
            return 70;
        }
        catch (InvalidProgramException e)
        {
            if (e.Message != "Generic type constraints do not match.") return 71;
        }

        if (Two<int, string>.Plain(ints) != 17) return 72;

        r = ExpectMissingField(74, "Field not found: 'Boxed`1._typed'.", () => Two<int, string>.TypedSecond(ints).Length);
        if (r != 0) return r;

        Two<int, int>.TypedFirst(ints) = 23;
        if (Two<int, int>.TypedFirst(ints) != 23) return 76;

        r = ExpectMissingMethod(80, "Method not found: 'Boxed`1.GetPlain'.", () => NonGeneric.GenericMethodAgainstType<int>(ints));
        if (r != 0) return r;

        r = ExpectMissingField(90, "Field not found: 'System.__Canon.x'.", () =>
        {
            string s = "a";
            return A<string>.ThroughRef(ref s);
        });
        if (r != 0) return r;

        r = ExpectMissingMethod(92, "Method not found: 'System.__Canon..ctor'.", () =>
        {
            Holder h = new Holder();
            A<Holder>.CanonConstructor(ref h);
            return 0;
        });
        if (r != 0) return r;

        S value = new S();
        A<S>.ThroughRef(ref value) = 5;
        if (A<S>.ThroughRef(ref value) != 5) return 94;

        r = ExpectMissingField(96, "Field not found: 'System.Int32[].NoSuch'.", () => A<int>.ArrayField(new int[1]));
        if (r != 0) return r;

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();
}
