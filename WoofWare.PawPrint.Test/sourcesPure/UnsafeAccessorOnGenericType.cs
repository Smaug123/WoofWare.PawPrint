using System.Runtime.CompilerServices;

// The documented way to reach a generic type's members: declare the accessor on a generic type of
// its own, so that the two spell their type variables at the same positions. CoreCLR compares the
// two signature blobs with no substitution on either side (vm/unsafeaccessors.cpp:399 and :409), so
// this positional agreement is the whole mechanism -- `sourcesPure/UnsafeAccessorGenericTarget.cs`
// pins the other side of it, that a *non*-generic accessor cannot reach the same members.
//
// A generic accessor *method* on a non-generic type does not work in its place: its `!!0` is a
// different element type from the target's `!0`, which that file also pins.
public class TestUnsafeAccessorOnGenericType
{
    private class Boxed<T>
    {
        private T _typed;
        private static T _shared;

        private Boxed(T seed)
        {
            _typed = seed;
        }

        private T Get() => _typed;

        private U Convert<U>(U u) => u;
    }

    private static class Accessors<T>
    {
        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern Boxed<T> New(T seed);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
        public static extern T Get(Boxed<T> b);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_typed")]
        public static extern ref T Typed(Boxed<T> b);

        [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_shared")]
        public static extern ref T Shared(Boxed<T> b);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Convert")]
        public static extern U Convert<U>(Boxed<T> b, U u);
    }

    public static int Main()
    {
        Boxed<int> ints = Accessors<int>.New(4);
        if (Accessors<int>.Get(ints) != 4) return 1;

        Accessors<int>.Typed(ints) = 9;
        if (Accessors<int>.Get(ints) != 9) return 2;

        // A different instantiation is a different type with its own static.
        if (Accessors<int>.Shared(null) != 0) return 3;
        Accessors<int>.Shared(null) = 6;
        if (Accessors<int>.Shared(null) != 6) return 4;

        Boxed<string> strings = Accessors<string>.New("hi");
        if (Accessors<string>.Get(strings) != "hi") return 5;
        if (Accessors<string>.Shared(null) != null) return 6;
        if (Accessors<int>.Shared(null) != 6) return 7;

        // A generic method on a generic type: the accessor's own `!!0` against the target's.
        if (Accessors<int>.Convert<string>(ints, "x") != "x") return 8;

        return 0;
    }
}
