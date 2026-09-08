using System.Runtime.CompilerServices;

// An accessor's signature is compared against the target's with custom modifiers ignored: CoreCLR
// sets `MetaSig::CompareState.IgnoreCustomModifiers` always for a field lookup
// (vm/unsafeaccessors.cpp:750) and on the first pass for a method (:587). Ordinary C# produces
// `modreq`s in all four positions below, and none of them is spelled on the accessor:
//
//   * `volatile int _v`      -> `int32 modreq(IsVolatile)`
//   * an `init` accessor     -> `void modreq(IsExternalInit)` return
//   * `ref readonly int`     -> `int32& modreq(InAttribute)` return
//   * `in int x`             -> `int32&` parameter, with whatever modifier the compiler attaches
//
// A comparison that took modifiers into account would report `MissingFieldException` and
// `MissingMethodException` for these, which is what real .NET does not do.
public class TestUnsafeAccessorCustomModifiers
{
    private class Target
    {
        private volatile int _volatile = 3;

        private int _plain = 4;

        private int Prop
        {
            get;
            init;
        }

        private ref readonly int RefReadonly() => ref _plain;

        private int InParameter(in int x) => x + 1;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_volatile")]
    private static extern ref int VolatileField(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "set_Prop")]
    private static extern void SetProp(Target t, int value);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "get_Prop")]
    private static extern int GetProp(Target t);

    // Deliberately spelled `ref int`, not `ref readonly int`: the modifier on the target's return
    // must not be what decides the match.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "RefReadonly")]
    private static extern ref int RefReadonly(Target t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "InParameter")]
    private static extern int InParameter(Target t, in int x);

    private static int Run()
    {
        Target t = new Target();

        if (VolatileField(t) != 3) return 1;
        VolatileField(t) = 30;
        if (VolatileField(t) != 30) return 2;

        SetProp(t, 7);
        if (GetProp(t) != 7) return 3;

        if (RefReadonly(t) != 4) return 4;
        RefReadonly(t) = 40;
        if (RefReadonly(t) != 40) return 5;

        int arg = 5;
        if (InParameter(t, in arg) != 6) return 6;

        return 0;
    }

    public static int Main() => Run();
}
