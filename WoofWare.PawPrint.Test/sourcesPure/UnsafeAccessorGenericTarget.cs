using System;
using System.Runtime.CompilerServices;

// How `[UnsafeAccessor]` behaves around generics, which is stricter than it looks. CoreCLR compares
// the declaration's signature blob against the candidate's with *no substitution on either side*
// (`pSubst1 = pSubst2 = NULL`, vm/unsafeaccessors.cpp:399 and :409), so a target spelling `!0`
// matches only a declaration spelling `!0` -- never one spelling the type that instantiates it.
//
// Consequences, all measured on real .NET 10:
//   * a generic *method* on a non-generic type is reachable, its `!!0` matching the accessor's own;
//   * a field of a generic type whose own type is concrete is reachable, spelling the same thing
//     on both sides;
//   * a field of a generic type whose type is `T` is not, `int32` being a different element type
//     from `!0`;
//   * a *method* of a generic type is never reachable from an accessor on a non-generic type: the
//     lookup can succeed, and then `VerifyDeclarationSatisfiesTargetConstraints` refuses it,
//     because the declaration supplies no class instantiation for the target's parameters.
public class TestUnsafeAccessorGenericTarget
{
    private class Plain
    {
        private T Echo<T>(T t) => t;

        private int Count<T>(T[] items) => items.Length;
    }

    private class Boxed<T>
    {
        private T _typed;
        private int _plain = 17;
        private int Get() => _plain;
    }

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Echo")]
    private static extern T Echo<T>(Plain p, T t);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Count")]
    private static extern int Count<T>(Plain p, T[] items);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_plain")]
    private static extern ref int ConcretelyTypedField(Boxed<int> b);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_typed")]
    private static extern ref int VariablyTypedField(Boxed<int> b);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int MethodOfGenericType(Boxed<int> b);

    public static int Main()
    {
        Plain p = new Plain();

        // A generic method on a non-generic type: `!!0` on both sides.
        if (Echo<int>(p, 5) != 5) return 1;
        if (Echo<string>(p, "hi") != "hi") return 2;
        if (Count<string>(p, new string[3]) != 3) return 3;

        Boxed<int> b = new Boxed<int>();

        // The field's own type is `int32` in the metadata, exactly as the accessor spells it.
        if (ConcretelyTypedField(b) != 17) return 4;
        ConcretelyTypedField(b) = 18;
        if (ConcretelyTypedField(b) != 18) return 5;

        // The field's type is `!0`; the accessor spells `int32`. Different element types.
        try
        {
            VariablyTypedField(b);
            return 6;
        }
        catch (MissingFieldException) { }

        // The lookup matches (`Get` takes nothing and returns `int32` on both sides), and the
        // constraint check then refuses: the accessor's declaring type has no type parameters to
        // supply for `Boxed<T>`'s.
        try
        {
            MethodOfGenericType(b);
            return 7;
        }
        catch (InvalidProgramException) { }

        return 0;
    }
}
