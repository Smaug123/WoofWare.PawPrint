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
        private void Typed(T t) { }
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

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Typed")]
    private static extern void VariablyTypedParameter(Boxed<int> b, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_typed")]
    private static extern ref T VariablyTypedFieldGenerically<T>(Boxed<T> b);

    private static int Run()
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

        // `Typed` takes `!0` and the accessor spells `int32`, so nothing matches at all -- the
        // lookup fails before the constraint check the previous case reaches. A comparison that
        // substituted `Boxed<int>`'s instantiation into the candidate would match this and report
        // the other exception.
        try
        {
            VariablyTypedParameter(b, 1);
            return 8;
        }
        catch (MissingMethodException) { }

        // The accessor's own `!!0` is a different element type from the target's `!0`, so making
        // the accessor generic does not reach the variably-typed field either.
        try
        {
            VariablyTypedFieldGenerically<int>(b);
            return 9;
        }
        catch (MissingFieldException) { }

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();
}
