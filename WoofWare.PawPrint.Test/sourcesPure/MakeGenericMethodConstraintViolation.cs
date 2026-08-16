using System;
using System.Reflection;

class NoParameterlessCtor
{
    private NoParameterlessCtor ()
    {
    }
}

class MyBase
{
}

ref struct MyRefStruct
{
    public int X;
}

class Holder<T>
{
    // A constraint on a *method* parameter may mention the declaring type's parameters, so the
    // check has to substitute both contexts before casting.
    public static void MDerivedFromTypeParameter<U> () where U : T
    {
    }
}

class Program
{
    static void MStruct<T> () where T : struct
    {
    }

    static void MClass<T> () where T : class
    {
    }

    static void MNew<T> () where T : new()
    {
    }

    static void MInterface<T> () where T : IComparable
    {
    }

    static void MBase<T> () where T : MyBase
    {
    }

    static void MSelfReferential<T> () where T : IComparable<T>
    {
    }

    static void MOrdinary<T> ()
    {
    }

    static MethodInfo Get (string name) =>
        typeof (Program).GetMethod (name, BindingFlags.Static | BindingFlags.NonPublic);

    static bool Throws (MethodInfo m, Type argument)
    {
        try
        {
            m.MakeGenericMethod (argument);
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }

    static int Main (string[] args)
    {
        // CoreCLR checks a generic method's constraints while *binding* the instantiation, inside
        // the RuntimeMethodHandle_GetStubIfNeededSlow QCall's callee. The managed
        // SanityCheckGenericArguments that runs before it only screens nulls, non-RuntimeType
        // arguments, and arity, so a constraint violation is not caught until then. Reflection
        // surfaces all three as ArgumentException.

        if (!Throws (Get ("MStruct"), typeof (string)))
            return 1;

        if (!Throws (Get ("MClass"), typeof (int)))
            return 2;

        if (!Throws (Get ("MNew"), typeof (NoParameterlessCtor)))
            return 3;

        // Nullable<T> does not satisfy `where T : struct`, which is the non-nullable-value
        // constraint rather than plain "is a value type".
        if (!Throws (Get ("MStruct"), typeof (int?)))
            return 4;

        // The general (base-type and interface) constraints from the GenericParamConstraint table
        // are checked for a method's parameters just as they are for a type's.
        if (!Throws (Get ("MInterface"), typeof (object)))
            return 5;

        if (!Throws (Get ("MBase"), typeof (string)))
            return 6;

        // `where T : IComparable<T>` is only satisfiable if the constraint is instantiated with the
        // supplied argument first; `object` does not satisfy it either way, but a check that
        // compared against the *uninstantiated* IComparable<T> would reject `int` too, which the
        // sibling MakeGenericMethodConstraintSatisfied.cs pins as accepted.
        if (!Throws (Get ("MSelfReferential"), typeof (object)))
            return 7;

        // A byref-like argument is refused unless the parameter carries `allows ref struct`.
        if (!Throws (Get ("MOrdinary"), typeof (MyRefStruct)))
            return 8;

        // A constraint on a method parameter that names the *declaring type's* parameter: for
        // Holder<MyBase>, `where U : T` is `where U : MyBase`.
        MethodInfo derived = typeof (Holder<MyBase>).GetMethod ("MDerivedFromTypeParameter");

        if (!Throws (derived, typeof (string)))
            return 9;

        // The *satisfying* mirror of each case above lives in
        // `sourcesPure/MakeGenericMethodConstraintSatisfied.cs`. The two polarities are kept in
        // separate files: an always-rejecting check would pass this file alone, and
        // an always-accepting one would pass that one alone.

        return 0;
    }
}
