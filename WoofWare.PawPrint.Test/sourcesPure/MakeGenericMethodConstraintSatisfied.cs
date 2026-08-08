using System;
using System.Reflection;

interface IMarker
{
}

class MyBase
{
}

class MyDerived : MyBase, IMarker
{
}

ref struct MyRefStruct
{
    public int X;
}

class Holder<T>
{
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

    static void MUnconstrained<T> ()
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

    static void MBoth<T> () where T : MyBase, IMarker
    {
    }

    static void MAllowsRef<T> () where T : allows ref struct
    {
    }

    static MethodInfo Get (string name) =>
        typeof (Program).GetMethod (name, BindingFlags.Static | BindingFlags.NonPublic);

    static int Main (string[] args)
    {
        // The mirror of MakeGenericMethodConstraintViolation.cs: arguments that *satisfy* the
        // constraint must bind rather than throw. Without this, an inverted condition in the
        // constraint check that `RuntimeMethodHandle_GetStubIfNeededSlow` performs (issue #743)
        // would be caught only by the violating cases, which an always-violating check would
        // still pass.

        if (Get ("MStruct").MakeGenericMethod (typeof (int)) == null)
            return 1;

        if (Get ("MClass").MakeGenericMethod (typeof (string)) == null)
            return 2;

        if (Get ("MNew").MakeGenericMethod (typeof (object)) == null)
            return 3;

        if (Get ("MUnconstrained").MakeGenericMethod (typeof (string)) == null)
            return 4;

        // A nullable value type satisfies `new()` but not `struct`; the sibling case asserts that
        // `MStruct` rejects the very same argument, so the pair pins that the non-nullable-value
        // constraint is checked as such rather than as a plain "is a value type".
        if (Get ("MNew").MakeGenericMethod (typeof (int?)) == null)
            return 5;

        // The general (base-type and interface) constraints, satisfied. `int` satisfies
        // `where T : IComparable` unboxed, and `where T : IComparable<T>` only once the constraint
        // has been instantiated with the supplied argument -- a check against the uninstantiated
        // IComparable<T> would reject it.
        if (Get ("MInterface").MakeGenericMethod (typeof (int)) == null)
            return 6;

        if (Get ("MSelfReferential").MakeGenericMethod (typeof (int)) == null)
            return 7;

        // The constraint type satisfies itself, and a derived type satisfies both constraints.
        if (Get ("MBase").MakeGenericMethod (typeof (MyBase)) == null)
            return 8;

        if (Get ("MBoth").MakeGenericMethod (typeof (MyDerived)) == null)
            return 9;

        // `allows ref struct` admits a byref-like argument, which the violation sibling pins as
        // refused without it.
        if (Get ("MAllowsRef").MakeGenericMethod (typeof (MyRefStruct)) == null)
            return 10;

        // A constraint naming the *declaring type's* parameter: for Holder<MyBase>, `where U : T`
        // is `where U : MyBase`, which MyDerived satisfies.
        MethodInfo derived = typeof (Holder<MyBase>).GetMethod ("MDerivedFromTypeParameter");

        if (derived.MakeGenericMethod (typeof (MyDerived)) == null)
            return 11;

        return 0;
    }
}
