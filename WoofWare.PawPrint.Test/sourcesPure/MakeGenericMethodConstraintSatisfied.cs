using System;
using System.Reflection;

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

    static MethodInfo Get (string name) =>
        typeof (Program).GetMethod (name, BindingFlags.Static | BindingFlags.NonPublic);

    static int Main (string[] args)
    {
        // The mirror of MakeGenericMethodConstraintViolation.cs: arguments that *satisfy* the
        // constraint must bind rather than throw. Without this, an inverted condition in the
        // constraint check that `RuntimeMethodHandle_GetStubIfNeededSlow` performs (issue #743)
        // would be caught only by the violating cases, which an always-violating check would
        // still pass.
        //
        // Parked in TestPureCases.unimplemented: binding *succeeds* here, and reflection then
        // continues into RuntimeType.GetMethodBase, which reaches the unimplemented
        // RuntimeMethodHandle.IsDynamicMethod InternalCall. That is an unrelated gap; when it
        // closes, this case should start passing and can be un-parked.

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

        return 0;
    }
}
