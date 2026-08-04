using System;
using System.Reflection;

class Program
{
    static void Generic<T>()
    {
    }

    static void NotGeneric()
    {
    }

    static int Main(string[] args)
    {
        // (a) A generic method definition: reflection always yields the open/uninstantiated
        // form of a method's own generic parameters, regardless of how it was looked up.
        MethodInfo genericDef = typeof (Program).GetMethod (
            "Generic",
            BindingFlags.Static | BindingFlags.NonPublic
        );

        if (genericDef == null)
            return 1;

        if (!genericDef.IsGenericMethodDefinition)
            return 2;

        // An ordinary non-generic method: must not be reported as a generic method definition.
        //
        // The third arm of the predicate -- a non-generic method declared on a *generic* type
        // (e.g. `class Container<T> { void NonGeneric() {} }`) -- is deliberately not exercised
        // here. Reflecting any method off a generic type currently fails for unrelated reasons
        // regardless of whether the type is open or closed: `Container<int>.GetMethod(...)`
        // reaches the unimplemented `RuntimeMethodHandle_GetStubIfNeededSlow` QCall (every method
        // lookup on a closed reference-type instantiation routes through
        // `RuntimeMethodHandle.GetStubIfNeeded`, whose fast path only short-circuits for
        // non-generic declaring types or the type's own open definition), while
        // `Container<>.GetMethod(...)` hits `TODO: RuntimeTypeHandle.GetNumVirtuals for open
        // generic type definition` inside `RuntimeType`'s candidate-gathering walk
        // (NativeRuntimeTypeQCall.fs). Both are pre-existing, unrelated gaps. The
        // "generic-declaring-type does not imply generic-method" arm of the predicate is instead
        // pinned directly against `NativeRuntimeMethodHandle.isGenericMethodDefinition` in
        // TestNativeRuntimeMethodHandle.fs.
        MethodInfo plain = typeof (Program).GetMethod ("NotGeneric", BindingFlags.Static | BindingFlags.NonPublic);

        if (plain == null)
            return 3;

        if (plain.IsGenericMethodDefinition)
            return 4;

        return 0;
    }
}
