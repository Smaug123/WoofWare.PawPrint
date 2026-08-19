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
        // Exercises the `RuntimeMethodHandle_GetMethodInstantiation` QCall, which materialises a
        // method's instantiation as a managed array handed back through an ObjectHandleOnStack.
        //
        // The declaring type is deliberately non-generic, so that the instantiation this reports is
        // unambiguously the *method's* own. A generic declaring type is reachable now (see
        // MethodIsGenericMethodDefinition.cs) and adds the question of which instantiation a handle
        // carries, which belongs with that file rather than here.
        MethodInfo genericDef = typeof (Program).GetMethod (
            "Generic",
            BindingFlags.Static | BindingFlags.NonPublic
        );

        if (genericDef == null)
            return 1;

        // `GetGenericArguments()` is `GetMethodInstantiationPublic(this) ?? Type.EmptyTypes`, i.e.
        // the Interop.BOOL.FALSE / `Type[]` arm of the QCall.
        //
        // For a generic method *definition* CoreCLR's `MethodDesc::LoadMethodInstantiation` reports
        // the method's own type variables rather than an empty instantiation, so this is `[T]`.
        Type[] arguments = genericDef.GetGenericArguments ();

        if (arguments.Length != 1)
            return 2;

        if (!arguments[0].IsGenericParameter)
            return 3;

        if (arguments[0].GenericParameterPosition != 0)
            return 4;

        // A non-generic method has an empty instantiation. CoreCLR's `CopyRuntimeTypeHandles`
        // returns NULL rather than a zero-length array for that case, so the QCall leaves the
        // caller's local null and the managed wrapper's `?? Type.EmptyTypes` supplies the empty
        // array; this pins that the QCall does not instead write a wrongly-shaped array.
        MethodInfo plain = typeof (Program).GetMethod ("NotGeneric", BindingFlags.Static | BindingFlags.NonPublic);

        if (plain == null)
            return 5;

        if (plain.GetGenericArguments ().Length != 0)
            return 6;

        // The third arm of the QCall -- a handle that *binds* concrete type arguments, which must
        // report those arguments rather than the method's type variables -- is not exercised here.
        // Producing such a handle through reflection requires `MakeGenericMethod`, which routes
        // through `RuntimeMethodHandle.GetStubIfNeeded`'s slow path and reaches the unimplemented
        // `RuntimeMethodHandle_GetStubIfNeededSlow` QCall. That arm is pinned directly against
        // `NativeRuntimeMethodHandle.methodInstantiationTargets` in TestNativeRuntimeMethodHandle.fs.

        return 0;
    }
}
