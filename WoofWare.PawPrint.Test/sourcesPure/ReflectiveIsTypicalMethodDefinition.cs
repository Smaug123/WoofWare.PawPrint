using System;
using System.Reflection;

// `RuntimeMethodHandle.IsTypicalMethodDefinition` reached the way a *guest* can reach it: by
// reflecting onto the private static FCall and invoking it.
//
// This matters because of what the argument then is. `StackFrameHelper.GetMethodBase`
// (StackFrameHelper.cs:148) — the BCL's only caller — allocates a `RuntimeMethodInfoStub` and passes
// that; a reflective caller instead passes whatever `Type.GetMethod` / `Type.GetConstructor` handed
// back, which is a `RuntimeMethodInfo` or a `RuntimeConstructorInfo`. All three implement
// `IRuntimeMethodInfo`, and they do not agree on which field holds the handle: the stub's `m_value`
// is a whole `RuntimeMethodHandleInternal`, while the other two keep its `IntPtr` payload in
// `m_handle` and rebuild the struct in the interface property. CoreCLR never notices, because its
// FCall reads the last field by *layout* — which is why `RuntimeMethodInfoStub` pads itself with
// eight unused reference fields to match `RuntimeMethodInfo`'s shape (RuntimeHandles.cs:931).
// PawPrint reads fields by name, so it must know all three.
//
// The shapes below are the ones the predicate distinguishes, and they vary its two halves
// independently: a handle is typical only when the method's own generic parameters are still the
// unbound formals *and* its declaring type's are too (method.cpp:1685).
class ReflectiveIsTypicalMethodDefinition
{
    class Holder<T>
    {
        internal static int Plain()
        {
            return 1;
        }
    }

    internal ReflectiveIsTypicalMethodDefinition()
    {
    }

    static int Plain()
    {
        return 1;
    }

    static T GenericMethod<T>(T t)
    {
        return t;
    }

    static MethodInfo fcall;

    static bool IsTypical(MethodBase method)
    {
        return (bool) fcall.Invoke(null, new object[] { method });
    }

    static int Main(string[] args)
    {
        fcall = typeof(RuntimeMethodHandle).GetMethod(
            "IsTypicalMethodDefinition",
            BindingFlags.NonPublic | BindingFlags.Static);

        if (fcall == null)
        {
            return 1;
        }

        BindingFlags anyStatic = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

        // A non-generic method on a non-generic type: both halves unbound, so typical.
        MethodInfo plain = typeof(ReflectiveIsTypicalMethodDefinition).GetMethod("Plain", anyStatic);

        if (plain == null || !IsTypical(plain))
        {
            return 2;
        }

        // A generic method *definition*: it has an instantiation, but that instantiation is its own
        // formals, so it is still typical. Reading the predicate as "has no generics" fails here.
        MethodInfo genericDefinition =
            typeof(ReflectiveIsTypicalMethodDefinition).GetMethod("GenericMethod", anyStatic);

        if (genericDefinition == null || !IsTypical(genericDefinition))
        {
            return 3;
        }

        // The same method with its type argument bound: no longer typical.
        MethodInfo bound = genericDefinition.MakeGenericMethod(typeof(int));

        if (IsTypical(bound))
        {
            return 4;
        }

        // A non-generic method whose *declaring type* is a closed generic: the class half alone
        // makes this untypical, and it is the half no other RuntimeMethodHandle predicate can see.
        MethodInfo onClosedGeneric = typeof(Holder<int>).GetMethod("Plain", anyStatic);

        if (onClosedGeneric == null || IsTypical(onClosedGeneric))
        {
            return 5;
        }

        // A constructor, which arrives as a RuntimeConstructorInfo rather than a RuntimeMethodInfo:
        // a third class, a third field to read the handle out of.
        ConstructorInfo ctor = typeof(ReflectiveIsTypicalMethodDefinition).GetConstructor(
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance,
            null,
            Type.EmptyTypes,
            null);

        if (ctor == null || !IsTypical(ctor))
        {
            return 6;
        }

        return 0;
    }
}
