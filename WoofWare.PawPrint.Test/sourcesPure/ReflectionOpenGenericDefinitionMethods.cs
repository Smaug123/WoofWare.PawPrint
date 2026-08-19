using System;
using System.Reflection;

public class ClosedBase
{
    public virtual int Describe(string value) => 1;

    public virtual int Named() => 2;

    public int Plain() => 3;
}

public class OpenDerived<T> : ClosedBase
{
    public override int Describe(string value) => 11;

    public virtual int Extra(T value) => 12;
}

public class GenericBase<T>
{
    public virtual int OnBase(T value) => 21;
}

public class OverridesClosedBase<T> : GenericBase<int>
{
    public override int OnBase(int value) => 31;
}

public static class Program
{
    // Enumerating the methods of an open generic type *definition*. Unlike GetConstructors
    // (sourcesPure/ReflectionOpenGenericConstructors.cs), PopulateMethods reads the definition's
    // method *table*: it calls RuntimeTypeHandle.GetNumVirtuals on each type in the base chain and
    // RuntimeMethodHandle.GetSlot on each virtual it meets, then dedupes by slot
    // (RuntimeType.CoreCLR.cs:640-716). So the layout has to be computed with the definition's own
    // type variables left symbolic; there is no instantiation to substitute.
    //
    // The dedupe is what makes that layout observable from a guest. Describe is declared twice in
    // metadata -- once on ClosedBase and once as OpenDerived<T>'s override -- and appears once in
    // GetMethods() only because both occupy the same slot. Plain is the control in the other
    // direction: nothing may dedupe a method that holds no vtable slot. PopulateMethods asks GetSlot
    // only for methods carrying MethodAttributes.Virtual, so Plain's own slot number is never read
    // here; the region of a definition's method table past its vtable has no guest observer yet,
    // because the caller that reads it without a Virtual guard is PopulateProperties, and a property
    // of an open definition stops earlier, in ModuleHandle.ResolveMethod.
    //
    // Both hierarchies here have a parent that mentions no type parameter. A parent that does --
    // `class D<T> : B<T>` -- is a separate gap in naming the parent at all, before any of this is
    // reached; sourcesPure/ReflectionOpenGenericDefinitionSharedParent.cs parks that shape.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo[] methods = typeof(OpenDerived<>).GetMethods();

        // Object contributes ToString, Equals, GetHashCode and GetType; OpenDerived<> contributes
        // Describe and Extra; ClosedBase contributes Named and Plain.
        if (methods.Length != 8) return 1;

        int describes = 0;
        int plains = 0;
        int nameds = 0;
        foreach (MethodInfo m in methods)
        {
            if (m.Name == "Describe") describes++;
            if (m.Name == "Plain") plains++;
            if (m.Name == "Named") nameds++;
        }

        // The override and the method it overrides share a slot, so exactly one survives.
        if (describes != 1) return 2;
        // A non-virtual inherited method is never deduped away: it holds no vtable slot at all.
        if (plains != 1) return 3;
        if (nameds != 1) return 4;

        // Which of the two Describe rows survived: the most-derived one, because the walk starts at
        // the reflected type. A layout that failed to match the override against the base slot
        // would report both, and check 2 would already have failed; this pins that the surviving
        // one is the override rather than the inherited declaration.
        MethodInfo describe = typeof(OpenDerived<>).GetMethod("Describe");
        if (describe.DeclaringType != typeof(OpenDerived<>)) return 5;

        // An inherited virtual that nothing overrides is reported by the type that declares it.
        if (typeof(OpenDerived<>).GetMethod("Named").DeclaringType != typeof(ClosedBase)) return 6;
        if (typeof(OpenDerived<>).GetMethod("Plain").DeclaringType != typeof(ClosedBase)) return 7;
        if (typeof(OpenDerived<>).GetMethod("ToString").DeclaringType != typeof(object)) return 8;

        if (!describe.IsVirtual) return 9;
        if (typeof(OpenDerived<>).GetMethod("Plain").IsVirtual) return 10;

        // DeclaredOnly asks the same layout question of one type in isolation.
        const BindingFlags declared = BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly;
        if (typeof(OpenDerived<>).GetMethods(declared).Length != 2) return 11;

        // A definition whose base is a *closed instantiation* of a generic type: the inherited
        // signature has to be read at the base's own arguments (`int`) rather than at the deriving
        // definition's parameter, or the override would not match the slot it takes.
        MethodInfo[] overriding = typeof(OverridesClosedBase<>).GetMethods();
        if (overriding.Length != 5) return 12;

        int onBases = 0;
        foreach (MethodInfo m in overriding)
        {
            if (m.Name == "OnBase") onBases++;
        }

        if (onBases != 1) return 13;
        if (typeof(OverridesClosedBase<>).GetMethod("OnBase").DeclaringType != typeof(OverridesClosedBase<>)) return 14;

        // The definition's methods are not the instantiation's: the MethodDef rows are shared, so
        // the declaring type is the only thing separating these handles.
        MethodInfo closedDescribe = typeof(OpenDerived<int>).GetMethod("Describe");
        if (closedDescribe.MetadataToken != describe.MetadataToken) return 15;
        if (closedDescribe.MethodHandle.Equals(describe.MethodHandle)) return 16;

        // Asking twice yields the same handle rather than minting a fresh one per query.
        if (!typeof(OpenDerived<>).GetMethod("Describe").MethodHandle.Equals(describe.MethodHandle)) return 17;

        return 0;
    }
}
