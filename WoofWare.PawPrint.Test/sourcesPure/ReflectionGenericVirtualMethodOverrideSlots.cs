using System;
using System.Reflection;

// Vtable slots for a *generic* virtual method that a derived type overrides. This is ordinary C#
// -- `class B : A` overriding `A`'s `virtual void M<T>(T)` -- and it exercises the one part of the
// slot matcher that cannot compare signatures the way CoreCLR does: `MetaSig::CompareMethodSigs`
// compares `ELEMENT_TYPE_MVAR` positionally, with no substitution, whereas PawPrint's matcher
// brings both signatures into a common form by concretising them, and there is no closed handle
// standing for "method generic parameter i".
//
// Every assertion below reads something `RuntimeType.RuntimeTypeCache.PopulateMethods` derives
// from slot numbers, so a matcher that bound the override to the wrong slot -- or to no slot --
// would be caught rather than merely producing a different failure:
//
//  - a `B.M<T>` that took a *new* slot instead of filling `A.M<T>`'s would leave both methods in
//    the enumeration, because the `overrides[methodSlot]` bitmap would not mark the base one as
//    superseded, so the "exactly one M" checks would see two.
//  - a `B.M<T>` bound to some *other* slot would report the wrong `DeclaringType`.
//  - `IsVirtual` comes from `slot < GetNumVirtuals(declaringType)`, so a slot number past the
//    vtable would report a virtual method as non-virtual.

public class Program
{
    private const BindingFlags Declared =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly;

    private const BindingFlags All = BindingFlags.Public | BindingFlags.Instance;

    private class Ga
    {
        public virtual void M<T> (T t)
        {
        }

        public virtual string Name () => "Ga";
    }

    private class Gb : Ga
    {
        public override void M<T> (T t)
        {
        }
    }

    private class Gc : Gb
    {
        public override string Name () => "Gc";
    }

    private static int CountNamed (Type t, string name)
    {
        int n = 0;
        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name)
            {
                n++;
            }
        }

        return n;
    }

    private static MethodInfo FirstNamed (Type t, string name)
    {
        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name)
            {
                return m;
            }
        }

        return null;
    }

    public static int Main ()
    {
        // Gb declares exactly one method: the override of M.
        MethodInfo[] declared = typeof (Gb).GetMethods (Declared);

        if (declared.Length != 1)
        {
            return 1;
        }

        if (declared[0].Name != "M")
        {
            return 2;
        }

        // The override supersedes the base declaration rather than taking a new slot.
        if (CountNamed (typeof (Gb), "M") != 1)
        {
            return 3;
        }

        if (FirstNamed (typeof (Gb), "M").DeclaringType != typeof (Gb))
        {
            return 4;
        }

        // Seen from the base itself, the same method is declared by the base.
        if (CountNamed (typeof (Ga), "M") != 1)
        {
            return 5;
        }

        if (FirstNamed (typeof (Ga), "M").DeclaringType != typeof (Ga))
        {
            return 6;
        }

        // One level further down, where M is inherited rather than declared, the most-derived
        // declaration is still Gb's.
        if (CountNamed (typeof (Gc), "M") != 1)
        {
            return 7;
        }

        if (FirstNamed (typeof (Gc), "M").DeclaringType != typeof (Gb))
        {
            return 8;
        }

        // A non-generic virtual on the same chain must keep working alongside it, so that a
        // failure here is specific to the generic method rather than to the walk as a whole.
        if (CountNamed (typeof (Gc), "Name") != 1)
        {
            return 9;
        }

        if (FirstNamed (typeof (Gc), "Name").DeclaringType != typeof (Gc))
        {
            return 10;
        }

        // IsVirtual is derived from the slot number, not read from metadata.
        if (!FirstNamed (typeof (Gb), "M").IsVirtual)
        {
            return 11;
        }

        if (!FirstNamed (typeof (Gc), "Name").IsVirtual)
        {
            return 12;
        }

        if (FirstNamed (typeof (Gb), "M").GetGenericArguments ().Length != 1)
        {
            return 13;
        }

        return 0;
    }
}
