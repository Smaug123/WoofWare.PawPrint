using System;
using System.Reflection;

// A generic virtual method hidden by `new virtual` and then overridden, which puts *two*
// identically-signed generic slots on the chain the override is matched against.
//
// Two rules meet here, and both would reject this ordinary C# if they were stated slightly wider:
//
//  - The constraint comparison CoreCLR runs on a generic override belongs to the slot it actually
//    fills. `B.M` adds `where T : class` relative to `A.M`, which it is free to do because `new
//    virtual` gives it a slot of its own; `C.M` overrides `B.M` and inherits that constraint.
//    Comparing `C.M` against *A*'s slot instead would see a constraint being added and refuse the
//    type, where CoreCLR selects the most-derived match first and only then compares.
//
//  - The guard against a tie between slots that only coincide once the declaring types' generic
//    arguments are substituted must not count generic *method* parameters. Signature comparison
//    matches those positionally and never substitutes them, so `M<T>(T)` on two different types
//    cannot be a substitution artifact -- but a guard that counted them would call this tie one.
//
// The assertions read slot-derived facts, so a match bound to the wrong slot shows up rather than
// merely producing a different failure: `A.M` and `C.M` occupy different slots and both stay
// visible, and `IsVirtual` is computed from the slot number.

public class Program
{
    private const BindingFlags All = BindingFlags.Public | BindingFlags.Instance;

    private class A
    {
        public virtual void M<T> (T t)
        {
        }
    }

    private class B : A
    {
        // Legal C#: `new virtual` introduces its own slot, so it may add a constraint.
        public new virtual void M<T> (T t)
            where T : class
        {
        }
    }

    private class C : B
    {
        public override void M<T> (T t)
        {
        }
    }

    private static Type[] DeclaringTypesNamed (Type t, string name)
    {
        int n = 0;

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name)
            {
                n++;
            }
        }

        Type[] result = new Type[n];
        int i = 0;

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name)
            {
                result[i] = m.DeclaringType;
                i++;
            }
        }

        return result;
    }

    private static bool Contains (Type[] types, Type wanted)
    {
        foreach (Type t in types)
        {
            if (t == wanted)
            {
                return true;
            }
        }

        return false;
    }

    public static int Main ()
    {
        // Seen from C: A's slot survives, and C's override supersedes B's declaration in the other.
        Type[] fromC = DeclaringTypesNamed (typeof (C), "M");

        if (fromC.Length != 2)
        {
            return 1;
        }

        if (!Contains (fromC, typeof (A)))
        {
            return 2;
        }

        if (!Contains (fromC, typeof (C)))
        {
            return 3;
        }

        if (Contains (fromC, typeof (B)))
        {
            return 4;
        }

        // Seen from B, where the override does not exist, the two declarations are A's and B's.
        Type[] fromB = DeclaringTypesNamed (typeof (B), "M");

        if (fromB.Length != 2)
        {
            return 5;
        }

        if (!Contains (fromB, typeof (A)))
        {
            return 6;
        }

        if (!Contains (fromB, typeof (B)))
        {
            return 7;
        }

        // Both are virtual, which is derived from the slot number rather than read from metadata.
        foreach (MethodInfo m in typeof (C).GetMethods (All))
        {
            if (m.Name == "M" && !m.IsVirtual)
            {
                return 8;
            }
        }

        // The constraint survived onto the override, so the comparison really did have one to
        // consider rather than finding two unconstrained parameters.
        foreach (MethodInfo m in typeof (C).GetMethods (All))
        {
            if (m.Name == "M" && m.DeclaringType == typeof (C))
            {
                Type parameter = m.GetGenericArguments ()[0];

                if ((parameter.GenericParameterAttributes & GenericParameterAttributes.ReferenceTypeConstraint) == 0)
                {
                    return 9;
                }
            }
        }

        return 0;
    }
}
