using System;
using System.Reflection;

// Matching a *generic* virtual override against the slot it fills is not settled by the signatures
// alone: CoreCLR compares the type parameters' constraints too, and refuses to load a type whose
// override demands more of a type argument than the method it overrides did
// (`MetaSig::CompareMethodConstraints`, siginfo.cpp:5108).
//
// C# forbids restating an override's constraints, but Roslyn copies them onto the override all the
// same, so every constrained generic override in ordinary C# reaches that comparison with both sides
// identical. A comparison that got the *common* case wrong would leave the override in a slot of its
// own — which is what these assertions read, exactly as
// `ReflectionGenericVirtualMethodOverrideSlots.cs` does for the unconstrained case.
//
// The three constraint kinds are covered separately because the rules governing them differ: a
// reference-type and a value-type constraint are compared for equality, whereas `new()` is also
// satisfied by the value-type constraint, and `allows ref struct` is compared in the opposite
// direction from the rest.

public class Program
{
    private const BindingFlags Declared =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly;

    private const BindingFlags All = BindingFlags.Public | BindingFlags.Instance;

    private class Base
    {
        public virtual void ReferenceConstrained<T> (T t)
            where T : class, IComparable<T>
        {
        }

        public virtual void ValueConstrained<T> (T t)
            where T : struct
        {
        }

        public virtual void NewConstrained<T> (T t)
            where T : new()
        {
        }

        public virtual void Unconstrained<T> (T t)
        {
        }
    }

    private class Derived : Base
    {
        public override void ReferenceConstrained<T> (T t)
        {
        }

        public override void ValueConstrained<T> (T t)
        {
        }

        public override void NewConstrained<T> (T t)
        {
        }

        public override void Unconstrained<T> (T t)
        {
        }
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
        // Each override supersedes its base declaration rather than taking a slot of its own; a
        // constraint comparison that wrongly rejected the match would leave two.
        string[] names =
        {
            "ReferenceConstrained",
            "ValueConstrained",
            "NewConstrained",
            "Unconstrained",
        };

        for (int i = 0; i < names.Length; i++)
        {
            if (CountNamed (typeof (Derived), names[i]) != 1)
            {
                return i + 1;
            }

            if (FirstNamed (typeof (Derived), names[i]).DeclaringType != typeof (Derived))
            {
                return i + 5;
            }

            // Derived from the slot number rather than read from metadata, so a method placed past
            // the end of the vtable reports itself non-virtual.
            if (!FirstNamed (typeof (Derived), names[i]).IsVirtual)
            {
                return i + 9;
            }
        }

        // The derived type declares exactly these four and nothing else, so a match that was
        // rejected would show up as an extra declared method rather than only as a count above.
        if (typeof (Derived).GetMethods (Declared).Length != names.Length)
        {
            return 13;
        }

        // The constraints survive onto the override, which is what makes the comparison reach them
        // at all rather than finding two unconstrained parameters.
        Type parameter = FirstNamed (typeof (Derived), "ValueConstrained").GetGenericArguments ()[0];

        if ((parameter.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0)
        {
            return 14;
        }

        return 0;
    }
}
