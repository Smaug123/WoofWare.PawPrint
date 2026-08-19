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

    class Container<T>
    {
        public void NonGeneric()
        {
        }

        public void OwnGeneric<U>()
        {
        }
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
        MethodInfo plain = typeof (Program).GetMethod ("NotGeneric", BindingFlags.Static | BindingFlags.NonPublic);

        if (plain == null)
            return 3;

        if (plain.IsGenericMethodDefinition)
            return 4;

        // (c) The third arm: a non-generic method declared on a *generic* type. Its declaring type
        // has generic parameters and the method has none, so the predicate has to ask about the
        // method rather than about where it lives. Both spellings of the declaring type are asked,
        // because they reach the predicate by different routes -- a closed instantiation through
        // `RuntimeMethodHandle.GetStubIfNeeded`'s rebind, the definition through the method table
        // laid out on the definition itself.
        MethodInfo onOpen = typeof (Container<>).GetMethod ("NonGeneric");

        if (onOpen == null)
            return 5;

        if (onOpen.IsGenericMethodDefinition)
            return 6;

        MethodInfo onClosed = typeof (Container<int>).GetMethod ("NonGeneric");

        if (onClosed == null)
            return 7;

        if (onClosed.IsGenericMethodDefinition)
            return 8;

        // Vacuity guard: the declaring types really are generic, so checks 6 and 8 are not passing
        // merely because nothing generic is in play.
        if (!typeof (Container<>).IsGenericTypeDefinition)
            return 9;

        if (!typeof (Container<int>).IsConstructedGenericType)
            return 10;

        // And the same declaring types do host a generic method definition, so the predicate is
        // discriminating between the method's own parameters and its declaring type's.
        MethodInfo ownGeneric = typeof (Container<>).GetMethod ("OwnGeneric");

        if (ownGeneric == null)
            return 11;

        if (!ownGeneric.IsGenericMethodDefinition)
            return 12;

        return 0;
    }
}
