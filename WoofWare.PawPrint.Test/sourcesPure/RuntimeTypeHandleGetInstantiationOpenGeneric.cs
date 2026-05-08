using System;

namespace RuntimeTypeHandleGetInstantiationOpenGeneric
{
    class Box<T> { }

    class Pair<T, U> { }

    class Program
    {
        static int Main(string[] args)
        {
            Type[] one = typeof(Box<>).GetGenericArguments();
            if (one == null) return 1;
            if (one.Length != 1) return 2;
            if (one[0] == null) return 3;

            Type[] two = typeof(Pair<,>).GetGenericArguments();
            if (two == null) return 4;
            if (two.Length != 2) return 5;
            if (two[0] == null) return 6;
            if (two[1] == null) return 7;

            // Each parameter slot must be a distinct RuntimeType instance: the two parameters
            // of Pair<,> have different positions, so they cannot share an allocation.
            if (object.ReferenceEquals(two[0], two[1])) return 8;

            // Generic-parameter RuntimeTypes must report IsGenericParameter = true. A closed
            // type's GetGenericArguments() entries (e.g. typeof(Box<int>).GetGenericArguments()[0])
            // are not parameters and must report false.
            if (!one[0].IsGenericParameter) return 9;
            if (!two[0].IsGenericParameter) return 10;
            if (!two[1].IsGenericParameter) return 11;
            if (typeof(Box<int>).GetGenericArguments()[0].IsGenericParameter) return 12;

            // Each generic-parameter RuntimeType must report its zero-based position within
            // the declaring type's parameter list. Pair<,>'s second parameter must report 1,
            // not 0, otherwise reflection callers like generic-constraint inspection would
            // collapse the two slots together.
            if (one[0].GenericParameterPosition != 0) return 13;
            if (two[0].GenericParameterPosition != 0) return 14;
            if (two[1].GenericParameterPosition != 1) return 15;

            // Name must come from metadata, not from the declaring type. CoreCLR's
            // RuntimeTypeHandle.ConstructName for a generic parameter emits only the
            // parameter name regardless of FormatNamespace / FormatAssembly.
            if (one[0].Name != "T") return 16;
            if (two[0].Name != "T") return 17;
            if (two[1].Name != "U") return 18;

            // IsGenericTypeParameter is the type-parameter-only refinement of
            // IsGenericParameter: it returns true iff IsGenericParameter is true and the
            // parameter's DeclaringMethod is null. Since these targets only ever model
            // type parameters, both must be true.
            if (!one[0].IsGenericTypeParameter) return 19;
            if (!two[0].IsGenericTypeParameter) return 20;
            if (!two[1].IsGenericTypeParameter) return 21;

            // Symmetrically, a type parameter must not be classified as a method-generic
            // parameter — IsGenericMethodParameter must be false.
            if (one[0].IsGenericMethodParameter) return 22;
            if (two[0].IsGenericMethodParameter) return 23;
            if (two[1].IsGenericMethodParameter) return 24;

            return 0;
        }
    }
}
