using System;

namespace TypeIsValueTypeIsEnumGenericParameter
{
    class Box<T> { }

    class StructBox<T> where T : struct { }

    class RefBox<T> where T : class { }

    class Pair<T, U> where T : struct where U : class { }

    class Program
    {
        static int Main(string[] args)
        {
            // Unconstrained type-generic parameter: IsValueType=false, IsEnum=false.
            // CoreCLR consults the parameter's base type (System.Object for an
            // unconstrained variable), and Object is neither a value type nor an enum.
            Type unc = typeof(Box<>).GetGenericArguments()[0];
            if (unc.IsValueType) return 1;
            if (unc.IsEnum) return 2;

            // `where T : struct` sets the NotNullableValueTypeConstraint flag, which
            // CoreCLR reads to short-circuit IsValueType to true. IsEnum still returns
            // false because the constraint is "any non-nullable value type", not Enum.
            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            if (!vt.IsValueType) return 3;
            if (vt.IsEnum) return 4;

            // `where T : class` sets the ReferenceTypeConstraint flag. IsValueType is
            // forced false; IsEnum is also false because the parameter is a reference
            // type variable.
            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            if (rt.IsValueType) return 5;
            if (rt.IsEnum) return 6;

            // The flags are per-parameter, not per-declaring-type, so a generic with
            // mixed constraints reports each parameter independently.
            Type[] pairParams = typeof(Pair<,>).GetGenericArguments();
            if (!pairParams[0].IsValueType) return 7;
            if (pairParams[0].IsEnum) return 8;
            if (pairParams[1].IsValueType) return 9;
            if (pairParams[1].IsEnum) return 10;

            return 0;
        }
    }
}
