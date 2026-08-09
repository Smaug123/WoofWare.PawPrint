using System;

namespace TypeIsValueTypeIsEnumGenericParameter
{
    class Box<T> { }

    class StructBox<T> where T : struct { }

    class RefBox<T> where T : class { }

    class Pair<T, U> where T : struct where U : class { }

    class EnumBox<T> where T : Enum { }

    class ChainBox<T, U> where T : Enum where U : T { }

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

            // `where T : Enum` sets no constraint flag: it is an ordinary class constraint,
            // and CoreCLR answers from the parameter's *base type*, which for a type variable
            // is its most specific non-interface class constraint — `System.Enum` here.
            // `IsValueTypeImpl` is `IsSubclassOf(typeof(ValueType))`, which walks
            // T -> Enum -> ValueType; `IsEnum` is `IsSubclassOf(typeof(Enum))`, which matches
            // one step earlier, since `IsSubclassOf` compares against `BaseType` rather than
            // requiring a strict descendant. Both answers therefore come out of a base-type
            // walk that no reading of the constraint flags alone can produce.
            Type en = typeof(EnumBox<>).GetGenericArguments()[0];
            if (!en.IsValueType) return 11;
            if (!en.IsEnum) return 12;

            // The walk does not chain through another type parameter. A parameter whose only
            // constraint is `where U : T` has base type `System.Object`, not `T`, so it
            // inherits nothing from T's own `Enum` constraint and both answers are false.
            Type chained = typeof(ChainBox<,>).GetGenericArguments()[1];
            if (chained.IsValueType) return 13;
            if (chained.IsEnum) return 14;

            return 0;
        }
    }
}
