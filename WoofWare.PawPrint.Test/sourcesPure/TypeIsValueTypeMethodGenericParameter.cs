using System;

namespace TypeIsValueTypeMethodGenericParameter
{
    class Program
    {
        public static void Unconstrained<T>() { }

        public static void Struct<T>() where T : struct { }

        public static void Class<T>() where T : class { }

        public static void EnumConstrained<T>() where T : Enum { }

        static Type Param(string name) =>
            typeof(Program).GetMethod(name).GetGenericArguments()[0];

        static int Main(string[] args)
        {
            // The method-level counterpart of TypeIsValueTypeIsEnumGenericParameter.cs.
            // CoreCLR answers identically for MVAR and VAR: `RuntimeType.IsValueTypeImpl`
            // sees `IsTypeDesc` either way and falls back to `IsSubclassOf(typeof(ValueType))`,
            // which consults the parameter's base type — the most specific non-interface
            // class constraint, or System.Object when there is none.
            //
            // The four cases are chosen so that no constant answer passes: an implementation
            // that always says false fails at check 2, and one that always says true fails
            // at check 1. An implementation reading only the NotNullableValueType/Reference
            // constraint *flags* and defaulting the rest to false passes 1, 2 and 3 but
            // fails at check 4, since `where T : Enum` sets no flag and needs the base-type
            // walk.
            if (Param("Unconstrained").IsValueType) return 1;
            if (!Param("Struct").IsValueType) return 2;
            if (Param("Class").IsValueType) return 3;
            if (!Param("EnumConstrained").IsValueType) return 4;

            // `IsEnum` is `IsSubclassOf(typeof(Enum))` over the same base type, and
            // `IsSubclassOf` matches at `BaseType`, so the Enum-constrained parameter is an
            // enum as well as a value type. The struct-constrained one stops at ValueType.
            if (!Param("EnumConstrained").IsEnum) return 5;
            if (Param("Struct").IsEnum) return 6;

            return 0;
        }
    }
}
