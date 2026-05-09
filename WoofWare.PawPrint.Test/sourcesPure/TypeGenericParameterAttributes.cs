using System;
using System.Reflection;

namespace TypeGenericParameterAttributes
{
    class Box<T> { }
    class StructBox<T> where T : struct { }
    class RefBox<T> where T : class { }
    class NewBox<T> where T : new() { }

    interface ICov<out T> { }
    interface IContra<in T> { }

    class Program
    {
        static int Main(string[] args)
        {
            // Unconstrained: None (0)
            GenericParameterAttributes unc =
                typeof(Box<>).GetGenericArguments()[0].GenericParameterAttributes;
            if (unc != GenericParameterAttributes.None) return 1;

            // where T : struct -> NotNullableValueTypeConstraint | DefaultConstructorConstraint
            GenericParameterAttributes st =
                typeof(StructBox<>).GetGenericArguments()[0].GenericParameterAttributes;
            if ((st & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0) return 2;
            if ((st & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 3;

            // where T : class -> ReferenceTypeConstraint
            GenericParameterAttributes rf =
                typeof(RefBox<>).GetGenericArguments()[0].GenericParameterAttributes;
            if ((rf & GenericParameterAttributes.ReferenceTypeConstraint) == 0) return 4;

            // where T : new() -> DefaultConstructorConstraint only (no struct/class flag)
            GenericParameterAttributes nw =
                typeof(NewBox<>).GetGenericArguments()[0].GenericParameterAttributes;
            if ((nw & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 5;
            if ((nw & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0) return 6;
            if ((nw & GenericParameterAttributes.ReferenceTypeConstraint) != 0) return 7;

            // out T (covariant)
            GenericParameterAttributes co =
                typeof(ICov<>).GetGenericArguments()[0].GenericParameterAttributes;
            if ((co & GenericParameterAttributes.Covariant) == 0) return 8;
            if ((co & GenericParameterAttributes.Contravariant) != 0) return 9;

            // in T (contravariant)
            GenericParameterAttributes contra =
                typeof(IContra<>).GetGenericArguments()[0].GenericParameterAttributes;
            if ((contra & GenericParameterAttributes.Contravariant) == 0) return 10;
            if ((contra & GenericParameterAttributes.Covariant) != 0) return 11;

            return 0;
        }
    }
}
