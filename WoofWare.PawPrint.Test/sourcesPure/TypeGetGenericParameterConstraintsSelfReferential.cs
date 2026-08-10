using System;
using System.Collections.Generic;

namespace TypeGetGenericParameterConstraintsSelfReferential
{
    // `where T : IComparable<T>` is the canonical self-referential (F-bounded) constraint, and
    // the shape most ordinary generic code reaches for. The constraint's own signature mentions
    // the parameter it constrains, so it cannot be resolved to a closed type: it is a generic
    // instantiation one of whose arguments is a type variable.
    class SelfBox<T> where T : struct, IComparable<T> { }

    // The embedded parameter need not be the constrained one.
    class Pair<A, B> where B : IComparable<A> { }

    // ... and it can be nested arbitrarily deep inside the constraint's shape.
    class NestedBox<T> where T : IComparable<List<T>> { }

    // The CRTP shape: the constraint is the declaring type applied to exactly its own
    // parameters, in order. This is what `IParsable<TSelf>` and the whole generic-math
    // hierarchy look like, and the runtime treats it specially — see the checks below.
    interface ISelf<T> where T : ISelf<T> { }

    // A *class* constraint that embeds a parameter, so the parameter's base type is itself an
    // open constructed type rather than Object or ValueType.
    class CBox<T> where T : Comparer<T> { }

    interface IWrap<T> { }

    // The CRTP collapse happening *inside* another instantiation: the inner `INested<T>` is the
    // typical instantiation and so collapses to the bare definition, which then appears as a
    // generic argument of `IWrap<>`. Legal metadata cannot spell a bare definition as an
    // argument, but canonicalisation can produce one, so it has to be accepted there.
    interface INested<T> where T : IWrap<INested<T>> { }

    // Two arguments, to pin the separator used when rendering an instantiation.
    class DictBox<A, B> where B : IDictionary<A, B> { }

    class Program
    {
        static int Main(string[] args)
        {
            Type t = typeof(SelfBox<>).GetGenericArguments()[0];

            // Roslyn writes the explicit row first and the synthetic System.ValueType row last,
            // as for `where T : struct, IDisposable` in the sibling file.
            Type[] cs = t.GetGenericParameterConstraints();
            if (cs.Length != 2) return 1;
            if (cs[1] != typeof(ValueType)) return 2;

            // The interesting one: an open constructed type. It is a generic type, its
            // definition is IComparable<>, and its single argument is the very parameter being
            // constrained — not a copy of it, the same Type object.
            if (!cs[0].IsGenericType) return 3;
            if (cs[0].GetGenericTypeDefinition() != typeof(IComparable<>)) return 4;
            if (cs[0].GetGenericArguments().Length != 1) return 5;
            if (cs[0].GetGenericArguments()[0] != t) return 6;

            // `IsInterface` is what CoreLib's `RuntimeType.GetBaseType()` asks of each
            // constraint before deciding a type variable's base type, so it has to be right on
            // an open constructed constraint specifically.
            if (!cs[0].IsInterface) return 7;
            if (!cs[0].ContainsGenericParameters) return 8;

            // An open constructed type is NOT a generic type *definition*: it has been applied
            // to arguments, even though those arguments are not closed.
            if (cs[0].IsGenericTypeDefinition) return 9;

            // Naming. A type containing generic parameters has no FullName.
            if (cs[0].Name != "IComparable`1") return 10;
            if (cs[0].Namespace != "System") return 11;
            if (cs[0].FullName != null) return 12;

            // Identity is canonical: asking twice yields the same object, in fresh arrays.
            Type[] csAgain = t.GetGenericParameterConstraints();
            if (ReferenceEquals(cs, csAgain)) return 13;
            if (!ReferenceEquals(cs[0], csAgain[0])) return 14;

            // The consequence that motivates all of this: the base-type walk can run, so a
            // parameter carrying both the `struct` flag and a self-referential constraint
            // answers correctly. `BaseType` is the property `IsValueType` is defined in terms
            // of for a type variable, and unlike `IsValueType` it is not short-circuited.
            if (t.BaseType != typeof(ValueType)) return 15;
            if (!t.IsValueType) return 16;

            // A constraint embedding a *different* parameter of the same type.
            Type a = typeof(Pair<,>).GetGenericArguments()[0];
            Type b = typeof(Pair<,>).GetGenericArguments()[1];
            Type[] bcs = b.GetGenericParameterConstraints();
            if (bcs.Length != 1) return 17;
            if (bcs[0].GetGenericTypeDefinition() != typeof(IComparable<>)) return 18;
            if (bcs[0].GetGenericArguments()[0] != a) return 19;
            // Interface-only constraints leave the base type as Object.
            if (b.BaseType != typeof(object)) return 20;

            // A constraint whose embedded parameter is nested inside a further instantiation.
            Type n = typeof(NestedBox<>).GetGenericArguments()[0];
            Type[] ncs = n.GetGenericParameterConstraints();
            if (ncs.Length != 1) return 21;
            if (ncs[0].GetGenericTypeDefinition() != typeof(IComparable<>)) return 22;
            Type inner = ncs[0].GetGenericArguments()[0];
            if (inner.GetGenericTypeDefinition() != typeof(List<>)) return 23;
            if (inner.GetGenericArguments()[0] != n) return 24;

            // The CRTP collapse. `ISelf<T>` applied to exactly its own parameter in order is
            // the *typical instantiation*, which the runtime represents as the generic type
            // definition itself — so the constraint here is reference-equal to
            // `typeof(ISelf<>)`, and reports IsGenericTypeDefinition. Any representation that
            // mints a distinct "ISelf applied to its own T" object fails these two checks.
            Type s = typeof(ISelf<>).GetGenericArguments()[0];
            Type[] scs = s.GetGenericParameterConstraints();
            if (scs.Length != 1) return 25;
            if (scs[0] != typeof(ISelf<>)) return 26;
            if (!scs[0].IsGenericTypeDefinition) return 27;

            // A non-interface constraint that embeds a parameter becomes the parameter's base
            // type, so `BaseType` itself hands back an open constructed type — the same object
            // the constraint array holds.
            Type c = typeof(CBox<>).GetGenericArguments()[0];
            Type[] ccs = c.GetGenericParameterConstraints();
            if (ccs.Length != 1) return 28;
            if (ccs[0].IsInterface) return 29;
            if (!ReferenceEquals(c.BaseType, ccs[0])) return 30;
            // Ordinary reflection over the returned constraint object itself, not just over the
            // parameter it constrains. `Comparer<T>`'s own base is Object, and everything is
            // assignable to Object.
            if (ccs[0].BaseType != typeof(object)) return 37;
            if (!typeof(object).IsAssignableFrom(ccs[0])) return 38;
            // An instantiation is not an array, pointer or byref, so it has no element type.
            if (cs[0].GetElementType() != null) return 42;

            // The collapse composed with nesting: the constraint is `IWrap<INested<T>>`, and its
            // argument — being the typical instantiation of INested — is `typeof(INested<>)`
            // itself. So a canonical *definition* legitimately appears as a generic argument,
            // even though no signature can spell one there.
            Type nested = typeof(INested<>).GetGenericArguments()[0];
            Type[] nestedCs = nested.GetGenericParameterConstraints();
            if (nestedCs.Length != 1) return 31;
            if (nestedCs[0].GetGenericTypeDefinition() != typeof(IWrap<>)) return 32;
            if (nestedCs[0].GetGenericArguments()[0] != typeof(INested<>)) return 33;
            // A collapsed definition appearing as an argument still renders its own formal
            // parameter list.
            if (nestedCs[0].ToString()
                != "TypeGetGenericParameterConstraintsSelfReferential.IWrap`1[TypeGetGenericParameterConstraintsSelfReferential.INested`1[T]]")
                return 36;

            // Multiple arguments: pins the separator between them.
            Type db = typeof(DictBox<,>).GetGenericArguments()[1];
            Type[] dbCs = db.GetGenericParameterConstraints();
            if (dbCs.Length != 1) return 34;
            if (dbCs[0].ToString() != "System.Collections.Generic.IDictionary`2[A,B]") return 35;

            // Rendering a bare generic definition differs by format: `ToString()` includes the
            // formal parameter list, `FullName` and `AssemblyQualifiedName` do not.
            if (typeof(List<>).ToString() != "System.Collections.Generic.List`1[T]") return 39;
            if (typeof(List<>).FullName != "System.Collections.Generic.List`1") return 40;
            if (!typeof(List<>).AssemblyQualifiedName.StartsWith("System.Collections.Generic.List`1, "))
                return 41;

            return 0;
        }
    }
}
