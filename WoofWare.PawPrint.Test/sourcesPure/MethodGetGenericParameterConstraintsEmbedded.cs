using System;
using System.Collections.Generic;

namespace MethodGetGenericParameterConstraintsEmbedded
{
    interface IWrap<T> { }

    class Holder<TOuter>
    {
        // The F-bounded shape, at method level: the constraint's signature mentions the very
        // parameter it constrains, as `!!0`.
        public static void SelfComparable<T>() where T : IComparable<T> { }

        // The same, but a class rather than an interface, so it becomes the parameter's base type.
        public static void ComparerBase<T>() where T : Comparer<T> { }

        // A method parameter constrained by the *declaring type's* parameter: `!0`, not `!!0`.
        public static void ToOuter<T>() where T : TOuter { }

        // One instantiation mentioning both a type variable and a method variable.
        public static void MixedInst<T>() where T : IDictionary<TOuter, T> { }

        // The declaring type applied to exactly its own formals — the typical instantiation,
        // which the runtime represents as the bare generic definition.
        public static void OuterTypical<T>() where T : Holder<TOuter> { }

        // The same collapse occurring inside another instantiation's argument list.
        public static void OuterTypicalNested<T>() where T : IWrap<Holder<TOuter>> { }
    }

    class Program
    {
        // Reached through a *closed* declaring type, not through `typeof(Holder<>)`: reflecting
        // over the methods of an open generic definition needs a vtable laid out on that
        // definition, which is its own gap (`ReflectionVirtualSlotsGenericDefinitionLayout.cs`).
        // It costs this file nothing, because a generic method of a constructed type is still its
        // own generic *definition*: real .NET hands back the identical parameter objects either
        // way, with constraints left unsubstituted — `Holder<int>.ToOuter<T>`'s constraint is
        // `TOuter`, not `int`.
        static Type Param(string name) =>
            typeof(Holder<int>).GetMethod(name).GetGenericArguments()[0];

        static Type[] Constraints(string name) => Param(name).GetGenericParameterConstraints();

        static int Main(string[] args)
        {
            // The method-level counterpart of TypeGetGenericParameterConstraintsSelfReferential.cs:
            // constraints whose signatures embed a type variable, so they cannot be resolved to a
            // closed type. A method parameter's constraint can embed *two* kinds of variable —
            // `!!n` for the method's own parameters and `!n` for the declaring type's — and
            // getting the two confused is the failure mode these checks exist to catch.

            Type outer = typeof(Holder<>).GetGenericArguments()[0];

            // `where T : IComparable<T>`: an open constructed type whose single argument is the
            // constrained parameter itself — the same Type object, not a copy.
            Type sc = Param("SelfComparable");
            Type[] scCs = Constraints("SelfComparable");
            if (scCs.Length != 1) return 1;
            if (scCs[0].GetGenericTypeDefinition() != typeof(IComparable<>)) return 2;
            if (scCs[0].GetGenericArguments().Length != 1) return 3;
            if (scCs[0].GetGenericArguments()[0] != sc) return 4;
            // Applied to an argument, so not a generic type *definition*. An implementation that
            // collapsed `IComparable<!!0>` to `typeof(IComparable<>)` — as it correctly does for
            // a type's own typical instantiation — fails here and at check 4.
            if (scCs[0].IsGenericTypeDefinition) return 5;
            if (!scCs[0].IsGenericType) return 6;
            if (!scCs[0].ContainsGenericParameters) return 7;
            // `IsInterface` is what `RuntimeType.GetBaseType` asks of each constraint, so an
            // interface constraint leaves the base type as Object.
            if (!scCs[0].IsInterface) return 8;
            if (sc.BaseType != typeof(object)) return 9;

            // `where T : Comparer<T>`: a non-interface embedded constraint, so it *is* the
            // parameter's base type — and the very same object the constraint array holds.
            Type cb = Param("ComparerBase");
            Type[] cbCs = Constraints("ComparerBase");
            if (cbCs.Length != 1) return 10;
            if (cbCs[0].IsInterface) return 11;
            if (cbCs[0].GetGenericTypeDefinition() != typeof(Comparer<>)) return 12;
            if (cbCs[0].GetGenericArguments()[0] != cb) return 13;
            if (!ReferenceEquals(cb.BaseType, cbCs[0])) return 14;

            // `where T : TOuter`: the constraint is the *declaring type's* parameter. This is the
            // one shape where confusing `!0` with `!!0` yields a wrong answer rather than a
            // crash — both parameters sit at position 0, so the wrong reading would hand back
            // the method's own T, which is a different Type object.
            Type to = Param("ToOuter");
            Type[] toCs = Constraints("ToOuter");
            if (toCs.Length != 1) return 15;
            if (!toCs[0].IsGenericParameter) return 16;
            if (toCs[0] != outer) return 17;
            // Not implied by the check above: it would also pass if the type's parameter and the
            // method's had collided onto one object, which is a question about how parameter
            // identity is keyed rather than about which one the constraint named.
            if (toCs[0] == to) return 18;
            if (toCs[0].GenericParameterPosition != 0) return 19;
            if (to.BaseType != typeof(object)) return 20;

            // Both kinds of variable inside one instantiation, in a signature where swapping the
            // two mappings would still produce two arguments of the right shape.
            Type mi = Param("MixedInst");
            Type[] miCs = Constraints("MixedInst");
            if (miCs.Length != 1) return 21;
            if (miCs[0].GetGenericTypeDefinition() != typeof(IDictionary<,>)) return 22;
            if (miCs[0].GetGenericArguments().Length != 2) return 23;
            if (miCs[0].GetGenericArguments()[0] != outer) return 24;
            if (miCs[0].GetGenericArguments()[1] != mi) return 25;
            if (!miCs[0].IsInterface) return 26;

            // `where T : Holder<TOuter>` is the declaring type applied to exactly its own formals.
            // The runtime treats that typical instantiation as the generic definition itself, so
            // the constraint is reference-equal to `typeof(Holder<>)`. Reaching that collapse
            // requires `!0` to have been mapped to the declaring type's parameter: map it to the
            // method's and no collapse happens.
            Type ot = Param("OuterTypical");
            Type[] otCs = Constraints("OuterTypical");
            if (otCs.Length != 1) return 27;
            if (otCs[0] != typeof(Holder<>)) return 28;
            if (!otCs[0].IsGenericTypeDefinition) return 29;
            if (!ReferenceEquals(ot.BaseType, otCs[0])) return 30;

            // The same collapse one level in: a canonical definition legitimately appears as a
            // generic argument, even though no signature can spell one there.
            Type[] otnCs = Constraints("OuterTypicalNested");
            if (otnCs.Length != 1) return 31;
            if (otnCs[0].GetGenericTypeDefinition() != typeof(IWrap<>)) return 32;
            if (otnCs[0].GetGenericArguments()[0] != typeof(Holder<>)) return 33;

            // Identity is canonical across calls: fresh arrays, same elements.
            if (ReferenceEquals(Constraints("SelfComparable"), scCs)) return 34;
            if (!ReferenceEquals(Constraints("SelfComparable")[0], scCs[0])) return 35;

            return 0;
        }
    }
}
