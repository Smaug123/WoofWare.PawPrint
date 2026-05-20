// ECMA-335 §I.8.7's variance rule composes: the per-parameter check is
// reference-assignability, which itself can require another variance walk
// when the argument is another variant generic instantiation. Concretely:
// `Func<Func<Derived>>` ⊑ `Func<Func<Base>>` requires the outer covariant
// step to ask whether `Func<Derived>` ⊑ `Func<Base>`, which is itself a
// covariant variance step.
//
// CoreCLR `MethodTable::CanCastByVarianceToInterfaceOrDelegate` recurses
// into `CanCastTo` for each generic argument, so nested instantiations
// naturally chain through the algorithm.

using System;

public class NestedBase
{
}

public class NestedDerived : NestedBase
{
}

public class TestNestedGenericVariance
{
    public static int Main(string[] argv)
    {
        // Two covariant steps: outer Func is covariant, inner Func is covariant.
        // Func<Func<Derived>> ⊑ Func<Func<Base>>.
        Func<Func<NestedDerived>> nested = () => () => new NestedDerived();
        object boxed = nested;
        if (!(boxed is Func<Func<NestedBase>>)) return 1;

        Func<Func<NestedBase>> asBase = (Func<Func<NestedBase>>) boxed;
        if (asBase == null) return 2;
        if (asBase()() == null) return 3;

        // Co/contra mix: Action<Func<Base>> ⊑ Action<Func<Derived>>.
        //   - Outer Action is contravariant: from-arg `Func<Base>` must be
        //     reference-assignable *from* to-arg `Func<Derived>`, i.e.
        //     `Func<Derived>` ⊑ `Func<Base>`.
        //   - Inner Func is covariant: Derived ⊑ Base. ✓
        Action<Func<NestedBase>> consumeMaker = m => { _ = m(); };
        object boxedConsume = consumeMaker;
        if (!(boxedConsume is Action<Func<NestedDerived>>)) return 4;

        Action<Func<NestedDerived>> asConsumeDerived = (Action<Func<NestedDerived>>) boxedConsume;
        if (asConsumeDerived == null) return 5;
        asConsumeDerived(() => new NestedDerived());

        // Negative nested: Func<Func<Base>> ⊄ Func<Func<Derived>>. Outer
        // covariant asks Func<Base> ⊑ Func<Derived>, which fails because
        // inner covariant requires Base ⊑ Derived (rejected).
        Func<Func<NestedBase>> nestedBase = () => () => new NestedBase();
        object boxedNegative = nestedBase;
        if (boxedNegative is Func<Func<NestedDerived>>) return 6;

        // Value-type leaf disables variance through the chain:
        // Func<Func<int>> ⊄ Func<Func<object>> because the inner step rejects
        // a value-typed `from` argument (CoreCLR `IsBoxedAndCanCastTo`).
        Func<Func<int>> nestedInt = () => () => 42;
        object boxedInt = nestedInt;
        if (boxedInt is Func<Func<object>>) return 7;

        return 0;
    }
}
