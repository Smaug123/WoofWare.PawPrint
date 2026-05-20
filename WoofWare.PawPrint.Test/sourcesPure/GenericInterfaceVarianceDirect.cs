// ECMA-335 §I.8.7.2 / CoreCLR `MethodTable::CanCastByVarianceToInterfaceOrDelegate`:
// when two generic instantiations share the same TypeDef and the def declares
// variance, assignability reduces to a per-parameter check.
//   - Covariant (`out T`): `from`-arg must be reference-typed and reference-
//     assignment-compatible with `to`-arg.
//   - Contravariant (`in T`): `to`-arg must be reference-typed and reference-
//     assignment-compatible with `from`-arg.
//   - Invariant: arguments must be identical.
// A value-typed argument disables variance (CoreCLR's `IsBoxedAndCanCastTo`
// precondition: `from` must be a reference type).

using System;

public interface ICovariant<out T>
{
}

public interface IContravariant<in T>
{
}

public interface IInvariant<T>
{
}

public class CovariantImpl<T> : ICovariant<T>
{
}

public class ContravariantImpl<T> : IContravariant<T>
{
}

public class InvariantImpl<T> : IInvariant<T>
{
}

public class Base
{
}

public class Derived : Base
{
}

public class TestGenericInterfaceVarianceDirect
{
    public static int Main(string[] argv)
    {
        // Covariant positive: ICovariant<Derived> ⊑ ICovariant<Base>.
        object covDerived = new CovariantImpl<Derived>();
        ICovariant<Base> asCovBase = (ICovariant<Base>) covDerived;
        if (asCovBase == null) return 1;
        if (!(covDerived is ICovariant<Base>)) return 2;

        // Covariant positive transitive: ICovariant<Derived> ⊑ ICovariant<object>.
        if (!(covDerived is ICovariant<object>)) return 3;

        // Covariant negative: ICovariant<Base> ⊄ ICovariant<Derived>.
        object covBase = new CovariantImpl<Base>();
        if (covBase is ICovariant<Derived>) return 4;
        bool threw = false;
        try
        {
            ICovariant<Derived> _ = (ICovariant<Derived>) covBase;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }
        if (!threw) return 5;

        // Contravariant positive: IContravariant<Base> ⊑ IContravariant<Derived>.
        object contraBase = new ContravariantImpl<Base>();
        IContravariant<Derived> asContraDerived = (IContravariant<Derived>) contraBase;
        if (asContraDerived == null) return 6;
        if (!(contraBase is IContravariant<Derived>)) return 7;

        // Contravariant negative: IContravariant<Derived> ⊄ IContravariant<Base>.
        object contraDerived = new ContravariantImpl<Derived>();
        if (contraDerived is IContravariant<Base>) return 8;
        threw = false;
        try
        {
            IContravariant<Base> _ = (IContravariant<Base>) contraDerived;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }
        if (!threw) return 9;

        // Invariant: same TypeDef but different generics is always rejected,
        // even if the args themselves are assignable.
        object invDerived = new InvariantImpl<Derived>();
        if (invDerived is IInvariant<Base>) return 10;
        threw = false;
        try
        {
            IInvariant<Base> _ = (IInvariant<Base>) invDerived;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }
        if (!threw) return 11;

        // Value-type arg disables variance: ICovariant<int> ⊄ ICovariant<object>.
        // CoreCLR's `IsBoxedAndCanCastTo` rejects a value-typed `from` parameter
        // regardless of the declared variance.
        object covInt = new CovariantImpl<int>();
        if (covInt is ICovariant<object>) return 12;
        threw = false;
        try
        {
            ICovariant<object> _ = (ICovariant<object>) covInt;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }
        if (!threw) return 13;

        // Identity check still passes through the variance walk: ICovariant<Base>
        // ⊑ ICovariant<Base>. (Variance walk's per-parameter `fromArg = toArg`
        // shortcut.)
        if (!(covBase is ICovariant<Base>)) return 14;

        return 0;
    }
}
