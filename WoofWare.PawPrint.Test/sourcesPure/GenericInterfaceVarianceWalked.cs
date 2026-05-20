// The variance check is reached not just by direct same-def queries, but also
// by the interface walk: a type's `ImplementedInterfaces` may be different
// instantiations than the cast target, with the variance rule selecting one
// during the walk. Concretely: `MyHolder<Derived> : ICovariant<Derived>`,
// and we ask `is ICovariant<Base>`. The walk descends into MyHolder's
// implemented interfaces, reaches `ICovariant<Derived>`, and applies the
// covariant rule against the target `ICovariant<Base>`.
//
// Also covers transitive walk: `IList<T> : ICollection<T> : IEnumerable<T>`,
// where `IList` and `ICollection` are invariant but `IEnumerable<T>` is
// covariant (BCL). A type implementing `IList<Derived>` should answer true
// for `is IEnumerable<Base>`.

using System.Collections.Generic;

public interface IBag<out T>
{
}

public interface IBagDerived<out T> : IBag<T>
{
}

public class BagDerivedImpl<T> : IBagDerived<T>
{
}

public class WalkedBase
{
}

public class WalkedDerived : WalkedBase
{
}

public class TestGenericInterfaceVarianceWalked
{
    public static int Main(string[] argv)
    {
        // Walk-through-implemented-interface variance: BagDerivedImpl<Derived>
        // implements IBagDerived<Derived> which inherits IBag<Derived>. Target
        // IBag<Base>: walk finds IBag<Derived> via the inheritance chain and
        // applies the covariant rule.
        object bag = new BagDerivedImpl<WalkedDerived>();
        if (!(bag is IBag<WalkedBase>)) return 1;

        IBag<WalkedBase> asBase = (IBag<WalkedBase>) bag;
        if (asBase == null) return 2;

        // BCL chain: List<Derived> implements IList<Derived>, ICollection<Derived>,
        // IEnumerable<Derived>, IReadOnlyList<Derived>, IReadOnlyCollection<Derived>,
        // plus several non-generic interfaces. IList and ICollection are invariant;
        // IEnumerable and IReadOnly* are covariant. So `is IEnumerable<Base>` should
        // succeed; `is IList<Base>` should fail; `is IReadOnlyList<Base>` should
        // succeed.
        List<WalkedDerived> list = new List<WalkedDerived>();
        object listObj = list;

        if (!(listObj is IEnumerable<WalkedBase>)) return 3;
        if (listObj is IList<WalkedBase>) return 4; // invariant — rejected
        if (!(listObj is IReadOnlyList<WalkedBase>)) return 5;
        if (!(listObj is IReadOnlyCollection<WalkedBase>)) return 6;
        if (listObj is ICollection<WalkedBase>) return 7; // invariant — rejected

        // Direct cast through the covariant chain.
        IEnumerable<WalkedBase> asEnumerable = (IEnumerable<WalkedBase>) listObj;
        if (asEnumerable == null) return 8;

        return 0;
    }
}
