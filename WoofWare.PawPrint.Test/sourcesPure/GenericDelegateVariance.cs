// ECMA-335 §I.8.7 also applies the variance walk to delegate types. The BCL
// `Func<TResult>` is covariant in TResult (`out`); `Action<T>` is contravariant
// in T (`in`). Mixed delegates like `Func<TArg, TResult>` are contravariant in
// TArg and covariant in TResult.
//
// CoreCLR's `MethodTable::CanCastToClass` walks the inheritance chain when the
// target has variance — for delegates this descends from MulticastDelegate.
// The per-step `CanCastByVarianceToInterfaceOrDelegate` applies the same
// algorithm as for interfaces.

using System;

public class DelegBase
{
}

public class DelegDerived : DelegBase
{
}

public class TestGenericDelegateVariance
{
    public static int Main(string[] argv)
    {
        // Covariant delegate: Func<DelegDerived> ⊑ Func<DelegBase>.
        Func<DelegDerived> makeDerived = () => new DelegDerived();
        object boxed = makeDerived;
        if (!(boxed is Func<DelegBase>)) return 1;

        Func<DelegBase> asBase = (Func<DelegBase>) boxed;
        if (asBase == null) return 2;
        if (asBase() == null) return 3;

        // Reverse covariant: Func<DelegBase> ⊄ Func<DelegDerived>.
        Func<DelegBase> makeBase = () => new DelegBase();
        object boxedBase = makeBase;
        if (boxedBase is Func<DelegDerived>) return 4;
        bool threw = false;
        try
        {
            Func<DelegDerived> _ = (Func<DelegDerived>) boxedBase;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }
        if (!threw) return 5;

        // Contravariant delegate: Action<DelegBase> ⊑ Action<DelegDerived>.
        Action<DelegBase> consumeBase = b => { _ = b; };
        object boxedConsume = consumeBase;
        if (!(boxedConsume is Action<DelegDerived>)) return 6;

        Action<DelegDerived> asConsumeDerived = (Action<DelegDerived>) boxedConsume;
        if (asConsumeDerived == null) return 7;
        asConsumeDerived(new DelegDerived());

        // Reverse contravariant: Action<DelegDerived> ⊄ Action<DelegBase>.
        Action<DelegDerived> consumeDerived = d => { _ = d; };
        object boxedConsumeDerived = consumeDerived;
        if (boxedConsumeDerived is Action<DelegBase>) return 8;

        // Mixed delegate `Func<TArg, TResult>` — contravariant TArg, covariant TResult.
        // Func<Base,Derived> ⊑ Func<Derived,Base>: TArg goes base→derived
        // (contravariant), TResult goes derived→base (covariant).
        Func<DelegBase, DelegDerived> bToD = _ => new DelegDerived();
        object boxedMix = bToD;
        if (!(boxedMix is Func<DelegDerived, DelegBase>)) return 9;

        // Value-type arg disables variance: Func<int> ⊄ Func<object>.
        Func<int> makeInt = () => 42;
        object boxedInt = makeInt;
        if (boxedInt is Func<object>) return 10;

        return 0;
    }
}
