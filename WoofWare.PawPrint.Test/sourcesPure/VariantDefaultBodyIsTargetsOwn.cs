// Default interface bodies through generic and variant interfaces. CoreCLR's candidate on an
// interface of the target's own definition is the target method itself, and only on an
// instantiation the receiver's entry can be cast to (`TryGetCandidateImplementation` in
// methodtable.cpp); on any other interface only a MethodImpl naming the target is one.

using System;

// A `new` method of a variant derived interface does not implement its base's method, even when
// the call reaches the base through variance.
public interface IBase<out T> { int Probe() => 10; }
public interface IDerived<out T> : IBase<T> { new int Probe() => 20; }
public class Shadows : IDerived<string> { }
public struct ShadowsStruct : IDerived<string> { }

// The same through an invariant generic interface, so that no variance is in play at all.
public interface IInvBase<T> { int Probe() => 30; }
public interface IInvDerived<T> : IInvBase<T> { new int Probe() => 40; }
public struct InvShadows : IInvDerived<int> { }

// Two instantiations of one variant interface, of which only `ICo<string>` can be cast to
// `ICo<object>`: `ICo<int>` cannot, because variance does not extend to a value-type argument.
// The default body therefore runs instantiated over `string`, and there is no ambiguity.
public interface ICo<out T> { int Which() => typeof(T) == typeof(string) ? 50 : 60; }
public class TwoInstantiations : ICo<int>, ICo<string> { }

// A MethodImpl naming a variance-compatible instantiation counts only once the exact search has
// found nothing. `ExactBeatsVariantOverride` has `IContra<string>` itself, whose own default body
// is found exactly, so `IOverridesObject`'s override of `IContra<object>.M` does not run although
// its interface is more specific. Without the exact entry, that override is what runs.
public interface IContra<in T> { int M() => 70; }
public interface IOverridesObject : IContra<object> { int IContra<object>.M() => 80; }
public class ExactBeatsVariantOverride : IOverridesObject, IContra<string> { }
public class OnlyVariantOverride : IOverridesObject { }

public static class Program
{
    static int Base<T>(T x) where T : IBase<object> => x.Probe();
    static int Derived<T>(T x) where T : IDerived<object> => x.Probe();
    static int InvBase<T>(T x) where T : IInvBase<int> => x.Probe();
    static int InvDerived<T>(T x) where T : IInvDerived<int> => x.Probe();

    public static int Main(string[] argv)
    {
        if (((IBase<object>)new Shadows()).Probe() != 10) return 1;
        if (((IDerived<object>)new Shadows()).Probe() != 20) return 2;
        if (((IBase<string>)new Shadows()).Probe() != 10) return 3;
        if (Base(new ShadowsStruct()) != 10) return 4;
        if (Derived(new ShadowsStruct()) != 20) return 5;
        if (InvBase(new InvShadows()) != 30) return 6;
        if (InvDerived(new InvShadows()) != 40) return 7;
        if (((ICo<object>)new TwoInstantiations()).Which() != 50) return 8;
        if (((IContra<string>)new ExactBeatsVariantOverride()).M() != 70) return 9;
        if (((IContra<string>)new OnlyVariantOverride()).M() != 80) return 10;
        if (((IContra<object>)new ExactBeatsVariantOverride()).M() != 80) return 11;
        return 0;
    }
}
