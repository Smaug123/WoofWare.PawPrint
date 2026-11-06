using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// `MethodBase.GetCurrentMethod()`. CoreCLR's `MethodBase_GetCurrentMethod` QCall walks to the first
// frame outside the one holding the stack-crawl mark, which is `GetCurrentMethod`'s caller, and
// answers that method's typical definition. Every caller here is `NoInlining`, because a JIT that
// inlined one would name its caller instead.

class Gen<T>
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static MethodBase Here<U>() => MethodBase.GetCurrentMethod();
}

class Constructed
{
    public readonly MethodBase Ctor;

    [MethodImpl(MethodImplOptions.NoInlining)]
    public Constructed() { Ctor = MethodBase.GetCurrentMethod(); }
}

class WithTypeInitializer
{
    public static readonly MethodBase Cctor = MethodBase.GetCurrentMethod();
}

public static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static MethodBase Here() => MethodBase.GetCurrentMethod();

    static bool Record(MethodBase m, ref MethodBase into)
    {
        into = m;
        return true;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Throw() { throw new InvalidOperationException(); }

    // A filter runs while the frames of the throw are still live; its caller is the method that
    // declares the filter.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static MethodBase InFilter()
    {
        MethodBase seen = null;
        try { Throw(); }
        catch (InvalidOperationException) when (Record(MethodBase.GetCurrentMethod(), ref seen)) { }
        return seen;
    }

    public static int Main(string[] args)
    {
        MethodBase here = Here();
        if (here == null) return 1;
        if (here.Name != "Here") return 2;
        if (here.DeclaringType != typeof(Program)) return 3;
        if (!(here is MethodInfo)) return 4;
        if (!here.IsStatic) return 5;
        // The answer comes from the declaring type's member cache, so it is the object reflection
        // hands back for the same method.
        if (!ReferenceEquals(here, Here())) return 6;
        if (!ReferenceEquals(here, typeof(Program).GetMethod("Here", BindingFlags.NonPublic | BindingFlags.Static)))
            return 7;

        // The typical definition: both instantiations are stripped, whichever were running.
        MethodBase gen = Gen<int>.Here<string>();
        if (gen.Name != "Here") return 10;
        if (gen.DeclaringType != typeof(Gen<>)) return 11;
        if (!gen.IsGenericMethodDefinition) return 12;
        if (!ReferenceEquals(gen, Gen<object>.Here<double>())) return 13;
        if (!ReferenceEquals(gen, typeof(Gen<>).GetMethod("Here"))) return 14;

        MethodBase ctor = new Constructed().Ctor;
        if (!(ctor is ConstructorInfo)) return 20;
        if (ctor.Name != ".ctor") return 21;
        if (ctor.DeclaringType != typeof(Constructed)) return 22;
        if (ctor.IsStatic) return 23;

        MethodBase cctor = WithTypeInitializer.Cctor;
        if (!(cctor is ConstructorInfo)) return 30;
        if (cctor.Name != ".cctor") return 31;
        if (!cctor.IsStatic) return 32;
        if (cctor.DeclaringType != typeof(WithTypeInitializer)) return 33;

        // A lambda is a method of its own.
        Func<MethodBase> lambda = () => MethodBase.GetCurrentMethod();
        MethodBase fromLambda = lambda();
        if (fromLambda.Name == "Main") return 40;
        if (!fromLambda.Name.StartsWith("<Main>")) return 41;

        // A delegate bound straight to `GetCurrentMethod` has no frame of its own between the
        // two, so the answer is whoever invoked the delegate.
        Func<MethodBase> direct = MethodBase.GetCurrentMethod;
        MethodBase viaDelegate = direct();
        if (viaDelegate.Name != "Main") return 50;
        if (viaDelegate.DeclaringType != typeof(Program)) return 51;

        // CoreCLR's walk hides the multicast invoke stub, so each element's caller is whoever
        // invoked the multicast delegate.
        MethodBase seenByFirst = null;
        Func<MethodBase> first = () => { seenByFirst = MethodBase.GetCurrentMethod(); return seenByFirst; };
        Func<MethodBase> multicast = (Func<MethodBase>)Delegate.Combine(first, direct);
        MethodBase viaMulticast = multicast();
        if (viaMulticast.Name != "Main") return 52;
        if (viaMulticast.DeclaringType != typeof(Program)) return 53;
        if (!seenByFirst.Name.StartsWith("<Main>")) return 54;

        MethodBase filter = InFilter();
        if (filter == null) return 60;
        if (filter.Name != "InFilter") return 61;

        return 0;
    }
}
