// `constrained. !!T callvirt` on a value-type T whose interface method T does not implement
// itself, so that only a default interface body supplies it.
//
// ECMA III.2.1: when T is a value type that does not implement the method, the receiver byref is
// dereferenced and boxed, and the call dispatches on the box. The default body therefore runs on a
// boxed *copy*: anything it changes through `this` is invisible in the caller's `x`. Each row
// below makes that copy observable, by having the default body mutate the receiver through a
// member the struct does implement, and then reading the original back.
//
// Measured on .NET 10 under default tiering, full opts (`DOTNET_TieredCompilation=0`) and min opts
// (`DOTNET_JITMinOpts=1`): a null receiver byref raises `NullReferenceException` on all three,
// because the copy into the box reads through it, even when the default body never reads `this`.

using System;
using System.Runtime.CompilerServices;

public interface ICounter
{
    void Increment();
    int Get();

    int BumpTwice()
    {
        Increment();
        Increment();
        return Get();
    }

    int Doubled => 2 * Get();

    int Probe(int a, int b) => a / b;

    int Ignores() => 7;

    int Generic<U>(U u) where U : ICounter
    {
        u.Increment();
        Increment();
        return 1000 * u.Get() + Get();
    }
}

// Overrides `ICounter.BumpTwice` with a default body of its own, which is the most specific
// implementation for a struct implementing `IDerivedCounter`.
public interface IDerivedCounter : ICounter
{
    int ICounter.BumpTwice()
    {
        Increment();
        Increment();
        Increment();
        return 100 + Get();
    }
}

public interface IGenericCounter<U>
{
    void Add(U u);
    int Get();

    int AddTwice(U u)
    {
        Add(u);
        Add(u);
        return Get();
    }
}

public struct Defaulted : ICounter
{
    public int Value;

    public void Increment()
    {
        Value++;
    }

    public int Get()
    {
        return Value;
    }
}

// Implements the method itself, so the call runs on the caller's `x` directly: the contrast row.
public struct Overrides : ICounter
{
    public int Value;

    public void Increment()
    {
        Value++;
    }

    public int Get()
    {
        return Value;
    }

    public int BumpTwice()
    {
        Value += 2;
        return Value;
    }
}

public struct DerivedDefaulted : IDerivedCounter
{
    public int Value;

    public void Increment()
    {
        Value++;
    }

    public int Get()
    {
        return Value;
    }
}

public struct GenericDefaulted : IGenericCounter<int>
{
    public int Value;

    public void Add(int u)
    {
        Value += u;
    }

    public int Get()
    {
        return Value;
    }
}

public class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static int BumpTwice<T>(ref T x) where T : ICounter
    {
        return x.BumpTwice();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Doubled<T>(ref T x) where T : ICounter
    {
        x.Increment();
        return x.Doubled;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Probe<T>(T x, int a, int b) where T : ICounter
    {
        return x.Probe(a, b);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Ignores<T>(ref T x) where T : ICounter
    {
        return x.Ignores();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Generic<T, U>(ref T x, U u) where T : ICounter where U : ICounter
    {
        return x.Generic(u);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int AddTwice<T>(ref T x, int u) where T : IGenericCounter<int>
    {
        return x.AddTwice(u);
    }

    public static int Main(string[] args)
    {
        // The default body mutates a boxed copy: it sees its own increments, the original does not.
        var d = new Defaulted { Value = 10 };
        if (BumpTwice(ref d) != 12)
        {
            return 1;
        }

        if (d.Value != 10)
        {
            return 2;
        }

        // Each call boxes afresh, so a second call starts from the original's value again.
        if (BumpTwice(ref d) != 12)
        {
            return 3;
        }

        // A struct implementing the method itself runs it on the original.
        var o = new Overrides { Value = 10 };
        if (BumpTwice(ref o) != 12)
        {
            return 4;
        }

        if (o.Value != 12)
        {
            return 5;
        }

        // A default property getter, after a direct call that did reach the original.
        if (Doubled(ref d) != 22)
        {
            return 6;
        }

        if (d.Value != 11)
        {
            return 7;
        }

        // The most specific default body comes from the derived interface.
        var dd = new DerivedDefaulted { Value = 5 };
        if (BumpTwice(ref dd) != 108)
        {
            return 8;
        }

        if (dd.Value != 5)
        {
            return 9;
        }

        // A default body of a generic interface.
        var g = new GenericDefaulted { Value = 1 };
        if (AddTwice(ref g, 3) != 7)
        {
            return 10;
        }

        if (g.Value != 1)
        {
            return 11;
        }

        // A generic default method: `u` is passed by value, so it is a copy too.
        var other = new Defaulted { Value = 4 };
        if (Generic(ref d, other) != 5012)
        {
            return 12;
        }

        if (d.Value != 11 || other.Value != 4)
        {
            return 13;
        }

        // An exception raised by the default body reaches the caller.
        try
        {
            Probe(new Defaulted(), 1, 0);
            return 14;
        }
        catch (DivideByZeroException)
        {
        }

        if (Probe(new Defaulted(), 6, 3) != 2)
        {
            return 15;
        }

        // A null receiver byref faults on the copy into the box, before the body runs.
        try
        {
            Ignores(ref Unsafe.NullRef<Defaulted>());
            return 16;
        }
        catch (NullReferenceException)
        {
        }

        if (Ignores(ref d) != 7)
        {
            return 17;
        }

        return 0;
    }
}
