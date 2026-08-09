using System;

// `ldvirtftn` over the generic token shapes: a virtual method on an instantiated generic
// type (a MemberReference whose parent is a TypeSpec) and a generic virtual method (a
// MethodSpec). Kept apart from `LdvirtftnVirtualDispatch.cs` so that the non-generic
// dispatch coverage does not depend on generic-virtual-method resolution.

public class LdvirtftnBox<T>
{
    private T _value;

    public LdvirtftnBox(T value)
    {
        _value = value;
    }

    public virtual T Get()
    {
        return _value;
    }
}

public class LdvirtftnDoubledBox : LdvirtftnBox<int>
{
    public LdvirtftnDoubledBox(int value)
        : base(value)
    {
    }

    public override int Get()
    {
        return base.Get() * 2;
    }
}

public class LdvirtftnMeasurer
{
    public virtual int Measure<T>(T item)
    {
        return 1;
    }
}

public class LdvirtftnDerivedMeasurer : LdvirtftnMeasurer
{
    public override int Measure<T>(T item)
    {
        return 2;
    }
}

public static class LdvirtftnGenericDispatch
{
    public static int Main(string[] argv)
    {
        // Virtual method on an instantiated generic type, overridden by a non-generic
        // derived type.
        LdvirtftnBox<int> box = new LdvirtftnDoubledBox(21);
        Func<int> get = box.Get;
        if (get() != 42)
        {
            return 1;
        }

        // Same call-site token, receiver that does not override.
        LdvirtftnBox<int> plainBox = new LdvirtftnBox<int>(7);
        Func<int> plainGet = plainBox.Get;
        if (plainGet() != 7)
        {
            return 2;
        }

        // Generic virtual method: the token is a MethodSpec, and dispatch must still find the
        // derived body.
        LdvirtftnMeasurer measurer = new LdvirtftnDerivedMeasurer();
        Func<string, int> measure = measurer.Measure<string>;
        if (measure("hello") != 2)
        {
            return 3;
        }

        LdvirtftnMeasurer baseMeasurer = new LdvirtftnMeasurer();
        Func<string, int> baseMeasure = baseMeasurer.Measure<string>;
        if (baseMeasure("hello") != 1)
        {
            return 4;
        }

        return 0;
    }
}
