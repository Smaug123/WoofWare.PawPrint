using System;

// An `init` accessor that is *virtual*, and overridden. The object initialiser emits
// `callvirt Base::set_Width`, so dispatch has to match the derived override against the base's
// vtable slot — and that match compares the two concretised signatures, including their return
// shapes. Both sides are `void modreq(IsExternalInit)`, so a translation that answered "returns
// nothing" for one and "returns System.Void" for the other would find no override and silently run
// the base accessor, writing the base's backing field instead of the derived one.

public class Base
{
    protected int stored;

    public virtual int Width
    {
        get { return stored; }
        init { stored = value; }
    }

    // The base's own store, readable independently, so "the override ran" and "the base ran" are
    // distinguishable rather than both looking like a correct answer.
    public int BaseStored
    {
        get { return stored; }
    }
}

public sealed class Derived : Base
{
    private int doubled;

    public override int Width
    {
        get { return doubled; }
        init { doubled = value * 2; }
    }

    public int Doubled
    {
        get { return doubled; }
    }
}

public static class Program
{
    public static int Main ()
    {
        Derived d = new Derived
        {
            Width = 21,
        };

        if (d.Doubled != 42)
        {
            return 1;
        }

        if (d.Width != 42)
        {
            return 2;
        }

        // The base accessor must not have run: if dispatch fell through to `Base.set_Width`, this
        // would be 21 rather than 0.
        if (d.BaseStored != 0)
        {
            return 3;
        }

        // Through a base-typed reference, so the call site names `Base::set_Width` and dispatch has
        // to find the override rather than the declaration.
        Base b = new Derived
        {
            Width = 5,
        };

        if (b.Width != 10)
        {
            return 4;
        }

        if (b.BaseStored != 0)
        {
            return 5;
        }

        // A base instance, so the un-overridden accessor is exercised too.
        Base plain = new Base
        {
            Width = 3,
        };

        if (plain.Width != 3)
        {
            return 6;
        }

        return 0;
    }
}
