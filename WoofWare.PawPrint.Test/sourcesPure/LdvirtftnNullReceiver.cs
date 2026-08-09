using System;

// ECMA-335 III.4.18: `ldvirtftn` throws NullReferenceException when the object reference is
// null. CoreCLR raises it from the dispatch helper (`ResolveVirtualFunctionPointer`,
// jithelpers.cpp), so it is an ordinary catchable managed exception rather than a fault —
// which is why this can be a differential test at all.

public abstract class LdvirtftnNullBase
{
    public abstract int Value();
}

public static class LdvirtftnNullReceiver
{
    private static LdvirtftnNullBase Nothing()
    {
        return null;
    }

    public static int Main(string[] argv)
    {
        LdvirtftnNullBase b = Nothing();

        try
        {
            // `dup; ldvirtftn LdvirtftnNullBase::Value` on a null receiver. The delegate is
            // never constructed: the throw happens at the `ldvirtftn` itself, before the
            // `newobj` that would otherwise reject a null target.
            Func<int> f = b.Value;
            return f == null ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }
}
