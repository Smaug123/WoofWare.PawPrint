// CoreCLR resolves a static virtual call only through MethodImpls
// (`MethodTable::TryResolveVirtualStaticMethodOnThisType`), never by name and signature. So a
// derived class's `new static` method of the same shape, which re-lists no interface, is not
// the implementation: `T.Probe()` for `T = Derived` runs the base class's.

public interface IProbe
{
    static abstract int Probe();
}

public class Base : IProbe
{
    public static int Probe() => 10;
}

public class Derived : Base
{
    public static new int Probe() => 20;
}

public static class Program
{
    static int Call<T>() where T : IProbe => T.Probe();

    public static int Main(string[] argv)
    {
        if (Call<Derived>() != 10) return 1;
        if (Call<Base>() != 10) return 2;
        return 0;
    }
}
