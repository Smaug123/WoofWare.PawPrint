// A derived interface's `new static virtual` method of the same shape as its base interface's is
// a different method, not an implementation of the base's. Called with the derived interface as
// the constrained type, it runs its own body, and a class implementing the derived interface runs
// it too; called with the derived interface through the base interface, the base's default body
// runs. `StaticVirtualDefaultBodyIgnoresNewSlot.cs` is the class called through the base.

public interface IBase
{
    static virtual int Probe() => 10;
}

public interface IDerived : IBase
{
    static new virtual int Probe() => 20;
}

public class Implements : IDerived { }

public static class Program
{
    static int CallDerived<T>() where T : IDerived => T.Probe();
    static int CallBase<T>() where T : IBase => T.Probe();

    public static int Main(string[] argv)
    {
        if (CallDerived<IDerived>() != 20) return 1;
        if (CallDerived<Implements>() != 20) return 2;
        if (CallBase<IDerived>() != 10) return 3;
        return 0;
    }
}
