// A derived interface's `new static virtual` method of the same shape as its base interface's is
// a different method. A class implementing the derived interface, called through the base
// interface, runs the base interface's default body: CoreCLR's default-body search
// (`MethodTable::FindDefaultInterfaceImplementation`) takes on each interface only the
// declaration itself or a MethodImpl naming it, never a method that merely shares its name and
// signature.

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
    static int CallBase<T>() where T : IBase => T.Probe();

    public static int Main(string[] argv)
    {
        if (CallBase<Implements>() != 10) return 1;
        return 0;
    }
}
