// A derived interface's `new` instance method of the same name and signature as its base
// interface's is a different method, not an implementation of the base's. On the target's own
// interface the default body is the declared method itself; on any other interface only a
// MethodImpl naming the target supplies one (`MethodTable::FindDefaultInterfaceImplementation`).
// So a call through `IBase` runs `IBase`'s body, even though `IDerived` is more specific.

public interface IBase { int Probe() => 10; }
public interface IDerived : IBase { new int Probe() => 20; }
public class C : IDerived { }
public struct S : IDerived { }

public static class Program
{
    static int Base<T>(T x) where T : IBase => x.Probe();
    static int Derived<T>(T x) where T : IDerived => x.Probe();

    public static int Main(string[] argv)
    {
        if (((IBase)new C()).Probe() != 10) return 1;
        if (((IDerived)new C()).Probe() != 20) return 2;
        if (Base(new S()) != 10) return 3;
        if (Derived(new S()) != 20) return 4;
        return 0;
    }
}
