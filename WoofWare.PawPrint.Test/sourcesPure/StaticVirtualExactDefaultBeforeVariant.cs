// Default bodies of a static virtual through a contravariant interface: `IString` implements
// `I<string>.Probe`, and `IObject : IString` implements `I<object>.Probe`, which variance makes
// compatible with `I<string>` too. CoreCLR looks for a default body on exactly the call's
// instantiation before it allows variance (`MethodTable::ResolveVirtualStaticMethod`), so a call
// through `I<string>` runs `IString`'s, though `IObject` is the more specific interface.

public interface I<in T>
{
    static abstract int Probe();
}

public interface IString : I<string>
{
    static int I<string>.Probe() => 1;
}

public interface IObject : IString, I<object>
{
    static int I<object>.Probe() => 2;
}

public class C : IObject { }

public static class Program
{
    static int CallString<T>() where T : I<string> => T.Probe();
    static int CallObject<T>() where T : I<object> => T.Probe();

    public static int Main(string[] argv)
    {
        if (CallString<C>() != 1) return 1;
        if (CallObject<C>() != 2) return 2;
        return 0;
    }
}
