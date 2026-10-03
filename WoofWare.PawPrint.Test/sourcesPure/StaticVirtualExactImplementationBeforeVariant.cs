// A class implementing a static virtual on two instantiations of a contravariant interface. Each
// MethodImpl is variance-compatible with a call through `I<string>`, but CoreCLR asks each type
// for an implementation on exactly the call's instantiation before it allows variance
// (`MethodTable::ResolveVirtualStaticMethod`), so the `I<string>` one runs.

public interface I<in T>
{
    static abstract int Probe();
}

public class Both : I<string>, I<object>
{
    static int I<string>.Probe() => 1;
    static int I<object>.Probe() => 2;
}

public class OnlyObject : I<object>
{
    static int I<object>.Probe() => 3;
}

public static class Program
{
    static int CallString<T>() where T : I<string> => T.Probe();
    static int CallObject<T>() where T : I<object> => T.Probe();

    public static int Main(string[] argv)
    {
        if (CallString<Both>() != 1) return 1;
        if (CallObject<Both>() != 2) return 2;
        if (CallString<OnlyObject>() != 3) return 3;
        return 0;
    }
}
