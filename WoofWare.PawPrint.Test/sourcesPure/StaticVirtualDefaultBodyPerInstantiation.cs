// A class implementing two instantiations of a generic interface has one default body for each:
// a static virtual call through `IProbe<int>` runs `IProbe<int>`'s, not `IProbe<string>`'s,
// though both are the same MethodDef.

public interface IProbe<T>
{
    static virtual T Probe(T value) => value;
}

public class Both : IProbe<int>, IProbe<string> { }

public static class Program
{
    static T Call<TSelf, T>(T value) where TSelf : IProbe<T> => TSelf.Probe(value);

    public static int Main(string[] argv)
    {
        if (Call<Both, int>(7) != 7) return 1;
        if (Call<Both, string>("x") != "x") return 2;
        return 0;
    }
}
