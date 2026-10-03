// A static virtual called through `IVariant<object>` with `IVariant<string>` as the constrained
// type, which covariance admits. The interface named as the type supplies its own default body,
// at the instantiation named: it runs as `IVariant<string>`'s, not as `IVariant<object>`'s
// (measured on .NET 10).

public interface IVariant<out T>
{
    static virtual int Probe() => typeof(T) == typeof(string) ? 1 : 2;
}

public static class Program
{
    static int Call<TSelf>() where TSelf : IVariant<object> => TSelf.Probe();

    public static int Main(string[] argv)
    {
        return Call<IVariant<string>>() == 1 ? 0 : 1;
    }
}
