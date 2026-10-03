// A class implementing only `IVariant<object>` satisfies `IVariant<string>` through
// contravariance. A static virtual call through `IVariant<string>` finds no exact
// implementation or default body, so CoreCLR's variant pass runs `IVariant<object>`'s default
// body (`MethodTable::ResolveVirtualStaticMethod`).

public interface IVariant<in T>
{
    static virtual int Probe() => typeof(T) == typeof(object) ? 1 : 2;
}

public class OnObject : IVariant<object> { }

public static class Program
{
    static int Call<TSelf>() where TSelf : IVariant<string> => TSelf.Probe();

    public static int Main(string[] argv)
    {
        if (Call<OnObject>() != 1) return 1;
        return 0;
    }
}
