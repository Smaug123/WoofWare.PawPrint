// A static virtual with a default body may be called with its declaring interface itself as the
// constrained type. No MethodImpl and no other interface supplies it, so CoreCLR falls back to
// the interface method's own body (`MethodTable::ResolveVirtualStaticMethod`).

public interface IProbe
{
    static virtual int Probe() => 10;
}

public static class Program
{
    static int Call<T>() where T : IProbe => T.Probe();

    public static int Main(string[] argv)
    {
        if (Call<IProbe>() != 10) return 1;
        return 0;
    }
}
