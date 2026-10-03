// The most specific default body of a static virtual: `ILeft : IBase` implements `IBase.Probe`,
// so for a class implementing `ILeft`, `ILeft`'s body is more specific than `IBase`'s own,
// whichever order the class's interface map holds them in
// (`MethodTable::FindDefaultInterfaceImplementation`).

public interface IBase
{
    static virtual int Probe() => 10;
}

public interface ILeft : IBase
{
    static int IBase.Probe() => 20;
}

public class LeftFirst : ILeft { }

public class BaseFirst : IBase, ILeft { }

public static class Program
{
    static int Call<T>() where T : IBase => T.Probe();

    public static int Main(string[] argv)
    {
        if (Call<LeftFirst>() != 20) return 1;
        if (Call<BaseFirst>() != 20) return 2;
        if (Call<IBase>() != 10) return 3;
        if (Call<ILeft>() != 20) return 4;
        return 0;
    }
}
