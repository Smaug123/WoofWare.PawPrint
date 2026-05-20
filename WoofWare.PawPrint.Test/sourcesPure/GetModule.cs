using System.Reflection;

public class GetModuleTests
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestGetModuleSucceeds();
        result |= TestSameModuleIdentity() << 1;
        result |= TestModuleAssemblyIdentity() << 2;
        result |= TestArrayTypeModuleIdentity() << 3;

        return result;
    }

    static int TestGetModuleSucceeds()
    {
        object module = typeof(GetModuleTests).Module;
        return object.ReferenceEquals(module, null) ? 1 : 0;
    }

    static int TestSameModuleIdentity()
    {
        object module1 = typeof(GetModuleTests).Module;
        object module2 = typeof(GetModuleHelper).Module;
        return object.ReferenceEquals(module1, module2) ? 0 : 1;
    }

    static int TestModuleAssemblyIdentity()
    {
        Module module = typeof(GetModuleTests).Module;
        object moduleAssembly = module.Assembly;
        object typeAssembly = typeof(GetModuleTests).Assembly;
        return object.ReferenceEquals(moduleAssembly, typeAssembly) ? 0 : 1;
    }

    static int TestArrayTypeModuleIdentity()
    {
        object module1 = typeof(GetModuleTests).Module;
        object module2 = typeof(GetModuleTests[]).Module;
        return object.ReferenceEquals(module1, module2) ? 0 : 1;
    }
}

class GetModuleHelper { }
