using System;
using System.Reflection;

public class AssemblyGetTypeTests
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestGetTypeFindsTopLevelByFullName();
        result |= TestGetTypeReturnsNullForMissing() << 1;
        result |= TestGetTypeReturnsSameInstance() << 2;
        result |= TestGetTypeFindsHelper() << 3;

        return result;
    }

    // typeof(X).Assembly.GetType("X") should equal typeof(X) for a top-level type
    // with no namespace.
    static int TestGetTypeFindsTopLevelByFullName()
    {
        Type direct = typeof(AssemblyGetTypeTests);
        Assembly asm = direct.Assembly;
        Type looked = asm.GetType("AssemblyGetTypeTests");
        if (looked == null) return 1;
        if (!object.ReferenceEquals(looked, direct)) return 1;
        return 0;
    }

    // Looking up a non-existent type should return null, not throw.
    static int TestGetTypeReturnsNullForMissing()
    {
        Assembly asm = typeof(AssemblyGetTypeTests).Assembly;
        Type looked = asm.GetType("NoSuchTypeShouldExistAnywhere");
        return looked == null ? 0 : 1;
    }

    // Two lookups of the same type must return the same RuntimeType instance.
    static int TestGetTypeReturnsSameInstance()
    {
        Assembly asm = typeof(AssemblyGetTypeTests).Assembly;
        Type a = asm.GetType("AssemblyGetTypeTests");
        Type b = asm.GetType("AssemblyGetTypeTests");
        if (a == null || b == null) return 1;
        return object.ReferenceEquals(a, b) ? 0 : 1;
    }

    // A second top-level type in the same assembly is also discoverable.
    static int TestGetTypeFindsHelper()
    {
        Type direct = typeof(AssemblyGetTypeHelper);
        Assembly asm = direct.Assembly;
        Type looked = asm.GetType("AssemblyGetTypeHelper");
        if (looked == null) return 1;
        if (!object.ReferenceEquals(looked, direct)) return 1;
        return 0;
    }
}

class AssemblyGetTypeHelper { }
