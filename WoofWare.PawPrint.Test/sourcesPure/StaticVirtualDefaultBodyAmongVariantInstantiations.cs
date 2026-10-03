// Which default body of a static virtual runs when the constrained type implements several
// instantiations of a variant interface. CoreCLR looks for a default body twice
// (`MethodTable::ResolveVirtualStaticMethod`): first for the exact instantiation the constraint
// names, and only if that finds nothing, for any instantiation in the constrained type's interface
// map that can be cast to it. Two equally specific bodies in the exact pass make the call throw,
// but in the variant pass the first one runs. Either way the body on the target's own interface
// definition is the target method itself, instantiated over the entry that supplied it.

using System;

public interface IName<out T>
{
    static virtual string Name() => typeof(T).Name;
}

// The exact `IName<object>` is in the map, so the exact pass finds it, although the
// `IName<string>` entry is variance-compatible too and is declared first.
public class ExactAndVariant : IName<string>, IName<object> { }

// `IName<int>` cannot be cast to `IName<object>`, so the variant pass sees only `IName<string>`.
public class OneCompatible : IName<int>, IName<string> { }

// Two entries that can both be cast to `IContraName<ArgumentException>`, and `IContraName<object>`
// can be cast to `IContraName<Exception>` besides. Neither counts as more specific: the first
// declared runs, without an `AmbiguousImplementationException`, for a class and a struct alike.
public interface IContraName<in T>
{
    static virtual string Name() => typeof(T).Name;
}

public class ObjectFirst : IContraName<object>, IContraName<Exception> { }
public class ExceptionFirst : IContraName<Exception>, IContraName<object> { }
public struct ObjectFirstStruct : IContraName<object>, IContraName<Exception> { }
public struct ExceptionFirstStruct : IContraName<Exception>, IContraName<object> { }

public static class Program
{
    static string Call<T>() where T : IName<object> => T.Name();
    static string CallContra<T>() where T : IContraName<ArgumentException> => T.Name();

    public static int Main(string[] argv)
    {
        if (Call<ExactAndVariant>() != "Object") return 1;
        if (Call<OneCompatible>() != "String") return 2;
        if (CallContra<ObjectFirst>() != "Object") return 3;
        if (CallContra<ExceptionFirst>() != "Exception") return 4;
        if (CallContra<ObjectFirstStruct>() != "Object") return 5;
        if (CallContra<ExceptionFirstStruct>() != "Exception") return 6;
        return 0;
    }
}
