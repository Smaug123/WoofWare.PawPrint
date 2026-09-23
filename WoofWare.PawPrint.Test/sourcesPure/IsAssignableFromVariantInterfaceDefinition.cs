using System;
using System.Collections.Generic;

public interface IIn<in T> where T : class { }

public class ClassIn : IIn<object> { }

public struct StructIn : IIn<object> { }

public interface IInterfaceIn : IIn<object> { }

public class Program
{
    // `typeof(IIn<>)` is the typical instantiation `IIn<T>`, a real MethodTable that a closed type
    // can cast to by contravariance when `T : class`. CoreCLR also uses that MethodTable as the
    // "special marker" in a value type's or interface's compressed interface map, and so refuses to
    // scan such a map for it; only a class, or the instantiation itself, is accepted.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        if (!typeof(IIn<>).IsAssignableFrom(typeof(ClassIn))) return 1;
        if (!typeof(IIn<>).IsAssignableFrom(typeof(IIn<object>))) return 2;
        if (typeof(IIn<>).IsAssignableFrom(typeof(StructIn))) return 3;
        if (typeof(IIn<>).IsAssignableFrom(typeof(IInterfaceIn))) return 4;
        if (typeof(IIn<>).IsAssignableFrom(typeof(IIn<string>))) return 5;
        // An open interface with no interface that could match, which is false however CoreCLR
        // treats its interface map.
        if (typeof(IEnumerable<>).IsAssignableFrom(typeof(IComparer<>))) return 6;
        return 0;
    }
}
