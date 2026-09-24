using System;
using System.Collections.Generic;

// `RuntimeMethodHandle.GetFunctionPointer` on an instance method of a generic type whose
// instantiation CoreCLR shares with others, where the entry point reads its type context from the
// receiver: a method of a class, which needs no instantiating stub (`MethodDesc::RequiresInstArg` is
// false), or a virtual method of a value type, whose unboxing stub reads it from the box. CoreCLR
// hands back one address for every instantiation sharing the code: the same for `GC<string>.Inst`
// as for `GC<object>.Inst`, and calling it on a `GC<string>` receiver answers for `string` whichever
// instantiation the pointer was asked for.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public class GC<T>
{
    public string Inst() => typeof(T).Name;

    public virtual string V() => "V" + typeof(T).Name;
}

public abstract class AbsG<T>
{
    public abstract string A();
}

public interface IName
{
    string Name();
}

public struct SV<T> : IName
{
    public int X;

    public string Name() => typeof(T).Name + X;
}

public static unsafe class Program
{
    static IntPtr Fp(Type t, string name) => t.GetMethod(name).MethodHandle.GetFunctionPointer();

    public static int Main(string[] args)
    {
        IntPtr instObject = Fp(typeof(GC<object>), "Inst");
        if (Fp(typeof(GC<string>), "Inst") != instObject) return 1;
        if (((delegate*<object, string>)instObject)(new GC<string>()) != "String") return 2;

        IntPtr vObject = Fp(typeof(GC<object>), "V");
        if (Fp(typeof(GC<string>), "V") != vObject) return 3;
        if (((delegate*<object, string>)vObject)(new GC<string>()) != "VString") return 4;

        // A value type is shared exactly when one of its own type arguments is.
        if (Fp(typeof(GC<KeyValuePair<string, int>>), "Inst") != Fp(typeof(GC<KeyValuePair<object, int>>), "Inst"))
            return 5;

        if (typeof(GC<string>).GetConstructor(Type.EmptyTypes).MethodHandle.GetFunctionPointer()
            != typeof(GC<object>).GetConstructor(Type.EmptyTypes).MethodHandle.GetFunctionPointer())
            return 6;

        // An abstract method has no code at all, but its address is still the shared one.
        if (Fp(typeof(AbsG<string>), "A") != Fp(typeof(AbsG<object>), "A")) return 7;

        // A value type's virtual method answers its unboxing stub, which reads the instantiation
        // from the box, so it too is shared.
        IntPtr nameObject = Fp(typeof(SV<object>), "Name");
        if (Fp(typeof(SV<string>), "Name") != nameObject) return 8;
        if (((delegate*<object, string>)nameObject)(new SV<string> { X = 4 }) != "String4") return 9;

        return 0;
    }
}
