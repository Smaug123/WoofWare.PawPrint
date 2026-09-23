using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// `RuntimeMethodHandle.GetFunctionPointer`, the only guest route to the QCall
// `RuntimeMethodHandle_GetFunctionPointer`. CoreCLR answers `GetMultiCallableAddrOfCode`, which is
// also what the JIT asks for when it imports `ldftn`, so where C# can emit an `ldftn` (`&M`) the two
// must be the same address. Every pointer is also called through, so an address that compares
// right but names the wrong body still fails.
//
// Instance methods of a generic class instantiated over a reference type, and virtual methods of a
// generic value type instantiated over one, are deliberately absent: CoreCLR hands back one address
// for every instantiation sharing that code, and PawPrint refuses the shape.
// `MethodHandleGetFunctionPointerSharedCode.cs` covers it.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public class Base
{
    public int F = 10;

    public virtual int V() => 1;

    public int NonVirt() => F + 1;
}

public class Derived : Base
{
    public override int V() => 2;
}

public abstract class Abs
{
    public abstract int A();
}

public class Conc : Abs
{
    public override int A() => 9;
}

public interface I
{
    int M();

    static abstract int SA();
}

public interface IG<T>
{
    string Dim() => "Dim" + typeof(T).Name;
}

public interface IGAbstract<T>
{
    string M();
}

public class IGImpl<T> : IG<T>
{
}

public interface IBump
{
    int BumpVirtual();

    string Named<U>();
}

public struct S : IBump
{
    public int X;

    public int Bump()
    {
        X += 5;
        return X;
    }

    public int BumpVirtual()
    {
        X += 7;
        return X;
    }

    public string Named<U>() => typeof(U).Name + X;

    public override string ToString() => "S" + X;
}

public struct SWithCtor
{
    public int Value;

    public SWithCtor()
    {
        Value = 5;
    }
}

public struct SG<T>
{
    public int Count;

    public string Inst()
    {
        Count++;
        return typeof(T).Name;
    }

    public override string ToString() => typeof(T).Name + Count;
}

public class GC<T>
{
    public static string Name() => typeof(T).Name;

    public string Inst() => typeof(T).Name;

    public string InstGen<U>() => typeof(T).Name + typeof(U).Name;
}

public static unsafe class Program
{
    const BindingFlags All =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance;

    static int Stat(int x) => x + 1;

    static string Gen<T>() => typeof(T).Name;

    static IntPtr Fp(Type t, string name) => t.GetMethod(name, All).MethodHandle.GetFunctionPointer();

    static IntPtr FpGeneric(Type t, string name, Type arg) =>
        t.GetMethod(name, All).MakeGenericMethod(arg).MethodHandle.GetFunctionPointer();

    const string ContainsGenericVariablesMessage =
        "Could not execute the method because either the method itself or the containing type is not fully instantiated.";

    // 0 if `GetFunctionPointer` raises CoreCLR's `ContainsGenericVariables` exception, else 1.
    static int RefusesOpen(MethodBase method)
    {
        try
        {
            method.MethodHandle.GetFunctionPointer();
            return 1;
        }
        catch (InvalidOperationException e)
        {
            return e.Message == ContainsGenericVariablesMessage ? 0 : 1;
        }
    }

    public static int Main(string[] args)
    {
        // A static method: `ldftn` and `GetFunctionPointer` agree, and repeat.
        IntPtr stat = Fp(typeof(Program), "Stat");
        if ((IntPtr)(delegate*<int, int>)&Stat != stat) return 1;
        if (Fp(typeof(Program), "Stat") != stat) return 2;
        if (((delegate*<int, int>)stat)(4) != 5) return 3;

        // A generic method over a reference type. CoreCLR hands out an instantiating stub per
        // instantiation, the same one `ldftn` gets, so `Gen<object>` has an address of its own.
        IntPtr genString = FpGeneric(typeof(Program), "Gen", typeof(string));
        if ((IntPtr)(delegate*<string>)&Gen<string> != genString) return 4;
        if (((delegate*<string>)genString)() != "String") return 5;
        if (FpGeneric(typeof(Program), "Gen", typeof(object)) == genString) return 6;

        // A generic method over a value type.
        IntPtr genInt = FpGeneric(typeof(Program), "Gen", typeof(int));
        if ((IntPtr)(delegate*<string>)&Gen<int> != genInt) return 7;
        if (((delegate*<string>)genInt)() != "Int32") return 8;

        // A static method of a generic class, over a reference type and over a value type. The
        // former is shared code needing an instantiating stub, so it too is per-instantiation.
        IntPtr nameString = Fp(typeof(GC<string>), "Name");
        if ((IntPtr)(delegate*<string>)&GC<string>.Name != nameString) return 9;
        if (((delegate*<string>)nameString)() != "String") return 10;
        if (Fp(typeof(GC<object>), "Name") == nameString) return 11;
        IntPtr nameInt = Fp(typeof(GC<int>), "Name");
        if ((IntPtr)(delegate*<string>)&GC<int>.Name != nameInt) return 12;
        if (((delegate*<string>)nameInt)() != "Int32") return 13;

        // An instance method takes its receiver as the first argument.
        var b = new Base();
        if (((delegate*<Base, int>)Fp(typeof(Base), "NonVirt"))(b) != 11) return 14;

        // Reached through a derived type, an inherited method is still the one method.
        if (typeof(Derived).GetMethod("NonVirt").MethodHandle.GetFunctionPointer() != Fp(typeof(Base), "NonVirt"))
            return 15;

        // A virtual method's pointer names its own body and never dispatches: the base
        // declaration's runs on a derived receiver.
        var d = new Derived();
        IntPtr baseV = Fp(typeof(Base), "V");
        IntPtr derivedV = Fp(typeof(Derived), "V");
        if (baseV == derivedV) return 16;
        if (((delegate*<Base, int>)baseV)(d) != 1) return 17;
        if (((delegate*<Base, int>)derivedV)(d) != 2) return 18;

        // A value type's non-virtual instance method: the *unboxed* entry point, taking `this` by
        // reference, so its writes land in the caller's local.
        var s = new S { X = 1 };
        if (((delegate*<ref S, int>)Fp(typeof(S), "Bump"))(ref s) != 6) return 19;
        if (s.X != 6) return 20;

        // A value type's virtual method, an interface implementation included: reflection hands
        // out the unboxing stub, so the pointer is the *boxed* entry point, taking a box as `this`
        // and writing into it.
        object boxed = new S { X = 1 };
        if (((delegate*<object, int>)Fp(typeof(S), "BumpVirtual"))(boxed) != 8) return 21;
        if (((S)boxed).X != 8) return 22;
        if (((delegate*<object, string>)Fp(typeof(S), "ToString"))(boxed) != "S8") return 23;
        if (((delegate*<object, string>)FpGeneric(typeof(S), "Named", typeof(string)))(boxed) != "String8")
            return 24;
        object boxedGeneric = new SG<int> { Count = 3 };
        if (((delegate*<object, string>)Fp(typeof(SG<int>), "ToString"))(boxedGeneric) != "Int323") return 25;
        if (Fp(typeof(SG<long>), "ToString") == Fp(typeof(SG<int>), "ToString")) return 26;

        // Likewise a value type's constructor, the pointer `ActivatorCache` keeps in `_pfnValueCtor`.
        var withCtor = default(SWithCtor);
        ((delegate*<ref SWithCtor, void>)typeof(SWithCtor).GetConstructor(Type.EmptyTypes).MethodHandle
            .GetFunctionPointer())(ref withCtor);
        if (withCtor.Value != 5) return 27;

        // A reference type's constructor, run over an unconstructed instance.
        var uninit = (Base)RuntimeHelpers.GetUninitializedObject(typeof(Base));
        if (uninit.F != 0) return 28;
        ((delegate*<Base, void>)typeof(Base).GetConstructor(Type.EmptyTypes).MethodHandle.GetFunctionPointer())(uninit);
        if (uninit.F != 10) return 29;

        // A value type's method over a reference type is shared code needing an instantiating
        // stub (a byref `this` carries no type), so it is per-instantiation.
        var sg = new SG<string>();
        IntPtr sgString = Fp(typeof(SG<string>), "Inst");
        if (((delegate*<ref SG<string>, string>)sgString)(ref sg) != "String") return 30;
        if (sg.Count != 1) return 31;
        if (Fp(typeof(SG<object>), "Inst") == sgString) return 32;

        // A default interface method over a reference type: per-instantiation, for the same reason.
        IntPtr dimString = Fp(typeof(IG<string>), "Dim");
        if (((delegate*<IG<string>, string>)dimString)(new IGImpl<string>()) != "DimString") return 33;
        if (Fp(typeof(IG<object>), "Dim") == dimString) return 34;

        // So is an abstract method of a generic interface over a reference type. It has no code
        // to share, and `RequiresInstArg` exempts it, but reflection hands out an instantiating
        // stub for every non-generic method of a generic interface regardless.
        if (Fp(typeof(IGAbstract<string>), "M") == Fp(typeof(IGAbstract<object>), "M")) return 35;

        // A generic method of a generic class, both over reference types: per-instantiation.
        IntPtr instGen = FpGeneric(typeof(GC<string>), "InstGen", typeof(string));
        if (((delegate*<GC<string>, string>)instGen)(new GC<string>()) != "StringString") return 36;
        if (FpGeneric(typeof(GC<string>), "InstGen", typeof(object)) == instGen) return 37;

        // Methods with no body still have an address, one per method.
        IntPtr abs = Fp(typeof(Abs), "A");
        if (abs == IntPtr.Zero) return 38;
        if (Fp(typeof(Abs), "A") != abs) return 39;
        if (Fp(typeof(Conc), "A") == abs) return 40;
        if (Fp(typeof(I), "M") == IntPtr.Zero) return 41;
        if (Fp(typeof(I), "SA") == IntPtr.Zero) return 42;

        // Anything still naming a generic variable has no code to point at.
        if (RefusesOpen(typeof(GC<>).GetMethod("Name")) != 0) return 43;
        if (RefusesOpen(typeof(GC<>).GetMethod("Inst")) != 0) return 44;
        if (RefusesOpen(typeof(Program).GetMethod("Gen", All)) != 0) return 45;
        if (RefusesOpen(typeof(GC<int>).GetMethod("InstGen")) != 0) return 46;
        if (RefusesOpen(typeof(GC<>).GetConstructor(Type.EmptyTypes)) != 0) return 47;

        return 0;
    }
}
