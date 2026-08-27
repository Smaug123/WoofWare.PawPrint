using System;
using System.Reflection;

public class Holder<T>
{
    public T Value;

    public T Get() => Value;

    public U Convert<U>(T value) => default(U);
}

public struct SHolder<T>
{
    public T Value;

    public T Get() => Value;

    public override string ToString() => "SHolder";
}

public interface IFace<T>
{
    T Read();
}

public static class Program
{
    // `MethodBase.GetMethodFromHandle(handle, typeHandle)` re-reflects a method handle through a
    // *different* instantiation of its declaring generic type. That is the shape that reaches
    // CoreCLR's `RuntimeMethodHandle.GetMethodFromCanonical`: `RuntimeType.GetMethodBase` sees
    // `reflectedType != declaredType`, walks the base chain to the instantiation whose generic
    // definition matches the handle's declaring definition, and asks for that type's parallel
    // MethodDesc before `GetStubIfNeeded` binds the exact one.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        RuntimeMethodHandle getOfInt = typeof(Holder<int>).GetMethod("Get").MethodHandle;

        MethodBase remapped =
            MethodBase.GetMethodFromHandle(getOfInt, typeof(Holder<string>).TypeHandle);
        if (remapped.DeclaringType != typeof(Holder<string>)) return 1;
        if (remapped.Name != "Get") return 2;
        if (((MethodInfo)remapped).ReturnType != typeof(string)) return 3;

        // The remap lands on the very MethodInfo the reflected type's own cache holds, so the
        // handle it carries is the one that cache keys on rather than merely an equal-looking one.
        if (!ReferenceEquals(remapped, typeof(Holder<string>).GetMethod("Get"))) return 4;

        // A *generic method* definition: `GetStubIfNeeded` returns early for a method that has its
        // own instantiation, so nothing downstream re-derives the answer and the remap itself is
        // what the guest ends up holding.
        RuntimeMethodHandle convertOfInt = typeof(Holder<int>).GetMethod("Convert").MethodHandle;

        MethodBase remappedConvert =
            MethodBase.GetMethodFromHandle(convertOfInt, typeof(Holder<string>).TypeHandle);
        if (!ReferenceEquals(remappedConvert, typeof(Holder<string>).GetMethod("Convert"))) return 5;

        // A value type: CoreCLR's canonical method table for a struct instantiation is the
        // instantiation itself rather than a shared one, and a virtual on a value type carries an
        // unboxing stub. Neither distinction exists in PawPrint, which shares no generic code.
        RuntimeMethodHandle sGetOfInt = typeof(SHolder<int>).GetMethod("Get").MethodHandle;

        MethodBase remappedSGet =
            MethodBase.GetMethodFromHandle(sGetOfInt, typeof(SHolder<string>).TypeHandle);
        if (!ReferenceEquals(remappedSGet, typeof(SHolder<string>).GetMethod("Get"))) return 6;

        RuntimeMethodHandle sToStringOfInt = typeof(SHolder<int>).GetMethod("ToString").MethodHandle;

        MethodBase remappedSToString =
            MethodBase.GetMethodFromHandle(sToStringOfInt, typeof(SHolder<string>).TypeHandle);
        if (remappedSToString.DeclaringType != typeof(SHolder<string>)) return 7;

        // An interface, whose base-chain walk terminates immediately.
        RuntimeMethodHandle readOfInt = typeof(IFace<int>).GetMethod("Read").MethodHandle;

        MethodBase remappedRead =
            MethodBase.GetMethodFromHandle(readOfInt, typeof(IFace<string>).TypeHandle);
        if (!ReferenceEquals(remappedRead, typeof(IFace<string>).GetMethod("Read"))) return 8;

        // A constructor, which reaches the other arm of `GetMethodBase`.
        RuntimeMethodHandle ctorOfInt = typeof(Holder<int>).GetConstructor(Type.EmptyTypes).MethodHandle;

        MethodBase remappedCtor =
            MethodBase.GetMethodFromHandle(ctorOfInt, typeof(Holder<string>).TypeHandle);
        if (!ReferenceEquals(remappedCtor, typeof(Holder<string>).GetConstructor(Type.EmptyTypes))) return 9;

        // Control: remapping onto the instantiation the handle already names changes nothing.
        MethodBase same = MethodBase.GetMethodFromHandle(getOfInt, typeof(Holder<int>).TypeHandle);
        if (!ReferenceEquals(same, typeof(Holder<int>).GetMethod("Get"))) return 10;

        return 0;
    }
}
