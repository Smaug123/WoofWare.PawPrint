using System;
using System.Reflection;

public class Holder<T>
{
    internal static U Pair<U>(T t, U u) => u;

    internal static int Plain(T t) => 0;
}

public static class Program
{
    const BindingFlags NonPublicStatic = BindingFlags.NonPublic | BindingFlags.Static;

    static readonly Type IRuntimeMethodInfo =
        typeof(RuntimeMethodHandle).Assembly.GetType("System.IRuntimeMethodInfo", true);

    static readonly Type InternalHandle =
        typeof(RuntimeMethodHandle).Assembly.GetType("System.RuntimeMethodHandleInternal", true);

    // `RuntimeMethodHandle.StripMethodInstantiation(IRuntimeMethodInfo)`, invoked by private
    // reflection so that nothing rebinds its answer onto an exact declaring type the way
    // `MethodInfo.GetGenericMethodDefinition` does. CoreCLR takes the stripped method from the
    // declaring type's *canonical* method table; for a method on the generic definition `Holder<>`
    // that table is `Holder<>`'s own typical instantiation (measured on CoreCLR). So the answer is
    // exactly the handle reflection hands out for the typical method definition.
    static object Strip(MethodInfo method)
    {
        MethodInfo strip = typeof(RuntimeMethodHandle).GetMethod(
            "StripMethodInstantiation", NonPublicStatic, null, new[] { IRuntimeMethodInfo }, null);

        return strip.Invoke(null, new object[] { method });
    }

    static IntPtr HandleOf(object runtimeMethodInfo)
    {
        object handle = IRuntimeMethodInfo.GetProperty("Value").GetValue(runtimeMethodInfo);

        return (IntPtr)InternalHandle
            .GetProperty("Value", BindingFlags.NonPublic | BindingFlags.Instance)
            .GetValue(handle);
    }

    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo pair = typeof(Holder<>).GetMethod("Pair", NonPublicStatic);
        MethodInfo plain = typeof(Holder<>).GetMethod("Plain", NonPublicStatic);

        // A method instantiation over the open definition: stripped onto the typical definition.
        if (HandleOf(Strip(pair.MakeGenericMethod(typeof(int)))) != pair.MethodHandle.Value) return 1;
        if (HandleOf(Strip(pair.MakeGenericMethod(typeof(string)))) != pair.MethodHandle.Value) return 2;

        // Already definitions: CoreCLR leaves the reflection object it was given in place.
        if (!ReferenceEquals(Strip(pair), pair)) return 3;
        if (!ReferenceEquals(Strip(plain), plain)) return 4;

        return 0;
    }
}
