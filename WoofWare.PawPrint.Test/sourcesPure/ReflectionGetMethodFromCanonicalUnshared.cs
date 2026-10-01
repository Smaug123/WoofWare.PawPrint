using System;
using System.Reflection;

public class Holder<T>
{
    internal static int Plain(T t) => 0;
}

public static class Program
{
    const BindingFlags NonPublicStatic = BindingFlags.NonPublic | BindingFlags.Static;

    static readonly Type InternalHandle =
        typeof(RuntimeMethodHandle).Assembly.GetType("System.RuntimeMethodHandleInternal", true);

    // `RuntimeMethodHandle.GetMethodFromCanonical`, invoked by private reflection so that nothing
    // rebinds its answer onto an exact type the way its one CoreLib caller, `RuntimeType.GetMethodBase`,
    // does. CoreCLR answers with the method on the named type's *canonical* method table, which
    // for `Holder<string>` is `Holder<System.__Canon>`. For the types named here the canonical
    // method table is the type itself: `Holder<int>` has no shareable argument, and the generic
    // definition `Holder<>` is its own typical instantiation (measured on CoreCLR). So the answer
    // is exactly the handle reflection hands out for that type's own `Plain`.
    static IntPtr Parallel(MethodInfo method, Type named)
    {
        MethodInfo fromCanonical = typeof(RuntimeMethodHandle).GetMethod(
            "GetMethodFromCanonical", NonPublicStatic, null, new[] { InternalHandle, named.GetType() }, null);

        object input = Activator.CreateInstance(
            InternalHandle, BindingFlags.NonPublic | BindingFlags.Instance, null,
            new object[] { method.MethodHandle.Value }, null);

        object answer = fromCanonical.Invoke(null, new object[] { input, named });

        return (IntPtr)InternalHandle
            .GetProperty("Value", BindingFlags.NonPublic | BindingFlags.Instance)
            .GetValue(answer);
    }

    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo plainOfString = typeof(Holder<string>).GetMethod("Plain", NonPublicStatic);

        if (Parallel(plainOfString, typeof(Holder<int>))
            != typeof(Holder<int>).GetMethod("Plain", NonPublicStatic).MethodHandle.Value)
            return 1;

        if (Parallel(plainOfString, typeof(Holder<>))
            != typeof(Holder<>).GetMethod("Plain", NonPublicStatic).MethodHandle.Value)
            return 2;

        return 0;
    }
}
