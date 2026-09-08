using System;
using System.Reflection;

// `MethodBase.Invoke` with a `ref` parameter whose element is a struct holding a reference.
// Before the byref is formed, `MethodBaseInvoker.CheckArguments` copies the boxed argument with
// `RuntimeHelpers.Box(ref GetRawData(box), handle)` (`TryByRefFastPath`, MethodBaseInvoker.cs:391),
// which is where a reference-holding struct stops today. The pass-through of the byref itself,
// and a reference-free struct through it, are covered by `ReflectionInvokeByRefParameter.cs`.
//
// Returns 0 on success, or the number of the first check that failed.
public struct WithRef
{
    public string Name;
    public int N;
}

public class Program
{
    private static void Fill (ref WithRef p)
    {
        p.Name = "filled";
        p.N = p.N + 5;
    }

    public static int Main (string[] args)
    {
        WithRef value = new WithRef
        {
            Name = null,
            N = 1,
        };
        object box = value;
        object[] fillArgs = new object[] { box };

        typeof (Program)
            .GetMethod ("Fill", BindingFlags.Static | BindingFlags.NonPublic)
            .Invoke (null, fillArgs);

        // 1: both fields written through the byref, in the array's (copied) box.
        if (!(fillArgs[0] is WithRef filled) || filled.Name != "filled" || filled.N != 6)
            return 1;

        // 2: the caller's own box is untouched.
        if (((WithRef) box).Name != null || ((WithRef) box).N != 1)
            return 2;

        return 0;
    }
}
