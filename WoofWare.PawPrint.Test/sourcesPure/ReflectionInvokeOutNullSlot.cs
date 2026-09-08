using System;
using System.Reflection;

// `MethodBase.Invoke` with a *null* in the `object[]` slot of an `out` value-type parameter.
// `RuntimeType.TryChangeType` (RuntimeType.cs:946) fills the slot with `default(T)` via
// `RuntimeType.AllocateValueType(elementType, null)`, which is `RuntimeHelpers.GetUninitializedObject`,
// before the byref is formed. The byref pass-through itself is covered by
// `ReflectionInvokeByRefParameter.cs`; this file is only about the allocation that precedes it.
//
// Returns 0 on success, or the number of the first check that failed.
public class Program
{
    private static void SetSeven (out int x)
    {
        x = 7;
    }

    public static int Main (string[] args)
    {
        MethodInfo tryParse =
            typeof (int).GetMethod ("TryParse", new[] { typeof (string), typeof (int).MakeByRefType () });

        if (tryParse == null)
            return 1;

        // 1: a corelib target, the `out` slot null.
        object[] parseArgs = new object[] { "5", null };
        object parsed = tryParse.Invoke (null, parseArgs);

        if (!(parsed is bool ok) || !ok || !(parseArgs[1] is int parsedValue) || parsedValue != 5)
            return 1;

        // 2: a user target that never reads the slot, so only the allocation and the write are
        // exercised.
        object[] setArgs = new object[] { null };

        typeof (Program)
            .GetMethod ("SetSeven", BindingFlags.Static | BindingFlags.NonPublic)
            .Invoke (null, setArgs);

        if (!(setArgs[0] is int seven) || seven != 7)
            return 2;

        return 0;
    }
}
