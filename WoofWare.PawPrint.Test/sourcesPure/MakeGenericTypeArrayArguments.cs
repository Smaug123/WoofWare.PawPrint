using System;
using System.Collections.Generic;

// `Type.MakeGenericType` with more than one argument. `RuntimeType.MakeGenericType` takes the
// `Instantiate(RuntimeType)` fast path only for exactly one argument (RuntimeType.CoreCLR.cs:3613);
// otherwise it calls `Instantiate(Type[])` (RuntimeHandles.cs:769), which copies the argument
// handles into an `IntPtr[]` and hands the QCall a `fixed` pointer over it. That is the buffer
// shape System.Text.Json reaches when it closes its five-parameter
// `SmallObjectWithParameterizedConstructorConverter`. Every other `MakeGenericType` guest passes
// one argument and never sees this overload.
//
// Returns 0 on success, or the number of the first check that failed.
public class Five<A, B, C, D, E>
{
    public int Marker = 5;
}

public class Two<T, U>
    where T : struct
{
    public T First;
    public U Second;
}

public class Program
{
    public static int Main (string[] args)
    {
        // 1: the two-argument shape, the smallest that takes the array overload; the closed type is
        // the same object the compiler's own `typeof` names.
        Type dict = typeof (Dictionary<,>).MakeGenericType (typeof (string), typeof (int));

        if (!ReferenceEquals (dict, typeof (Dictionary<string, int>)))
            return 1;

        // 2: five distinct arguments, read back one by one so a stride that lands on the wrong
        // cell fails on a specific index rather than in aggregate.
        Type[] wanted = new[] { typeof (byte), typeof (string), typeof (object), typeof (long), typeof (Guid) };
        Type five = typeof (Five<,,,,>).MakeGenericType (wanted);
        Type[] got = five.GetGenericArguments ();

        if (got.Length != 5)
            return 2;

        for (int i = 0; i < 5; i++)
        {
            if (!ReferenceEquals (got[i], wanted[i]))
                return 10 + i;
        }

        if (!ReferenceEquals (five, typeof (Five<byte, string, object, long, Guid>)))
            return 2;

        // 3: a constraint violation through the same overload. The QCall reads the buffer before it
        // validates constraints, so the read has to work on the rejecting path too.
        bool rejected = false;

        try
        {
            typeof (Two<,>).MakeGenericType (typeof (string), typeof (int));
        }
        catch (ArgumentException)
        {
            rejected = true;
        }

        if (!rejected)
            return 3;

        // 4: the closed handle is a usable type, not merely a Type object.
        Type two = typeof (Two<,>).MakeGenericType (typeof (int), typeof (string));
        object instance = Activator.CreateInstance (two);

        if (!(instance is Two<int, string> typed))
            return 4;

        typed.First = 9;
        typed.Second = "nine";

        if (typed.First != 9 || typed.Second != "nine")
            return 4;

        object marker = Activator.CreateInstance (five);

        if (!(marker is Five<byte, string, object, long, Guid> fiveInstance) || fiveInstance.Marker != 5)
            return 4;

        return 0;
    }
}
