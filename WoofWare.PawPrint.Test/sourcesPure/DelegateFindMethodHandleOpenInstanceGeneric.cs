using System;
using System.Reflection;

// `Delegate.Method` on an *open* delegate over an instance method whose declaring type is a generic
// instantiation.
//
// `Delegate.GetMethodImpl` branches on `_methodPtrAux`: a nonzero one means an open delegate, whose
// declaring type is read off `Invoke`'s first parameter, and a zero one means a closed delegate,
// whose declaring type is recovered by walking `_target.GetType()`'s base chain when that type is
// generic (Delegate.CoreCLR.cs:189). An open delegate's `_target` is the delegate itself, so taking
// the closed branch for one would report the wrong declaring type, or fault if `_target` were null.
//
// `Delegate.CreateDelegate(Type, MethodInfo)` is the only route to an open instance delegate, since
// C# has no method-group syntax for one.
//
// Returns 0 on success, or the number of the first check that failed.

public class GenericHolder<T>
{
    public T Value;

    public GenericHolder (T value)
    {
        Value = value;
    }

    public string Describe (int n)
    {
        return Value.ToString () + n.ToString ();
    }
}

public class PlainHolder
{
    public string Describe (int n)
    {
        return "plain" + n.ToString ();
    }
}

public static class Program
{
    public static int Main ()
    {
        // 1: control. Off a *non*-generic declaring type neither branch of `GetMethodImpl` reads
        // anything but the handle.
        Func<PlainHolder, int, string> plain = (Func<PlainHolder, int, string>)
            typeof (PlainHolder).GetMethod ("Describe").CreateDelegate (typeof (Func<PlainHolder, int, string>));

        if (plain.Method.Name != "Describe" || plain.Method.DeclaringType != typeof (PlainHolder))
        {
            return 1;
        }

        Func<GenericHolder<string>, int, string> generic = (Func<GenericHolder<string>, int, string>)
            typeof (GenericHolder<string>)
                .GetMethod ("Describe")
                .CreateDelegate (typeof (Func<GenericHolder<string>, int, string>));

        // 2: binding and invoking the generic-declaring shape.
        if (generic (new GenericHolder<string> ("q"), 1) != "q1")
        {
            return 2;
        }

        // 3-4: `Method`, through the open branch.
        if (generic.Method.Name != "Describe")
        {
            return 3;
        }

        if (generic.Method.DeclaringType != typeof (GenericHolder<string>))
        {
            return 4;
        }

        return 0;
    }
}
