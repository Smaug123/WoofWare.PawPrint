using System;
using System.Reflection;

// `Delegate.Method` on an *open* delegate over an instance method whose declaring type is a generic
// instantiation. Binding it works; asking which method it points at does not.
//
// `Delegate.GetMethodImpl` branches on `_methodPtrAux`: a nonzero one means an open delegate, whose
// target is read straight off the handle, and a zero one means a closed delegate, whose declaring
// type is recovered by walking `_target.GetType()`'s base chain when that type is generic
// (Delegate.CoreCLR.cs:189). PawPrint writes no `_methodPtrAux`, so CoreLib takes the closed branch
// and dereferences a null `_target`; `Delegate_FindMethodHandle` refuses rather than hand back a
// method that would then fault inside CoreLib.
//
// This became reachable when `Delegate_BindToMethodInfo` learned to bind a metadata method:
// `Delegate.CreateDelegate(Type, MethodInfo)` is the only route to an open instance delegate, since
// C# has no method-group syntax for one. Un-park with issue #959, which is the same
// `_methodPtrAux`/`_invocationCount` representation gap.
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
        // 1: control. Off a *non*-generic declaring type CoreLib's closed branch never walks a base
        // chain, so a null `_target` is harmless and the QCall answers.
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

        // 2: binding and invoking the generic-declaring shape works; only `Method` does not.
        if (generic (new GenericHolder<string> ("q"), 1) != "q1")
        {
            return 2;
        }

        // 3: and this is what stops.
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
