using System;
using System.Reflection;

// `CreateDelegate` over a method whose declaring type is an open generic *definition* —
// `typeof(G<>).GetMethod("M")`. Legal to ask, and real .NET answers, but with an exception in both
// directions rather than a delegate.
//
// A static target reaches `MethodDesc::TryGetMultiCallableAddrOfCode` (method.cpp:2091-2093) while
// `BindToMethod` is looking for a code address, and gets `InvalidOperationException` with
// `IDS_EE_CODEEXECUTION_CONTAINSGENERICVAR` — a native throw, not a managed prologue check. An
// instance target instead fails the compatibility check and comes back as a bind failure, which the
// managed caller turns into `ArgumentException`.
//
// PawPrint refuses both earlier, in `requireClosedDeclaringType`: a target's signature has to be
// read against an exact instantiation, and a definition has none — reflection over one names its
// parameters with `RuntimeTypeHandleTarget.GenericParameter`, which no `ConcreteTypeHandle` can
// stand in for. Un-park when a method handle can carry a formal declaring context, which is the
// same representation `MakeGenericMethodOnOpenDefinition.cs` waits on.
//
// Returns 0 on success, or the number of the first check that failed.

public class OpenDefnHolder<T>
{
    public string Instance (int n)
    {
        return n.ToString ();
    }

    public static string Static (int n)
    {
        return n.ToString ();
    }
}

public static class Program
{
    public static int Main ()
    {
        // 1: controls. The closed instantiation's methods bind and run, so what follows is about
        // the definition rather than about these signatures.
        Func<int, string> closedStatic = (Func<int, string>)
            typeof (OpenDefnHolder<string>).GetMethod ("Static").CreateDelegate (typeof (Func<int, string>));

        if (closedStatic (7) != "7")
        {
            return 1;
        }

        Func<int, string> closedInstance = (Func<int, string>)
            typeof (OpenDefnHolder<string>)
                .GetMethod ("Instance")
                .CreateDelegate (typeof (Func<int, string>), new OpenDefnHolder<string> ());

        if (closedInstance (8) != "8")
        {
            return 2;
        }

        MethodInfo definitionStatic = typeof (OpenDefnHolder<>).GetMethod ("Static");
        MethodInfo definitionInstance = typeof (OpenDefnHolder<>).GetMethod ("Instance");

        // 3: the lookups themselves succeed, so the refusal is about binding.
        if (definitionStatic == null || definitionInstance == null)
        {
            return 3;
        }

        // 4: a static target dies looking for a code address.
        try
        {
            definitionStatic.CreateDelegate (typeof (Func<int, string>));
            return 4;
        }
        catch (InvalidOperationException)
        {
        }

        // 5: an instance target is instead reported as a bind failure.
        try
        {
            definitionInstance.CreateDelegate (typeof (Func<int, string>));
            return 5;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}
