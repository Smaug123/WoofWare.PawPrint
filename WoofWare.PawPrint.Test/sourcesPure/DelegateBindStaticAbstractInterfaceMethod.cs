using System;
using System.Reflection;

// `CreateDelegate` over a *static abstract* interface method — the one shape that is both static
// and virtual, so `MethodInfo.DispatchesVirtually` (which folds `not IsStatic` in) says false about
// it while CoreCLR's `pTargetMethod->IsVirtual()` says true.
//
// Measured on real .NET: the binding *succeeds*, taking `BindToMethod`'s virtual-call-stub branch
// (comdelegate.cpp:1237) because the declaring type is not a value type, and the resulting delegate
// raises `EntryPointNotFoundException` when invoked — a stub over a method with no body.
// PawPrint refuses the binding instead: it writes no `_methodPtrAux`, and an abstract target has no
// body for `_methodPtr` to name, so there is nothing honest to store.
//
// Un-park when a delegate can hold a target that has no body — at which point the answer is to
// bind, and to raise `EntryPointNotFoundException` from the invocation rather than from here.
//
// Returns 0 on success, or the number of the first check that failed.

public interface IStaticAbstract
{
    static abstract string Describe ();
}

public class StaticAbstractImpl : IStaticAbstract
{
    public static string Describe ()
    {
        return "impl";
    }
}

public static class Program
{
    public static int Main ()
    {
        // 1: control. The *implementation* is an ordinary static method, and binding it works.
        Func<string> onImpl = (Func<string>)
            typeof (StaticAbstractImpl).GetMethod ("Describe").CreateDelegate (typeof (Func<string>));

        if (onImpl () != "impl")
        {
            return 1;
        }

        MethodInfo declaration = typeof (IStaticAbstract).GetMethod ("Describe");

        // 2: the declaration really is the both-static-and-virtual shape, so the refusal is about
        // that and not about something else on this path.
        if (!declaration.IsStatic || !declaration.IsVirtual || !declaration.IsAbstract)
        {
            return 2;
        }

        // 3: binding the declaration succeeds.
        Func<string> onDeclaration = (Func<string>) declaration.CreateDelegate (typeof (Func<string>));

        if (onDeclaration == null)
        {
            return 3;
        }

        // 4: and `Target`/`Method` describe it as an open delegate over the declaration.
        if (onDeclaration.Target != null || onDeclaration.Method.DeclaringType != typeof (IStaticAbstract))
        {
            return 4;
        }

        // 5: invoking it is what fails, because the stub has no body to reach.
        try
        {
            onDeclaration ();
            return 5;
        }
        catch (EntryPointNotFoundException)
        {
        }

        return 0;
    }
}
