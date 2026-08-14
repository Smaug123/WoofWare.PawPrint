using System;
using System.Reflection.Emit;

public class Program
{
    // The only guest-visible window onto what `Delegate_BindToMethodInfo` wrote into `_methodPtr`.
    //
    // `Delegate.Equals` starts with an optimistic all-fields check —
    // `_target == d._target && _methodPtr == d._methodPtr && _methodPtrAux == d._methodPtrAux`
    // (Delegate.CoreCLR.cs:96) — and only falls back to comparing `_methodBase` if that fails. So
    // two delegates over *distinct* dynamic methods take the early TRUE if and only if the handler
    // gave both the same `_methodPtr`. That is what check 1 kills: an implementation storing one
    // shared sentinel there passes every check in `DynamicMethodDelegateBinding.cs`, because
    // nothing in that file compares two delegates.
    //
    // Distinctness is the interesting property because it is the one a naive identity would get
    // wrong. The two methods below agree on name, signature and module; CoreCLR still gives them
    // different `DynamicMethodDesc` addresses, and PawPrint's registry id is the projection of that
    // — which is why `DynamicMethodHandle` deliberately carries nothing descriptive.
    //
    // Impure for the same reason as its sibling: the dynamic-code switch is overridden by the
    // harness registration.

    private static DynamicMethod IntToInt()
    {
        DynamicMethod dm = new DynamicMethod("Probe", typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    public static int Main(string[] args)
    {
        DynamicMethod first = IntToInt();
        DynamicMethod second = IntToInt();

        Delegate fromFirst = first.CreateDelegate(typeof(Func<int, int>));
        Delegate fromSecond = second.CreateDelegate(typeof(Func<int, int>));
        Delegate fromFirstAgain = first.CreateDelegate(typeof(Func<int, int>));

        // Two indistinguishable-looking dynamic methods are still two methods.
        if (fromFirst.Equals(fromSecond))
        {
            return 1;
        }

        // ...and one dynamic method bound twice is one method, so the two delegates are equal even
        // though they are separate objects. This is the direction that fails if the handler minted
        // a *fresh* identity per binding rather than reading the method's own.
        if (!fromFirst.Equals(fromFirstAgain))
        {
            return 2;
        }

        if (ReferenceEquals(fromFirst, fromFirstAgain))
        {
            return 3;
        }

        return 0;
    }
}
