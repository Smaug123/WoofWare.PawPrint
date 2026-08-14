using System;
using System.Reflection.Emit;

// A delegate type with a byref parameter, which C# has no built-in name for. It is here to reach
// `IsLocationAssignable`'s byref rule (comdelegate.cpp:2382-2384): a byref forces strict matching,
// so `int&` and `int` are incompatible in both directions even though relaxed matching is on.
public delegate int RefIntToInt(ref int x);

public class Program
{
    // `Delegate_BindToMethodInfo` decides whether a target method can back a delegate type, and if
    // so whether the delegate is open (Invoke supplies every argument) or closed (Invoke supplies
    // one fewer, and the missing first argument is bound now). This walks the shapes a
    // `DynamicMethod` can actually produce, positive and negative.
    //
    // Every body here is token-free — `ldarg`, `ldnull`, `ldind`, `ret` — because a dynamic method
    // whose IL carries a metadata token is refused when it is minted: those operands name entries
    // in the method's `DynamicScope`, not rows in this assembly. Binding does not execute the body,
    // so what it contains is irrelevant to what is being tested; it only has to be something
    // `GetILGenerator` will bake, since `GetMethodDescriptor` refuses an empty body.
    //
    // Returns 0 on success, or the number of the first check that failed. Impure because PawPrint
    // declares dynamic code unsupported by default and the harness registers this case with the
    // switch overridden — see `DynamicCodeSupportedOverride.cs` for that contract.

    /// `(int) -> int`, returning its argument.
    private static DynamicMethod IntToInt()
    {
        DynamicMethod dm = new DynamicMethod("Probe", typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// `(string, int) -> int`, returning its second argument. One argument wider than
    /// `Func&lt;int, int&gt;`, so binding it to that delegate must close over the first.
    private static DynamicMethod StringIntToInt()
    {
        DynamicMethod dm = new DynamicMethod(
            "Probe",
            typeof(int),
            new Type[] { typeof(string), typeof(int) },
            typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// `(object, int) -> int`. Same shape as the above, but the bound argument's declared type is
    /// wider than the object supplied, so the first-argument check has to do real work rather than
    /// finding the two types identical.
    private static DynamicMethod ObjectIntToInt()
    {
        DynamicMethod dm = new DynamicMethod(
            "Probe",
            typeof(int),
            new Type[] { typeof(object), typeof(int) },
            typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// `(int) -> string`. The return type is narrower than `Func&lt;int, object&gt;`'s, which is
    /// legal: return types are matched callee-to-caller, the opposite direction from arguments.
    private static DynamicMethod IntToString()
    {
        DynamicMethod dm = new DynamicMethod("Probe", typeof(string), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldnull);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// Did binding fail the way an incompatible signature must fail? `CreateDelegate` turns the
    /// QCall's FALSE into `ArgumentException` (Delegate.CoreCLR.cs:391), so a refusal that arrives
    /// as anything else — including a host crash — is a different answer and fails the check.
    private static bool Rejects(DynamicMethod dm, Type delegateType)
    {
        try
        {
            dm.CreateDelegate(delegateType);
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }

    public static int Main(string[] args)
    {
        DynamicMethod intToInt = IntToInt();

        // Open: `Func<int, int>` supplies the one argument the target takes.
        Delegate open = intToInt.CreateDelegate(typeof(Func<int, int>));
        if (open == null)
        {
            return 1;
        }

        if (open.Target != null)
        {
            return 2;
        }

        // Not a check on this QCall's field writes: `DynamicMethod.CreateDelegate` stashes itself
        // into `_methodBase` in managed code immediately afterwards (DynamicMethod.CoreCLR.cs:60),
        // and `Delegate.GetMethodImpl` hands that straight back. It is here because it pins that
        // binding left the delegate in a state where the managed accessors work at all.
        if (!ReferenceEquals(open.Method, intToInt))
        {
            return 3;
        }

        // Closed over a string: the target takes one more argument than `Invoke`, so the first is
        // bound now. Reference equality, not just non-null: a handler that stored some other
        // object — or the delegate itself, as CoreCLR's *open* path does — would pass a null check.
        DynamicMethod stringIntToInt = StringIntToInt();
        string bound = "hello";
        Delegate closed = stringIntToInt.CreateDelegate(typeof(Func<int, int>), bound);
        if (!ReferenceEquals(closed.Target, bound))
        {
            return 4;
        }

        // Closed over null. This is the case that separates "the shape comes from the arity" from
        // "the shape comes from whether a target was supplied": both classifications accept the
        // binding, and under PawPrint's field convention both produce a delegate whose `_target` is
        // null, so nothing observable here distinguishes them *yet* — but the arity is the only
        // thing that will tell invocation how many arguments to shuffle.
        Delegate closedOverNull = stringIntToInt.CreateDelegate(typeof(Func<int, int>), null);
        if (closedOverNull == null)
        {
            return 5;
        }

        if (closedOverNull.Target != null)
        {
            return 6;
        }

        // Closed over a string where the target declares `object`: the bound argument arrived as an
        // `object`, so it is already boxed and the objref-ness check is skipped.
        Delegate closedWidening = ObjectIntToInt().CreateDelegate(typeof(Func<int, int>), bound);
        if (!ReferenceEquals(closedWidening.Target, bound))
        {
            return 7;
        }

        // Covariant return: the target returns `string` where `Invoke` returns `object`.
        if (IntToString().CreateDelegate(typeof(Func<int, object>)) == null)
        {
            return 8;
        }

        // Arity out of whack: three arguments supplied, one taken. Neither open nor closed, which
        // is a different rejection from the two below and worth having on its own.
        if (!Rejects(intToInt, typeof(Func<int, int, int, int>)))
        {
            return 9;
        }

        // One argument taken, none supplied, so this classifies as *closed* — and is then rejected
        // by the rule that a delegate closed over a static method's first argument needs that
        // argument to be an object reference, which `int` is not.
        if (!Rejects(intToInt, typeof(Action)))
        {
            return 10;
        }

        // Argument type mismatch, both directions of the assignability check.
        if (!Rejects(intToInt, typeof(Func<string, int>)))
        {
            return 11;
        }

        if (!Rejects(intToInt, typeof(Func<int, string>)))
        {
            return 12;
        }

        // `int&` supplied where the target declares `int`. The byref is on the delegate's side,
        // which is why this check is possible at all: a dynamic method with a byref *parameter*
        // would need `typeof(int).MakeByRefType()`, and `RuntimeTypeHandle_MakeByRef` is not
        // implemented (measured — it is where an earlier draft of this file stopped).
        if (!Rejects(intToInt, typeof(RefIntToInt)))
        {
            return 13;
        }

        return 0;
    }
}
