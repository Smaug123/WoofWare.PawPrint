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

    /// `(int, int) -> int`. One argument wider than `Func&lt;int, int&gt;`, like
    /// <see cref="StringIntToInt" />, but with a first parameter that is *not* an object
    /// reference — which is the only thing that stops it binding.
    private static DynamicMethod IntIntToInt()
    {
        DynamicMethod dm = new DynamicMethod(
            "Probe",
            typeof(int),
            new Type[] { typeof(int), typeof(int) },
            typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// `(object) -> int`. Its parameter is wider than anything a delegate can supply without
    /// boxing, which is what makes it the probe for the objref-ness rule.
    private static DynamicMethod ObjectToInt()
    {
        DynamicMethod dm = new DynamicMethod("Probe", typeof(int), new Type[] { typeof(object) }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4_0);
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

    /// As above, for the overload that supplies a bound first argument.
    private static bool Rejects(DynamicMethod dm, Type delegateType, object target)
    {
        try
        {
            dm.CreateDelegate(delegateType, target);
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

        // Closed over a *boxed value type*, again where the target declares `object`. This is the
        // case that distinguishes "the bound argument arrived boxed" from "the two types are both
        // object references": `int` is not an object reference, so an implementation that dropped
        // the boxed flag and compared objref-ness would refuse this binding, which real .NET
        // accepts.
        object boxed = 42;
        Delegate closedOverBoxed = ObjectIntToInt().CreateDelegate(typeof(Func<int, int>), boxed);
        if (!ReferenceEquals(closedOverBoxed.Target, boxed))
        {
            return 8;
        }

        // Covariant return: the target returns `string` where `Invoke` returns `object`.
        if (IntToString().CreateDelegate(typeof(Func<int, object>)) == null)
        {
            return 9;
        }

        // An enum on the delegate's side against the underlying integer on the target's, in both
        // argument and return position. Neither is a cast: they are admitted by the rule that two
        // types with the same *verifier* element type are interchangeable when either is an enum.
        // A dynamic method cannot declare an enum parameter itself — `SignatureHelper` would spell
        // it `ELEMENT_TYPE_INTERNAL`, which is refused when the method is minted — so the enum can
        // only ever be on the delegate's side, which is exactly where these put it.
        if (intToInt.CreateDelegate(typeof(Func<DayOfWeek, int>)) == null)
        {
            return 10;
        }

        if (intToInt.CreateDelegate(typeof(Func<int, DayOfWeek>)) == null)
        {
            return 11;
        }

        // Arity out of whack: three arguments supplied, one taken. Neither open nor closed, which
        // is a different rejection from the two below and worth having on its own.
        if (!Rejects(intToInt, typeof(Func<int, int, int, int>)))
        {
            return 12;
        }

        // The arity says open — `Invoke` supplies the one argument the target takes — but a target
        // object was handed in, so there is nothing for it to bind to.
        if (!Rejects(intToInt, typeof(Func<int, int>), bound))
        {
            return 13;
        }

        // One argument taken, none supplied, so this classifies as *closed* rather than as an
        // arity mismatch — and is then rejected on its return type, `Action` returning void where
        // the target returns `int`. (Measured, not assumed: an earlier version of this comment
        // claimed the objref rule below rejected it, and mutating that rule out left this check
        // passing. Void-versus-value is decided first.)
        if (!Rejects(intToInt, typeof(Action)))
        {
            return 14;
        }

        // Argument type mismatch, both directions of the assignability check.
        if (!Rejects(intToInt, typeof(Func<string, int>)))
        {
            return 15;
        }

        if (!Rejects(intToInt, typeof(Func<int, string>)))
        {
            return 16;
        }

        // No primitive widening: `long` and `int` have different verifier element types and
        // neither is an enum, so relaxed matching does not admit the one into the other even
        // though the conversion is lossless. This is the pair the enum rule above must *not*
        // accidentally let through.
        if (!Rejects(intToInt, typeof(Func<long, int>)))
        {
            return 17;
        }

        // `int&` supplied where the target declares `int`. The byref is on the delegate's side,
        // which is why this check is possible at all: a dynamic method with a byref *parameter*
        // would need `typeof(int).MakeByRefType()`, and `RuntimeTypeHandle_MakeByRef` is not
        // implemented (measured — it is where an earlier draft of this file stopped).
        if (!Rejects(intToInt, typeof(RefIntToInt)))
        {
            return 18;
        }

        // The objref-ness rule, in argument and then in return position. Both of these are casts
        // that *succeed* — an `int` is castable to `object` — and are still refused, because
        // nothing boxes the value on the way through a delegate. This is the one rule that a bare
        // "are they identical, or is one castable to the other" implementation gets wrong, and it
        // gets it wrong in the unsafe direction.
        DynamicMethod objectToInt = ObjectToInt();
        if (!Rejects(objectToInt, typeof(Func<int, int>)))
        {
            return 19;
        }

        if (!Rejects(intToInt, typeof(Func<int, object>)))
        {
            return 20;
        }

        // The control for both: the identical shape with an object *reference* on the narrow side
        // is accepted. Without this, 19 and 20 are also satisfied by an implementation that simply
        // refuses every widening.
        if (objectToInt.CreateDelegate(typeof(Func<string, int>)) == null)
        {
            return 21;
        }

        // The closed-over-static objref rule, isolated. `(int, int) -> int` against
        // `Func<int, int>` classifies as closed on arity, its remaining argument and its return
        // type both match, and no target is supplied — so the *only* thing left to reject it is
        // that a delegate closed over a static method's first argument requires that argument to
        // be an object reference. `int` is not one. Check 14 does not cover this: it is rejected
        // earlier, on its return type.
        DynamicMethod intIntToInt = IntIntToInt();
        if (!Rejects(intIntToInt, typeof(Func<int, int>)))
        {
            return 22;
        }

        // The same with a target supplied, which cannot help: the rule is about the parameter's
        // declared type, not about what was passed.
        if (!Rejects(intIntToInt, typeof(Func<int, int>), boxed))
        {
            return 23;
        }

        return 0;
    }
}
