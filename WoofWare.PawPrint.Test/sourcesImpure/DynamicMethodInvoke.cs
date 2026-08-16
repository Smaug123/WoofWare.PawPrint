using System;
using System.Reflection.Emit;

public class Program
{
    // Executing the body of a method minted by `Reflection.Emit`.
    //
    // Every body here is token-free — `ldarg`, `ldloc`, `stloc`, `ldc`, `add`, `br`, `ret` —
    // because a dynamic method whose IL carries a metadata token is refused when it is minted:
    // those operands index the method's `DynamicScope`, not this assembly's tables.
    //
    // Returns 0 on success, or the number of the first check that failed.

    private static DynamicMethod Doubler()
    {
        DynamicMethod dm = new DynamicMethod("Double", typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    /// `(string, int) -> int`: 100 + the second argument when the first is null, else 5 + it.
    /// The branch is what makes the *value* of the bound argument observable rather than just its
    /// presence, which is what distinguishes closed-over-null from closed-over-something.
    private static DynamicMethod FirstArgSensitive()
    {
        DynamicMethod dm = new DynamicMethod(
            "Sensitive",
            typeof(int),
            new Type[] { typeof(string), typeof(int) },
            typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        Label isNull = il.DefineLabel();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse, isNull);
        il.Emit(OpCodes.Ldc_I4, 5);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);
        il.MarkLabel(isNull);
        il.Emit(OpCodes.Ldc_I4, 100);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    public static int Main(string[] args)
    {
        // The headline: an open delegate over a dynamic method, invoked.
        Func<int, int> doubler = (Func<int, int>) Doubler().CreateDelegate(typeof(Func<int, int>));
        if (doubler(21) != 42)
        {
            return 1;
        }

        // Twice, to pin that the method is re-entrant — it is rebuilt from the registry at each
        // invocation rather than cached, so a builder that consumed state on first use would pass
        // the check above and fail this one.
        if (doubler(1) != 2)
        {
            return 2;
        }

        DynamicMethod sensitive = FirstArgSensitive();

        // Closed over a string: the bound argument is passed as arg 0.
        Func<int, int> closed = (Func<int, int>) sensitive.CreateDelegate(typeof(Func<int, int>), "hello");
        if (closed(7) != 12)
        {
            return 3;
        }

        // Closed over *null*, which is the case that separates "push the bound argument" from
        // "push it if it is non-null". PawPrint stores a null `_target` for both this and an open
        // delegate, so an implementation reading null as "nothing to push" would hand the callee
        // one argument too few — and would be caught here and nowhere else. Real .NET returns 107.
        Func<int, int> closedOverNull = (Func<int, int>) sensitive.CreateDelegate(typeof(Func<int, int>), null);
        if (closedOverNull(7) != 107)
        {
            return 4;
        }

        // Locals: a body that declares one, stores through it and reads it back, so the
        // concretised `LocalVars` are actually exercised.
        DynamicMethod withLocal =
            new DynamicMethod("WithLocal", typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator localIl = withLocal.GetILGenerator();
        localIl.DeclareLocal(typeof(int));
        localIl.Emit(OpCodes.Ldarg_0);
        localIl.Emit(OpCodes.Ldc_I4, 3);
        localIl.Emit(OpCodes.Add);
        localIl.Emit(OpCodes.Stloc_0);
        localIl.Emit(OpCodes.Ldloc_0);
        localIl.Emit(OpCodes.Ret);

        if (((Func<int, int>) withLocal.CreateDelegate(typeof(Func<int, int>)))(4) != 7)
        {
            return 5;
        }

        // A void return, which takes a different path out of the frame than a value return does.
        DynamicMethod act = new DynamicMethod("Act", typeof(void), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator actIl = act.GetILGenerator();
        actIl.Emit(OpCodes.Ret);
        ((Action<int>) act.CreateDelegate(typeof(Action<int>)))(3);

        // Two distinct dynamic methods with the same name and signature must run *their own*
        // bodies: this is what a registry lookup keyed on anything descriptive would get wrong.
        DynamicMethod tripler =
            new DynamicMethod("Double", typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
        ILGenerator triplerIl = tripler.GetILGenerator();
        triplerIl.Emit(OpCodes.Ldarg_0);
        triplerIl.Emit(OpCodes.Ldarg_0);
        triplerIl.Emit(OpCodes.Add);
        triplerIl.Emit(OpCodes.Ldarg_0);
        triplerIl.Emit(OpCodes.Add);
        triplerIl.Emit(OpCodes.Ret);

        Func<int, int> triple = (Func<int, int>) tripler.CreateDelegate(typeof(Func<int, int>));
        if (triple(5) != 15 || doubler(5) != 10)
        {
            return 6;
        }

        return 0;
    }
}
