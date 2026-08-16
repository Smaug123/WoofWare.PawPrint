using System;
using System.Collections.Generic;
using System.Reflection.Emit;

// A `catch` clause naming a type that cannot be a clause type. `BeginCatchBlock` accepts any
// `RuntimeType`, so this is emittable, and it is the runtime that refuses -- when it compiles the
// method, which is the first invocation.

public class Program
{
    private static DynamicMethod New(string name) =>
        new DynamicMethod(name, typeof(int), new Type[] { typeof(object) }, typeof(Program).Module);

    private static Type Threw(Action a)
    {
        try
        {
            a();
            return null;
        }
        catch (Exception e)
        {
            return e.GetType();
        }
    }

    // Returns 0 on success, or the number of the first check that failed. Every expectation was
    // measured on the host's real .NET before being written down.
    public static int Main()
    {
        // 1. The refusal is `InvalidProgramException`, and it comes from the *invocation*, not from
        // `CreateDelegate`: binding a delegate to the method succeeds.
        {
            DynamicMethod dm = New("OpenGeneric");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(List<>));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 1);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);

            Func<object, int> f = null;
            if (Threw (() => { f = (Func<object, int>) dm.CreateDelegate(typeof(Func<object, int>)); }) != null)
            {
                return 1;
            }

            if (Threw (() => f(new InvalidOperationException())) != typeof(InvalidProgramException))
            {
                return 2;
            }
        }

        // 2. The clause is refused even though nothing in the body ever throws, so nothing ever
        // reaches it: real .NET compiles the whole method before running any of it. An
        // implementation resolving clause types lazily, when an exception is dispatched, runs this
        // method happily and answers 5.
        {
            DynamicMethod dm = New("OpenGenericQuiet");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldc_I4, 5);
            il.Emit(OpCodes.Stloc, loc);
            il.BeginCatchBlock(typeof(List<>));
            il.Emit(OpCodes.Pop);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);

            Func<object, int> f = (Func<object, int>) dm.CreateDelegate(typeof(Func<object, int>));
            if (Threw (() => f(null)) != typeof(InvalidProgramException))
            {
                return 3;
            }
        }

        // 3. The same refusal reached through a `call` from another dynamic method rather than
        // through a delegate: the callee's first compilation happens at the caller's call site, so
        // the caller's own handler sees it. Measured on real .NET, where this catch takes it.
        {
            DynamicMethod callee = New("OpenGenericCallee");
            {
                ILGenerator il = callee.GetILGenerator();
                LocalBuilder loc = il.DeclareLocal(typeof(int));
                il.BeginExceptionBlock();
                il.Emit(OpCodes.Ldc_I4, 5);
                il.Emit(OpCodes.Stloc, loc);
                il.BeginCatchBlock(typeof(List<>));
                il.Emit(OpCodes.Pop);
                il.EndExceptionBlock();
                il.Emit(OpCodes.Ldloc, loc);
                il.Emit(OpCodes.Ret);
            }
            DynamicMethod caller = New("OpenGenericCaller");
            {
                ILGenerator il = caller.GetILGenerator();
                LocalBuilder loc = il.DeclareLocal(typeof(int));
                il.BeginExceptionBlock();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Call, callee);
                il.Emit(OpCodes.Stloc, loc);
                il.BeginCatchBlock(typeof(InvalidProgramException));
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Ldc_I4, 77);
                il.Emit(OpCodes.Stloc, loc);
                il.EndExceptionBlock();
                il.Emit(OpCodes.Ldloc, loc);
                il.Emit(OpCodes.Ret);
            }

            Func<object, int> f = (Func<object, int>) caller.CreateDelegate(typeof(Func<object, int>));
            if (f(null) != 77)
            {
                return 4;
            }
        }

        return 0;
    }
}
