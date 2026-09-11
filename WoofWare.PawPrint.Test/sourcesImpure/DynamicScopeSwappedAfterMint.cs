using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

// After a dynamic method is minted, its scope's token list is an ordinary List<object> that
// reflection can write. Swapping the entry a `call` names for a boxed RuntimeMethodHandle puts
// in method position something PawPrint resolves only when the call executes; here the call
// is behind a branch the guest never takes, so the body must still run.
public class Program
{
    public static int Helper(int x) => x * 10;

    private static int Callee(int x) => x + 1;

    public static int Main(string[] args)
    {
        DynamicMethod inner = new DynamicMethod("Inner", typeof(int), new[] { typeof(int) }, typeof(Program).Module);
        ILGenerator innerIl = inner.GetILGenerator();
        innerIl.Emit(OpCodes.Ldarg_0);
        innerIl.Emit(OpCodes.Ret);

        DynamicMethod method = new DynamicMethod("Pick", typeof(int), new[] { typeof(bool) }, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        Label skip = il.DefineLabel();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brfalse, skip);
        il.Emit(OpCodes.Ldc_I4_7);
        il.Emit(OpCodes.Call, inner);
        il.Emit(OpCodes.Ret);
        il.MarkLabel(skip);
        il.Emit(OpCodes.Ldc_I4_3);
        il.Emit(OpCodes.Ret);
        Func<bool, int> pick = (Func<bool, int>)method.CreateDelegate(typeof(Func<bool, int>));

        // DynamicILGenerator.m_scope is a DynamicScope whose m_tokens is the List<object> the
        // emitted tokens index (token 0x06000000 | index).
        object scope = il.GetType().GetField("m_scope", BindingFlags.Instance | BindingFlags.NonPublic).GetValue(il);
        List<object> tokens = (List<object>)scope.GetType().GetField("m_tokens", BindingFlags.Instance | BindingFlags.NonPublic).GetValue(scope);
        int slot = tokens.IndexOf(inner);
        if (slot < 0) return 2;
        tokens[slot] = typeof(Program).GetMethod("Helper").MethodHandle;

        return pick(args.Length > 100) == 3 ? 0 : 1;
    }
}
