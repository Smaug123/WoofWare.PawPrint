using System;
using System.Reflection.Emit;

// An emitted method calling another emitted method that has never been invoked: at the
// caller's first execution the callee is not yet minted, so what the call pops and pushes has
// to be read from the DynamicMethod object itself.
public class Program
{
    private static Func<int, int> Emit()
    {
        DynamicMethod inner = new DynamicMethod("Inner", typeof(int), new[] { typeof(int), typeof(int) }, typeof(Program).Module);
        ILGenerator innerIl = inner.GetILGenerator();
        innerIl.Emit(OpCodes.Ldarg_0);
        innerIl.Emit(OpCodes.Ldarg_1);
        innerIl.Emit(OpCodes.Add);
        innerIl.Emit(OpCodes.Ret);

        DynamicMethod outer = new DynamicMethod("Outer", typeof(int), new[] { typeof(int) }, typeof(Program).Module);
        ILGenerator outerIl = outer.GetILGenerator();
        outerIl.Emit(OpCodes.Ldarg_0);
        outerIl.Emit(OpCodes.Ldc_I4_2);
        outerIl.Emit(OpCodes.Call, inner);
        outerIl.Emit(OpCodes.Ldc_I4_1);
        outerIl.Emit(OpCodes.Add);
        outerIl.Emit(OpCodes.Ret);

        return (Func<int, int>)outer.CreateDelegate(typeof(Func<int, int>));
    }

    public static int Main(string[] args)
    {
        Func<int, int> outer = Emit();
        return outer(args.Length + 3) == 6 ? 0 : 1;
    }
}
