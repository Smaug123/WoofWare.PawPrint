using System;
using System.Reflection.Emit;

// A branch on a literal is folded by CoreCLR's importer in optimised code (a dynamic method is
// always optimised), so the arm it drops is never imported. Here that arm pushes an int32 to the
// join the live arm reaches with a float32: CoreCLR runs the body, but an analysis that imports
// both arms finds the join untyped, and cannot say whether the float32 entering it is widened.
public class Program
{
    // () => { slot = 1 != 0 ? 16777216f : 0; slot += 1f; slot += 1f; return (double)slot; }
    private static Func<double> EmitUntypedJoin()
    {
        DynamicMethod method = new DynamicMethod("UntypedJoin", typeof(double), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        Label singleArm = il.DefineLabel();
        Label join = il.DefineLabel();

        il.Emit(OpCodes.Ldc_I4_1);
        il.Emit(OpCodes.Brtrue, singleArm);
        il.Emit(OpCodes.Ldc_I4_0);
        il.Emit(OpCodes.Br, join);
        il.MarkLabel(singleArm);
        il.Emit(OpCodes.Ldc_R4, 16777216f);
        il.MarkLabel(join);
        il.Emit(OpCodes.Ldc_R4, 1f);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ldc_R4, 1f);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Conv_R8);
        il.Emit(OpCodes.Ret);

        return (Func<double>)method.CreateDelegate(typeof(Func<double>));
    }

    public static int Main(string[] args)
    {
        // Only the float32 arm is imported, so the slot stays single and each addition rounds.
        if (EmitUntypedJoin()() != 16777216.0) return 1;
        return 0;
    }
}
