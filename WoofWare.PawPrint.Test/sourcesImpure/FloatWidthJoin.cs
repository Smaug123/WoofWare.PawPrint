using System;
using System.Reflection.Emit;

// A stack slot's float width at a control-flow join is decided over every incoming path: when
// one arm pushes a double, CoreCLR's importer types the slot double and widens the float32 arm
// on arrival, so arithmetic after the join is double even when the float32 arm executed. No
// compiler emits this shape (they widen the float32 arm themselves), so it is emitted here.
public class Program
{
    // (bool selectSingle) => { float64 slot = selectSingle ? 16777216f : 16777216d; slot += 1f; slot += 1f; return (double)slot; }
    private static Func<bool, double> EmitMixedJoin()
    {
        DynamicMethod method = new DynamicMethod("MixedJoin", typeof(double), new[] { typeof(bool) }, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        Label singleArm = il.DefineLabel();
        Label join = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brtrue, singleArm);
        il.Emit(OpCodes.Ldc_R8, 16777216.0);
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

        return (Func<bool, double>)method.CreateDelegate(typeof(Func<bool, double>));
    }

    // Both arms float32: the slot stays single and the additions round after each step.
    private static Func<bool, double> EmitSingleJoin()
    {
        DynamicMethod method = new DynamicMethod("SingleJoin", typeof(double), new[] { typeof(bool) }, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        Label otherArm = il.DefineLabel();
        Label join = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Brtrue, otherArm);
        il.Emit(OpCodes.Ldc_R4, 16777216f);
        il.Emit(OpCodes.Br, join);
        il.MarkLabel(otherArm);
        il.Emit(OpCodes.Ldc_R4, 16777216f);
        il.MarkLabel(join);
        il.Emit(OpCodes.Ldc_R4, 1f);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ldc_R4, 1f);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Conv_R8);
        il.Emit(OpCodes.Ret);

        return (Func<bool, double>)method.CreateDelegate(typeof(Func<bool, double>));
    }

    public static int Main(string[] args)
    {
        Func<bool, double> mixed = EmitMixedJoin();
        if (mixed(true) != 16777218.0) return 1;
        if (mixed(false) != 16777218.0) return 2;

        Func<bool, double> single = EmitSingleJoin();
        if (single(true) != 16777216.0) return 3;
        if (single(false) != 16777216.0) return 4;

        return 0;
    }
}
