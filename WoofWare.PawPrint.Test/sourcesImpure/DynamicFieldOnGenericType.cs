using System;
using System.Reflection;
using System.Reflection.Emit;

// A field of a generic type's parameter type, loaded by an emitted method through a
// `DynamicScope` entry: the field's declared type is `!0`, but the handle names the field on
// `Box<float>`, so the load pushes a float32 and may meet a float32 literal at a join.
public class Box<T>
{
    public T Item;
}

public class Program
{
    // The box arrives as `object` and is cast inside: a generic instantiation in the minted
    // method's own signature is not something PawPrint reads yet, and the field load is the
    // point.
    private static Func<object, bool, float> Emit()
    {
        DynamicMethod method = new DynamicMethod("Pick", typeof(float), new[] { typeof(object), typeof(bool) }, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        Label literal = il.DefineLabel();
        Label join = il.DefineLabel();

        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Brtrue, literal);
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Castclass, typeof(Box<float>));
        il.Emit(OpCodes.Ldfld, typeof(Box<float>).GetField("Item"));
        il.Emit(OpCodes.Br, join);
        il.MarkLabel(literal);
        il.Emit(OpCodes.Ldc_R4, 2f);
        il.MarkLabel(join);
        il.Emit(OpCodes.Ldc_R4, 1f);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);

        return (Func<object, bool, float>)method.CreateDelegate(typeof(Func<object, bool, float>));
    }

    public static int Main(string[] args)
    {
        Func<object, bool, float> pick = Emit();
        Box<float> box = new Box<float> { Item = 4f + args.Length };
        if (pick(box, false) != 5f) return 1;
        if (pick(box, true) != 3f) return 2;
        return 0;
    }
}
