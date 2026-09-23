using System;
using System.Reflection;
using System.Reflection.Emit;

public class Program
{
    // A primitive's single instance field -- `System.Int32::m_value` -- holds the value itself, at
    // offset 0 and as wide as the type, so the field *is* its container: `ldflda` of it yields the
    // container's own address, `ldfld` reads the container, and `stfld` overwrites it. C# never
    // emits these (inside CoreLib, Roslyn reads `m_value` as `ldarg.0; ldind`), so they are spelled
    // here in IL.
    //
    // Every expectation was measured on the host's real .NET, which returns 0 for this program.
    // Returns 0 on success, or the number of the first check that failed.

    private const BindingFlags Instance = BindingFlags.Instance | BindingFlags.NonPublic;

    private static readonly FieldInfo IntField = typeof(int).GetField("m_value", Instance)!;
    private static readonly FieldInfo LongField = typeof(long).GetField("m_value", Instance)!;
    private static readonly FieldInfo DoubleField = typeof(double).GetField("m_value", Instance)!;
    private static readonly FieldInfo BoolField = typeof(bool).GetField("m_value", Instance)!;
    private static readonly FieldInfo CharField = typeof(char).GetField("m_value", Instance)!;

    private static Func<int> IntMethod(Action<ILGenerator> emit)
    {
        var dm = new DynamicMethod("D", typeof(int), Type.EmptyTypes, typeof(Program).Module, skipVisibility: true);
        emit(dm.GetILGenerator());
        return (Func<int>) dm.CreateDelegate(typeof(Func<int>));
    }

    public static int Main(string[] argv)
    {
        // `ldflda` through a local's address, then a store through the result.
        var viaAddress = IntMethod(il =>
        {
            il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldc_I4_5);
            il.Emit(OpCodes.Stloc_0);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldflda, IntField);
            il.Emit(OpCodes.Ldc_I4, 9);
            il.Emit(OpCodes.Stind_I4);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ret);
        });
        if (viaAddress() != 9) return 1;

        // Projecting twice is still the container.
        var twice = IntMethod(il =>
        {
            il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldflda, IntField);
            il.Emit(OpCodes.Ldflda, IntField);
            il.Emit(OpCodes.Ldc_I4, 17);
            il.Emit(OpCodes.Stind_I4);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ret);
        });
        if (twice() != 17) return 2;

        // `ldfld` through the address.
        var load = IntMethod(il =>
        {
            il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldc_I4, 7);
            il.Emit(OpCodes.Stloc_0);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldfld, IntField);
            il.Emit(OpCodes.Ret);
        });
        if (load() != 7) return 3;

        // `stfld` through the address.
        var store = IntMethod(il =>
        {
            il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldc_I4, 11);
            il.Emit(OpCodes.Stfld, IntField);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ret);
        });
        if (store() != 11) return 4;

        // An array element's address.
        var element = IntMethod(il =>
        {
            il.DeclareLocal(typeof(int[]));
            il.Emit(OpCodes.Ldc_I4_3);
            il.Emit(OpCodes.Newarr, typeof(int));
            il.Emit(OpCodes.Stloc_0);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ldelema, typeof(int));
            il.Emit(OpCodes.Ldflda, IntField);
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Stind_I4);
            il.Emit(OpCodes.Ldloc_0);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ldelem_I4);
            il.Emit(OpCodes.Ret);
        });
        if (element() != 42) return 5;

        // An eight-byte primitive: `stfld` through the address, then `ldfld` through it.
        var wide = IntMethod(il =>
        {
            il.DeclareLocal(typeof(long));
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldc_I8, 1L << 40);
            il.Emit(OpCodes.Stfld, LongField);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldfld, LongField);
            il.Emit(OpCodes.Ldc_I8, 1L << 40);
            il.Emit(OpCodes.Ceq);
            il.Emit(OpCodes.Ret);
        });
        if (wide() != 1) return 6;

        // A floating-point primitive.
        var floating = IntMethod(il =>
        {
            il.DeclareLocal(typeof(double));
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldc_R8, 2.5);
            il.Emit(OpCodes.Stfld, DoubleField);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldfld, DoubleField);
            il.Emit(OpCodes.Ldc_R8, 2.5);
            il.Emit(OpCodes.Ceq);
            il.Emit(OpCodes.Ret);
        });
        if (floating() != 1) return 7;

        // `bool` and `char`, whose stack form is an int32.
        var boolean = IntMethod(il =>
        {
            il.DeclareLocal(typeof(bool));
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Stfld, BoolField);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldfld, BoolField);
            il.Emit(OpCodes.Ret);
        });
        if (boolean() != 1) return 8;

        var character = IntMethod(il =>
        {
            il.DeclareLocal(typeof(char));
            il.Emit(OpCodes.Ldc_I4, (int) 'q');
            il.Emit(OpCodes.Stloc_0);
            il.Emit(OpCodes.Ldloca_S, (byte) 0);
            il.Emit(OpCodes.Ldflda, CharField);
            il.Emit(OpCodes.Ldind_U2);
            il.Emit(OpCodes.Ret);
        });
        if (character() != 'q') return 9;

        return 0;
    }
}
