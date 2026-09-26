using System;
using System.Collections.Generic;
using System.Reflection.Emit;

public struct Point
{
    public int X;
    public int Y;
}

public class Box
{
    public int Value;
}

public enum Small : byte
{
    A = 3,
    B = 200,
}

public delegate int RefReader(ref Point p);

public class Program
{
    // Dynamic methods whose signatures and locals name types SignatureHelper cannot spell as simple
    // element types. With no module to spell them as tokens, it writes each as
    // ELEMENT_TYPE_INTERNAL followed by the eight bytes of the type's handle, and a constructed
    // generic as GENERICINST over its definition's handle. Every body is an identity or a field
    // read, so what is under test is that the signature and locals decode to the right types.
    //
    // Returns 0 on success, or the number of the first check that failed.

    private static DynamicMethod Identity(Type type, bool throughLocal)
    {
        DynamicMethod dm = new DynamicMethod("Identity", type, new Type[] { type }, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);

        if (throughLocal)
        {
            LocalBuilder local = il.DeclareLocal(type);
            il.Emit(OpCodes.Stloc, local);
            il.Emit(OpCodes.Ldloc, local);
        }

        il.Emit(OpCodes.Ret);
        return dm;
    }

    private static T Through<T>(T value, bool throughLocal)
    {
        Func<T, T> f = (Func<T, T>) Identity(typeof(T), throughLocal).CreateDelegate(typeof(Func<T, T>));
        return f(value);
    }

    public static int Main(string[] args)
    {
        for (int pass = 0; pass < 2; pass++)
        {
            // The second pass stores the argument through a local of the same type, so the
            // locals signature carries each type as well as the method signature.
            bool throughLocal = pass == 1;
            int offset = pass * 20;

            Point p = Through(new Point { X = 3, Y = 4 }, throughLocal);
            if (p.X != 3 || p.Y != 4)
            {
                return offset + 1;
            }

            Box box = new Box { Value = 7 };
            if (!ReferenceEquals(Through(box, throughLocal), box))
            {
                return offset + 2;
            }

            // An enum is spelled ELEMENT_TYPE_INTERNAL like any other value type, not as its
            // underlying integer; two widths.
            if (Through(DayOfWeek.Friday, throughLocal) != DayOfWeek.Friday)
            {
                return offset + 3;
            }

            if (Through(Small.B, throughLocal) != Small.B)
            {
                return offset + 4;
            }

            // Constructed generics: GENERICINST over a class definition and over a struct one.
            List<int> list = new List<int> { 1, 2 };
            if (!ReferenceEquals(Through(list, throughLocal), list))
            {
                return offset + 5;
            }

            KeyValuePair<int, string> pair = Through(new KeyValuePair<int, string>(9, "nine"), throughLocal);
            if (pair.Key != 9 || pair.Value != "nine")
            {
                return offset + 6;
            }

            if (Through<int?>(12, throughLocal) != 12)
            {
                return offset + 7;
            }

            if (Through<int?>(null, throughLocal) != null)
            {
                return offset + 8;
            }

            // An array of a nominal type: SZARRAY over the run.
            Point[] points = new Point[] { new Point { X = 5 } };
            if (!ReferenceEquals(Through(points, throughLocal), points))
            {
                return offset + 9;
            }
        }

        // A byref to a nominal type, read through: BYREF over the run, and a field of the struct.
        DynamicMethod readX = new DynamicMethod(
            "ReadX",
            typeof(int),
            new Type[] { typeof(Point).MakeByRefType() },
            typeof(Program).Module);
        ILGenerator il = readX.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldfld, typeof(Point).GetField("X"));
        il.Emit(OpCodes.Ret);

        RefReader reader = (RefReader) readX.CreateDelegate(typeof(RefReader));
        Point q = new Point { X = 41, Y = 1 };
        if (reader(ref q) != 41)
        {
            return 50;
        }

        return 0;
    }
}
