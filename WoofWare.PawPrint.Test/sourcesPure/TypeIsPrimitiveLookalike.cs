using System;

// A type is identified by its assembly as well as its name. A guest assembly may declare its own
// `System.Double`, and real .NET treats it as the ordinary struct it is: `Type.IsPrimitive` is
// `IsPrimitiveType(GetCorElementType())`, and a guest struct's MethodTable reports
// ELEMENT_TYPE_VALUETYPE whatever it is called. Only corelib's `System.Double` is ELEMENT_TYPE_R8.
namespace System
{
    public struct Double
    {
        // An `int` rather than a `double` so that a mis-binding to corelib's `System.Double`,
        // which has no such field, is a compile error rather than a silently different test.
        public int V;
    }

    public struct Int64
    {
        public int V;
    }

    public struct Int32
    {
        public int V;
    }

    public struct IntPtr
    {
        public int V;
    }
}

public class Program
{
    static bool IsPrimitiveOf<T>() => typeof(T).IsPrimitive;

    public static int Main(string[] args)
    {
        // The lookalikes are ordinary value types.
        if (typeof(System.Double).IsPrimitive) return 1;
        if (typeof(System.Int64).IsPrimitive) return 2;
        if (typeof(System.Int32).IsPrimitive) return 3;
        if (typeof(System.IntPtr).IsPrimitive) return 4;

        // The keywords still name corelib's types, which the lookalikes do not displace.
        if (!typeof(double).IsPrimitive) return 5;
        if (!typeof(long).IsPrimitive) return 6;
        if (!typeof(int).IsPrimitive) return 7;
        if (!typeof(nint).IsPrimitive) return 8;

        // Reached through a generic method's type parameter, so the handle comes from
        // substitution rather than from a `ldtoken` of the literal type.
        if (IsPrimitiveOf<System.Double>()) return 9;
        if (!IsPrimitiveOf<double>()) return 10;

        // `GetTypeCode` classifies by the same element type: a struct that is not `Decimal`,
        // `DateTime` or an enum is `TypeCode.Object`.
        if (Type.GetTypeCode(typeof(System.Double)) != TypeCode.Object) return 11;
        if (Type.GetTypeCode(typeof(System.Int32)) != TypeCode.Object) return 12;
        if (Type.GetTypeCode(typeof(double)) != TypeCode.Double) return 13;
        if (Type.GetTypeCode(typeof(int)) != TypeCode.Int32) return 14;

        // A value of lookalike type is the guest's struct, with its own field.
        System.Double d = new System.Double { V = 42 };
        if (d.V != 42) return 15;

        return 0;
    }
}
