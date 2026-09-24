using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct Basic { public byte A; public int B; public byte C; public long D; }

    [StructLayout(LayoutKind.Sequential, Pack = 2)]
    struct Pack2 { public byte A; public int B; public long C; }

    [StructLayout(LayoutKind.Explicit, Size = 20)]
    struct Expl { [FieldOffset(4)] public int A; [FieldOffset(0)] public long B; [FieldOffset(12)] public byte C; }

    [StructLayout(LayoutKind.Sequential)]
    struct Inner { public byte X; public int Y; }

    [StructLayout(LayoutKind.Sequential)]
    struct Nested { public byte A; public Inner I; public short Z; }

    enum E16 : short { A }

    [StructLayout(LayoutKind.Sequential)]
    struct WithEnum { public byte A; public E16 E; public long L; }

    // Neither is blittable as a field, so CoreCLR answers from the native layout rather than
    // the managed one. DateTime marshals as an 8-byte OLE date.
    [StructLayout(LayoutKind.Sequential)]
    struct WithDecimal { public byte A; public decimal D; public byte B; }

    [StructLayout(LayoutKind.Sequential)]
    struct WithDateTime { public byte A; public DateTime D; public byte B; }

    [StructLayout(LayoutKind.Sequential)]
    struct WithPrivate { public byte A; private int P; public int Get() => P; }

    [StructLayout(LayoutKind.Sequential)]
    struct WithStatic { public int A; public static int S; }

    // Closed over a value type, so the instantiation is its own canonical form and has a layout.
    [StructLayout(LayoutKind.Sequential)]
    struct Gen<T> { public byte A; public T V; }

    // CoreCLR aligns Int128 to 16 by name, natively as well as in managed layout, though its
    // two ulong fields alone would imply 8.
    [StructLayout(LayoutKind.Sequential)]
    struct WithInt128 { public byte A; public Int128 I; public byte B; }

    [StructLayout(LayoutKind.Auto)]
    struct AutoS { public int A; public int B; }

    [StructLayout(LayoutKind.Sequential)]
    struct HoldsAuto { public byte A; public AutoS S; }

    const string CannotMarshalSuffix =
        "' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed.";

    // 0 if `OffsetOf(t, name)` is refused as CoreCLR's `IDS_CANNOT_MARSHAL`, naming `typeName`.
    static int CannotMarshal(Type t, string name, string typeName, int failure)
    {
        try
        {
            Marshal.OffsetOf(t, name);
            return failure;
        }
        catch (ArgumentException e)
        {
            if (e.GetType() != typeof(ArgumentException)) return failure + 1;
            if (e.Message != "Type '" + typeName + CannotMarshalSuffix) return failure + 2;
            if (e.ParamName != null) return failure + 3;
            return 0;
        }
    }

    public static int Main(string[] args)
    {
        if (Marshal.OffsetOf(typeof(Basic), "A") != (IntPtr)0) return 1;
        if (Marshal.OffsetOf(typeof(Basic), "B") != (IntPtr)4) return 2;
        if (Marshal.OffsetOf(typeof(Basic), "C") != (IntPtr)8) return 3;
        if (Marshal.OffsetOf<Basic>("D") != (IntPtr)16) return 4;

        if (Marshal.OffsetOf<Pack2>("B") != (IntPtr)2) return 5;
        if (Marshal.OffsetOf<Pack2>("C") != (IntPtr)6) return 6;

        if (Marshal.OffsetOf<Expl>("A") != (IntPtr)4) return 7;
        if (Marshal.OffsetOf<Expl>("B") != (IntPtr)0) return 8;
        if (Marshal.OffsetOf<Expl>("C") != (IntPtr)12) return 9;

        if (Marshal.OffsetOf<Nested>("I") != (IntPtr)4) return 10;
        if (Marshal.OffsetOf<Nested>("Z") != (IntPtr)12) return 11;

        if (Marshal.OffsetOf<WithEnum>("E") != (IntPtr)2) return 12;
        if (Marshal.OffsetOf<WithEnum>("L") != (IntPtr)8) return 13;

        if (Marshal.OffsetOf<WithDecimal>("D") != (IntPtr)8) return 14;
        if (Marshal.OffsetOf<WithDecimal>("B") != (IntPtr)24) return 15;
        if (Marshal.OffsetOf<WithDateTime>("D") != (IntPtr)8) return 16;
        if (Marshal.OffsetOf<WithDateTime>("B") != (IntPtr)16) return 17;

        // `OffsetOf` looks the field up with `BindingFlags.NonPublic` as well as `Public`.
        if (Marshal.OffsetOf<WithPrivate>("P") != (IntPtr)4) return 18;

        if (Marshal.OffsetOf<Gen<int>>("V") != (IntPtr)4) return 19;
        if (Marshal.OffsetOf<Gen<long>>("V") != (IntPtr)8) return 20;

        // A primitive's own single field, and a CoreLib struct's.
        if (Marshal.OffsetOf<int>("m_value") != (IntPtr)0) return 22;
        if (Marshal.OffsetOf<decimal>("_lo64") != (IntPtr)8) return 23;
        if (Marshal.OffsetOf<IntPtr>("_value") != (IntPtr)0) return 24;
        // Neither is blittable, and each marshals at a different width from its managed one, but
        // its single field still starts the image.
        if (Marshal.OffsetOf<bool>("m_value") != (IntPtr)0) return 25;
        if (Marshal.OffsetOf<char>("m_value") != (IntPtr)0) return 26;

        if (Marshal.OffsetOf<WithInt128>("I") != (IntPtr)16) return 27;
        if (Marshal.OffsetOf<WithInt128>("B") != (IntPtr)32) return 28;
        if (Marshal.SizeOf<WithInt128>() != 48) return 29;

        int r;
        // No layout at all: an auto-layout struct, an ordinary class, an enum, and DateTime,
        // which CoreLib declares auto-layout even though it marshals fine as a *field*.
        if ((r = CannotMarshal(typeof(AutoS), "A", "Program+AutoS", 30)) != 0) return r;
        if ((r = CannotMarshal(typeof(Program), "Dummy", "Program", 40)) != 0) return r;
        if ((r = CannotMarshal(typeof(E16), "value__", "Program+E16", 50)) != 0) return r;
        if ((r = CannotMarshal(typeof(DateTime), "_dateData", "System.DateTime", 60)) != 0) return r;
        // Has a layout, but a field that has none.
        if ((r = CannotMarshal(typeof(HoldsAuto), "A", "Program+HoldsAuto", 70)) != 0) return r;
        // An open generic definition never has a layout, whatever its fields.
        if ((r = CannotMarshal(typeof(Gen<>), "A", "Program+Gen`1[T]", 80)) != 0) return r;

        // The managed wrapper's own checks, which never reach the QCall.
        try
        {
            Marshal.OffsetOf(typeof(Basic), "Nope");
            return 90;
        }
        catch (ArgumentException e)
        {
            if (e.GetType() != typeof(ArgumentException)) return 91;
            if (e.ParamName != "fieldName") return 92;
        }

        try
        {
            Marshal.OffsetOf(typeof(WithStatic), "S");
            return 93;
        }
        catch (ArgumentException e)
        {
            if (e.GetType() != typeof(ArgumentException)) return 94;
            if (e.ParamName != "fieldName") return 95;
        }

        try
        {
            Marshal.OffsetOf(typeof(Basic), null);
            return 96;
        }
        catch (ArgumentNullException e)
        {
            if (e.ParamName != "name") return 97;
        }

        try
        {
            Marshal.OffsetOf(null, "A");
            return 98;
        }
        catch (ArgumentNullException e)
        {
            if (e.ParamName != "t") return 99;
        }

        return 0;
    }

    public int Dummy;
}
