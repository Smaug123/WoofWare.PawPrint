using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    class LayoutClass { public byte A; public int B; }

    [StructLayout(LayoutKind.Sequential)]
    class Derived : LayoutClass { public long C; }

    [StructLayout(LayoutKind.Explicit)]
    class ExplicitClass { [FieldOffset(4)] public int A; [FieldOffset(0)] public byte B; }

    // A declared `Size` pads the base's native image, and a derived class's fields start after it.
    [StructLayout(LayoutKind.Sequential, Size = 12)]
    class SizedBase { public long L; }

    [StructLayout(LayoutKind.Sequential)]
    class DerivedFromSized : SizedBase { public int I; }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    class PackedBase { public byte B; public int I; }

    [StructLayout(LayoutKind.Sequential)]
    class DerivedFromPacked : PackedBase { public long L; }

    public static int Main(string[] args)
    {
        if (Marshal.OffsetOf<LayoutClass>("A") != (IntPtr)0) return 1;
        if (Marshal.OffsetOf<LayoutClass>("B") != (IntPtr)4) return 2;

        if (Marshal.OffsetOf<Derived>("C") != (IntPtr)8) return 3;
        // An inherited field is answered from the base class that declares it.
        if (Marshal.OffsetOf<Derived>("B") != (IntPtr)4) return 4;

        if (Marshal.OffsetOf<ExplicitClass>("A") != (IntPtr)4) return 5;
        if (Marshal.OffsetOf<ExplicitClass>("B") != (IntPtr)0) return 6;

        if (Marshal.OffsetOf<DerivedFromSized>("I") != (IntPtr)12) return 7;
        if (Marshal.OffsetOf<DerivedFromSized>("L") != (IntPtr)0) return 8;

        if (Marshal.OffsetOf<DerivedFromPacked>("I") != (IntPtr)1) return 9;
        if (Marshal.OffsetOf<DerivedFromPacked>("L") != (IntPtr)8) return 10;

        return 0;
    }
}
