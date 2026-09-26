using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    class LayoutClass { public byte A; public int B; }

    [StructLayout(LayoutKind.Sequential)]
    class Derived : LayoutClass { public long C; }

    // A class with no fields has a native size of 1, but a derived class's fields start at 0:
    // CoreCLR takes the padding byte back out "ONLY for inheritance situations".
    [StructLayout(LayoutKind.Sequential)]
    class Empty { }

    [StructLayout(LayoutKind.Sequential)]
    class FromEmpty : Empty { public int I; }

    // A declared `Size` is the derived class's own part: the base's size is added to it.
    [StructLayout(LayoutKind.Sequential, Size = 3)]
    class SizedFromLayout : LayoutClass { public byte X; }

    // `Pack` caps the alignment the base contributes, so the size is not rounded to 8.
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    class PackedFromAligned : Derived { public byte X; }

    // A `bool` is a four-byte BOOL natively, so this class is not blittable, and its size and
    // offsets come from the native layout rather than the managed one.
    [StructLayout(LayoutKind.Sequential)]
    class WithBool : LayoutClass { public bool Flag; }

    // No [StructLayout]: a class is auto-layout by default, so it has no native layout of its own,
    // but a field it inherits is answered from the base class that declares it.
    class AutoFromLayout : LayoutClass { public int D; }

    // CoreCLR refuses a VARIANT_BOOL field in a build without COM interop, which makes the class
    // that declares it, and every class derived from it, unmarshalable.
    [StructLayout(LayoutKind.Sequential)]
    class WithIllegal { public int A; [MarshalAs(UnmanagedType.VariantBool)] public bool K; }

    [StructLayout(LayoutKind.Sequential)]
    class FromIllegal : WithIllegal { public int B; }

    static bool SizeRefused(Type t)
    {
        try
        {
            Marshal.SizeOf(t);
            return false;
        }
        catch (ArgumentException e)
        {
            return e.GetType() == typeof(ArgumentException)
                && e.Message == "Type '" + t.FullName + "' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed."
                && e.ParamName == null;
        }
    }

    static bool OffsetRefused(Type t, string field)
    {
        try
        {
            Marshal.OffsetOf(t, field);
            return false;
        }
        catch (ArgumentException e)
        {
            return e.GetType() == typeof(ArgumentException)
                && e.Message == "Type '" + t.FullName + "' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed."
                && e.ParamName == null;
        }
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<LayoutClass>() != 8) return 1;
        if (Marshal.SizeOf<Derived>() != 16) return 2;
        if (Marshal.SizeOf(new Derived()) != 16) return 3;
        if (Marshal.OffsetOf<Derived>("A") != (IntPtr)0) return 4;

        if (Marshal.SizeOf<Empty>() != 1) return 5;
        if (Marshal.SizeOf<FromEmpty>() != 4) return 6;
        if (Marshal.OffsetOf<FromEmpty>("I") != (IntPtr)0) return 7;

        if (Marshal.SizeOf<SizedFromLayout>() != 11) return 10;
        if (Marshal.OffsetOf<SizedFromLayout>("X") != (IntPtr)8) return 11;

        if (Marshal.SizeOf<PackedFromAligned>() != 17) return 12;
        if (Marshal.OffsetOf<PackedFromAligned>("X") != (IntPtr)16) return 13;

        if (Marshal.SizeOf<WithBool>() != 12) return 14;
        if (Marshal.OffsetOf<WithBool>("Flag") != (IntPtr)8) return 15;

        if (!SizeRefused(typeof(AutoFromLayout))) return 16;
        if (Marshal.OffsetOf<AutoFromLayout>("B") != (IntPtr)4) return 17;
        // The refusal names the auto-layout class, which declares `D`.
        if (!OffsetRefused(typeof(AutoFromLayout), "D")) return 18;

        if (!SizeRefused(typeof(WithIllegal))) return 19;
        if (!SizeRefused(typeof(FromIllegal))) return 20;
        if (!OffsetRefused(typeof(FromIllegal), "B")) return 21;

        if (!SizeRefused(typeof(object))) return 22;
        if (!SizeRefused(typeof(string))) return 23;
        if (!SizeRefused(typeof(int[]))) return 24;

        return 0;
    }
}
