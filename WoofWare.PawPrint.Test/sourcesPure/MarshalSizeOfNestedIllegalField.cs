using System;
using System.Runtime.InteropServices;

public class Program
{
    // Off Windows there is no COM interop, so CoreCLR refuses `VariantBool` on a `bool` field.
    // The refused field still takes one byte, 1-aligned, of the struct's native layout, and makes
    // that struct unmarshalable.
    struct Refused
    {
        public long L;
        [MarshalAs(UnmanagedType.VariantBool)] public bool B;
        public byte T;
    }

    // A struct with no layout of its own is refused as a field in the same way.
    [StructLayout(LayoutKind.Auto)]
    struct NoLayout
    {
        public int A;
        public int B;
    }

    struct HoldsNoLayout
    {
        public short S;
        public NoLayout N;
    }

    // But a struct that merely holds one of those is laid out as usual, the held struct
    // contributing its own native size: `Refused` is `{ long; byte; byte }` natively, 16 bytes and
    // 8-aligned, and `HoldsNoLayout` is `{ short; byte }`, 4 bytes and 2-aligned.
    struct Outer
    {
        public byte X;
        public Refused R;
        public byte Y;
        public HoldsNoLayout H;
    }

    static bool CannotMarshal(Type t, string name)
    {
        try
        {
            Marshal.SizeOf(t);
            return false;
        }
        catch (ArgumentException e)
        {
            return e.GetType() == typeof(ArgumentException)
                && e.Message == "Type '" + name + "' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed.";
        }
    }

    public static int Main(string[] args)
    {
        if (!CannotMarshal(typeof(Refused), "Program+Refused")) return 1;
        if (!CannotMarshal(typeof(HoldsNoLayout), "Program+HoldsNoLayout")) return 2;

        if (Marshal.SizeOf<Outer>() != 32) return 3;
        if (Marshal.OffsetOf<Outer>("X") != (IntPtr)0) return 4;
        if (Marshal.OffsetOf<Outer>("R") != (IntPtr)8) return 5;
        if (Marshal.OffsetOf<Outer>("Y") != (IntPtr)24) return 6;
        if (Marshal.OffsetOf<Outer>("H") != (IntPtr)26) return 7;

        return 0;
    }
}
