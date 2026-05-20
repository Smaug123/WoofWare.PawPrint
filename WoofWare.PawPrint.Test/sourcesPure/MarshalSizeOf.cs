using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct Packed
    {
        public byte B;
        public int I;
        public byte B2;
    }

    [StructLayout(LayoutKind.Explicit, Size = 10)]
    struct ExplicitWithSize
    {
        [FieldOffset(0)]
        public int I;

        [FieldOffset(2)]
        public short S;
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf(typeof(Packed)) != 6) return 1;
        if (Marshal.SizeOf(typeof(ExplicitWithSize)) != 10) return 2;
        return 0;
    }
}
