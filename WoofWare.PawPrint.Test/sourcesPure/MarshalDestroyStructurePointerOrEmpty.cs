using System;
using System.Runtime.InteropServices;

public unsafe class Program
{
    // CoreCLR blits a pointer field, and a struct without fields, so each of these structs is
    // blittable, and `Marshal.DestroyStructure` leaves its native image alone. Measured on .NET 10
    // and on .NET 11 RC1.
    [StructLayout(LayoutKind.Sequential)]
    struct WithPointer
    {
        public int* P;
        public int N;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct Empty
    {
    }

    [StructLayout(LayoutKind.Sequential, Size = 12)]
    struct Sized
    {
    }

    public static int Main(string[] args)
    {
        IntPtr ptr = Marshal.AllocHGlobal(16);
        try
        {
            Marshal.WriteInt64(ptr, 0x1122334455667788L);
            Marshal.WriteInt64(ptr, 8, 0x0102030405060708L);

            Marshal.DestroyStructure<WithPointer>(ptr);
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 1;
            if (Marshal.ReadInt64(ptr, 8) != 0x0102030405060708L) return 2;

            Marshal.DestroyStructure<Empty>(ptr);
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 3;

            Marshal.DestroyStructure<Sized>(ptr);
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 4;
            if (Marshal.ReadInt64(ptr, 8) != 0x0102030405060708L) return 5;

            return 0;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
    }
}
