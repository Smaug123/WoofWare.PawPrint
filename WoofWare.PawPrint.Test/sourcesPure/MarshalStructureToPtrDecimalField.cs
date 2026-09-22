using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct WithDecimal
    {
        public int Id;
        public decimal Value;
    }

    public static int Main(string[] args)
    {
        // CoreCLR never treats a `System.Decimal` field as blittable (fieldmarshaler.cpp:266),
        // so this struct goes through a synthesised marshal stub in both directions rather than
        // a memmove. Native `DECIMAL` is 8-byte aligned (its `Lo64` is a `ULONGLONG`), so the
        // native image puts `Value` at offset 8 and is 24 bytes.
        var s = new WithDecimal { Id = 7, Value = 1.5m };
        int size = Marshal.SizeOf<WithDecimal>();
        if (size != 24) return 1;
        IntPtr ptr = Marshal.AllocHGlobal(size);
        try
        {
            Marshal.StructureToPtr(s, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 7) return 2;
            var roundtrip = Marshal.PtrToStructure<WithDecimal>(ptr);
            if (roundtrip.Id != 7) return 3;
            if (roundtrip.Value != 1.5m) return 4;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
