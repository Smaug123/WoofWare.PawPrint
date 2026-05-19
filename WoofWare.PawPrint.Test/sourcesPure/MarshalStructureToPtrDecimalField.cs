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
        // CoreCLR's `MarshalInfo` routes `System.Decimal` *fields* through marshal-stub
        // synthesis (`NFT_DECIMAL` in `fieldmarshaler.cpp`) rather than the memmove fast
        // path: managed `Decimal` is 16 bytes with 4-byte field alignment, but native
        // `DECIMAL` is 16 bytes with 8-byte alignment (its `Lo64` union member is
        // `ULONGLONG`). Inside a sequential outer struct, the Decimal field's native offset
        // therefore diverges from its managed offset — `{ int Id; decimal Value; }` is
        // 20 bytes managed but 24 bytes native (4 bytes of padding after `Id` to realign
        // the Decimal to an 8-byte boundary). PawPrint's `MarshalNative_TryGetStructMarshalStub`
        // `isBlittableField` walk recursed into Decimal's four `Int32` fields, declared the
        // parent struct strictly-numeric blittable, and would take the memmove fast path —
        // silently emitting the managed (20-byte, 4-byte-aligned) byte image into a native
        // 24-byte buffer. This test pins the expected behaviour and tracks the future
        // Decimal-marshal-stub work.
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
