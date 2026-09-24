using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class Program
{
    // A `bool` field is not blittable: by default it marshals as a four-byte Win32 BOOL, and
    // `[MarshalAs(U1)]`/`[MarshalAs(I1)]` make it a one-byte C bool. Either way true is written as
    // 1 whatever byte the managed `bool` holds, and any non-zero native value reads back as a
    // `bool` holding 1.
    struct WinBool { public byte A; public bool B; public byte C; }

    struct CBool { public byte A; [MarshalAs(UnmanagedType.U1)] public bool B; public byte C; }

    struct SignedCBool { public byte A; [MarshalAs(UnmanagedType.I1)] public bool B; public byte C; }

    struct ExplicitWinBool { public byte A; [MarshalAs(UnmanagedType.Bool)] public bool B; }

    const int BufferSize = 16;
    const byte Dirty = 0xAB;

    static void Soil(IntPtr ptr)
    {
        for (int i = 0; i < BufferSize; i++) Marshal.WriteByte(ptr, i, Dirty);
    }

    static bool BytesAre(IntPtr ptr, params byte[] expected)
    {
        for (int i = 0; i < expected.Length; i++)
        {
            if (Marshal.ReadByte(ptr, i) != expected[i]) return false;
        }
        return true;
    }

    static byte BitsOf(bool b) => Unsafe.As<bool, byte>(ref b);

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<WinBool>() != 12) return 1;
        if (Marshal.SizeOf<CBool>() != 3) return 2;
        if (Marshal.SizeOf<SignedCBool>() != 3) return 3;
        if (Marshal.SizeOf<ExplicitWinBool>() != 8) return 4;

        IntPtr ptr = Marshal.AllocHGlobal(BufferSize);
        try
        {
            // Out: the BOOL is 0 or 1, padding is zeroed rather than left dirty, and nothing past
            // the native image is touched.
            Soil(ptr);
            Marshal.StructureToPtr(new WinBool { A = 7, B = true, C = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0, 0, 1, 0, 0, 0, 9, 0, 0, 0, Dirty)) return 10;

            Soil(ptr);
            Marshal.StructureToPtr(new WinBool { A = 7, B = false, C = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0)) return 11;

            // A managed `bool` whose byte is 2 is still written as 1.
            var odd = new WinBool { A = 7, C = 9 };
            Unsafe.As<bool, byte>(ref odd.B) = 2;
            Soil(ptr);
            Marshal.StructureToPtr(odd, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0, 0, 1, 0, 0, 0, 9, 0, 0, 0)) return 12;

            var oddC = new CBool { A = 7, C = 9 };
            Unsafe.As<bool, byte>(ref oddC.B) = 0xFF;
            Soil(ptr);
            Marshal.StructureToPtr(oddC, ptr, false);
            if (!BytesAre(ptr, 7, 1, 9, Dirty)) return 13;

            Soil(ptr);
            Marshal.StructureToPtr(new SignedCBool { A = 7, B = false, C = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 9, Dirty)) return 14;

            Soil(ptr);
            Marshal.StructureToPtr(new ExplicitWinBool { A = 7, B = true }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0, 0, 1, 0, 0, 0, Dirty)) return 15;

            // In: any non-zero BOOL, in any of its bytes, is true, and the managed bool holds 1.
            foreach (int native in new[] { 1, 2, 0x100, 0x10000, 0x1000000, -1 })
            {
                Soil(ptr);
                Marshal.WriteInt32(ptr, 4, native);
                var back = Marshal.PtrToStructure<WinBool>(ptr);
                if (!back.B) return 20;
                if (BitsOf(back.B) != 1) return 21;
                if (back.A != Dirty || back.C != Dirty) return 22;
            }

            Soil(ptr);
            Marshal.WriteInt32(ptr, 4, 0);
            if (Marshal.PtrToStructure<WinBool>(ptr).B) return 23;

            foreach (byte native in new byte[] { 1, 2, 0x80, 0xFF })
            {
                Soil(ptr);
                Marshal.WriteByte(ptr, 1, native);
                var back = Marshal.PtrToStructure<CBool>(ptr);
                if (BitsOf(back.B) != 1) return 30;
                var signed = Marshal.PtrToStructure<SignedCBool>(ptr);
                if (BitsOf(signed.B) != 1) return 31;
            }

            Soil(ptr);
            Marshal.WriteByte(ptr, 1, 0);
            if (Marshal.PtrToStructure<CBool>(ptr).B) return 32;

            // Not blittable, so destroying the structure runs the stub's Cleanup, which zeroes it.
            Soil(ptr);
            Marshal.DestroyStructure<CBool>(ptr);
            if (!BytesAre(ptr, 0, 0, 0, Dirty)) return 40;

            Soil(ptr);
            Marshal.DestroyStructure<WinBool>(ptr);
            if (!BytesAre(ptr, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Dirty)) return 41;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }

        return 0;
    }
}
