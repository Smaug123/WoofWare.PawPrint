using System;
using System.Runtime.InteropServices;

public class Program
{
    // A `char` field's native form depends on its struct's `CharSet`. Under Ansi (the default, and
    // what `Auto` means off Windows) it is one byte, converted by CoreLib's
    // `StubHelpers.AnsiCharMarshaler`, which off Windows keeps the first byte of the char's UTF-8
    // encoding on the way out and decodes a lone byte as UTF-8 on the way back. Under Unicode it
    // is the UTF-16 code unit itself, and the struct is blittable. `[MarshalAs]` overrides the
    // `CharSet` either way.
    struct AnsiChar { public byte A; public char C; public byte B; }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
    struct AutoChar { public byte A; public char C; public byte B; }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct UnicodeChar { public byte A; public char C; public byte B; }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct UnicodeCharAsByte { public byte A; [MarshalAs(UnmanagedType.U1)] public char C; public byte B; }

    struct AnsiCharAsU2 { public byte A; [MarshalAs(UnmanagedType.U2)] public char C; public byte B; }

    [BestFitMapping(false, ThrowOnUnmappableChar = true)]
    struct NoBestFit { public byte A; public char C; }

    // Several conversions in one struct, which the stub requests one at a time and must pair up
    // with the right fields.
    struct Mixed { public char First; public DateTime When; public bool Flag; public char Second; }

    // UTF-16 `char`s, but a `bool` makes the struct non-blittable, so the stub copies the chars.
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct UnicodeWithBool { public char C; public bool B; public char D; }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
    struct AutoTStr { [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 3)] public string S; public byte B; }

    const int BufferSize = 32;
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

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<AnsiChar>() != 3) return 1;
        if (Marshal.SizeOf<AutoChar>() != 3) return 2;
        if (Marshal.SizeOf<UnicodeChar>() != 6) return 3;
        if (Marshal.SizeOf<UnicodeCharAsByte>() != 3) return 4;
        if (Marshal.SizeOf<AnsiCharAsU2>() != 6) return 5;
        if (Marshal.SizeOf<Mixed>() != 24) return 6;
        if (Marshal.OffsetOf<Mixed>("Flag") != (IntPtr)16) return 7;
        if (Marshal.OffsetOf<Mixed>("Second") != (IntPtr)20) return 8;
        if (Marshal.SizeOf<AutoTStr>() != 4) return 9;

        IntPtr ptr = Marshal.AllocHGlobal(BufferSize);
        try
        {
            Soil(ptr);
            Marshal.StructureToPtr(new AnsiChar { A = 7, C = 'A', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0x41, 9, Dirty)) return 10;

            // U+00E9 is C3 A9 in UTF-8, U+20AC is E2 82 AC, and a lone surrogate encodes as the
            // replacement character EF BF BD.
            Soil(ptr);
            Marshal.StructureToPtr(new AnsiChar { A = 7, C = '\u00E9', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0xC3, 9)) return 11;

            Soil(ptr);
            Marshal.StructureToPtr(new AutoChar { A = 7, C = '\u20AC', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0xE2, 9)) return 12;

            Soil(ptr);
            Marshal.StructureToPtr(new UnicodeCharAsByte { A = 7, C = '\uD800', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0xEF, 9)) return 13;

            Soil(ptr);
            Marshal.StructureToPtr(new NoBestFit { A = 7, C = '\u00E9' }, ptr, false);
            if (!BytesAre(ptr, 7, 0xC3)) return 14;

            Soil(ptr);
            Marshal.StructureToPtr(new UnicodeChar { A = 7, C = '\u1234', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0x34, 0x12, 9, 0)) return 15;

            Soil(ptr);
            Marshal.StructureToPtr(new AnsiCharAsU2 { A = 7, C = '\u00E9', B = 9 }, ptr, false);
            if (!BytesAre(ptr, 7, 0, 0xE9, 0, 9, 0)) return 16;

            // Back: an ASCII byte is its own char, and a byte UTF-8 cannot decode alone is U+FFFD.
            Soil(ptr);
            Marshal.WriteByte(ptr, 1, 0x41);
            if (Marshal.PtrToStructure<AnsiChar>(ptr).C != 'A') return 20;
            if (Marshal.PtrToStructure<AnsiChar>(ptr).A != Dirty) return 21;

            Marshal.WriteByte(ptr, 1, 0xC3);
            if (Marshal.PtrToStructure<AnsiChar>(ptr).C != '\uFFFD') return 22;

            Marshal.WriteInt16(ptr, 2, 0x1234);
            if (Marshal.PtrToStructure<UnicodeChar>(ptr).C != '\u1234') return 23;

            // A struct of UTF-16 chars is blittable, so destroying it touches nothing; an ANSI
            // char is not, so the stub's Cleanup zeroes the image.
            Soil(ptr);
            Marshal.DestroyStructure<UnicodeChar>(ptr);
            if (!BytesAre(ptr, Dirty, Dirty, Dirty, Dirty, Dirty, Dirty)) return 30;

            Soil(ptr);
            Marshal.DestroyStructure<AnsiCharAsU2>(ptr);
            if (!BytesAre(ptr, Dirty, Dirty, Dirty, Dirty, Dirty, Dirty)) return 31;

            Soil(ptr);
            Marshal.DestroyStructure<AnsiChar>(ptr);
            if (!BytesAre(ptr, 0, 0, 0, Dirty)) return 32;

            var mixed = new Mixed { First = 'x', When = new DateTime(2001, 2, 3), Flag = true, Second = '\u00FF' };
            Soil(ptr);
            Marshal.StructureToPtr(mixed, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != (byte)'x') return 40;
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ptr, 8)) != mixed.When.ToOADate()) return 41;
            if (Marshal.ReadInt32(ptr, 16) != 1) return 42;
            if (Marshal.ReadByte(ptr, 20) != 0xC3) return 43;

            var round = Marshal.PtrToStructure<Mixed>(ptr);
            if (round.First != 'x') return 50;
            if (round.When != mixed.When) return 51;
            if (!round.Flag) return 52;
            if (round.Second != '\uFFFD') return 53;

            if (Marshal.SizeOf<UnicodeWithBool>() != 12) return 60;
            Soil(ptr);
            Marshal.StructureToPtr(new UnicodeWithBool { C = '\u1234', B = true, D = '\u00E9' }, ptr, false);
            if (!BytesAre(ptr, 0x34, 0x12, 0, 0, 1, 0, 0, 0, 0xE9, 0, 0, 0)) return 61;
            var unicodeRound = Marshal.PtrToStructure<UnicodeWithBool>(ptr);
            if (unicodeRound.C != '\u1234' || !unicodeRound.B || unicodeRound.D != '\u00E9') return 62;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }

        return 0;
    }
}
