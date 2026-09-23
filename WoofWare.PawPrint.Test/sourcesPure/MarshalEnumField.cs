using System;
using System.Runtime.InteropServices;

public class Program
{
    // CoreCLR's `MarshalInfo` reads a field's element type through `PeekElemTypeNormalized`, which
    // turns an enum into its underlying primitive, so an enum field marshals exactly as that
    // primitive would: same width, same alignment, blittable. The enum type itself is auto-layout,
    // which is why it cannot be marshalled on its own (checked at the end).
    enum E8 : byte { A = 0xFE }
    enum ES8 : sbyte { A = -2 }
    enum E16 : short { A = -12345 }
    enum EU16 : ushort { A = 54321 }
    enum E32 { A = int.MinValue }
    enum EU32 : uint { A = 0xDEADBEEF }
    enum E64 : long { A = long.MinValue + 1 }
    enum EU64 : ulong { A = ulong.MaxValue - 1 }
    [Flags] enum F : ushort { X = 1, Y = 4, Z = 0x8000 }

    // Every underlying width, interleaved with bytes so that each enum's own alignment decides
    // where it lands. Offsets measured on real .NET with Marshal.OffsetOf.
    [StructLayout(LayoutKind.Sequential)]
    struct AllWidths
    {
        public byte Pad0;
        public E16 S;
        public byte Pad1;
        public E64 L;
        public E8 B;
        public EU32 UI;
        public ES8 SB;
        public EU64 UL;
        public EU16 US;
        public E32 I;
        public F Flags;
    }

    const int OffS = 2, OffPad1 = 4, OffL = 8, OffB = 16, OffUI = 20, OffSB = 24, OffUL = 32, OffUS = 40,
        OffI = 44, OffFlags = 48, SizeAllWidths = 56;

    [StructLayout(LayoutKind.Sequential)]
    struct Inner
    {
        public byte Tag;
        public E32 Kind;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct Outer
    {
        public E8 Head;
        public Inner In;
        public E16 Tail;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct Ex
    {
        [FieldOffset(0)] public E64 Wide;
        [FieldOffset(0)] public E32 Low;
        [FieldOffset(9)] public E8 Odd;
        [FieldOffset(12)] public EU16 Hi;
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    struct Packed
    {
        public byte B;
        public E64 L;
        public E16 S;
    }

    // The Decimal makes this non-blittable, so it is marshalled field by field through a
    // synthesised stub rather than memmoved: the enum fields are written and read individually.
    [StructLayout(LayoutKind.Sequential)]
    struct WithDecimal
    {
        public E16 Kind;
        public decimal M;
        public EU64 Tail;
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<AllWidths>() != SizeAllWidths) return 1;
        if (Marshal.SizeOf<Inner>() != 8) return 2;
        if (Marshal.SizeOf<Outer>() != 16) return 3;
        if (Marshal.SizeOf<Ex>() != 16) return 4;
        if (Marshal.SizeOf<Packed>() != 11) return 5;
        if (Marshal.SizeOf<WithDecimal>() != 32) return 6;
        if (Marshal.SizeOf(typeof(AllWidths)) != SizeAllWidths) return 7;

        IntPtr ptr = Marshal.AllocHGlobal(64);
        try
        {
            for (int i = 0; i < 64; i++) Marshal.WriteByte(ptr, i, 0xCC);

            var a = new AllWidths
            {
                Pad0 = 0x11, S = E16.A, Pad1 = 0x22, L = E64.A, B = E8.A, UI = EU32.A, SB = ES8.A,
                UL = EU64.A, US = EU16.A, I = E32.A, Flags = F.X | F.Z,
            };
            Marshal.StructureToPtr(a, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != 0x11) return 10;
            if (Marshal.ReadInt16(ptr, OffS) != -12345) return 11;
            if (Marshal.ReadByte(ptr, OffPad1) != 0x22) return 12;
            if (Marshal.ReadInt64(ptr, OffL) != long.MinValue + 1) return 13;
            if (Marshal.ReadByte(ptr, OffB) != 0xFE) return 14;
            if ((uint)Marshal.ReadInt32(ptr, OffUI) != 0xDEADBEEF) return 15;
            if ((sbyte)Marshal.ReadByte(ptr, OffSB) != -2) return 16;
            if ((ulong)Marshal.ReadInt64(ptr, OffUL) != ulong.MaxValue - 1) return 17;
            if ((ushort)Marshal.ReadInt16(ptr, OffUS) != 54321) return 18;
            if (Marshal.ReadInt32(ptr, OffI) != int.MinValue) return 19;
            if ((ushort)Marshal.ReadInt16(ptr, OffFlags) != 0x8001) return 20;

            var ra = Marshal.PtrToStructure<AllWidths>(ptr);
            if (ra.Pad0 != 0x11 || ra.S != E16.A || ra.Pad1 != 0x22 || ra.L != E64.A || ra.B != E8.A) return 21;
            if (ra.UI != EU32.A || ra.SB != ES8.A || ra.UL != EU64.A || ra.US != EU16.A || ra.I != E32.A) return 22;
            if (ra.Flags != (F.X | F.Z) || !ra.Flags.HasFlag(F.Z) || ra.Flags.HasFlag(F.Y)) return 23;

            // A value no enum member names still round-trips: the field is its underlying integer.
            Marshal.WriteInt16(ptr, OffFlags, 0x7FFE);
            Marshal.WriteInt32(ptr, OffI, 42);
            var unnamed = Marshal.PtrToStructure<AllWidths>(ptr);
            if ((ushort)unnamed.Flags != 0x7FFE) return 24;
            if ((int)unnamed.I != 42) return 25;

            var o = new Outer { Head = E8.A, In = new Inner { Tag = 9, Kind = E32.A }, Tail = E16.A };
            Marshal.StructureToPtr(o, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != 0xFE) return 30;
            if (Marshal.ReadByte(ptr, 4) != 9) return 31;
            if (Marshal.ReadInt32(ptr, 8) != int.MinValue) return 32;
            if (Marshal.ReadInt16(ptr, 12) != -12345) return 33;
            var ro = Marshal.PtrToStructure<Outer>(ptr);
            if (ro.Head != E8.A || ro.In.Tag != 9 || ro.In.Kind != E32.A || ro.Tail != E16.A) return 34;

            var e = new Ex { Wide = (E64)0x0102030405060708L, Hi = EU16.A };
            e.Odd = (E8)0x77;
            Marshal.StructureToPtr(e, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 0x05060708) return 40;
            if (Marshal.ReadByte(ptr, 9) != 0x77) return 41;
            if ((ushort)Marshal.ReadInt16(ptr, 12) != 54321) return 42;
            Marshal.WriteInt64(ptr, 0, 0x4444444455555555L);
            var re = Marshal.PtrToStructure<Ex>(ptr);
            if ((long)re.Wide != 0x4444444455555555L) return 43;
            if ((int)re.Low != 0x55555555) return 44;
            if (re.Hi != EU16.A) return 45;

            var p = new Packed { B = 3, L = E64.A, S = E16.A };
            Marshal.StructureToPtr(p, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != 3) return 50;
            if (Marshal.ReadInt64(ptr, 1) != long.MinValue + 1) return 51;
            if (Marshal.ReadInt16(ptr, 9) != -12345) return 52;
            var rp = Marshal.PtrToStructure<Packed>(ptr);
            if (rp.B != 3 || rp.L != E64.A || rp.S != E16.A) return 53;

            var d = new WithDecimal { Kind = E16.A, M = 1.50m, Tail = EU64.A };
            Marshal.StructureToPtr(d, ptr, false);
            if (Marshal.ReadInt16(ptr, 0) != -12345) return 60;
            if ((ulong)Marshal.ReadInt64(ptr, 24) != ulong.MaxValue - 1) return 61;
            var rd = Marshal.PtrToStructure<WithDecimal>(ptr);
            if (rd.Kind != E16.A || rd.M != 1.50m || rd.Tail != EU64.A) return 62;
            Marshal.WriteInt16(ptr, 0, 5);
            Marshal.WriteInt64(ptr, 24, 6);
            var nd = Marshal.PtrToStructure<WithDecimal>(ptr);
            if ((short)nd.Kind != 5 || (ulong)nd.Tail != 6 || nd.M != 1.50m) return 63;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }

        // An enum on its own is auto-layout, so it has no native layout to marshal.
        try
        {
            Marshal.SizeOf<E32>();
            return 70;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}
