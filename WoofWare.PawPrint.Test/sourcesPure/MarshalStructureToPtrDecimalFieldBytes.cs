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

    [StructLayout(LayoutKind.Sequential)]
    struct DecimalBetween
    {
        public byte Head;
        public decimal Value;
        public int Tail;
    }

    [StructLayout(LayoutKind.Sequential, Pack = 4)]
    struct PackedDecimal
    {
        public int Id;
        public decimal Value;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct ExplicitDecimal
    {
        [FieldOffset(0)] public int Id;
        [FieldOffset(4)] public decimal Value;
    }

    const int BufferSize = 64;
    const byte Dirty = 0xAB;

    static void Soil(IntPtr ptr)
    {
        for (int i = 0; i < BufferSize; i++) Marshal.WriteByte(ptr, i, Dirty);
    }

    static bool IsZero(IntPtr ptr, int from, int to)
    {
        for (int i = from; i < to; i++)
        {
            if (Marshal.ReadByte(ptr, i) != 0) return false;
        }
        return true;
    }

    static bool IsDirty(IntPtr ptr, int from, int to)
    {
        for (int i = from; i < to; i++)
        {
            if (Marshal.ReadByte(ptr, i) != Dirty) return false;
        }
        return true;
    }

    // Native `DECIMAL` (oleauto.h): `USHORT wReserved; BYTE scale; BYTE sign; ULONG Hi32;
    // ULONGLONG Lo64`, where `sign` is 0x80 for a negative value and 0 otherwise. The expected
    // image is derived from `decimal.GetBits` (lo, mid, hi, flags) independently of the struct
    // under test, so a Decimal written with its words out of order fails here.
    static bool HoldsDecimal(IntPtr ptr, int offset, decimal d)
    {
        int[] bits = decimal.GetBits(d);
        int scale = (bits[3] >> 16) & 0xFF;
        bool negative = bits[3] < 0;

        if (Marshal.ReadInt16(ptr, offset) != 0) return false;
        if (Marshal.ReadByte(ptr, offset + 2) != scale) return false;
        if (Marshal.ReadByte(ptr, offset + 3) != (negative ? 0x80 : 0)) return false;
        if (Marshal.ReadInt32(ptr, offset + 4) != bits[2]) return false;
        long lo64 = (long)(((ulong)(uint)bits[1] << 32) | (uint)bits[0]);
        if (Marshal.ReadInt64(ptr, offset + 8) != lo64) return false;
        return true;
    }

    // Deterministic LCG, so the sweep is the same on every run and on every runtime.
    static uint state = 0x2545F491;

    static uint Next()
    {
        state = state * 1664525 + 1013904223;
        return state;
    }

    static decimal RandomDecimal()
    {
        int lo = (int)Next();
        int mid = (int)Next();
        uint shape = Next();
        // Vary the magnitude: an all-zero high word, a small one, or a full-width one.
        int hi = (shape % 3) switch
        {
            0 => 0,
            1 => (int)(Next() & 0xFF),
            _ => (int)Next(),
        };
        bool negative = (shape & 0x100) != 0;
        byte scale = (byte)((shape >> 16) % 29);
        return new decimal(lo, mid, hi, negative, scale);
    }

    static int CheckWithDecimal(IntPtr ptr, decimal d, int id)
    {
        Soil(ptr);
        Marshal.StructureToPtr(new WithDecimal { Id = id, Value = d }, ptr, false);
        if (Marshal.ReadInt32(ptr, 0) != id) return 1;
        // Four bytes of native padding realign the DECIMAL to 8. The stub zeroes the image
        // before writing fields, so they are zero rather than whatever the buffer held.
        if (!IsZero(ptr, 4, 8)) return 2;
        if (!HoldsDecimal(ptr, 8, d)) return 3;
        if (!IsDirty(ptr, 24, BufferSize)) return 4;
        return 0;
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<WithDecimal>() != 24) return 1;
        if (Marshal.SizeOf<DecimalBetween>() != 32) return 2;
        if (Marshal.SizeOf<PackedDecimal>() != 20) return 3;
        if (Marshal.SizeOf<ExplicitDecimal>() != 24) return 4;

        IntPtr ptr = Marshal.AllocHGlobal(BufferSize);
        try
        {
            decimal[] fixedValues =
            {
                1.5m,
                -1.5m,
                0m,
                new decimal(0, 0, 0, true, 3),
                decimal.MaxValue,
                decimal.MinValue,
                0.0000000000000000000000000001m,
                -123456789.987654321m,
            };
            for (int i = 0; i < fixedValues.Length; i++)
            {
                int r = CheckWithDecimal(ptr, fixedValues[i], i + 1);
                if (r != 0) return 10 + r;
            }

            for (int i = 0; i < 48; i++)
            {
                int r = CheckWithDecimal(ptr, RandomDecimal(), -i);
                if (r != 0) return 20 + r;
            }

            decimal between = -7.25m;
            Soil(ptr);
            Marshal.StructureToPtr(new DecimalBetween { Head = 0x5A, Value = between, Tail = 0x1234567 }, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != 0x5A) return 30;
            if (!IsZero(ptr, 1, 8)) return 31;
            if (!HoldsDecimal(ptr, 8, between)) return 32;
            if (Marshal.ReadInt32(ptr, 24) != 0x1234567) return 33;
            if (!IsZero(ptr, 28, 32)) return 34;
            if (!IsDirty(ptr, 32, BufferSize)) return 35;

            // `Pack = 4` caps DECIMAL's alignment, so it follows `Id` directly.
            decimal packed = 42.001m;
            Soil(ptr);
            Marshal.StructureToPtr(new PackedDecimal { Id = 9, Value = packed }, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 9) return 40;
            if (!HoldsDecimal(ptr, 4, packed)) return 41;
            if (!IsDirty(ptr, 20, BufferSize)) return 42;

            // Explicit layout places the DECIMAL where it is told to, and the image is still
            // rounded up to DECIMAL's 8-byte alignment, leaving four zeroed bytes of tail padding.
            decimal expl = -0.5m;
            Soil(ptr);
            Marshal.StructureToPtr(new ExplicitDecimal { Id = 11, Value = expl }, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 11) return 50;
            if (!HoldsDecimal(ptr, 4, expl)) return 51;
            if (!IsZero(ptr, 20, 24)) return 52;
            if (!IsDirty(ptr, 24, BufferSize)) return 53;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
