using System;
using System.Runtime.InteropServices;

public class Program
{
    interface IPointerHolder
    {
        bool Holds(IntPtr p);
    }

    // Every field kind the struct-marshal stub copies verbatim. The Decimal is what sends the
    // struct through the stub at all: CoreCLR never treats a Decimal field as blittable, so the
    // other fields are unmarshalled one by one alongside it rather than memmoved.
    [StructLayout(LayoutKind.Sequential)]
    struct Mixed : IPointerHolder
    {
        public byte B;
        public sbyte SB;
        public short S;
        public ushort US;
        public int I;
        public uint UI;
        public long L;
        public ulong UL;
        public float F;
        public double D;
        public short C;
        public IntPtr P;
        public UIntPtr UP;
        public decimal M;

        // An instance call on the field itself, so it reads `IntPtr._value` through the field's
        // home rather than loading the field's value.
        public bool Holds(IntPtr p) => P.Equals(p);
    }

    // Overlapping copied fields. Both read the same native bytes, so unlike a DateTime overlap the
    // order they are unmarshalled in cannot show; what can is that each of them is read.
    [StructLayout(LayoutKind.Explicit)]
    struct Overlapping
    {
        [FieldOffset(0)] public long Wide;
        [FieldOffset(0)] public int Narrow;
        [FieldOffset(8)] public decimal M;
    }

    // Native offsets of `Mixed`'s fields, measured on real .NET with Marshal.OffsetOf.
    const int OffB = 0, OffSB = 1, OffS = 2, OffUS = 4, OffI = 8, OffUI = 12, OffL = 16, OffUL = 24,
        OffF = 32, OffD = 40, OffC = 48, OffP = 56, OffUP = 64, OffM = 72, SizeMixed = 88;

    static bool Same(Mixed a, Mixed b) =>
        a.B == b.B && a.SB == b.SB && a.S == b.S && a.US == b.US && a.I == b.I && a.UI == b.UI
        && a.L == b.L && a.UL == b.UL
        && BitConverter.SingleToInt32Bits(a.F) == BitConverter.SingleToInt32Bits(b.F)
        && BitConverter.DoubleToInt64Bits(a.D) == BitConverter.DoubleToInt64Bits(b.D) && a.C == b.C
        && a.P == b.P && a.UP == b.UP && a.M == b.M
        && decimal.GetBits(a.M).AsSpan().SequenceEqual(decimal.GetBits(b.M));

    // Deterministic LCG, so the sweep is the same on every run and on every runtime.
    static ulong state = 0x2545F4914F6CDD1D;

    static ulong Next()
    {
        state = state * 6364136223846793005UL + 1442695040888963407UL;
        return state;
    }

    static Mixed RandomMixed(IntPtr pointee)
    {
        ulong a = Next(), b = Next(), c = Next();
        return new Mixed
        {
            B = (byte)a, SB = (sbyte)(a >> 8), S = (short)(a >> 16), US = (ushort)(a >> 32),
            I = (int)b, UI = (uint)(b >> 32), L = (long)Next(), UL = Next(),
            F = BitConverter.Int32BitsToSingle((int)c), D = BitConverter.Int64BitsToDouble((long)Next()),
            C = (short)(c >> 32), P = pointee + (int)((c >> 48) % 4), UP = (UIntPtr)Next(),
            M = new decimal((int)Next(), (int)Next(), (int)Next(), (c & 1) != 0, (byte)((c >> 1) % 29)),
        };
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<Mixed>() != SizeMixed) return 1;

        IntPtr ptr = Marshal.AllocHGlobal(SizeMixed);
        IntPtr bytes = Marshal.AllocHGlobal(SizeMixed);
        IntPtr pointee = Marshal.AllocHGlobal(4);
        try
        {
            // Round trip: every copied field comes back exactly, including a pointer that the
            // guest got from AllocHGlobal and a Decimal's scale (1.50m is not 1.5m bit for bit).
            var s = new Mixed
            {
                B = 0xFE, SB = -7, S = -12345, US = 54321, I = int.MinValue, UI = 0xDEADBEEF,
                L = long.MinValue + 1, UL = ulong.MaxValue - 1, F = -0.0f, D = double.Epsilon,
                C = -3, P = pointee, UP = (UIntPtr)0x1234, M = 1.50m,
            };
            Marshal.StructureToPtr(s, ptr, false);
            var r = Marshal.PtrToStructure<Mixed>(ptr);
            if (!Same(s, r)) return 10;
            Marshal.WriteInt32(r.P, 0, 99);
            if (Marshal.ReadInt32(pointee, 0) != 99) return 11;

            // The non-generic overload hands back the very box the stub unmarshalled into, so
            // what the stub stored there is what the guest sees, field shapes and all.
            object boxed = Marshal.PtrToStructure(ptr, typeof(Mixed));
            if (!((IPointerHolder)boxed).Holds(pointee)) return 13;
            if (!Same(s, (Mixed)boxed)) return 14;

            // For every field kind copied verbatim, a round trip is the identity, bit for bit
            // (NaN payloads and negative zeros included, since `Same` compares floats' bits).
            for (int i = 0; i < 24; i++)
            {
                var x = RandomMixed(pointee);
                Marshal.StructureToPtr(x, ptr, false);
                if (!Same(x, Marshal.PtrToStructure<Mixed>(ptr))) return 12;
            }

            // A native image written piecemeal rather than by StructureToPtr: the stub reads each
            // field from wherever its native offset says, whatever wrote the bytes.
            for (int i = 0; i < SizeMixed; i++) Marshal.WriteByte(bytes, i, 0);
            Marshal.WriteByte(bytes, OffB, 0x81);
            Marshal.WriteByte(bytes, OffSB, 0x80);
            Marshal.WriteInt16(bytes, OffS, 0x1234);
            Marshal.WriteInt16(bytes, OffUS, unchecked((short)0xFFFE));
            Marshal.WriteInt32(bytes, OffI, 0x01020304);
            Marshal.WriteInt32(bytes, OffUI, -2);
            Marshal.WriteInt64(bytes, OffL, 0x0102030405060708L);
            Marshal.WriteInt64(bytes, OffUL, -3L);
            Marshal.WriteInt32(bytes, OffF, BitConverter.SingleToInt32Bits(2.5f));
            Marshal.WriteInt64(bytes, OffD, BitConverter.DoubleToInt64Bits(-1e300));
            Marshal.WriteInt16(bytes, OffC, 2);
            Marshal.WriteInt64(bytes, OffP, 0x7766);
            Marshal.WriteInt64(bytes, OffUP, 0x5544);
            // Native DECIMAL: wReserved, scale, sign, Hi32, Lo64. -12.345m is 12345 at scale 3.
            Marshal.WriteInt16(bytes, OffM, 0);
            Marshal.WriteByte(bytes, OffM + 2, 3);
            Marshal.WriteByte(bytes, OffM + 3, 0x80);
            Marshal.WriteInt32(bytes, OffM + 4, 0);
            Marshal.WriteInt64(bytes, OffM + 8, 12345);
            var n = Marshal.PtrToStructure<Mixed>(bytes);
            if (n.B != 0x81) return 20;
            if (n.SB != -128) return 21;
            if (n.S != 0x1234) return 22;
            if (n.US != 0xFFFE) return 23;
            if (n.I != 0x01020304) return 24;
            if (n.UI != 0xFFFFFFFE) return 25;
            if (n.L != 0x0102030405060708L) return 26;
            if (n.UL != 0xFFFFFFFFFFFFFFFD) return 27;
            if (n.F != 2.5f) return 28;
            if (n.D != -1e300) return 29;
            if (n.C != 2) return 30;
            if (n.P != (IntPtr)0x7766) return 31;
            if (n.UP != (UIntPtr)0x5544) return 32;
            if (n.M != -12.345m) return 33;

            // `Narrow` is declared after `Wide`, so on the way out it overwrites Wide's low half.
            // On the way back, Wide's high half survives only if Wide itself was read.
            var o = new Overlapping { Wide = 0x1111111122222222L, M = 7m };
            o.Narrow = 0x33333333;
            Marshal.StructureToPtr(o, ptr, false);
            if (Marshal.ReadInt64(ptr, 0) != 0x1111111133333333L) return 40;
            Marshal.WriteInt64(ptr, 0, 0x4444444455555555L);
            var ro = Marshal.PtrToStructure<Overlapping>(ptr);
            if (ro.Wide != 0x4444444455555555L) return 41;
            if (ro.Narrow != 0x55555555) return 42;
            if (ro.M != 7m) return 43;
        }
        finally
        {
            Marshal.FreeHGlobal(pointee);
            Marshal.FreeHGlobal(bytes);
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
