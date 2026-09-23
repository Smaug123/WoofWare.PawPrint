using System;
using System.Runtime.CompilerServices;

// The read mirror of `ReinterpretSameWidthStoreIntoNamedCell.cs`: every primitive field read through
// a same-width `Unsafe.As` view of a different primitive type. `byte`, `sbyte` and `bool` are read as
// one another, `char` and `short` as each other, `uint`, `int` and `float` across one another, and
// `ulong`, `long` and `double` likewise. The guest sees the field's bit pattern read as the view's type
// -- 200 as `sbyte` is -56, 1.5f as `int` is 0x3FC00000, and a negative zero read as `long` keeps its
// sign bit, which a numeric conversion would not.
//
// The fields sit in an `[InlineArray]` slot whose element holds a reference, reached through a local,
// an array element and a class field, because storage holding a reference has no byte image and the
// read has to name the cell. The last block repeats a few of the reads over storage with
// no reference, which is served bytewise.
public class TestReinterpretSameWidthReadFromNamedCell
{
    private sealed class Box { public int V; }

    private struct Elem
    {
        public byte U8;
        public sbyte I8;
        public bool Flag;
        public char C;
        public short I16;
        public uint U32;
        public int I32;
        public float F32;
        public ulong U64;
        public long I64;
        public double F64;
        public Box Payload;
    }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    private struct Plain
    {
        public byte U8;
        public char C;
        public int I32;
        public float F32;
        public double F64;
    }

    private sealed class Holder { public Buffer Buf; }

    private static void Fill(ref Elem e, int payload)
    {
        e.U8 = 200;
        e.I8 = -1;
        e.Flag = true;
        e.C = '\uFFFE';
        e.I16 = -1;
        e.U32 = 0x80000000u;
        e.I32 = unchecked((int)0xC0200000);
        e.F32 = 1.5f;
        e.U64 = ulong.MaxValue;
        e.I64 = 0x3FF8000000000000L;
        e.F64 = -0.0;
        e.Payload = new Box { V = payload };
    }

    private static int CheckElem(ref Elem e, int baseCode)
    {
        if (Unsafe.As<byte, sbyte>(ref e.U8) != -56) return baseCode + 1;
        if (!Unsafe.As<byte, bool>(ref e.U8)) return baseCode + 2;
        if (Unsafe.As<sbyte, byte>(ref e.I8) != 255) return baseCode + 3;
        if (Unsafe.As<bool, byte>(ref e.Flag) != 1) return baseCode + 4;
        if (Unsafe.As<char, short>(ref e.C) != -2) return baseCode + 5;
        if (Unsafe.As<short, char>(ref e.I16) != '\uFFFF') return baseCode + 6;
        if (Unsafe.As<uint, int>(ref e.U32) != int.MinValue) return baseCode + 7;
        if (Unsafe.As<int, float>(ref e.I32) != -2.5f) return baseCode + 8;
        if (Unsafe.As<float, int>(ref e.F32) != 0x3FC00000) return baseCode + 9;
        if (Unsafe.As<float, uint>(ref e.F32) != 0x3FC00000u) return baseCode + 10;
        if (Unsafe.As<ulong, long>(ref e.U64) != -1L) return baseCode + 11;
        if (Unsafe.As<long, double>(ref e.I64) != 1.5) return baseCode + 12;
        if (Unsafe.As<long, ulong>(ref e.I64) != 0x3FF8000000000000UL) return baseCode + 13;
        // A negative zero read as an integer is its sign bit alone.
        if (Unsafe.As<double, long>(ref e.F64) != unchecked((long)0x8000000000000000UL)) return baseCode + 14;
        // The same field read through its own type is untouched by the views above.
        if (e.U8 != 200 || e.F32 != 1.5f) return baseCode + 15;
        return 0;
    }

    public static int Main(string[] argv)
    {
        Buffer b = default;
        Fill(ref b[0], 50);
        Fill(ref b[1], 60);

        int r = CheckElem(ref b[1], 0);
        if (r != 0) return r;
        r = CheckElem(ref b[0], 20);
        if (r != 0) return r;
        if (b[1].Payload.V != 60 || b[0].Payload.V != 50) return 40;

        Buffer[] arr = new Buffer[2];
        Fill(ref arr[1][1], 70);
        r = CheckElem(ref arr[1][1], 40);
        if (r != 0) return r;
        if (arr[1][1].Payload.V != 70) return 60;

        Holder h = new Holder();
        Fill(ref h.Buf[1], 80);
        r = CheckElem(ref h.Buf[1], 60);
        if (r != 0) return r;
        if (h.Buf[1].Payload.V != 80) return 80;

        // The same reads over storage holding no reference.
        Plain p = default;
        p.U8 = 200;
        p.C = '\uFFFE';
        p.I32 = unchecked((int)0xC0200000);
        p.F32 = 1.5f;
        p.F64 = -0.0;
        if (Unsafe.As<byte, sbyte>(ref p.U8) != -56) return 101;
        if (Unsafe.As<char, short>(ref p.C) != -2) return 102;
        if (Unsafe.As<int, float>(ref p.I32) != -2.5f) return 103;
        if (Unsafe.As<float, int>(ref p.F32) != 0x3FC00000) return 104;
        if (Unsafe.As<double, long>(ref p.F64) != unchecked((long)0x8000000000000000UL)) return 105;

        return 0;
    }
}
