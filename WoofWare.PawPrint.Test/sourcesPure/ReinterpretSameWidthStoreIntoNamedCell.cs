using System;
using System.Runtime.CompilerServices;

// Every same-width primitive `stind` into a primitive field of a different type: `stind.i1` into a
// `byte`, `sbyte` and `bool`, `stind.i2` into a `char` and a `short`, `stind.i4`/`stind.r4` across
// `uint`, `int` and `float`, `stind.i8`/`stind.r8` across `ulong`, `long` and `double`. The guest reads
// each field back through its own type and sees the stored bit pattern read as that type -- a
// negative zero stays negative, which a numeric conversion would not preserve.
//
// The fields sit in an `[InlineArray]` slot whose element holds a reference, reached through a local,
// an array element and a class field, because storage holding a reference has no byte image and the
// store has to name the cell. The last block repeats a few of the stores over storage with no
// reference, which is served bytewise.
public class TestReinterpretSameWidthStoreIntoNamedCell
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
    }

    private sealed class Holder { public Buffer Buf; }

    private static int CheckElem(ref Elem e, int baseCode)
    {
        Unsafe.As<byte, sbyte>(ref e.U8) = -1;
        if (e.U8 != 255) return baseCode + 1;
        Unsafe.As<sbyte, byte>(ref e.I8) = 200;
        if (e.I8 != -56) return baseCode + 2;
        Unsafe.As<bool, byte>(ref e.Flag) = 1;
        if (!e.Flag) return baseCode + 3;
        Unsafe.As<char, short>(ref e.C) = -1;
        if (e.C != '\uFFFF') return baseCode + 4;
        Unsafe.As<short, char>(ref e.I16) = '\uFFFE';
        if (e.I16 != -2) return baseCode + 5;
        Unsafe.As<uint, int>(ref e.U32) = -1;
        if (e.U32 != uint.MaxValue) return baseCode + 6;
        Unsafe.As<int, uint>(ref e.I32) = 0x80000000u;
        if (e.I32 != int.MinValue) return baseCode + 7;
        Unsafe.As<float, int>(ref e.F32) = 0x3FC00000;
        if (e.F32 != 1.5f) return baseCode + 8;
        Unsafe.As<int, float>(ref e.I32) = -2.5f;
        if (e.I32 != unchecked((int)0xC0200000)) return baseCode + 9;
        Unsafe.As<ulong, long>(ref e.U64) = -1L;
        if (e.U64 != ulong.MaxValue) return baseCode + 10;
        Unsafe.As<double, long>(ref e.F64) = 0x3FF8000000000000L;
        if (e.F64 != 1.5) return baseCode + 11;
        Unsafe.As<long, double>(ref e.I64) = 1.5;
        if (e.I64 != 0x3FF8000000000000L) return baseCode + 12;
        // A negative zero survives as a bit pattern: numeric conversion would lose its sign.
        Unsafe.As<double, long>(ref e.F64) = unchecked((long)0x8000000000000000UL);
        if (!double.IsNegative(e.F64) || e.F64 != 0.0) return baseCode + 13;
        // Plain `ref byte` over the cell, no `Unsafe.As` of our own.
        ref byte q = ref e.U8;
        q = 9;
        if (e.U8 != 9) return baseCode + 14;
        return 0;
    }

    public static int Main(string[] argv)
    {
        Buffer b = default;
        b[0] = new Elem { U8 = 5, Payload = new Box { V = 50 } };
        b[1] = new Elem { U8 = 6, Payload = new Box { V = 60 } };

        int r = CheckElem(ref b[1], 0);
        if (r != 0) return r;
        if (b[1].Payload.V != 60 || b[0].U8 != 5 || b[0].Payload.V != 50) return 20;
        r = CheckElem(ref b[0], 20);
        if (r != 0) return r;

        Buffer[] arr = new Buffer[2];
        arr[1][1].Payload = new Box { V = 70 };
        r = CheckElem(ref arr[1][1], 40);
        if (r != 0) return r;
        if (arr[1][1].Payload.V != 70) return 60;

        Holder h = new Holder();
        h.Buf[1].Payload = new Box { V = 80 };
        r = CheckElem(ref h.Buf[1], 60);
        if (r != 0) return r;
        if (h.Buf[1].Payload.V != 80) return 80;

        // The same stores over storage holding no reference.
        Plain p = default;
        Unsafe.As<byte, sbyte>(ref p.U8) = -1;
        if (p.U8 != 255) return 81;
        Unsafe.As<char, short>(ref p.C) = -1;
        if (p.C != '\uFFFF') return 82;
        Unsafe.As<float, int>(ref p.F32) = 0x3FC00000;
        if (p.F32 != 1.5f) return 83;
        Unsafe.As<int, float>(ref p.I32) = -2.5f;
        if (p.I32 != unchecked((int)0xC0200000)) return 84;
        Unsafe.As<long, double>(ref p.I64) = 1.5;
        if (p.I64 != 0x3FF8000000000000L) return 85;
        Unsafe.As<bool, byte>(ref p.Flag) = 1;
        if (!p.Flag) return 86;

        return 0;
    }
}
