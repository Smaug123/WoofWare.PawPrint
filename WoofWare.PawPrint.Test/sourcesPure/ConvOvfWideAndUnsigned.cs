using System;

public class ConvOvfWideAndUnsigned
{
    // Each checked conversion below compiles to one of the `conv.ovf.*` opcodes that the other
    // ConvOvf* files do not reach: `conv.ovf.i2`, `conv.ovf.i8`, `conv.ovf.u8`, and the `.un`
    // forms Roslyn emits for an unsigned source (`i1.un`, `i2.un`, `u2.un`, `u4.un`, `i8.un`,
    // `u.un`). None of these C# conversions compiles to `conv.ovf.u8.un`; TestNullaryIlOp checks
    // that one against the host's own opcode instead. Each conversion is taken on both sides of
    // its range, since one that never overflowed and one that always did would each pass half of
    // them. The sources are fields, not constants, so that Roslyn cannot fold the conversion away.

    private static volatile int s_int = 0;
    private static long s_long = 0L;
    private static double s_double = 0.0;
    private static uint s_uint = 0;
    private static ulong s_ulong = 0;

    private static int s_failure = 0;

    private static void Expect(bool condition, int code)
    {
        if (!condition && s_failure == 0)
        {
            s_failure = code;
        }
    }

    private static bool Overflows(Action conversion)
    {
        try
        {
            conversion();
            return false;
        }
        catch (OverflowException)
        {
            return true;
        }
    }

    public static int Main(string[] args)
    {
        // conv.ovf.i2, from int, long and double.
        s_int = -32768;
        Expect(checked((short)s_int) == -32768, 1);
        s_int = -32769;
        Expect(Overflows(() => { short _ = checked((short)s_int); }), 2);
        s_long = 32767L;
        Expect(checked((short)s_long) == 32767, 3);
        // The low 16 bits are 5, which is in range: only a range check of the full width overflows.
        s_long = 0x1_0000_0005L;
        Expect(Overflows(() => { short _ = checked((short)s_long); }), 4);
        s_double = -32768.9;
        Expect(checked((short)s_double) == -32768, 5);
        s_double = 32768.0;
        Expect(Overflows(() => { short _ = checked((short)s_double); }), 6);

        // conv.ovf.i8 from double: 2^63 is the smallest double that overflows, and -2^63 is in
        // range. This is the conversion Kestrel's TimeExtensions.ToTicks performs.
        s_double = -9223372036854775808.0;
        Expect(checked((long)s_double) == long.MinValue, 7);
        s_double = 9223372036854775808.0;
        Expect(Overflows(() => { long _ = checked((long)s_double); }), 8);
        s_double = 1.5e9 * 1000.0;
        Expect(checked((long)s_double) == 1_500_000_000_000L, 9);

        // conv.ovf.u8, from int, long and double: any negative source overflows, and a fraction
        // above -1.0 truncates to zero.
        s_int = 0;
        Expect(checked((ulong)s_int) == 0UL, 10);
        s_int = -1;
        Expect(Overflows(() => { ulong _ = checked((ulong)s_int); }), 11);
        s_long = long.MaxValue;
        Expect(checked((ulong)s_long) == 9223372036854775807UL, 12);
        s_long = long.MinValue;
        Expect(Overflows(() => { ulong _ = checked((ulong)s_long); }), 13);
        s_double = -0.75;
        Expect(checked((ulong)s_double) == 0UL, 14);
        s_double = 18446744073709551616.0;
        Expect(Overflows(() => { ulong _ = checked((ulong)s_double); }), 15);

        // conv.ovf.i1.un, conv.ovf.i2.un and conv.ovf.u2.un, from uint: a uint with its top bit
        // set is a large positive number here, not a negative one.
        s_uint = 127;
        Expect(checked((sbyte)s_uint) == 127, 16);
        s_uint = 0xFFFF_FFFF;
        Expect(Overflows(() => { sbyte _ = checked((sbyte)s_uint); }), 17);
        s_uint = 32767;
        Expect(checked((short)s_uint) == 32767, 18);
        s_uint = 32768;
        Expect(Overflows(() => { short _ = checked((short)s_uint); }), 19);
        s_uint = 65535;
        Expect(checked((ushort)s_uint) == 65535, 20);
        s_uint = 65536;
        Expect(Overflows(() => { ushort _ = checked((ushort)s_uint); }), 21);

        // conv.ovf.u4.un and conv.ovf.i8.un, from ulong.
        s_ulong = 0xFFFF_FFFFUL;
        Expect(checked((uint)s_ulong) == 0xFFFF_FFFFU, 22);
        s_ulong = 0x1_0000_0000UL;
        Expect(Overflows(() => { uint _ = checked((uint)s_ulong); }), 23);
        s_ulong = 0x7FFF_FFFF_FFFF_FFFFUL;
        Expect(checked((long)s_ulong) == long.MaxValue, 24);
        s_ulong = 0x8000_0000_0000_0000UL;
        Expect(Overflows(() => { long _ = checked((long)s_ulong); }), 25);

        // conv.ovf.u.un, from ulong: every ulong fits in a 64-bit nuint.
        s_ulong = ulong.MaxValue;
        Expect(checked((nuint)s_ulong) == nuint.MaxValue, 26);

        return s_failure;
    }
}
