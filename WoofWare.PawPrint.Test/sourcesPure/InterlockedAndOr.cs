using System;
using System.Threading;

class Program
{
    class Holder
    {
        public int Slot = 0b1010;
        public long Wide = 0b1010L;
    }

    static int s_staticSlot = 0b0110;
    static long s_staticWide = 0b0110L;

    static int Main(string[] args)
    {
        // Int32. Both And and Or return the ORIGINAL value, not the new one.
        int i = 0b1100;
        if (Interlocked.Or(ref i, 0b0011) != 0b1100 || i != 0b1111) return 1;
        // Idempotent: re-applying the same mask leaves the location alone.
        if (Interlocked.Or(ref i, 0b0011) != 0b1111 || i != 0b1111) return 2;
        if (Interlocked.And(ref i, 0b0101) != 0b1111 || i != 0b0101) return 3;
        if (Interlocked.And(ref i, 0b0101) != 0b0101 || i != 0b0101) return 4;
        // Absorbing and identity elements.
        if (Interlocked.And(ref i, 0) != 0b0101 || i != 0) return 5;
        if (Interlocked.Or(ref i, -1) != 0 || i != -1) return 6;
        if (Interlocked.And(ref i, -1) != -1 || i != -1) return 7;
        if (Interlocked.Or(ref i, 0) != -1 || i != -1) return 8;

        // Sign bit: the operands are bit patterns, so int.MinValue must survive.
        int neg = int.MinValue;
        if (Interlocked.Or(ref neg, 1) != int.MinValue || neg != int.MinValue + 1) return 9;
        if (Interlocked.And(ref neg, int.MinValue) != int.MinValue + 1 || neg != int.MinValue) return 10;
        int signClear = -1;
        if (Interlocked.And(ref signClear, int.MaxValue) != -1 || signClear != int.MaxValue) return 11;
        if (Interlocked.Or(ref signClear, int.MinValue) != int.MaxValue || signClear != -1) return 12;

        // UInt32: the BCL overload is a non-intrinsic `Unsafe.As<uint, int>` forwarder onto
        // the signed one, so this exercises a different path to the same primitive.
        uint u = 0xF0F0F0F0U;
        if (Interlocked.Or(ref u, 0x0F0F0F0FU) != 0xF0F0F0F0U || u != uint.MaxValue) return 13;
        if (Interlocked.And(ref u, 0x0000FFFFU) != uint.MaxValue || u != 0x0000FFFFU) return 14;
        if (Interlocked.And(ref u, 0U) != 0x0000FFFFU || u != 0U) return 15;
        if (Interlocked.Or(ref u, 0x80000000U) != 0U || u != 0x80000000U) return 16;

        // Int64.
        long l = 0x00000000FFFFFFFFL;
        if (Interlocked.Or(ref l, unchecked((long)0xFFFFFFFF00000000UL)) != 0x00000000FFFFFFFFL || l != -1L) return 17;
        if (Interlocked.And(ref l, 0x0F0F0F0F0F0F0F0FL) != -1L || l != 0x0F0F0F0F0F0F0F0FL) return 18;
        if (Interlocked.And(ref l, 0L) != 0x0F0F0F0F0F0F0F0FL || l != 0L) return 19;
        if (Interlocked.Or(ref l, long.MinValue) != 0L || l != long.MinValue) return 20;
        if (Interlocked.And(ref l, long.MaxValue) != long.MinValue || l != 0L) return 21;

        // UInt64: same `Unsafe.As` forwarder story as uint.
        ulong ul = 0xFFFFFFFF00000000UL;
        if (Interlocked.Or(ref ul, 0x00000000FFFFFFFFUL) != 0xFFFFFFFF00000000UL || ul != ulong.MaxValue) return 22;
        if (Interlocked.And(ref ul, 0x00FF00FF00FF00FFUL) != ulong.MaxValue || ul != 0x00FF00FF00FF00FFUL) return 23;
        if (Interlocked.And(ref ul, 0UL) != 0x00FF00FF00FF00FFUL || ul != 0UL) return 24;
        if (Interlocked.Or(ref ul, 0x8000000000000000UL) != 0UL || ul != 0x8000000000000000UL) return 25;

        // Instance fields.
        Holder holder = new Holder();
        if (Interlocked.Or(ref holder.Slot, 0b0101) != 0b1010 || holder.Slot != 0b1111) return 26;
        if (Interlocked.And(ref holder.Slot, 0b1001) != 0b1111 || holder.Slot != 0b1001) return 27;
        if (Interlocked.Or(ref holder.Wide, 0b0101L) != 0b1010L || holder.Wide != 0b1111L) return 28;
        if (Interlocked.And(ref holder.Wide, 0b1001L) != 0b1111L || holder.Wide != 0b1001L) return 29;

        // Static fields.
        if (Interlocked.Or(ref s_staticSlot, 0b1001) != 0b0110 || s_staticSlot != 0b1111) return 30;
        if (Interlocked.And(ref s_staticSlot, 0b0011) != 0b1111 || s_staticSlot != 0b0011) return 31;
        if (Interlocked.Or(ref s_staticWide, 0b1001L) != 0b0110L || s_staticWide != 0b1111L) return 32;
        if (Interlocked.And(ref s_staticWide, 0b0011L) != 0b1111L || s_staticWide != 0b0011L) return 33;

        // Array elements. There is no narrow-integer overload, so int/long only.
        int[] ints = new int[] { 0b0001, 0b0010 };
        if (Interlocked.Or(ref ints[1], 0b0100) != 0b0010 || ints[1] != 0b0110) return 34;
        if (Interlocked.And(ref ints[1], 0b0100) != 0b0110 || ints[1] != 0b0100) return 35;
        if (ints[0] != 0b0001) return 36;

        long[] longs = new long[] { 0b0001L, 0b0010L };
        if (Interlocked.Or(ref longs[0], 0b1000L) != 0b0001L || longs[0] != 0b1001L) return 37;
        if (Interlocked.And(ref longs[0], 0b1000L) != 0b1001L || longs[0] != 0b1000L) return 38;
        if (longs[1] != 0b0010L) return 39;

        return 0;
    }
}
