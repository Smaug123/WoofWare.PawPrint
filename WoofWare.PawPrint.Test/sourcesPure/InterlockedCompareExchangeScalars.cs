using System;
using System.Threading;

class Program
{
    class Holder
    {
        public int Slot = -7;
        public byte Narrow = 250;
    }

    static short StaticSlot = -11;

    static int Main(string[] args)
    {
        sbyte sb = -1;
        if (Interlocked.CompareExchange(ref sb, (sbyte)5, (sbyte)4) != -1 || sb != -1) return 1;
        if (Interlocked.CompareExchange(ref sb, (sbyte)5, (sbyte)-1) != -1 || sb != 5) return 2;

        byte b = 250;
        if (Interlocked.CompareExchange(ref b, (byte)7, (byte)8) != 250 || b != 250) return 3;
        if (Interlocked.CompareExchange(ref b, (byte)7, (byte)250) != 250 || b != 7) return 4;

        short s = -1234;
        if (Interlocked.CompareExchange(ref s, (short)2222, (short)3333) != -1234 || s != -1234) return 5;
        if (Interlocked.CompareExchange(ref s, (short)2222, (short)-1234) != -1234 || s != 2222) return 6;

        ushort us = 60000;
        if (Interlocked.CompareExchange(ref us, (ushort)17, (ushort)18) != 60000 || us != 60000) return 7;
        if (Interlocked.CompareExchange(ref us, (ushort)17, (ushort)60000) != 60000 || us != 17) return 8;

        int i = -100;
        if (Interlocked.CompareExchange(ref i, 10, -101) != -100 || i != -100) return 9;
        if (Interlocked.CompareExchange(ref i, 10, -100) != -100 || i != 10) return 10;

        uint ui = uint.MaxValue;
        if (Interlocked.CompareExchange(ref ui, 42U, 41U) != uint.MaxValue || ui != uint.MaxValue) return 11;
        if (Interlocked.CompareExchange(ref ui, 42U, uint.MaxValue) != uint.MaxValue || ui != 42U) return 12;

        long l = long.MinValue;
        if (Interlocked.CompareExchange(ref l, 99L, 98L) != long.MinValue || l != long.MinValue) return 13;
        if (Interlocked.CompareExchange(ref l, 99L, long.MinValue) != long.MinValue || l != 99L) return 14;

        ulong ul = ulong.MaxValue;
        if (Interlocked.CompareExchange(ref ul, 123UL, 122UL) != ulong.MaxValue || ul != ulong.MaxValue) return 15;
        if (Interlocked.CompareExchange(ref ul, 123UL, ulong.MaxValue) != ulong.MaxValue || ul != 123UL) return 16;

        Holder holder = new Holder();
        if (Interlocked.CompareExchange(ref holder.Slot, 12, 0) != -7 || holder.Slot != -7) return 17;
        if (Interlocked.CompareExchange(ref holder.Slot, 12, -7) != -7 || holder.Slot != 12) return 18;
        if (Interlocked.CompareExchange(ref holder.Narrow, (byte)9, (byte)8) != 250 || holder.Narrow != 250) return 19;
        if (Interlocked.CompareExchange(ref holder.Narrow, (byte)9, (byte)250) != 250 || holder.Narrow != 9) return 20;

        byte[] bytes = new byte[] { 33, 44 };
        if (Interlocked.CompareExchange(ref bytes[1], (byte)55, (byte)43) != 44 || bytes[1] != 44) return 21;
        if (Interlocked.CompareExchange(ref bytes[1], (byte)55, (byte)44) != 44 || bytes[1] != 55) return 22;

        if (Interlocked.CompareExchange(ref StaticSlot, (short)22, (short)0) != -11 || StaticSlot != -11) return 23;
        if (Interlocked.CompareExchange(ref StaticSlot, (short)22, (short)-11) != -11 || StaticSlot != 22) return 24;

        UIntPtr up = UIntPtr.Zero;
        UIntPtr old = Interlocked.CompareExchange(ref up, new UIntPtr(77UL), new UIntPtr(1UL));
        if (old != UIntPtr.Zero || up != UIntPtr.Zero) return 25;

        old = Interlocked.CompareExchange(ref up, new UIntPtr(77UL), UIntPtr.Zero);
        if (old != UIntPtr.Zero || up != new UIntPtr(77UL)) return 26;

        return 0;
    }
}
