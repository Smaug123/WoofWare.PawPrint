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
        if (Interlocked.Exchange(ref sb, (sbyte)5) != -1 || sb != 5) return 1;
        if (Interlocked.Exchange(ref sb, (sbyte)-100) != 5 || sb != -100) return 2;

        byte b = 250;
        if (Interlocked.Exchange(ref b, (byte)7) != 250 || b != 7) return 3;
        if (Interlocked.Exchange(ref b, (byte)200) != 7 || b != 200) return 4;

        short s = -1234;
        if (Interlocked.Exchange(ref s, (short)2222) != -1234 || s != 2222) return 5;
        if (Interlocked.Exchange(ref s, (short)-5555) != 2222 || s != -5555) return 6;

        ushort us = 60000;
        if (Interlocked.Exchange(ref us, (ushort)17) != 60000 || us != 17) return 7;
        if (Interlocked.Exchange(ref us, (ushort)40000) != 17 || us != 40000) return 8;

        int i = -100;
        if (Interlocked.Exchange(ref i, 10) != -100 || i != 10) return 9;
        if (Interlocked.Exchange(ref i, int.MinValue) != 10 || i != int.MinValue) return 10;

        uint ui = uint.MaxValue;
        if (Interlocked.Exchange(ref ui, 42U) != uint.MaxValue || ui != 42U) return 11;
        if (Interlocked.Exchange(ref ui, 0U) != 42U || ui != 0U) return 12;

        long l = long.MinValue;
        if (Interlocked.Exchange(ref l, 99L) != long.MinValue || l != 99L) return 13;
        if (Interlocked.Exchange(ref l, long.MaxValue) != 99L || l != long.MaxValue) return 14;

        ulong ul = ulong.MaxValue;
        if (Interlocked.Exchange(ref ul, 123UL) != ulong.MaxValue || ul != 123UL) return 15;
        if (Interlocked.Exchange(ref ul, 0UL) != 123UL || ul != 0UL) return 16;

        Holder holder = new Holder();
        if (Interlocked.Exchange(ref holder.Slot, 12) != -7 || holder.Slot != 12) return 17;
        if (Interlocked.Exchange(ref holder.Slot, int.MaxValue) != 12 || holder.Slot != int.MaxValue) return 18;
        if (Interlocked.Exchange(ref holder.Narrow, (byte)9) != 250 || holder.Narrow != 9) return 19;
        if (Interlocked.Exchange(ref holder.Narrow, (byte)0) != 9 || holder.Narrow != 0) return 20;

        byte[] bytes = new byte[] { 33, 44 };
        if (Interlocked.Exchange(ref bytes[1], (byte)55) != 44 || bytes[1] != 55) return 21;
        if (Interlocked.Exchange(ref bytes[0], (byte)200) != 33 || bytes[0] != 200) return 22;

        if (Interlocked.Exchange(ref StaticSlot, (short)22) != -11 || StaticSlot != 22) return 23;
        if (Interlocked.Exchange(ref StaticSlot, (short)0) != 22 || StaticSlot != 0) return 24;

        // IntPtr starting at zero: exercises the ManagedPointer Null → NativeInt decode path.
        IntPtr ip = IntPtr.Zero;
        IntPtr ipOld = Interlocked.Exchange(ref ip, new IntPtr(77));
        if (ipOld != IntPtr.Zero || ip != new IntPtr(77)) return 25;
        ipOld = Interlocked.Exchange(ref ip, new IntPtr(-9999));
        if (ipOld != new IntPtr(77) || ip != new IntPtr(-9999)) return 26;

        UIntPtr up = UIntPtr.Zero;
        UIntPtr upOld = Interlocked.Exchange(ref up, new UIntPtr(77UL));
        if (upOld != UIntPtr.Zero || up != new UIntPtr(77UL)) return 27;
        upOld = Interlocked.Exchange(ref up, new UIntPtr(0UL));
        if (upOld != new UIntPtr(77UL) || up != UIntPtr.Zero) return 28;

        return 0;
    }
}
