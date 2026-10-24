using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Clearing or filling a range of native or stackalloc memory that holds a pointer replaces the
// pointer with the bytes written, and leaves the bytes outside the range as they were.
public struct LongThenPointer
{
    public long A;
    public IntPtr B;
}

public static unsafe class Program
{
    static int ClearNativeMemory(IntPtr target)
    {
        byte* p = (byte*)NativeMemory.Alloc(24);
        p[0] = 0x11;
        Marshal.WriteIntPtr((IntPtr)p, 8, target);
        p[16] = 0x22;

        NativeMemory.Clear(p, 24);

        for (int i = 0; i < 24; i++)
        {
            if (p[i] != 0) return 1;
        }

        if (Marshal.ReadIntPtr((IntPtr)p, 8) != IntPtr.Zero) return 2;

        NativeMemory.Free(p);
        return 0;
    }

    static int ClearOnlyThePointer(IntPtr target)
    {
        byte* p = (byte*)Marshal.AllocHGlobal(24);
        Marshal.WriteIntPtr((IntPtr)p, 8, target);

        for (int i = 0; i < 8; i++)
        {
            p[i] = 0x33;
            p[16 + i] = 0x33;
        }

        NativeMemory.Clear(p + 8, 8);

        for (int i = 0; i < 8; i++)
        {
            if (p[i] != 0x33) return 1;
            if (p[8 + i] != 0) return 2;
            if (p[16 + i] != 0x33) return 3;
        }

        Marshal.FreeHGlobal((IntPtr)p);
        return 0;
    }

    static int FillNativeMemory(IntPtr target)
    {
        byte* p = (byte*)NativeMemory.Alloc(16);
        Marshal.WriteIntPtr((IntPtr)p, 0, target);
        Marshal.WriteIntPtr((IntPtr)p, 8, target);

        Unsafe.InitBlockUnaligned(p, 0xAB, 16);

        for (int i = 0; i < 16; i++)
        {
            if (p[i] != 0xAB) return 1;
        }

        if (*(ulong*)(p + 8) != 0xABABABABABABABABUL) return 2;

        NativeMemory.Free(p);
        return 0;
    }

    static int ClearFieldThroughStructPointer(IntPtr target)
    {
        LongThenPointer* pair = (LongThenPointer*)NativeMemory.Alloc(16);
        pair->A = 0x5555555555555555L;
        pair->B = target;

        NativeMemory.Clear(&pair->B, 8);

        if (pair->A != 0x5555555555555555L) return 1;
        if (pair->B != IntPtr.Zero) return 2;

        NativeMemory.Free(pair);
        return 0;
    }

    static int ClearStackMemory(IntPtr target)
    {
        byte* s = stackalloc byte[16];
        s[0] = 0x44;
        *(IntPtr*)(s + 8) = target;

        NativeMemory.Clear(s, 16);

        for (int i = 0; i < 16; i++)
        {
            if (s[i] != 0) return 1;
        }

        if (*(IntPtr*)(s + 8) != IntPtr.Zero) return 2;

        return 0;
    }

    public static int Main(string[] args)
    {
        IntPtr target = Marshal.AllocHGlobal(4);
        int result;

        result = ClearNativeMemory(target);
        if (result != 0) return 10 + result;

        result = ClearOnlyThePointer(target);
        if (result != 0) return 20 + result;

        result = FillNativeMemory(target);
        if (result != 0) return 30 + result;

        result = ClearStackMemory(target);
        if (result != 0) return 40 + result;

        result = ClearFieldThroughStructPointer(target);
        if (result != 0) return 50 + result;

        Marshal.FreeHGlobal(target);
        return 0;
    }
}
