using System;
using System.Runtime.CompilerServices;

public unsafe class Program
{
    // `Unsafe.InitBlock` and `Unsafe.InitBlockUnaligned` are how a guest reaches `initblk`
    // (ECMA-335 III.3.36) deliberately, rather than incidentally through a byte-uniform
    // `stackalloc` initializer. All four overloads carry `[Intrinsic]`: the JIT replaces each
    // with the opcode, so what is asserted here is the opcode's semantics reached through the
    // public API. Three of the four CoreLib bodies throw `PlatformNotSupportedException` and the
    // fourth is a byte loop, so no assertion here can be satisfied by interpreting a body.

    // The fill stops short of the end of its storage, so a fill that ran to the end of the array
    // cannot pass.
    private static int InitBlockByref()
    {
        byte[] buf = new byte[8];
        Unsafe.InitBlock(ref buf[0], 7, 5);

        for (int i = 0; i < 5; i++)
        {
            if (buf[i] != 7) return 1;
        }

        for (int i = 5; i < 8; i++)
        {
            if (buf[i] != 0) return 2;
        }

        return 0;
    }

    // 200 rather than a small value, so a fill that sign-extended its byte or kept only seven
    // bits cannot pass.
    private static int InitBlockUnalignedByref()
    {
        byte[] buf = new byte[8];
        Unsafe.InitBlockUnaligned(ref buf[0], 200, 3);

        for (int i = 0; i < 3; i++)
        {
            if (buf[i] != 200) return 3;
        }

        for (int i = 3; i < 8; i++)
        {
            if (buf[i] != 0) return 4;
        }

        return 0;
    }

    // Starting one byte in, so a fill that ignored the destination it was handed and started at
    // the base of its storage cannot pass.
    private static int InitBlockPointer()
    {
        byte* p = stackalloc byte[8];

        for (int i = 0; i < 8; i++) p[i] = 0;

        Unsafe.InitBlock(p + 1, 0xAB, 4);

        if (p[0] != 0) return 5;

        for (int i = 1; i < 5; i++)
        {
            if (p[i] != 0xAB) return 6;
        }

        for (int i = 5; i < 8; i++)
        {
            if (p[i] != 0) return 7;
        }

        return 0;
    }

    private static int InitBlockUnalignedPointer()
    {
        byte* p = stackalloc byte[8];

        for (int i = 0; i < 8; i++) p[i] = 0;

        Unsafe.InitBlockUnaligned(p + 2, 0xFF, 3);

        if (p[0] != 0 || p[1] != 0) return 8;

        for (int i = 2; i < 5; i++)
        {
            if (p[i] != 0xFF) return 9;
        }

        for (int i = 5; i < 8; i++)
        {
            if (p[i] != 0) return 10;
        }

        return 0;
    }

    // The count is in bytes, and the destination is an array element, i.e. a typed cell rather
    // than the flat byte pool a `stackalloc` hands out. Twelve bytes over four ints: a count read
    // as an element count would fill only the first three bytes, and one that filled to the end
    // of the array would take the fourth element with it.
    private static int CountsBytesOverTypedCells()
    {
        int[] a = new int[4];
        Unsafe.InitBlock(ref Unsafe.As<int, byte>(ref a[0]), 0x07, 12);

        for (int i = 0; i < 3; i++)
        {
            if (a[i] != 0x07070707) return 11;
        }

        if (a[3] != 0) return 12;

        return 0;
    }

    private static int ZeroCountWritesNothing()
    {
        byte[] buf = new byte[4];
        buf[0] = 1;
        buf[1] = 2;
        Unsafe.InitBlock(ref buf[0], 9, 0);

        if (buf[0] != 1 || buf[1] != 2 || buf[2] != 0 || buf[3] != 0) return 13;

        return 0;
    }

    // A zero-length fill must not dereference its destination at all.
    private static int NullPointerWithZeroCount()
    {
        Unsafe.InitBlock((void*)null, 9, 0);
        return 0;
    }

    private static int NullPointerWithNonzeroCount()
    {
        try
        {
            Unsafe.InitBlock((void*)null, 9, 1);
        }
        catch (NullReferenceException)
        {
            return 0;
        }

        return 14;
    }

    // The byref overload's null, which reaches the runtime as a null managed pointer rather than
    // as a zero unmanaged address.
    private static int NullByrefWithZeroCount()
    {
        Unsafe.InitBlock(ref Unsafe.NullRef<byte>(), 9, 0);
        return 0;
    }

    private static int NullByrefWithNonzeroCount()
    {
        try
        {
            Unsafe.InitBlock(ref Unsafe.NullRef<byte>(), 9, 1);
        }
        catch (NullReferenceException)
        {
            return 0;
        }

        return 15;
    }

    // A zero fill wide enough to cover an object reference. A reference has no byte image, so the
    // byte walk cannot serve this; only the whole-cell step can, and that step is available only
    // because the fill byte is zero.
    private static int ZeroFillOverObjectReference()
    {
        object[] arr = new object[3];
        object keep0 = new object();
        object keep2 = new object();
        arr[0] = keep0;
        arr[1] = new object();
        arr[2] = keep2;

        Unsafe.InitBlock(ref Unsafe.As<object, byte>(ref arr[1]), 0, (uint)IntPtr.Size);

        if (arr[1] != null) return 16;
        if (!ReferenceEquals(arr[0], keep0)) return 17;
        if (!ReferenceEquals(arr[2], keep2)) return 18;

        return 0;
    }

    public static int Main(string[] args)
    {
        int r = InitBlockByref();
        if (r != 0) return r;
        r = InitBlockUnalignedByref();
        if (r != 0) return r;
        r = InitBlockPointer();
        if (r != 0) return r;
        r = InitBlockUnalignedPointer();
        if (r != 0) return r;
        r = CountsBytesOverTypedCells();
        if (r != 0) return r;
        r = ZeroCountWritesNothing();
        if (r != 0) return r;
        r = NullPointerWithZeroCount();
        if (r != 0) return r;
        r = NullPointerWithNonzeroCount();
        if (r != 0) return r;
        r = NullByrefWithZeroCount();
        if (r != 0) return r;
        r = NullByrefWithNonzeroCount();
        if (r != 0) return r;
        r = ZeroFillOverObjectReference();
        if (r != 0) return r;
        return 0;
    }
}
