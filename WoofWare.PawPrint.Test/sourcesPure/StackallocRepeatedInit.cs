using System;

public unsafe class Program
{
    // The Roslyn-reachable route to `initblk` (ECMA-335 III.3.36): a `stackalloc` whose
    // initializer is byte-uniform lowers to `localloc; dup; <value>; <size>; initblk`. A
    // `stackalloc` whose initializer is *not* byte-uniform lowers to `cpblk` over a blob in
    // `<PrivateImplementationDetails>` instead, so every initializer here is byte-uniform on
    // purpose.
    //
    // The destination is what makes this the important case rather than a convenience: `localloc`
    // pushes a byref onto a `StackMemoryByte` root, which is a flat pool of bytes rather than a
    // pool of typed cells, so the fill goes down PawPrint's byte walk. Guests that reach `initblk`
    // at all reach it through this shape.

    // A plain byte block. `localloc` under `.locals init` hands back zeroed memory, so only a
    // nonzero fill is observable at all.
    private static int FillBytes()
    {
        byte* p = stackalloc byte[5] { 7, 7, 7, 7, 7 };

        for (int i = 0; i < 5; i++)
        {
            if (p[i] != 7) return 1;
        }

        return 0;
    }

    // 255 rather than a small value, so a fill that sign-extended its byte, or that kept only
    // seven bits, cannot pass.
    private static int FillMaxByte()
    {
        byte* p = stackalloc byte[7] { 255, 255, 255, 255, 255, 255, 255 };

        for (int i = 0; i < 7; i++)
        {
            if (p[i] != 255) return 2;
        }

        return 0;
    }

    // `initblk`'s size operand counts *bytes*. Roslyn emits `ldc.i4.7; ldc.i4.s 12; initblk` for
    // this, i.e. twelve bytes for three ints, so an implementation that took the size for an
    // element count would fill only the first three bytes and leave the rest zeroed.
    private static int FillIntsCountsBytes()
    {
        int* p = stackalloc int[3] { 0x07070707, 0x07070707, 0x07070707 };

        for (int i = 0; i < 3; i++)
        {
            if (p[i] != 0x07070707) return 3;
        }

        return 0;
    }

    // The same lowering with the block reached through a `Span<byte>` rather than a raw pointer.
    private static int FillThroughSpan()
    {
        Span<byte> s = stackalloc byte[6] { 9, 9, 9, 9, 9, 9 };

        for (int i = 0; i < s.Length; i++)
        {
            if (s[i] != 9) return 4;
        }

        return 0;
    }

    // Two blocks, each with its own `initblk`. The first is filled before the second is even
    // allocated, so a fill that ran past the end of the second block would have to reach back into
    // the first one to be seen here — which is exactly what a size read as "one past" would do on
    // a runtime that lays consecutive `localloc`s adjacently.
    private static int FillDoesNotOverrun()
    {
        byte* guard = stackalloc byte[4] { 3, 3, 3, 3 };
        byte* target = stackalloc byte[5] { 7, 7, 7, 7, 7 };

        for (int i = 0; i < 5; i++)
        {
            if (target[i] != 7) return 5;
        }

        for (int i = 0; i < 4; i++)
        {
            if (guard[i] != 3) return 6;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int r = FillBytes();
        if (r != 0) return r;
        r = FillMaxByte();
        if (r != 0) return r;
        r = FillIntsCountsBytes();
        if (r != 0) return r;
        r = FillThroughSpan();
        if (r != 0) return r;
        r = FillDoesNotOverrun();
        if (r != 0) return r;
        return 0;
    }
}
