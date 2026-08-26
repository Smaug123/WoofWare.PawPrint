using System;

public unsafe class Program
{
    // The Roslyn-reachable route to `cpblk` (ECMA-335 III.3.30). A `stackalloc` whose initializer
    // is byte-uniform lowers to `initblk` (that is `StackallocRepeatedInit.cs`); one that is *not*
    // byte-uniform lowers instead to a `cpblk` out of a blob in `<PrivateImplementationDetails>`,
    // under an `unaligned.` prefix:
    //
    //     localloc; dup; ldsflda <blob>; ldc.i4 <size>; unaligned. 1; cpblk
    //
    // The destination is what makes this worth having rather than a convenience: `localloc` pushes
    // a byref onto a `StackMemoryByte` root, a flat pool of bytes rather than a pool of typed
    // cells, and the source is a static field over an RVA blob. Guests that reach `cpblk` at all
    // reach it through this shape.

    // A plain byte block, every element distinct, so a copy that repeated one byte or stopped
    // early cannot pass.
    private static int CopyBytes()
    {
        byte* p = stackalloc byte[5] { 1, 2, 3, 4, 5 };

        for (int i = 0; i < 5; i++)
        {
            if (p[i] != i + 1) return 1;
        }

        return 0;
    }

    // 255 and 128 among the values, so a copy that sign-extended a byte, or kept only seven bits,
    // cannot pass.
    private static int CopyHighBytes()
    {
        byte* p = stackalloc byte[4] { 255, 128, 0, 127 };

        if (p[0] != 255) return 2;
        if (p[1] != 128) return 3;
        if (p[2] != 0) return 4;
        if (p[3] != 127) return 5;
        return 0;
    }

    // Wider elements: `unaligned. 4; cpblk` over sixteen bytes rather than four. A size read as an
    // element count would copy four bytes and leave the rest zero.
    private static int CopyInts()
    {
        int* p = stackalloc int[4] { 10, -20, int.MaxValue, int.MinValue };

        if (p[0] != 10) return 6;
        if (p[1] != -20) return 7;
        if (p[2] != int.MaxValue) return 8;
        if (p[3] != int.MinValue) return 9;
        return 0;
    }

    // Eight-byte elements, so the copy crosses whatever block size the walk uses.
    private static int CopyLongs()
    {
        long* p = stackalloc long[3] { 1L, long.MaxValue, -1L };

        if (p[0] != 1L) return 10;
        if (p[1] != long.MaxValue) return 11;
        if (p[2] != -1L) return 12;
        return 0;
    }

    // The copy must not run past the block it was given. `after` is allocated second, so a copy
    // that overran `p` would land in it — and `.locals init` zeroes `localloc` memory, so an
    // overrun is visible as a nonzero byte.
    private static int CopyStopsAtTheEnd()
    {
        byte* p = stackalloc byte[3] { 9, 8, 7 };
        byte* after = stackalloc byte[4];

        if (p[0] != 9 || p[1] != 8 || p[2] != 7) return 13;

        for (int i = 0; i < 4; i++)
        {
            if (after[i] != 0) return 14;
        }

        return 0;
    }

    // Two blocks initialised from two different blobs: a copy that read the wrong static would
    // give one of them the other's contents.
    private static int TwoDistinctBlobs()
    {
        byte* a = stackalloc byte[4] { 1, 2, 3, 4 };
        byte* b = stackalloc byte[4] { 5, 6, 7, 8 };

        for (int i = 0; i < 4; i++)
        {
            if (a[i] != i + 1) return 15;
            if (b[i] != i + 5) return 16;
        }

        return 0;
    }

    public static int Main(string[] args)
    {
        int r = CopyBytes();
        if (r != 0) return r;
        r = CopyHighBytes();
        if (r != 0) return r;
        r = CopyInts();
        if (r != 0) return r;
        r = CopyLongs();
        if (r != 0) return r;
        r = CopyStopsAtTheEnd();
        if (r != 0) return r;
        r = TwoDistinctBlobs();
        if (r != 0) return r;
        return 0;
    }
}
