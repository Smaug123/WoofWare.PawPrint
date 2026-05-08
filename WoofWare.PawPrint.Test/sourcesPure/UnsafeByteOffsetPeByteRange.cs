using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestUnsafeByteOffsetPeByteRange
{
    // ReadOnlySpan<byte> bound from a byte literal is backed by PE image
    // bytes (RuntimeHelpers.CreateSpan), giving byrefs whose root is a
    // PeByteRange. Unsafe.ByteOffset between two such byrefs must produce
    // the honest signed byte delta, not a synthetic cross-storage sentinel.
    private static ReadOnlySpan<byte> Bytes => [10, 20, 30, 40, 50, 60, 70, 80];

    public static int Test1()
    {
        ReadOnlySpan<byte> data = Bytes;
        ref byte first = ref MemoryMarshal.GetReference(data);
        ref byte same = ref MemoryMarshal.GetReference(data);
        if ((long)Unsafe.ByteOffset(ref first, ref same) != 0L)
            return 1;
        return 0;
    }

    public static int Test2()
    {
        ReadOnlySpan<byte> data = Bytes;
        ref byte first = ref MemoryMarshal.GetReference(data);
        ref byte third = ref Unsafe.Add(ref first, 2);
        if ((long)Unsafe.ByteOffset(ref first, ref third) != 2L)
            return 2;
        if ((long)Unsafe.ByteOffset(ref third, ref first) != -2L)
            return 3;
        return 0;
    }

    public static int Test3()
    {
        ReadOnlySpan<byte> data = Bytes;
        ref byte first = ref MemoryMarshal.GetReference(data);
        ref byte third = ref Unsafe.Add(ref first, 2);
        ref byte sixth = ref Unsafe.Add(ref first, 5);
        if ((long)Unsafe.ByteOffset(ref third, ref sixth) != 3L)
            return 4;
        if ((long)Unsafe.ByteOffset(ref sixth, ref third) != -3L)
            return 5;
        return 0;
    }

    // Cross-storage ByteOffset between a PE byte range and an array byref
    // must be anti-symmetric and non-zero (the synthetic sentinel path).
    public static int Test4()
    {
        ReadOnlySpan<byte> data = Bytes;
        ref byte peRef = ref MemoryMarshal.GetReference(data);
        byte[] heap = new byte[] { 99 };
        long forward = (long)Unsafe.ByteOffset(ref peRef, ref heap[0]);
        long backward = (long)Unsafe.ByteOffset(ref heap[0], ref peRef);
        if (forward + backward != 0L)
            return 6;
        if (forward == 0L)
            return 7;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        r = Test4();
        if (r != 0) return r;
        return 0;
    }
}
