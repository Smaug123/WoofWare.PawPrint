using System.Runtime.InteropServices;
using System.Threading;

public unsafe class NativeMemoryAlignedAllocFreeTests
{
    public static int TestAlignedAllocWriteReadFree()
    {
        nuint[] alignments = { 1, 8, 16, 64, 128 };
        nuint[] sizes = { 4, 1, 100, 128, 3 };

        for (int i = 0; i < alignments.Length; i++)
        {
            byte* ptr = (byte*)NativeMemory.AlignedAlloc(sizes[i], alignments[i]);
            if (ptr == null) return 10 * i + 1;

            // CoreLib raises every alignment to at least sizeof(void*) before the
            // P/Invoke, so even alignment 1 comes back 8-byte aligned.
            if (((long)ptr & 7) != 0) return 10 * i + 2;

            for (int j = 0; j < (int)sizes[i]; j++)
            {
                ptr[j] = (byte)(j + i);
            }

            for (int j = 0; j < (int)sizes[i]; j++)
            {
                if (ptr[j] != (byte)(j + i)) return 10 * i + 3;
            }

            NativeMemory.AlignedFree(ptr);
        }

        return 0;
    }

    public static int TestAlignedAllocZeroBytes()
    {
        // A zero-byte request is rounded up to one alignment unit, so the
        // result is a real block that must be freed.
        void* ptr = NativeMemory.AlignedAlloc((nuint)0, (nuint)16);
        if (ptr == null) return 1;

        NativeMemory.AlignedFree(ptr);
        return 0;
    }

    public static int TestAlignedFreeNull()
    {
        NativeMemory.AlignedFree(null);
        return 0;
    }

    public static int TestStateWordAsLowLevelThreadBlockerUsesIt()
    {
        // The shape of .NET 11's `LowLevelThreadBlocker`: a cache-line-aligned
        // int, initialised by a plain store and then changed atomically.
        int* state = (int*)NativeMemory.AlignedAlloc((nuint)128, (nuint)128);
        if (state == null) return 1;

        *state = 0;
        if (Interlocked.CompareExchange(ref *state, 1, 0) != 0) return 2;
        if (*state != 1) return 3;
        if (Interlocked.Exchange(ref *state, 0) != 1) return 4;
        if (*state != 0) return 5;

        NativeMemory.AlignedFree(state);
        return 0;
    }

    public static int TestNonPowerOfTwoAlignmentThrows()
    {
        try
        {
            void* ptr = NativeMemory.AlignedAlloc((nuint)16, (nuint)3);
            NativeMemory.AlignedFree(ptr);
            return 1;
        }
        catch (System.ArgumentException)
        {
            return 0;
        }
    }

    public static int TestOversizeThrowsOom()
    {
        // Rounding this size up to the alignment does not overflow, so the
        // request reaches the native allocator, which cannot satisfy it and
        // returns null; CoreLib turns that into an OutOfMemoryException.
        try
        {
            void* ptr = NativeMemory.AlignedAlloc(nuint.MaxValue - 15, (nuint)16);
            NativeMemory.AlignedFree(ptr);
            return 1;
        }
        catch (System.OutOfMemoryException)
        {
            return 0;
        }
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = NativeMemoryAlignedAllocFreeTests.TestAlignedAllocWriteReadFree();
        if (result != 0) return 1000 + result;

        result = NativeMemoryAlignedAllocFreeTests.TestAlignedAllocZeroBytes();
        if (result != 0) return 2000 + result;

        result = NativeMemoryAlignedAllocFreeTests.TestAlignedFreeNull();
        if (result != 0) return 3000 + result;

        result = NativeMemoryAlignedAllocFreeTests.TestStateWordAsLowLevelThreadBlockerUsesIt();
        if (result != 0) return 4000 + result;

        result = NativeMemoryAlignedAllocFreeTests.TestNonPowerOfTwoAlignmentThrows();
        if (result != 0) return 5000 + result;

        result = NativeMemoryAlignedAllocFreeTests.TestOversizeThrowsOom();
        if (result != 0) return 6000 + result;

        return 0;
    }
}
