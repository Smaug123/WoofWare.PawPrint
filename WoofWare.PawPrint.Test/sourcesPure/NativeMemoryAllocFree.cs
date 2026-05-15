using System.Runtime.InteropServices;

public unsafe class NativeMemoryAllocFreeTests
{
    public static int TestAllocWriteReadFree()
    {
        byte* ptr = (byte*)NativeMemory.Alloc((nuint)4);
        if (ptr == null) return 1;

        ptr[0] = 0x12;
        ptr[1] = 0x34;
        ptr[2] = 0x56;
        ptr[3] = 0x78;

        if (ptr[0] != 0x12) return 2;
        if (ptr[1] != 0x34) return 3;
        if (ptr[2] != 0x56) return 4;
        if (ptr[3] != 0x78) return 5;

        NativeMemory.Free(ptr);

        return 0;
    }

    public static int TestAllocZeroed()
    {
        byte* ptr = (byte*)NativeMemory.AllocZeroed((nuint)8);
        if (ptr == null) return 1;

        for (int i = 0; i < 8; i++)
        {
            if (ptr[i] != 0) return 2 + i;
        }

        NativeMemory.Free(ptr);

        return 0;
    }

    public static int TestFreeNull()
    {
        // C's free(NULL) is a no-op; NativeMemory.Free filters null before the
        // P/Invoke, but the test still exercises the documented contract.
        NativeMemory.Free(null);
        return 0;
    }

    public static int TestAllocHGlobalRoundTrip()
    {
        System.IntPtr handle = Marshal.AllocHGlobal(2);
        if (handle == System.IntPtr.Zero) return 1;

        byte* ptr = (byte*)handle;
        ptr[0] = 0xAB;
        ptr[1] = 0xCD;

        if (ptr[0] != 0xAB) return 2;
        if (ptr[1] != 0xCD) return 3;

        Marshal.FreeHGlobal(handle);
        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = NativeMemoryAllocFreeTests.TestAllocWriteReadFree();
        if (result != 0) return 1000 + result;

        result = NativeMemoryAllocFreeTests.TestAllocZeroed();
        if (result != 0) return 2000 + result;

        result = NativeMemoryAllocFreeTests.TestFreeNull();
        if (result != 0) return 3000 + result;

        result = NativeMemoryAllocFreeTests.TestAllocHGlobalRoundTrip();
        if (result != 0) return 4000 + result;

        return 0;
    }
}
