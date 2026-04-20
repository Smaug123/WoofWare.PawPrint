using System;

public class Program
{
    private static IntPtr nullPtr;
    private static IntPtr nonNullPtr;

    static int Main(string[] args)
    {
        // nullPtr is default-initialized to IntPtr.Zero
        // nonNullPtr we set to a non-zero value
        nonNullPtr = new IntPtr(42);

        // Test 1: null IntPtr == IntPtr.Zero
        if (nullPtr != IntPtr.Zero) return 1;

        // Test 2: non-null IntPtr != IntPtr.Zero
        if (nonNullPtr == IntPtr.Zero) return 2;

        // Test 3: non-null IntPtr == itself
        if (nonNullPtr != new IntPtr(42)) return 3;

        return 0;
    }
}
