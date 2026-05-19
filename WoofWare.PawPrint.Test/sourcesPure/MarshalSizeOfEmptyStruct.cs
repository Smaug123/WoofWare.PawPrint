using System;
using System.Runtime.InteropServices;

public class Program
{
    struct Empty
    {
    }

    [StructLayout(LayoutKind.Sequential)]
    struct EmptySequential
    {
    }

    public static int Main(string[] args)
    {
        // CoreCLR's `EEClassNativeLayoutInfo::CollectNativeLayoutFieldMetadataThrowing`
        // (classlayoutinfo.cpp:984-988) bumps a computed zero-sized native layout to
        // 1 byte so the type has a distinct address. `Marshal.SizeOf<T>()` of an
        // empty struct therefore returns 1, not 0, even though the struct has no
        // managed fields and a managed CLI size of 0.
        if (Marshal.SizeOf(typeof(Empty)) != 1) return 1;
        if (Marshal.SizeOf(typeof(EmptySequential)) != 1) return 2;
        return 0;
    }
}
