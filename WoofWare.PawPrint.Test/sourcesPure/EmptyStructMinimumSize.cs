// CoreCLR enforces that no value class has length 0: "Like C++ we enforce that there can be
// no 0 length structures. Thus for a value class with no fields, we 'pad' the length to be 1"
// (methodtablebuilder.cpp:8568, the auto-layout path). Sequential and explicit layout reach the
// same rule through `EEClassLayoutInfo::SetInstanceBytesSize` (class.h:497), which is literally
// `return size == 0 ? 1 : size;`. So the padding is universal across layout kinds, and it
// applies to a declared `Size = 0` just as it does to a struct that simply has no fields.
//
// The padding is observable: it is the managed size, so it drives `Unsafe.SizeOf<T>`, the array
// element stride, and the size an enclosing struct reserves for a field of the type.

using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class Program
{
    struct Empty { }

    [StructLayout(LayoutKind.Sequential)]
    struct EmptySequential { }

    [StructLayout(LayoutKind.Explicit)]
    struct EmptyExplicit { }

    [StructLayout(LayoutKind.Explicit, Size = 0)]
    struct EmptyExplicitSizeZero { }

    // A declared `Pack` with no `Size` is a distinct route to the same place: a ClassLayout row
    // exists, so the metadata layout is *not* the default one, but it carries no size to act as a
    // floor. (`TypeLayout.IsDefault` requires both size and packing to be zero.)
    [StructLayout(LayoutKind.Sequential, Pack = 4)]
    struct EmptyPacked { }

    struct ContainsEmpty { public Empty A; }

    struct ContainsTwoEmpties { public Empty A; public Empty B; }

    struct EmptyThenByte { public Empty A; public byte B; }

    public static int Main(string[] args)
    {
        if (Unsafe.SizeOf<Empty>() != 1) return 1;
        if (Unsafe.SizeOf<EmptySequential>() != 1) return 2;
        if (Unsafe.SizeOf<EmptyExplicit>() != 1) return 3;
        if (Unsafe.SizeOf<EmptyExplicitSizeZero>() != 1) return 4;
        if (Unsafe.SizeOf<EmptyPacked>() != 1) return 10;

        // The padding is recursive: an empty field occupies a real byte in its container.
        if (Unsafe.SizeOf<ContainsEmpty>() != 1) return 5;
        if (Unsafe.SizeOf<ContainsTwoEmpties>() != 2) return 6;
        if (Unsafe.SizeOf<EmptyThenByte>() != 2) return 7;

        // Array element stride follows the managed size, so distinct elements are distinct
        // addresses. If the stride were 0 every element would alias element 0.
        Empty[] arr = new Empty[3];
        ref Empty first = ref arr[0];
        ref Empty second = ref arr[1];
        if (Unsafe.AreSame(ref first, ref second)) return 8;

        long stride = Unsafe.ByteOffset(ref first, ref second).ToInt64();
        if (stride != 1) return 9;

        return 0;
    }
}
