using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// CoreCLR reaches `HandleAutoLayout` by two routes (`MethodTableBuilder::PlaceInstanceFields`,
// methodtablebuilder.cpp:8212): the type declares `LayoutKind.Auto`, or the type contains GC
// references and is therefore promoted to auto layout whatever it declared. PawPrint implements
// the second route only.
//
// It cannot implement the first: `Layout` (WoofWare.PawPrint.Domain/TypeInfo.fs:47) is built from
// the `ClassLayout` metadata table, which carries only `Pack` and `Size`. The LayoutKind lives in
// `TypeAttributes.LayoutMask`, which `Layout` discards, so a type declared `LayoutKind.Auto` with
// no reference in it is indistinguishable from a sequential one at the point where fields are
// laid out. Closing this means widening `Layout` to carry the kind and threading it through every
// construction site, which is its own change.
//
// Two rules are missing as a result: auto layout buckets fields by size class (so the two bytes
// below become adjacent), and it rounds a value class no larger than a pointer up to the next
// power of two.
public class TestStructLayoutAutoWithoutReferences
{
    [StructLayout(LayoutKind.Auto)] private struct AutoBucketed { public byte B; public int I; public byte C; }
    [StructLayout(LayoutKind.Auto)] private struct AutoThreeBytes { public byte A; public byte B; public byte C; }
    [StructLayout(LayoutKind.Auto)] private struct AutoThreeShorts { public short A; public short B; public short C; }

    // The controls: declared sequential, so the declared order and size stand. PawPrint already
    // agrees on all of these, which is what makes the cases above a LayoutKind gap rather than a
    // bucketing one.
    [StructLayout(LayoutKind.Sequential)] private struct SeqBucketed { public byte B; public int I; public byte C; }
    [StructLayout(LayoutKind.Sequential)] private struct SeqThreeBytes { public byte A; public byte B; public byte C; }

    private static int Check<T>(int expected, int code) where T : struct
        => Unsafe.SizeOf<T>() == expected ? 0 : code;

    public static int Main(string[] argv)
    {
        int r;
        if ((r = Check<SeqBucketed>(12, 1)) != 0) return r;
        if ((r = Check<SeqThreeBytes>(3, 2)) != 0) return r;
        if ((r = Check<AutoBucketed>(8, 3)) != 0) return r;
        if ((r = Check<AutoThreeBytes>(4, 4)) != 0) return r;
        if ((r = Check<AutoThreeShorts>(8, 5)) != 0) return r;
        return 0;
    }
}
