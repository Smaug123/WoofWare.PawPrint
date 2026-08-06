using System;
using System.Runtime.CompilerServices;

// `Int128`/`UInt128` require 16-byte alignment, which CoreCLR carries as a per-type
// `GetAlignmentRequirement()` on the layout info rather than deriving it from the fields
// (`MethodTable::GetFieldAlignmentRequirement`, methodtable.cpp:8853, and the
// `IsInt128OrHasInt128Fields` flag threaded through `MethodTableBuilder`). PawPrint derives a
// value type's alignment structurally from its fields, so `Int128` — two `ulong`s — comes out
// 8-aligned, and anything embedding it is sized as if it were.
//
// This is orthogonal to how GC-containing types are laid out: the first case below has no
// reference in it at all and diverges identically, so it is not the auto-layout rule. It needs
// a nominal required-alignment concept for `Int128`, `UInt128` and the `Vector` family, which
// is its own change.
public class TestStructLayoutInt128Alignment
{
    private sealed class Box { public int V; }

    // No GC reference anywhere: plain sequential layout, and still wrong.
    private struct NoGcWide { public long L; public Int128 I; }

    // With a reference, so the auto-layout path: the by-value `Int128` should land at 16.
    private struct GcWide { public Box O; public Int128 I; }
    private struct GcWideOuter { public byte Pre; public GcWide Nested; }

    private static int Check<T>(int expected, int code) where T : struct
        => Unsafe.SizeOf<T>() == expected ? 0 : code;

    public static int Main(string[] argv)
    {
        int r;
        if ((r = Check<NoGcWide>(32, 1)) != 0) return r;
        if ((r = Check<GcWide>(32, 2)) != 0) return r;
        // The reported alignment of a GC-containing type is capped at the pointer size even when
        // it holds a 16-aligned field, so the enclosing type places it at 8, not 16.
        if ((r = Check<GcWideOuter>(40, 3)) != 0) return r;
        return 0;
    }
}
