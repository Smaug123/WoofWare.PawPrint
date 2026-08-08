using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A cell whose own byte range is unaliased, but which sits under an ancestor field that *is*
// overlapped by an explicit-layout sibling.
//
// `Outer` overlays `Alias` on the second half of `Whole`. That overlap is long-over-long, which
// the CLR permits — only overlapping a reference with a non-reference is rejected — so the type
// loads and `Unsafe.As<Outer, object>(ref outer)` names `Whole`'s reference field at offset 0.
// The range [0, 8) is not aliased by anything: `Alias` starts at 8. `CellPathsExactlyCovering`
// therefore returns `Whole/R`, and the question this file was written to settle is whether
// `getCellAtPath` can walk that path when an ancestor on it is aliased.
//
// It does not get that far. The program fails at `outer.Whole.R = ...`, an ordinary nested field
// write with no reinterpret in it at all: an explicit-layout struct with any overlap is stored
// byte-backed, so `CliValueType.DereferenceFieldById` reconstructs `Whole` from bytes via
// `OfBytesLike`, which refuses a non-primitive template — and `Whole` contains a reference. So
// reference-containing explicit-layout structs cannot be field-accessed at all, several layers
// below anything cell naming does, and this shape cannot currently distinguish a resolver that
// handles aliased ancestors from one that does not.
//
// Parked on that gap rather than on the resolver. It is not a regression: the failing path
// (`applyProjectionsForWriteIfChanged` → `getFieldById`) is untouched by the cell resolver.
public class TestReinterpretCellUnderAliasedAncestor
{
    private sealed class Box { public int V; }

    private struct Inner { public Box R; public long X; }

    [StructLayout(LayoutKind.Explicit)]
    private struct Outer
    {
        [FieldOffset(0)] public Inner Whole;
        [FieldOffset(8)] public long Alias;
    }

    public static int Main(string[] argv)
    {
        Outer outer = default;
        outer.Whole.R = new Box { V = 5 };
        outer.Whole.X = 99;

        ref object asRef = ref Unsafe.As<Outer, object>(ref outer);

        if (!ReferenceEquals(asRef, outer.Whole.R)) return 1;
        if (outer.Alias != 99) return 2;

        Box replacement = new Box { V = 6 };
        asRef = replacement;

        if (!ReferenceEquals(outer.Whole.R, replacement)) return 3;
        if (outer.Whole.R.V != 6) return 4;
        if (outer.Alias != 99) return 5;

        return 0;
    }
}
