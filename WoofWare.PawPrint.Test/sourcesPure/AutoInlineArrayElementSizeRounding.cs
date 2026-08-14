using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// CoreCLR lays an `[InlineArray(N)]` type out as its *one* declared field and then multiplies the
// resulting instance size by N (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:
// 8612 for the auto route, :8663 for sequential). So on the auto route the size rounding applies
// to a single element, and the whole run is N of the rounded element.
//
// PawPrint instead materialises N storage slots (`InlineArrayStorage.expand`) and lays them out
// together, so the rounding would apply once to the whole run. `CliValueType.AutoLayoutGoverns`
// therefore keeps inline-array expansions on the pre-existing (sequential) route, which is exactly
// right whenever an element's size already equals its rounded size — the overwhelmingly common
// case, including every primitive element — and wrong for an element whose size does not.
//
// `AutoNarrow` below is that residual case, and the only one in this file that fails: its `S3`
// element is 3 bytes, which auto layout rounds to 4 before multiplying, so the type is 12 bytes
// and not 9. Closing it needs the repeat count to reach the sizing code, which is its own change.
// The other three cases pass today and are here to pin what the carve-out must not disturb.
public class TestAutoInlineArrayElementSizeRounding
{
    [StructLayout(LayoutKind.Sequential)] private struct S3 { public byte A; public byte B; public byte C; }

    [StructLayout(LayoutKind.Auto)][InlineArray(3)] private struct AutoNarrow { private S3 _item; }
    [StructLayout(LayoutKind.Sequential)][InlineArray(3)] private struct SeqNarrow { private S3 _item; }
    [StructLayout(LayoutKind.Auto)][InlineArray(3)] private struct AutoInt { private int _item; }

    // A single-element inline array mints no extra storage slot, so it is not an "expansion" and
    // the carve-out does not apply to it: declared auto layout governs, and rounds 3 up to 4.
    [StructLayout(LayoutKind.Auto)][InlineArray(1)] private struct AutoOne { private S3 _item; }

    private static int Check<T>(int expected, int code) where T : struct
        => Unsafe.SizeOf<T>() == expected ? 0 : code;

    public static int Main(string[] argv)
    {
        int r;
        if ((r = Check<S3>(3, 1)) != 0) return r;
        if ((r = Check<SeqNarrow>(9, 2)) != 0) return r;
        if ((r = Check<AutoInt>(12, 3)) != 0) return r;
        if ((r = Check<AutoOne>(4, 4)) != 0) return r;
        // The one that fails: PawPrint lays out three 3-byte slots and reports 9.
        if ((r = Check<AutoNarrow>(12, 5)) != 0) return r;
        return 0;
    }
}
