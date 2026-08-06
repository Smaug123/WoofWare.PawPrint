using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// CoreCLR lays out *any* value type containing GC references with auto layout, even when the
// metadata says `LayoutKind.Sequential` (`MethodTableBuilder::PlaceInstanceFields`,
// methodtablebuilder.cpp). Auto layout ignores `Pack` and the explicit `Size`, buckets fields by
// power-of-two size class (largest class first, GC pointers at the front of the pointer-sized
// class), and places by-value struct fields last.
//
// The consequence that matters is that this changes the *size*, not merely the field order: a
// declared-order walk cannot pack `{byte; object; byte}` into 16 bytes because it has no way to
// put the two bytes adjacent. Size is observable through `sizeof`, `Unsafe.SizeOf<T>` and array
// element stride, so getting it wrong is a live divergence rather than a latent one.
//
// The expected values here were all measured on the real runtime; every one of them also runs
// under the real runtime as part of this test, so they cannot silently rot.
public class TestStructLayoutGcAuto
{
    private sealed class Box { public int V; }

    private enum EInt : int { A, B }

    private struct PlainInt { public int I; }
    private struct GcWrap { public Box O; }
    private struct PtrWrap { public IntPtr H; }

    // --- bucketing: these need field reordering to get the size right ---
    private struct Small3 { public byte B; public Box O; public byte C; }
    private struct Bucketed { public byte B1; public long L1; public byte B2; public Box O; public byte B3; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    private struct BucketedPack1 { public byte B1; public long L1; public byte B2; public Box O; public byte B3; }

    // --- `Pack` is ignored once a GC reference is present ---
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct GcObjByte { public Box O; public byte B; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct GcByteObj { public byte B; public Box O; }
    [StructLayout(LayoutKind.Sequential, Pack = 2)] private struct GcPack2 { public Box O; public byte B; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct GcTwoObj { public Box O; public Box P; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct GcObjLongByte { public Box O; public long L; public byte B; }
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct GcByteIntObj { public byte B; public int I; public Box O; }

    // --- an explicit `Size` is ignored too, in both directions ---
    [StructLayout(LayoutKind.Sequential, Size = 64)] private struct GcSizeTooBig { public Box O; public byte B; }
    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 3)] private struct GcSizeTooSmall { public Box O; public byte B; }

    // --- nesting ---
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct NestGc { public byte B; public GcObjByte Inner; }
    // A by-value field that itself contains a reference is placed at pointer alignment.
    private struct GcInner { public Box O; public long L; }
    private struct GcOuter { public byte Pre; public GcInner Nested; public byte Post; }

    // --- field classification: which fields bucket as primitives, which are placed last ---
    // Enums normalise to their underlying integer, so they bucket.
    private struct EnumVsInt { public EInt E; public int I; public Box O; }
    // A genuine struct is a by-value field, placed last.
    private struct StructVsInt { public PlainInt S; public int I; public Box O; }
    // Single-field BCL-shaped wrappers are still genuine structs.
    private struct GcWrapVsInt { public GcWrap W; public int I; public Box O; }
    private struct PtrWrapVsInt { public PtrWrap W; public int I; public Box O; }
    // A bare IntPtr, by contrast, is a pointer-sized primitive.
    private struct NIntVsInt { public IntPtr N; public int I; public Box O; }
    private struct BoolChar { public bool Bo; public char C; public Box O; }

    // --- controls: no GC reference, so sequential layout stands and nothing here may change ---
    [StructLayout(LayoutKind.Sequential, Pack = 1)] private struct NoGcPack1 { public byte B; public long L; }
    private struct NoGcThreeBytes { public byte A; public byte B; public byte C; }
    private struct NoGcBucketed { public byte B1; public long L1; public byte B2; public long L2; public byte B3; }
    [StructLayout(LayoutKind.Explicit, Size = 9)] private struct NoGcExplicitSize9 { [FieldOffset(0)] public long L; }

    // --- controls: explicit layout is *not* switched to auto, so `Size` is honoured, but the
    //     final size of a GC-containing type is still rounded up to a pointer multiple ---
    [StructLayout(LayoutKind.Explicit)] private struct ExpGcTail { [FieldOffset(0)] public Box O; [FieldOffset(8)] public byte B; }
    [StructLayout(LayoutKind.Explicit, Size = 64)] private struct ExpGcSize64 { [FieldOffset(0)] public Box O; }
    [StructLayout(LayoutKind.Explicit, Size = 9)] private struct ExpGcSize9 { [FieldOffset(0)] public Box O; }
    [StructLayout(LayoutKind.Explicit, Size = 4)] private struct ExpGcSize4 { [FieldOffset(0)] public Box O; }

    private static int failures;
    private static int firstFailure;
    private static int index;

    private static void Check<T>(int expected) where T : struct
    {
        index++;
        int actual = Unsafe.SizeOf<T>();
        if (actual != expected)
        {
            Console.WriteLine($"#{index} {typeof(T).Name}: size {actual}, expected {expected}");
            failures++;
            if (firstFailure == 0) firstFailure = index;
        }
    }

    public static int Main(string[] argv)
    {
        Check<Small3>(16);
        Check<Bucketed>(24);
        Check<BucketedPack1>(24);

        Check<GcObjByte>(16);
        Check<GcByteObj>(16);
        Check<GcPack2>(16);
        Check<GcTwoObj>(16);
        Check<GcObjLongByte>(24);
        Check<GcByteIntObj>(16);

        Check<GcSizeTooBig>(16);
        Check<GcSizeTooSmall>(16);

        Check<NestGc>(24);
        Check<GcInner>(16);
        Check<GcOuter>(24);

        Check<EnumVsInt>(16);
        Check<StructVsInt>(16);
        Check<GcWrapVsInt>(24);
        Check<PtrWrapVsInt>(24);
        Check<NIntVsInt>(24);
        Check<BoolChar>(16);

        Check<NoGcPack1>(9);
        Check<NoGcThreeBytes>(3);
        Check<NoGcBucketed>(40);
        Check<NoGcExplicitSize9>(9);

        Check<ExpGcTail>(16);
        Check<ExpGcSize64>(64);
        Check<ExpGcSize9>(16);
        Check<ExpGcSize4>(8);

        if (failures != 0) return firstFailure;

        // The sizes above are what the layout *reports*. These exercise whether it is actually
        // used consistently: distinct values in every field must survive a round-trip (which a
        // layout that aliases two fields would fail), and `Array.Clear` must reach every byte.
        Bucketed v = default;
        v.B1 = 1; v.L1 = 0x1122334455667788L; v.B2 = 2; v.O = new Box { V = 9 }; v.B3 = 3;
        if (v.B1 != 1 || v.L1 != 0x1122334455667788L || v.B2 != 2 || v.O.V != 9 || v.B3 != 3) return 100;

        GcOuter w = default;
        w.Pre = 7; w.Nested.O = new Box { V = 5 }; w.Nested.L = -1L; w.Post = 8;
        if (w.Pre != 7 || w.Nested.O.V != 5 || w.Nested.L != -1L || w.Post != 8) return 101;

        Small3[] arr = new Small3[3];
        for (int i = 0; i < arr.Length; i++) { arr[i].B = (byte)(i + 1); arr[i].O = new Box { V = i }; arr[i].C = (byte)(i + 10); }
        Array.Clear(arr, 1, 1);
        if (arr[1].B != 0 || arr[1].O != null || arr[1].C != 0) return 102;
        if (arr[0].B != 1 || arr[0].O == null || arr[0].C != 10) return 103;
        if (arr[2].B != 3 || arr[2].O == null || arr[2].C != 12) return 104;

        Array.Clear(arr);
        for (int i = 0; i < arr.Length; i++)
            if (arr[i].B != 0 || arr[i].O != null || arr[i].C != 0) return 105;

        return 0;
    }
}
