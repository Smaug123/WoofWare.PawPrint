using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `[InlineArray(N)]` makes a value type N repeats of its single declared instance field.
// CoreCLR lays the type out *as if it had that one field*, then multiplies the resulting
// instance size by N (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612 for
// the auto-layout route, :8663 for sequential); the multiplication happens after the single-slot
// alignment rounding, and the type's alignment is unchanged. There is only ever one FieldDesc —
// the N-1 repeats are storage, not fields, and reflection never sees them (asserted separately by
// `InlineArrayReflectionFieldCount.cs`).
//
// The size is observable through `sizeof`, `Unsafe.SizeOf<T>`, array element stride and the offset
// of anything laid out after an inline array inside a larger struct, so getting the stride wrong is
// a live divergence rather than a latent one.
//
// Every expected value here also runs under the real runtime as part of this test, so none of them
// can silently rot. The systematic sweep over element shape / N / `Pack` lives in
// `TestInlineArrayLayout.fs`, which generates programs and takes the real runtime as its oracle;
// this file covers the shapes whose *behaviour* (not just size) is worth pinning down.
public class TestInlineArrayLayout
{
    private sealed class Box { public int V; }

    private struct Three { public byte A; public byte B; public byte C; }
    private struct Five { public int I; public byte B; }
    private struct MixedRef { public byte B; public Box O; }

    [InlineArray(1)] private struct One { private int _item; }
    [InlineArray(3)] private struct BufInt { private int _item; }
    [InlineArray(3)] private struct BufByte { private byte _item; }
    [InlineArray(2)] private struct BufLong { private long _item; }
    [InlineArray(2)] private struct BufThree { private Three _item; }
    [InlineArray(2)] private struct BufFive { private Five _item; }
    [InlineArray(2)] private struct BufObj { private Box _item; }
    [InlineArray(3)] private struct BufMixed { private MixedRef _item; }
    [InlineArray(2)] private struct BufBufInt { private BufInt _item; }
    [InlineArray(2)] private struct BufNInt { private IntPtr _item; }

    // `Pack` applies to the single-slot layout, so it can change the stride.
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    [InlineArray(2)] private struct BufLongPack1 { private long _item; }

    // An inline array embedded in a larger struct: its own alignment (that of the element, not of
    // the whole N-slot run) decides where it lands, and everything after it must clear all N slots.
    private struct Holder { public byte Lead; public BufInt Buf; public byte Tail; }

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
        Check<One>(4);
        Check<BufInt>(12);
        Check<BufByte>(3);
        Check<BufLong>(16);
        Check<Three>(3);
        Check<BufThree>(6);
        Check<Five>(8);
        Check<BufFive>(16);
        Check<BufObj>(16);
        Check<MixedRef>(16);
        Check<BufMixed>(48);
        Check<BufBufInt>(24);
        Check<BufNInt>(16);
        Check<BufLongPack1>(16);
        Check<Holder>(20);

        if (failures != 0) return firstFailure;

        // Marshalling walks the same repeated layout: `Marshal.SizeOf` reports N slots, not one.
        if (Marshal.SizeOf<BufInt>() != 12) return 100;
        if (Marshal.SizeOf<BufThree>() != 6) return 101;

        // Every slot is independently addressable, and slot 0 is still the declared field.
        BufInt b = default;
        for (int i = 0; i < 3; i++) b[i] = 100 + i;
        if (b[0] != 100 || b[1] != 101 || b[2] != 102) return 102;

        // Assigning the whole aggregate copies all N slots, not just the first.
        BufInt c = b;
        c[1] = -1;
        if (b[1] != 101) return 103;
        if (c[0] != 100 || c[1] != -1 || c[2] != 102) return 104;

        // The inline array embedded in a larger struct must not overlap its neighbours.
        Holder h = default;
        h.Lead = 7;
        h.Tail = 9;
        for (int i = 0; i < 3; i++) h.Buf[i] = 200 + i;
        if (h.Lead != 7 || h.Tail != 9) return 105;
        if (h.Buf[0] != 200 || h.Buf[1] != 201 || h.Buf[2] != 202) return 106;

        // Array element stride is the whole N-slot run.
        BufInt[] arr = new BufInt[3];
        for (int i = 0; i < arr.Length; i++)
            for (int j = 0; j < 3; j++)
                arr[i][j] = (i * 10) + j;
        for (int i = 0; i < arr.Length; i++)
            for (int j = 0; j < 3; j++)
                if (arr[i][j] != (i * 10) + j) return 107;

        Array.Clear(arr, 1, 1);
        if (arr[1][0] != 0 || arr[1][1] != 0 || arr[1][2] != 0) return 108;
        if (arr[0][0] != 0 || arr[0][1] != 1 || arr[0][2] != 2) return 109;
        if (arr[2][0] != 20 || arr[2][1] != 21 || arr[2][2] != 22) return 110;

        // A nested inline array indexes through two levels of stride.
        BufBufInt nested = default;
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                nested[i][j] = (i * 100) + j;
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                if (nested[i][j] != (i * 100) + j) return 111;

        return 0;
    }
}
