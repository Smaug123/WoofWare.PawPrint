using System;
using System.Runtime.CompilerServices;

// The implicit `Span<T>`/`ReadOnlySpan<T>` conversion from an `[InlineArray(N)]` value, which C#
// lowers to `MemoryMarshal.CreateSpan(ref Unsafe.As<TBuffer, TElement>(ref buffer), N)`.
//
// Split out from `InlineArrayLayout.cs` deliberately: that file covers indexing and stride, which
// need only the N-slot layout, whereas this one additionally needs a span to be constructible over
// interior storage and walked by `Unsafe.Add`. If the span machinery has a gap of its own, it
// should not hold the layout coverage hostage.
public class TestInlineArraySpan
{
    [InlineArray(3)] private struct BufInt { private int _item; }

    public static int Main(string[] argv)
    {
        BufInt b = default;
        for (int i = 0; i < 3; i++) b[i] = 100 + i;

        Span<int> span = b;
        if (span.Length != 3) return 1;
        if (span[0] != 100 || span[1] != 101 || span[2] != 102) return 2;

        span[2] = 555;
        if (b[2] != 555) return 3;

        ReadOnlySpan<int> ro = b;
        if (ro.Length != 3) return 4;
        if (ro[0] != 100 || ro[1] != 101 || ro[2] != 555) return 5;

        int total = 0;
        foreach (int v in b) total += v;
        if (total != 100 + 101 + 555) return 6;

        return 0;
    }
}
