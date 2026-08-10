using System;

namespace SpanOpEqualityTest
{
    // The `Span<T>` sibling of `ReadOnlySpanOpEquality.cs`. `Span<T>.op_Equality` has IL of
    // the same shape as the ReadOnlySpan one — `_length` compare, then `Unsafe.AreSame` over
    // the two `_reference` byrefs — but it is a distinct method on a distinct type, so it
    // needs its own allowlist entry and its own coverage.
    //
    // Unlike the ReadOnlySpan file, this one can use `array.AsSpan(...)` freely: those
    // overloads return `Span<T>`, which is exactly the type under test here.
    class Program
    {
        static int Main(string[] args)
        {
            int[] arr = new int[4];
            arr[0] = 1;
            arr[1] = 2;
            arr[2] = 3;
            arr[3] = 4;

            // Three constructions over the same array: the implicit conversion, `AsSpan`, and
            // the explicit constructor.
            Span<int> viaConversion = arr;
            Span<int> viaAsSpan = arr.AsSpan();
            Span<int> viaCtor = new Span<int>(arr);

            if (!(viaConversion == viaAsSpan))
            {
                return 1;
            }

            if (!(viaAsSpan == viaCtor))
            {
                return 2;
            }

            // Distinct arrays with identical contents are not equal: this is reference
            // equality, not content equality.
            int[] sameContents = new int[4];
            sameContents[0] = 1;
            sameContents[1] = 2;
            sameContents[2] = 3;
            sameContents[3] = 4;

            if (viaConversion == sameContents.AsSpan())
            {
                return 3;
            }

            // op_Inequality, whose body is a call to op_Equality plus a `ceq` against 0.
            if (viaConversion != viaAsSpan)
            {
                return 4;
            }

            if (!(viaConversion != sameContents.AsSpan()))
            {
                return 5;
            }

            // Same array and same start, different length: decided by the `_length` check.
            if (arr.AsSpan(0, 2) == arr.AsSpan(0, 3))
            {
                return 6;
            }

            // Same array and same length, different start: identical `_length`, so this is
            // decided purely by the byref comparison.
            if (arr.AsSpan(0, 2) == arr.AsSpan(1, 2))
            {
                return 7;
            }

            // Two routes to the same interior offset: `Slice` walks the span's byref forward,
            // `AsSpan(start, length)` advances the array's data reference directly.
            if (!(viaConversion.Slice(1, 2) == arr.AsSpan(1, 2)))
            {
                return 8;
            }

            // `default` is null-backed with length 0, and `Span<T>.Empty` is `default`.
            Span<int> defaultSpan = default;

            if (!(defaultSpan == default))
            {
                return 9;
            }

            if (!(Span<int>.Empty == defaultSpan))
            {
                return 10;
            }

            // A zero-length slice of a live array is length 0 like `default`, but its byref
            // points at `arr[1]` rather than being null, so the two are NOT equal. An
            // implementation short-circuiting on "both empty" would wrongly say they are.
            Span<int> emptyButRooted = arr.AsSpan(1, 0);

            if (emptyButRooted.Length != 0)
            {
                return 11;
            }

            if (emptyButRooted == defaultSpan)
            {
                return 12;
            }

            // Two zero-length slices at the same offset are equal, so the previous case is
            // about the address rather than about zero-length spans at large.
            if (!(emptyButRooted == arr.AsSpan(1, 0)))
            {
                return 13;
            }

            // Zero-length slices at different offsets are not equal.
            if (emptyButRooted == arr.AsSpan(2, 0))
            {
                return 14;
            }

            // A non-empty span is not equal to the empty one in either direction.
            if (viaConversion == defaultSpan)
            {
                return 15;
            }

            if (defaultSpan == viaConversion)
            {
                return 16;
            }

            // Writing through one span is visible through a span that compares equal to it,
            // which is what "same storage" is supposed to mean.
            viaConversion[2] = 99;

            if (viaAsSpan[2] != 99)
            {
                return 17;
            }

            // ... and not visible through one that does not.
            if (sameContents.AsSpan()[2] != 3)
            {
                return 18;
            }

            return 0;
        }
    }
}
