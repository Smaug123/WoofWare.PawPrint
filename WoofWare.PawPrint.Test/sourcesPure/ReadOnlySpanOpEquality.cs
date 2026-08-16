using System;

namespace ReadOnlySpanOpEqualityTest
{
    // `ReadOnlySpan<T>.op_Equality` is *reference* equality, not content equality: two spans
    // are equal iff they have the same length and their `_reference` byrefs are the same
    // address. So this file is really a test of `Unsafe.AreSame` over span byrefs — that two
    // spans built by different routes onto the same storage normalise to one pointer, and
    // that spans onto different storage (or the same storage at a different offset) do not.
    //
    // Cases that a weaker implementation would get wrong are called out individually below;
    // in particular "both are empty" must NOT imply equal, since a zero-length slice of a
    // live array carries a non-null byref while `default` carries a null one.
    //
    // Every span here is explicitly a `ReadOnlySpan<T>`: `array.AsSpan(...)` returns a
    // `Span<T>`, whose own `op_Equality` is a distinct method that PawPrint does not yet
    // serve, so comparing two of those directly would test something else (and fail).
    class Program
    {
        static int Main(string[] args)
        {
            int[] arr = new int[4];
            arr[0] = 1;
            arr[1] = 2;
            arr[2] = 3;
            arr[3] = 4;

            // Three different constructions over the same array must all compare equal: the
            // implicit conversion (`ReadOnlySpan<T>.op_Implicit` -> `.ctor(T[])`), the
            // explicit constructor, and `AsSpan` widened through `Span<T>.op_Implicit`.
            ReadOnlySpan<int> viaConversion = arr;
            ReadOnlySpan<int> viaCtor = new ReadOnlySpan<int>(arr);
            ReadOnlySpan<int> viaAsSpan = arr.AsSpan();

            if (!(viaConversion == viaCtor))
            {
                return 1;
            }

            if (!(viaCtor == viaAsSpan))
            {
                return 2;
            }

            // Distinct arrays with identical contents are NOT equal. This is what separates
            // op_Equality from SequenceEqual, so an implementation that compared contents
            // would fail here.
            int[] sameContents = new int[4];
            sameContents[0] = 1;
            sameContents[1] = 2;
            sameContents[2] = 3;
            sameContents[3] = 4;

            ReadOnlySpan<int> otherStorage = sameContents;

            if (viaConversion == otherStorage)
            {
                return 3;
            }

            // ... and `!=` is the negation of the above two, exercising op_Inequality (whose
            // whole body is a call to op_Equality plus a `ceq` against 0).
            if (viaConversion != viaCtor)
            {
                return 4;
            }

            if (!(viaConversion != otherStorage))
            {
                return 5;
            }

            // Same array and same start, different length: caught by the `_length` check
            // before the byrefs are ever compared.
            if (new ReadOnlySpan<int>(arr, 0, 2) == new ReadOnlySpan<int>(arr, 0, 3))
            {
                return 6;
            }

            // Same array and same length, different start: identical `_length`, so this one
            // is decided purely by the byref comparison.
            if (new ReadOnlySpan<int>(arr, 0, 2) == new ReadOnlySpan<int>(arr, 1, 2))
            {
                return 7;
            }

            // Two routes to the same interior offset: `Slice` on a whole-array span, which
            // walks the span's byref forward, and the three-argument constructor, which
            // advances the array's data reference directly.
            if (!(viaConversion.Slice(1, 2) == new ReadOnlySpan<int>(arr, 1, 2)))
            {
                return 8;
            }

            // `default` is null-backed with length 0; `ReadOnlySpan<T>.Empty` is `default`.
            ReadOnlySpan<int> defaultSpan = default;

            if (!(defaultSpan == default))
            {
                return 9;
            }

            if (!(ReadOnlySpan<int>.Empty == defaultSpan))
            {
                return 10;
            }

            // A zero-length slice of a live array has length 0 like `default`, but its byref
            // points at `arr[1]` rather than being null — so the two are NOT equal. An
            // implementation that short-circuited on "both empty" would wrongly say they are.
            ReadOnlySpan<int> emptyButRooted = new ReadOnlySpan<int>(arr, 1, 0);

            if (emptyButRooted.Length != 0)
            {
                return 11;
            }

            if (emptyButRooted == defaultSpan)
            {
                return 12;
            }

            // Two zero-length slices at the *same* offset are equal, though, so the previous
            // case is about the address and not about zero-length spans at large.
            if (!(emptyButRooted == new ReadOnlySpan<int>(arr, 1, 0)))
            {
                return 13;
            }

            // Zero-length slices at different offsets of the same array are not equal.
            if (emptyButRooted == new ReadOnlySpan<int>(arr, 2, 0))
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

            // The same again over a string-backed `ReadOnlySpan<char>`, whose byref roots at
            // the string's character data rather than at array elements — a different byref
            // flavour reaching the same comparison. (`string.AsSpan` returns a ReadOnlySpan
            // directly, unlike the array overloads used above.)
            string s = "hello";

            if (!(s.AsSpan() == s.AsSpan()))
            {
                return 17;
            }

            if (s.AsSpan(0, 2) == s.AsSpan(1, 2))
            {
                return 18;
            }

            if (!(s.AsSpan(1, 2) == s.AsSpan(1, 2)))
            {
                return 19;
            }

            if (!(s.AsSpan().Slice(1, 2) == s.AsSpan(1, 2)))
            {
                return 20;
            }

            // A span over a char array is never the same storage as one over a string, even
            // when the characters agree and the lengths match.
            char[] chars = new char[5];
            chars[0] = 'h';
            chars[1] = 'e';
            chars[2] = 'l';
            chars[3] = 'l';
            chars[4] = 'o';

            ReadOnlySpan<char> fromArray = chars;

            if (fromArray.Length != s.AsSpan().Length)
            {
                return 21;
            }

            if (fromArray == s.AsSpan())
            {
                return 22;
            }

            return 0;
        }
    }
}
