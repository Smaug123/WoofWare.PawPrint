using System;

namespace DefaultSpanSequenceEqualTest
{
    // Guards null-backed spans across the bitwise-equatable span intrinsics. A `default`
    // span carries a null byref in `_reference`, and every intrinsic of this shape
    // reinterprets `MemoryMarshal.GetReference(span)` through `Unsafe.As<T, byte>` before
    // it ever looks at the length. That reinterpret is address-preserving and never
    // dereferences, so it has to be defined on a null byref
    // (`ManagedPointerSource.reinterpretAs`); a reinterpret that refuses null fails with
    // "cannot project from null managed pointer".
    //
    // `ReadOnlySpan<T>.Empty` is `default`, so it is affected identically; a zero-length
    // *array*-backed span is not, because its `_reference` points at the array.
    //
    // SequenceEqual is named here because it is the oldest of the affected intrinsics;
    // StartsWith and EndsWith went the same way, which is why all three are covered.
    class Program
    {
        static int Main(string[] args)
        {
            ReadOnlySpan<int> defaultSpan = default;

            if (!defaultSpan.SequenceEqual(defaultSpan))
            {
                return 1;
            }

            if (!defaultSpan.StartsWith(defaultSpan))
            {
                return 2;
            }

            if (!defaultSpan.EndsWith(defaultSpan))
            {
                return 3;
            }

            // ReadOnlySpan<T>.Empty is `default`, so it is null-backed in the same way.
            if (!ReadOnlySpan<int>.Empty.SequenceEqual(ReadOnlySpan<int>.Empty))
            {
                return 4;
            }

            int[] nonEmpty = new int[2];
            nonEmpty[0] = 1;
            nonEmpty[1] = 2;

            // A non-empty span still starts and ends with the empty default span.
            if (!((ReadOnlySpan<int>)nonEmpty).StartsWith(defaultSpan))
            {
                return 5;
            }

            if (!((ReadOnlySpan<int>)nonEmpty).EndsWith(ReadOnlySpan<int>.Empty))
            {
                return 6;
            }

            return 0;
        }
    }
}
