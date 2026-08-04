using System;

namespace DefaultSpanSequenceEqualTest
{
    // Records a gap that predates the MemoryExtensions prefix/suffix work: a `default`
    // span carries a null byref in `_reference`, and every bitwise-equatable span
    // intrinsic of this shape reinterprets `MemoryMarshal.GetReference(span)` through
    // `Unsafe.As<T, byte>` before it ever looks at the length. Appending that
    // reinterpret projection to a null managed pointer fails with
    // "cannot project from null managed pointer" (ManagedPointerSource.fs:477),
    // even though the real runtime never dereferences the pointer when the length is 0.
    //
    // `ReadOnlySpan<T>.Empty` is `default`, so it is affected identically; a zero-length
    // *array*-backed span is not, because its `_reference` points at the array.
    //
    // SequenceEqual is named here because it is the oldest of the affected intrinsics;
    // StartsWith and EndsWith fail the same way, so fixing the null-byref projection
    // should make all of them work at once.
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
