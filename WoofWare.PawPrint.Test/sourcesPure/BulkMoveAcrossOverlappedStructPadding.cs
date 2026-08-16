using System;
using System.Runtime.InteropServices;

// The sibling of `BulkMoveAcrossStructPadding.cs` for padding that *two* fields cover.
//
// Explicit layout may overlay two identical reference-containing structs at the same offset —
// CoreCLR permits overlapping GC references when the overlapping fields have the same type — and
// then every byte of the inner struct's trailing filler is padding within each field while lying
// inside both field extents. `CliType.TryPaddingRunAt` refuses that byte: with two fields over it
// there is no single one to descend through, so it cannot say whose padding it is, and it returns
// `None` exactly as it does for an aliased *data* byte.
//
// That refusal is not currently reachable, which is why this case is parked rather than fixed. An
// explicit-layout struct with any overlap is stored byte-backed, and a byte-backed value holding
// object references cannot be field-accessed at all: the plain `src[i].First.N = i + 1` below
// stops in `CliType.OfBytesLike` with "non-primitive template ObjectRef None", long before any
// bulk move. Measured, not assumed — allocating the array alone succeeds, and it is the first
// field write that fails.
//
// So this file is a marker for a consequence, not a reproduction of one: when the byte-backed
// representation learns to hold references (the gap `ReinterpretCellUnderAliasedAncestor.cs` is
// parked on), a bulk copy over such an array becomes reachable and `TryPaddingRunAt`'s
// two-fields-cover-it branch is exercised for the first time. Un-park it then, and expect
// to have to teach that branch that padding shared by fields which are padding *there too* is
// still padding.
public class TestBulkMoveAcrossOverlappedStructPadding
{
    private sealed class Box
    {
        public int Value;
    }

    // 24 bytes: references at 0 and 8, `N` at 16, trailing padding at [20, 24).
    private struct Inner
    {
        public Box A;
        public Box B;
        public int N;
    }

    [StructLayout(LayoutKind.Explicit)]
    private struct Overlaid
    {
        [FieldOffset(0)]
        public Inner First;

        [FieldOffset(0)]
        public Inner Second;
    }

    // 24 * 700 = 16800 bytes, and 16384 = 682 * 24 + 16, so the second chunk of
    // `Buffer.BulkMoveWithWriteBarrier` starts 16 bytes into element 682 and its cursor reaches
    // the shared filler at [20, 24) of that element.
    private const int Count = 700;

    public static int Run()
    {
        Overlaid[] src = new Overlaid[Count];

        for (int i = 0; i < Count; i++)
        {
            src[i].First.N = i + 1;

            if (i % 89 == 0 || (i >= 680 && i <= 685))
            {
                src[i].First.A = new Box { Value = i * 3 + 1 };
                src[i].First.B = new Box { Value = i * 5 + 2 };
            }
        }

        Overlaid[] dest = new Overlaid[Count];
        Array.Copy(src, dest, Count);

        for (int i = 0; i < Count; i++)
        {
            if (dest[i].First.N != i + 1) return 100 + (i % 90);

            // The two fields alias, so reading through `Second` must see what was written
            // through `First`.
            if (dest[i].Second.N != i + 1) return 200 + (i % 90);

            if (i % 89 == 0 || (i >= 680 && i <= 685))
            {
                if (dest[i].First.A == null || dest[i].First.A.Value != i * 3 + 1) return 300 + (i % 90);
                if (dest[i].First.B == null || dest[i].First.B.Value != i * 5 + 2) return 400 + (i % 90);
                if (!ReferenceEquals(dest[i].Second.A, dest[i].First.A)) return 500 + (i % 90);
            }
            else
            {
                if (dest[i].First.A != null) return 600 + (i % 90);
            }
        }

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int r = TestBulkMoveAcrossOverlappedStructPadding.Run();
        if (r != 0) return r;
        return 0;
    }
}
