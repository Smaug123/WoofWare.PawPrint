using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace AreSameSequentialStructDistinctFieldsTest
{
    [StructLayout(LayoutKind.Sequential)]
    struct S
    {
        public int X;
        public int Y;
    }

    // The direct negative of `AreSameExplicitLayoutOverlappingFields.cs`: two fields of an
    // ordinary struct, laid out at *different* offsets. Real .NET's
    // `Unsafe.AreSame(ref s.X, ref s.Y)` is `false`.
    //
    // This is the mainstream shape, not an exotic one. Both byrefs are `[Field _]` over one
    // root, so structural comparison falls to the arm that declines every field divergence —
    // it cannot answer `false` here without also answering `false` for the explicit-layout
    // struct, where the identical pair of chains means one address. Comparing byrefs to two
    // different fields of *any* struct was refused until byref comparison gained access to
    // field offsets; the explicit-layout guests are what the corpus happened to contain, not
    // the extent of what was refused.
    //
    // It is also the oracle that stops the overlapping guests from being satisfiable the wrong
    // way. All of them expect `true`, so an implementation that called any two precise
    // locations in one `ByteStorageIdentity` equal — ignoring their offsets entirely — would
    // pass every one, while misreporting every non-overlapping pair of fields in the runtime.
    // Mutation-tested against exactly that: dropping the offset comparison fails this guest
    // and leaves its overlapping siblings green.
    class Program
    {
        static int Main(string[] args)
        {
            S s = default;
            s.X = 3;
            s.Y = 4;

            if (Unsafe.AreSame(ref s.X, ref s.Y))
            {
                return 1;
            }

            // Positive control on the same struct, so the check above cannot pass merely
            // because this shape answers `false` unconditionally — which is what it did before
            // the refusal that preceded this, and would be a regression to it.
            if (!Unsafe.AreSame(ref s.X, ref s.X))
            {
                return 2;
            }

            // The fields are genuinely independent, so the `false` above is not merely a
            // nominal distinction between two names for one address.
            s.Y = 5;

            if (s.X != 3)
            {
                return 3;
            }

            return 0;
        }
    }
}
