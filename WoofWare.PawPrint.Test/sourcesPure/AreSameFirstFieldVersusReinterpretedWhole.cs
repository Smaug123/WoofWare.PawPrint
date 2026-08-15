using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace AreSameFirstFieldVersusReinterpretedWholeTest
{
    struct A
    {
        public int X;
    }

    // A byref to a struct's first field and a byref to the whole struct reinterpreted as that
    // field's type are the same address, and real .NET's `Unsafe.AreSame` says so.
    //
    // Structural comparison cannot tell. Once the trailing `ReinterpretAs` is stripped, one
    // side is `Byref (local, [Field X])` and the other the bare `Byref (local, [])`; whether
    // those alias depends on whether `X` sits at offset 0 of its declaring type, which is
    // field-offset layout that byref comparison does not carry. So it declines rather than
    // guessing, and `StorageLocation.resolveCeq` decides it by resolving both to byte
    // coordinates in the one local's storage: `X`'s offset is 0, so both are 0.
    //
    // It used to answer `false` and return 3 (both halves diverging, 1 for the direct
    // `Unsafe.AreSame` and 2 for the same comparison through `ReadOnlySpan<T>.op_Equality`),
    // and then, once the refusal landed, to fail loudly at the first half. The two halves are
    // still reported as independent bits rather than short-circuiting, so a regression in
    // either says which one.
    //
    // The first half is the point: it involves no span at all, so this is a byref-comparison
    // fact rather than anything about spans, and it is reachable by any guest calling
    // `Unsafe.AreSame` directly. Note the resolution is *not* total — a reference- or
    // pointer-containing value has no byte image, so such a pair still gets no coordinate and
    // is still refused; `TestByrefComparison.fs` covers that side, which no guest can assert.
    class Program
    {
        static int Main(string[] args)
        {
            A a = default;
            a.X = 5;

            ref int viaField = ref a.X;
            ref int viaReinterpret = ref Unsafe.As<A, int>(ref a);

            int result = 0;

            if (!Unsafe.AreSame(ref viaField, ref viaReinterpret))
            {
                result += 1;
            }

            ReadOnlySpan<int> spanFromField = MemoryMarshal.CreateReadOnlySpan(ref viaField, 1);
            ReadOnlySpan<int> spanFromReinterpret = MemoryMarshal.CreateReadOnlySpan(ref viaReinterpret, 1);

            if (!(spanFromField == spanFromReinterpret))
            {
                result += 2;
            }

            return result;
        }
    }
}
