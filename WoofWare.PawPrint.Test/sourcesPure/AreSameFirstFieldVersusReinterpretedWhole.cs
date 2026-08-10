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
    // field's type are the same address, and real .NET's `Unsafe.AreSame` says so. PawPrint
    // says they differ: `ManagedPointerSource.normaliseForComparison` strips the trailing
    // `ReinterpretAs`, which leaves a whole-value root on one side and a `Field` projection on
    // the other, and nothing equates the two.
    //
    // Both halves are reported independently rather than short-circuiting, so the exit code
    // says which of them diverged: 1 is the direct `Unsafe.AreSame`, 2 is the same comparison
    // reached through `ReadOnlySpan<T>.op_Equality`, 3 is both. Measured 3 against 0.
    //
    // The first half is the point: it involves no span at all, so this is a defect in the
    // byref normalisation under `Unsafe.AreSame` rather than anything about spans, and it is
    // reachable today by any guest that calls `Unsafe.AreSame` directly. Fixing it means
    // canonicalising "whole value" against "field at offset 0" in `ManagedPointerSource`,
    // which reaches every consumer of byref comparison — `ceq` on byrefs, `Unsafe.ByteOffset`,
    // the address-ordering predicates — and so wants its own change.
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
