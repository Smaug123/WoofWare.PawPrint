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
    // PawPrint cannot tell. Once the trailing `ReinterpretAs` is stripped, one side is
    // `Byref (local, [Field X])` and the other the bare `Byref (local, [])`; whether those
    // alias depends on whether `X` sits at offset 0 of its declaring type, and field-offset
    // layout is not something byref comparison carries. So it refuses the comparison, naming
    // both byrefs, rather than answering — this guest fails loudly at the first half below.
    //
    // It used to answer `false` and return 3 (both halves diverging, 1 for the direct
    // `Unsafe.AreSame` and 2 for the same comparison through `ReadOnlySpan<T>.op_Equality`).
    // The two halves are still reported as independent bits rather than short-circuiting, so
    // if the refusal is ever replaced by a real answer the exit code still says which half is
    // wrong.
    //
    // The first half is the point: it involves no span at all, so this is a byref-comparison
    // gap rather than anything about spans, and it is reachable by any guest calling
    // `Unsafe.AreSame` directly. Closing it needs either an "is this field at offset 0"
    // predicate or full byte-offset byref identity; the latter cannot be total while
    // reference- and pointer-containing values remain byte-imageless.
    //
    // PawPrint's side of this — that it refuses rather than guesses — is asserted by
    // `TestByrefComparison.fs`, because a parked guest is only ever run against real .NET.
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
