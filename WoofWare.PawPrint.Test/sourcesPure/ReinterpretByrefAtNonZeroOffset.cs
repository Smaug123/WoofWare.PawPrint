using System;
using System.Runtime.CompilerServices;

// A reinterpreting byref walked forward past the first field: `[ReinterpretAs string; ByteOffset 8]`
// over storage that contains object references.
//
// No `[InlineArray]` here at all — `Unsafe.As` plus `Unsafe.Add` reaches the same byref shape over
// an ordinary two-field struct, so this pins the byref behaviour on its own. It cannot go through
// the byte-view path: `ObjectRef` storage has no byte rendering, so the access is only serviceable
// by naming the cell the byte range covers.
//
// The zero-offset case (`[ReinterpretAs string]` over single-field storage) is already covered by
// `InlineArrayOfReferenceType.cs`; this is the same question one field along.
public struct TwoRefs
{
    public string A;
    public string B;
}

public class TestReinterpretByrefAtNonZeroOffset
{
    public static int Main(string[] argv)
    {
        TwoRefs pair = default;
        pair.A = "first";

        // Write through the reinterpreted byref at the second field's offset.
        ref string second = ref Unsafe.Add(ref Unsafe.As<TwoRefs, string>(ref pair), 1);
        second = "second";

        // Observed through ordinary field access, so the write elision alone is under test.
        if (pair.A != "first") return 1;
        if (pair.B != "second") return 2;

        // And read back through the same shape.
        if (Unsafe.Add(ref Unsafe.As<TwoRefs, string>(ref pair), 1) != "second") return 3;

        return 0;
    }
}
