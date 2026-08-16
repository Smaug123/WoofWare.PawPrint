using System;
using System.Runtime.CompilerServices;

// Element *1* of an `[InlineArray(2)]` struct, which is a different gap from the zero-offset
// element-0 cases in `InlineArrayOfReferenceType.cs` / `InlineArrayOfReferenceTypeReadOnly.cs`.
//
// Indexing past the first element lowers to `Unsafe.Add(ref Unsafe.As<TBuffer, TElement>(ref
// buffer), 1)`, i.e. `[ReinterpretAs TElement; ByteOffset <one element width>]` — 4 here, for
// `int`. That offset only has somewhere to land once the struct's storage is N slots rather than
// its one declared field, which is what `InlineArrayStorage.expand` gives it.
//
// A primitive element is used deliberately: it removes the reference-typed-storage question
// entirely, so this isolates "inline arrays have N slots" from "byte views over references".
// `InlineArrayReferenceSecondSlot.cs` is the same index over a reference element, which needs
// both.
[InlineArray(2)]
public struct ScratchBufferInt
{
    private int _item;
}

public class TestInlineArraySecondSlot
{
    public static int Main(string[] argv)
    {
        ScratchBufferInt buffer = default;
        buffer[0] = 11;
        buffer[1] = 22;

        if (buffer[0] != 11) return 1;
        if (buffer[1] != 22) return 2;

        return 0;
    }
}
