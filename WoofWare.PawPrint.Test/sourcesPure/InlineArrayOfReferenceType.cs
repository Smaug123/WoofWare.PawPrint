using System;
using System.Runtime.CompilerServices;

// Write side, in isolation: storing a reference through a zero-offset `ReinterpretAs` byref.
//
// Indexing an `[InlineArray(N)]` struct lowers to `Unsafe.As<TBuffer, TElement>(ref buffer)`,
// so `buffer[0] = ...` is a byref whose projections are `[ReinterpretAs TElement]` with no
// trailing `Field`/`ByteOffset`, over storage whose single field spans the whole cell.
//
// Deliberately kept to element 0 and to a *write*, so that landing the write-side fix alone
// makes this pass:
//   - element 1 would be `[ReinterpretAs string; ByteOffset 8]`, which is the separate
//     inline-array-tail gap covered by `InlineArraySecondSlot.cs`;
//   - reading back through `buffer[0]` would hit the separately parked reinterpret *read*
//     gap covered by `InlineArrayOfReferenceTypeReadOnly.cs`.
// The result is therefore observed through the `First` property, which reads `_item`
// directly as an ordinary field access rather than through a reinterpret byref.
[InlineArray(2)]
public struct ScratchBuffer<T>
{
    private T _item;

    // Ordinary `ldfld` of the single field: no `ReinterpretAs` involved.
    public T First => _item;
}

public class TestInlineArrayOfReferenceType
{
    public static int Main(string[] argv)
    {
        ScratchBuffer<string> buffer = default;
        buffer[0] = "hello";

        if (buffer.First != "hello") return 1;

        return 0;
    }
}
