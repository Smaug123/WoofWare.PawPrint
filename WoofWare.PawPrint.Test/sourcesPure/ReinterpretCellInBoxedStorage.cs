using System;
using System.Runtime.CompilerServices;

// The third root shape that can hold a value type containing object references: a *box*.
//
// An interface call on a boxed struct passes `this` as a byref to the boxed payload, so indexing
// the inline array inside `Describe` roots at `ByrefRoot.HeapValue` with an empty structural
// prefix — the same blind spot as `ReinterpretCellInArrayElementStorage.cs`, reached through a
// different specialised byte reader. The precise-field reader on that path only recognises a field
// *starting* at the requested offset, which `Tag` is not: it sits at offset 8 inside a slot that
// itself starts at 0 or 16.
public class TestReinterpretCellInBoxedStorage
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    private interface IDescribe { int Describe(int k); }

    [InlineArray(2)]
    private struct Buffer : IDescribe
    {
        private Elem _item;

        public int Describe(int k)
        {
            Elem slot = this[k];
            return (slot.Tag * 1000) + slot.Payload.V;
        }
    }

    public static int Main(string[] argv)
    {
        Buffer buffer = default;
        buffer[0] = new Elem { Tag = 1, Payload = new Box { V = 10 } };
        buffer[1] = new Elem { Tag = 2, Payload = new Box { V = 20 } };

        IDescribe boxed = buffer;

        if (boxed.Describe(0) != 1010) return 1;
        if (boxed.Describe(1) != 2020) return 2;

        return 0;
    }
}
