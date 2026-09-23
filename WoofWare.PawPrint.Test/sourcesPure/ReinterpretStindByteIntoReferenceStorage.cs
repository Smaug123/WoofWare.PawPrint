using System;
using System.Runtime.CompilerServices;

// `stind.i1` through a `ref byte` that points into an `[InlineArray]` slot whose element holds a
// reference: `ref byte q = ref buffer[k].Tag; q = 9;`.
//
// `buffer[k].Tag = 9` is the same address written by `stfld`, which hands the byref layer a value
// already of the field's type, `System.Byte`. `stind.i1` does not know the type it stores into and
// hands over a signed `int8`. Storage holding a reference has no byte image, so the only route to its
// `Tag` byte is to name that cell, and the store must then leave the cell a `System.Byte` holding the
// payload's bits: the reads after it go through `ldfld Tag`, which names the cell by its own type.
public class TestReinterpretStindByteIntoReferenceStorage
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    public static int Main(string[] argv)
    {
        Buffer b = default;
        b[0] = new Elem { Tag = 5, Payload = new Box { V = 50 } };
        b[1] = new Elem { Tag = 6, Payload = new Box { V = 60 } };

        ref byte q = ref b[1].Tag;
        q = 9;
        if (b[1].Tag != 9) return 1;
        if (b[1].Payload.V != 60 || b[0].Tag != 5) return 2;

        // Control: slot 0 has no `ByteOffset`, and fails the same way.
        ref byte q0 = ref b[0].Tag;
        q0 = 7;
        if (b[0].Tag != 7) return 3;

        return 0;
    }
}
