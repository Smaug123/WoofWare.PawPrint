using System;
using System.Runtime.CompilerServices;

// The read mirror of `ReinterpretSameWidthStoreIntoNamedCell.cs`: a primitive field of an
// `[InlineArray]` slot whose element holds a reference, read through a same-width `Unsafe.As` view
// of a different primitive type. The guest observes the field's bit pattern read as the view's
// type: 200 as `sbyte` is -56, and 1.5f as `int` is 0x3FC00000.
public class TestReinterpretSameWidthReadFromNamedCell
{
    private sealed class Box { public int V; }

    private struct Elem { public byte U8; public float F32; public Box Payload; }

    [InlineArray(2)]
    private struct Buffer
    {
        private Elem _item;
    }

    public static int Main(string[] argv)
    {
        Buffer b = default;
        b[1] = new Elem { U8 = 200, F32 = 1.5f, Payload = new Box { V = 60 } };
        if (Unsafe.As<byte, sbyte>(ref b[1].U8) != -56) return 1;
        if (Unsafe.As<float, int>(ref b[1].F32) != 0x3FC00000) return 2;
        if (b[1].Payload.V != 60) return 3;
        return 0;
    }
}
