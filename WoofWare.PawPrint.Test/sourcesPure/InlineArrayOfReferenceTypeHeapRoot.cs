using System;
using System.Runtime.CompilerServices;

// The same element-0 reference write as `InlineArrayOfReferenceType.cs`, but reached through a
// *heap* root rather than a local: `ldflda` on a class field, and `ldelema` on an array element.
//
// The byref shape is the same (`[ReinterpretAs string]` with no trailing projection over
// transparent single-field storage), but the write dispatcher picks its writer from the root, so
// a fix that only covers locals leaves these failing. Both roots are exercised here because they
// take different paths to the same writer.
[InlineArray(2)]
public struct ScratchBufferHeap<T>
{
    private T _item;

    // Ordinary `ldfld` of the single field: no `ReinterpretAs` involved.
    public T First => _item;
}

public class Holder
{
    public ScratchBufferHeap<string> Buffer;
}

public class TestInlineArrayOfReferenceTypeHeapRoot
{
    public static int Main(string[] argv)
    {
        // Root: a field of a heap-allocated class.
        Holder holder = new Holder();
        holder.Buffer[0] = "hello";
        if (holder.Buffer.First != "hello") return 1;

        // Root: an element of a heap-allocated array.
        ScratchBufferHeap<string>[] arr = new ScratchBufferHeap<string>[2];
        arr[1][0] = "world";
        if (arr[1].First != "world") return 2;
        if (arr[0].First != null) return 3;

        return 0;
    }
}
