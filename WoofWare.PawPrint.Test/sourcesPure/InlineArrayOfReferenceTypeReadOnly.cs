using System;
using System.Runtime.CompilerServices;

// Isolates the READ side of the InlineArrayOfReferenceType.cs shape: does *reading* element 0
// of a default-initialized (all-null) `[InlineArray(2)]` struct over a reference-typed element
// fail the same way the write does, or does it succeed? No write through the reinterpret
// happens here, so if this passes while InlineArrayOfReferenceType.cs fails, the defect is
// specifically write-side.
[InlineArray(2)]
public struct ScratchBufferReadOnly<T>
{
    private T _item;
}

public class TestInlineArrayOfReferenceTypeReadOnly
{
    public static int Main(string[] argv)
    {
        ScratchBufferReadOnly<string> buffer = default;
        string a = buffer[0];

        if (a != null) return 1;

        return 0;
    }
}
