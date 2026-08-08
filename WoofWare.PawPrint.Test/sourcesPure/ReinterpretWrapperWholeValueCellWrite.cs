using System;
using System.Runtime.CompilerServices;

// Writing through a transparent single-field wrapper whose field is a *value type containing a
// reference*, rather than a bare reference.
//
// `Unsafe.As<Elem, Wrapper>(ref e).Value = ...` names the whole of `e`: the wrapper's only field
// starts at offset 0 and spans the wrapper, so the write replaces the storage outright. The
// storage is byte-unaddressable (it holds a reference), so the bytewise reinterpret path cannot
// serve this; the elision classifier has to, and its write handler has to accept the same kinds of
// value the classifier accepts as compatible.
public class TestReinterpretWrapperWholeValueCellWrite
{
    private sealed class Box { public int V; }

    private struct Elem { public byte Tag; public Box Payload; }

    private struct Wrapper { public Elem Value; }

    public static int Main(string[] argv)
    {
        Elem e = default;

        Unsafe.As<Elem, Wrapper>(ref e).Value = new Elem { Tag = 7, Payload = new Box { V = 70 } };

        if (e.Tag != 7) return 1;
        if (e.Payload == null) return 2;
        if (e.Payload.V != 70) return 3;

        // Overwrite through the same route, to check the write is a replacement rather than a merge.
        Unsafe.As<Elem, Wrapper>(ref e).Value = new Elem { Tag = 8, Payload = null };

        if (e.Tag != 8) return 4;
        if (e.Payload != null) return 5;

        // And read back through the wrapper.
        Elem viaWrapper = Unsafe.As<Elem, Wrapper>(ref e).Value;
        if (viaWrapper.Tag != 8 || viaWrapper.Payload != null) return 6;

        return 0;
    }
}
