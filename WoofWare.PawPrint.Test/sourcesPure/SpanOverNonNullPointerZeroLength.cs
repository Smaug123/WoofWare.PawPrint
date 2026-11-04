using System;

// A zero-length span over a pointer that is not null but points at nothing. The span's
// constructor never dereferences it, so any non-null bit pattern is allowed; CoreCLR's
// `BaseAssemblySpec::Init` relies on this, handing CoreLib `(PBYTE)1` for an assembly reference
// that has no public key.

public class SpanOverNonNullPointerZeroLength
{
    public static unsafe int Main(string[] argv)
    {
        var readOnly = new ReadOnlySpan<byte>((void*)1, 0);
        if (!readOnly.IsEmpty) return 1;

        byte[] copy = readOnly.ToArray();
        if (copy == null) return 2;
        if (copy.Length != 0) return 3;

        var writable = new Span<int>((void*)8, 0);
        if (writable.Length != 0) return 4;

        return 0;
    }
}
