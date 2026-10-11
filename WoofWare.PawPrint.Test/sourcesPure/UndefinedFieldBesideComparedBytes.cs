using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A struct copied out of `stackalloc` memory in which only its first field was written, so its
// second field holds bytes nothing wrote, compared a byte range at a time with `SequenceEqual`
// over only the first field's bytes. The comparison reads only bytes that were written, so the
// exit code is the same whatever the stack held.

[module: SkipLocalsInit]

struct Pair
{
    public int Written;
    public int Unwritten;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        Pair* source = stackalloc Pair[1];
        source->Written = 42;
        // `source->Unwritten` is never written.

        Pair local = *source;
        ReadOnlySpan<byte> written =
            MemoryMarshal.AsBytes(MemoryMarshal.CreateReadOnlySpan(ref local, 1)).Slice(0, sizeof(int));

        byte[] expected = BitConverter.GetBytes(42);
        if (!written.SequenceEqual(expected)) return 1;

        byte[] different = BitConverter.GetBytes(43);
        if (written.SequenceEqual(different)) return 2;

        return 0;
    }
}
