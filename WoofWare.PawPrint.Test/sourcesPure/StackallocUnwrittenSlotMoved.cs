using System.Runtime.CompilerServices;

// Every method here skips zero-initialising its `stackalloc`, as CoreLib's do, so the slots
// nothing writes hold whatever the stack held. Reading one is legal; what the program then does
// with it must not depend on its content. Each value read from an unwritten slot below is only
// moved — into a local, an argument, a return value, a struct field, another buffer — and then
// overwritten or dropped, which is the shape of the `LibraryImport` stub that marshals a
// `ReadOnlySpan<string>` into a stack buffer. The exit code depends only on bytes that were
// written, so it is the same on every runtime whatever the stack held.

[module: SkipLocalsInit]

struct Pair
{
    public long Unwritten;
    public int Written;
}

unsafe class Program
{
    static long PassThrough(long value) => value;

    static long Load(long* p) => *p;

    static int Main(string[] args)
    {
        long* buffer = stackalloc long[4];
        buffer[1] = 17;

        // Into a local, then overwritten before any use: the stub's exact shape.
        long slot = buffer[0];
        slot = buffer[1];
        if (slot != 17) return 1;

        // Through an argument and a return value, then dropped.
        long moved = PassThrough(Load(buffer + 2));
        moved = 3;

        // Into a struct field beside a defined one, and the struct copied.
        Pair pair;
        pair.Unwritten = buffer[3];
        pair.Written = 5;
        Pair copy = pair;
        if (copy.Written != 5) return 2;

        // Into another buffer, and read back only where that buffer was written afterwards.
        long* other = stackalloc long[2];
        other[0] = buffer[0];
        other[0] = 23;
        other[1] = copy.Unwritten;
        if (other[0] != 23) return 3;

        return (int)(moved + copy.Written + other[0] - 31);
    }
}
