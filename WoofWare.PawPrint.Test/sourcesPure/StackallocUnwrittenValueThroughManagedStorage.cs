using System;
using System.Runtime.CompilerServices;

// A value read from a slot of a `stackalloc` buffer nothing wrote, carried through managed storage
// — an array element, a static field, an object's field, a field of a struct in an array — and
// read back through a span, copied array to array, and finally overwritten everywhere. Nothing
// here depends on the value's content, so the exit code is the same whatever the stack held.

[module: SkipLocalsInit]

class Holder
{
    public int Field;
}

struct Wrapped
{
    public int Inner;
    public int Tag;
}

unsafe class Program
{
    static int Static;

    static int Main(string[] args)
    {
        int* buffer = stackalloc int[2];
        int unwritten = buffer[0];

        int[] array = new int[3];
        array[1] = unwritten;
        Static = unwritten;
        var holder = new Holder { Field = unwritten };

        Wrapped[] structs = new Wrapped[2];
        structs[0].Inner = unwritten;
        structs[0].Tag = 7;

        // Read back through a span over the array: a byref to the element, not the array opcode.
        int viaSpan = array.AsSpan()[1];
        viaSpan = 1;

        // Copied element by element, the undefined one included.
        int[] copy = new int[3];
        array.AsSpan().CopyTo(copy);
        Wrapped[] structCopy = (Wrapped[])structs.Clone();

        array[1] = 2;
        copy[1] = 3;
        Static = 4;
        holder.Field = 5;
        structs[0].Inner = 6;
        structCopy[0].Inner = 8;

        return viaSpan + array[1] + copy[1] + Static + holder.Field + structs[0].Inner + structCopy[0].Inner
            + structCopy[0].Tag == 1 + 2 + 3 + 4 + 5 + 6 + 8 + 7 ? 0 : 1;
    }
}
