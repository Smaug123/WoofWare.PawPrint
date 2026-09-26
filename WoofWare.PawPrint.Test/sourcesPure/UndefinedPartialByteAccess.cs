using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A value partly read from `stackalloc` memory nothing wrote, held in managed storage — a local,
// an array element, an object's field — and then read or written a byte at a time through a
// pointer or a reinterpreting byref. Only bytes that were written are ever read back, and every
// unwritten byte copied in is overwritten before the value it sits in is used, so the exit code is
// the same whatever the stack held.

[module: SkipLocalsInit]

class Holder
{
    public int Field;
}

struct Pair
{
    public int Partly;
    public int Whole;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* source = stackalloc byte[4];
        source[0] = 42;
        // Bytes 1-3 of `source` are never written.

        // A local holding a partly defined int: its defined low byte reads back.
        int local = *(int*)source;
        if (*(byte*)&local != 42) return 1;

        // Every byte of it overwritten through a byte pointer: it is then an ordinary int.
        int filled = *(int*)source;
        byte* q = (byte*)&filled;
        q[0] = 1;
        q[1] = 2;
        q[2] = 3;
        q[3] = 4;
        if (filled != 0x04030201) return 2;

        // An unwritten byte moved into the middle of a defined int, then overwritten again.
        int target = 0x11223344;
        *((byte*)&target + 1) = source[1];
        *((byte*)&target + 1) = 0x33;
        if (target != 0x11223344) return 3;

        // The same into a stackalloc'd int.
        int* cell = stackalloc int[1];
        *cell = 0x11223344;
        *((byte*)cell + 2) = source[2];
        *((byte*)cell + 2) = 0x22;
        if (*cell != 0x11223344) return 4;

        // An array element holding the partly defined int, read a byte at a time.
        int[] array = new int[2];
        array[0] = *(int*)source;
        if (MemoryMarshal.AsBytes(array.AsSpan())[0] != 42) return 5;

        // ... and overwritten a byte at a time through a pinned pointer.
        fixed (int* pinned = array)
        {
            byte* b = (byte*)pinned;
            b[1] = 0;
            b[2] = 0;
            b[3] = 0;
        }

        if (array[0] != 42) return 6;

        // An object's field, read through a reinterpreting byref.
        var holder = new Holder { Field = *(int*)source };
        if (Unsafe.As<int, byte>(ref holder.Field) != 42) return 7;

        // A boxed struct with a partly defined field, read a byte at a time through the box.
        Pair pair;
        pair.Partly = *(int*)source;
        pair.Whole = 5;
        object boxed = pair;
        ref byte boxedBytes = ref Unsafe.As<Pair, byte>(ref Unsafe.Unbox<Pair>(boxed));
        if (boxedBytes != 42) return 9;
        if (Unsafe.Add(ref boxedBytes, 4) != 5) return 10;

        // A byte-wise copy out of the local carries the unwritten bytes along; only the written
        // one is read back.
        int copy;
        Unsafe.CopyBlock(&copy, &local, 4);
        if (*(byte*)&copy != 42) return 8;

        return 0;
    }
}
