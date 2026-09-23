using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Reading a cell of a pointer-element array through a byte view of that array's data.
//
// `MemoryMarshal.GetArrayDataReference(Array)` hands back a `ref byte`, which PawPrint
// anchors as a byte-stride cursor over the array. A pointer cell has no byte image, so the
// only read such a cursor can serve is one covering exactly one cell at the width of a native
// int, and that read must hand over the pointer the cell holds -- the same value a plain read
// of the cell would give -- so that dereferencing it reaches the original pointee.
// `TestPointerArrayCellByteView.fs` covers the reads which would need the cell's bytes, and
// which PawPrint therefore refuses.
//
// Returns 0 on success, or the number of the first check that failed.
public class Program
{
    public static unsafe int Main (string[] args)
    {
        int first = 11;
        int second = 22;

        int*[] cells = new int*[2];
        cells[0] = &first;
        cells[1] = &second;

        ref byte data = ref MemoryMarshal.GetArrayDataReference ((Array) cells);

        // 1: the first cell, at offset zero of the data.
        IntPtr got = Unsafe.ReadUnaligned<IntPtr> (ref data);

        if (*(int*) got != 11)
            return 1;

        // 2: the second cell, one whole stride along.
        ref byte next = ref Unsafe.Add (ref data, sizeof (IntPtr));
        IntPtr alsoGot = Unsafe.ReadUnaligned<IntPtr> (ref next);

        if (*(int*) alsoGot != 22)
            return 2;

        return 0;
    }
}
