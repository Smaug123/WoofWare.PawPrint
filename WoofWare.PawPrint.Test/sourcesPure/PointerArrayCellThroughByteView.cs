using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Reading a cell of a pointer-element array through a byte view of that array's data.
//
// `MemoryMarshal.GetArrayDataReference(Array)` hands back a `ref byte`, which PawPrint
// anchors as a byte-stride cursor unconditionally. Reading a whole cell back through it
// then has to notice that the cursor sits exactly on a cell boundary and hand over the
// cell itself: `readArrayBytesAs`'s shape-matching short-circuit does that for numeric and
// reference cells, but has no arm pairing a stored `RuntimePointer` with the `NativeInt`
// template a `nint`-shaped read presents, so the read falls through to a byte-by-byte walk
// that a pointer cell has no image for. The write side already accepts that pairing, and so
// does a bare read of the same cell.
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
