using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

/// <summary>
/// A managed pointer of the form `[Field; ReinterpretAs byte-view; ByteOffset]` may
/// address bytes outside the immediate field cell. The interpreter must hoist past the
/// trailing Field projection so the read/write resolves against the enclosing parent
/// cell that is large enough to contain the byte range.
/// </summary>
public class ByrefFieldByteViewCrossing
{
    [StructLayout(LayoutKind.Sequential)]
    private struct Pair
    {
        public int A;
        public int B;
    }

    private class Holder
    {
        public Pair P;
    }

    // Read across a field boundary on a heap-rooted struct field: starting from a
    // byte view at field A, stepping forward by sizeof(int) lands at the start of
    // field B. The 4-byte read does not fit in A's cell and must hoist past the
    // trailing Field projection to read from the enclosing Pair cell.
    private static int HeapFieldByteViewRead()
    {
        Holder h = new Holder();
        h.P.A = 0x11223344;
        h.P.B = unchecked((int)0xDDCCBBAA);
        ref byte b = ref Unsafe.As<int, byte>(ref h.P.A);
        ref byte bAtB = ref Unsafe.Add(ref b, 4);
        int read = Unsafe.ReadUnaligned<int>(ref bAtB);
        if (read != unchecked((int)0xDDCCBBAA))
            return 1;
        return 0;
    }

    // Symmetric write path: writing 4 bytes at the boundary must update field B
    // through the hoisted prefix without disturbing field A.
    private static int HeapFieldByteViewWrite()
    {
        Holder h = new Holder();
        h.P.A = 0x11223344;
        h.P.B = 0;
        ref byte b = ref Unsafe.As<int, byte>(ref h.P.A);
        ref byte bAtB = ref Unsafe.Add(ref b, 4);
        Unsafe.WriteUnaligned<int>(ref bAtB, unchecked((int)0xDDCCBBAA));
        if (h.P.A != 0x11223344)
            return 2;
        if (h.P.B != unchecked((int)0xDDCCBBAA))
            return 3;
        return 0;
    }

    // Straddling read: bytes 2..5 from field A's start cover the upper half of A
    // and the lower half of B. The read cannot fit in A's cell at any offset and
    // must hoist to the parent Pair cell.
    private static int HeapFieldByteViewStraddleRead()
    {
        Holder h = new Holder();
        h.P.A = 0x44332211;
        h.P.B = unchecked((int)0x88776655);
        ref byte b = ref Unsafe.As<int, byte>(ref h.P.A);
        ref byte bAt2 = ref Unsafe.Add(ref b, 2);
        int v = Unsafe.ReadUnaligned<int>(ref bAt2);
        // Little-endian bytes at offsets 2..5 of the Pair are 0x33, 0x44, 0x55, 0x66.
        if (v != 0x66554433)
            return 4;
        return 0;
    }

    // Straddling write across the field boundary: bytes 2..5 of the Pair are zeroed,
    // leaving the low half of A and the high half of B intact.
    private static int HeapFieldByteViewStraddleWrite()
    {
        Holder h = new Holder();
        h.P.A = unchecked((int)0xFFFFFFFF);
        h.P.B = unchecked((int)0xFFFFFFFF);
        ref byte b = ref Unsafe.As<int, byte>(ref h.P.A);
        ref byte bAt2 = ref Unsafe.Add(ref b, 2);
        Unsafe.WriteUnaligned<int>(ref bAt2, 0x00000000);
        if (h.P.A != 0x0000FFFF)
            return 5;
        if (h.P.B != unchecked((int)0xFFFF0000))
            return 6;
        return 0;
    }

    // Same shape on a stack-resident struct root: ensure local-rooted byrefs
    // exercise the hoisting path identically to heap-rooted ones.
    private static int LocalFieldByteViewRead()
    {
        Pair s;
        s.A = 0x11223344;
        s.B = unchecked((int)0xDDCCBBAA);
        ref byte b = ref Unsafe.As<int, byte>(ref s.A);
        ref byte bAtB = ref Unsafe.Add(ref b, 4);
        int read = Unsafe.ReadUnaligned<int>(ref bAtB);
        if (read != unchecked((int)0xDDCCBBAA))
            return 7;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r;
        r = HeapFieldByteViewRead();
        if (r != 0) return r;
        r = HeapFieldByteViewWrite();
        if (r != 0) return r;
        r = HeapFieldByteViewStraddleRead();
        if (r != 0) return r;
        r = HeapFieldByteViewStraddleWrite();
        if (r != 0) return r;
        r = LocalFieldByteViewRead();
        if (r != 0) return r;
        return 0;
    }
}
