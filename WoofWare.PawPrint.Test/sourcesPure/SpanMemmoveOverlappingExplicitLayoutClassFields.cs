using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class Program
{
    // Under `[StructLayout(LayoutKind.Explicit)]` on a class, two distinct
    // fields can occupy overlapping byte ranges of one object. A
    // `Memmove`-style copy whose endpoints are byrefs to two such fields
    // therefore overlaps, and must copy backwards when the source starts
    // below the destination.
    //
    // PawPrint used to resolve each `ByrefRoot.HeapObjectField` to its own
    // storage container (`ByteStorageIdentity.HeapObjectField`), so this pair
    // read as "distinct storages, disjoint, copy forwards" — and the forward
    // loop re-read source bytes it had already overwritten. Measured before
    // the fix: check 7 below saw 0x0000000001010101 (the clobbered read,
    // reported as exit 8) where real .NET's backwards memmove leaves
    // 0x0000000002020202. One heap object is one storage; a field is a view
    // into it at its layout offset, which is what the resolution now says.
    [StructLayout(LayoutKind.Explicit)]
    class AliasClass
    {
        [FieldOffset(0)]
        public int A;

        [FieldOffset(0)]
        public int B;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct Inner
    {
        public long F0;
        public long F1;
    }

    [StructLayout(LayoutKind.Explicit)]
    struct OverlapStruct
    {
        [FieldOffset(0)]
        public Inner A;

        [FieldOffset(8)]
        public Inner B;
    }

    [StructLayout(LayoutKind.Explicit)]
    class OverlapClass
    {
        [FieldOffset(0)]
        public Inner A;

        [FieldOffset(8)]
        public Inner B;
    }

    // Copies 12 bytes from `ref src` to `ref dest`. The callers lay `dest`
    // out 8 bytes after `src`, so the ranges overlap by 4 bytes and Memmove
    // must copy backwards: a forwards copy reads its final 4 source bytes
    // after having overwritten them.
    static void OverlappingCopy(ref Inner src, ref Inner dest)
    {
        Span<byte> s = MemoryMarshal.CreateSpan(ref Unsafe.As<Inner, byte>(ref src), 12);
        Span<byte> d = MemoryMarshal.CreateSpan(ref Unsafe.As<Inner, byte>(ref dest), 12);
        s.CopyTo(d);
    }

    public static int Main(string[] args)
    {
        // Control: plain field aliasing on an explicit-layout class, no block
        // copy involved. This pins the premise that the heap object's storage
        // really does alias the two fields (one CliValueType with overlap
        // replay); if this ever regresses, the Memmove checks below stop
        // being about copy direction.
        AliasClass c = new AliasClass();
        c.A = 7;
        c.B = 9;
        if (c.A != 9)
        {
            return 1;
        }

        // Control: the same overlapping copy on an explicit-layout *struct*
        // local. Both byrefs resolve into the same stack local with honest
        // field offsets, so the direction decision was already correct here.
        OverlapStruct os = default;
        os.A.F0 = 0x0101010101010101L;
        os.A.F1 = 0x0202020202020202L;
        OverlappingCopy(ref os.A, ref os.B);
        // memmove(dest = base+8, src = base+0, len = 12):
        // bytes[8..20] = old bytes[0..12].
        if (os.A.F0 != 0x0101010101010101L)
        {
            return 2;
        }
        if (os.B.F0 != 0x0101010101010101L)
        {
            return 3;
        }
        if (os.B.F1 != 0x0000000002020202L)
        {
            return 4;
        }

        // The repro: the same copy where each endpoint is a byref to a field
        // of an explicit-layout *class*, i.e. two distinct
        // `ByrefRoot.HeapObjectField` roots on one object.
        OverlapClass oc = new OverlapClass();
        oc.A.F0 = 0x0101010101010101L;
        oc.A.F1 = 0x0202020202020202L;
        OverlappingCopy(ref oc.A, ref oc.B);
        if (oc.A.F0 != 0x0101010101010101L)
        {
            return 5;
        }
        if (oc.B.F0 != 0x0101010101010101L)
        {
            return 6;
        }
        if (oc.B.F1 != 0x0000000002020202L)
        {
            // Distinguish the forward-copy clobber (the last 4 source bytes
            // read after being overwritten) from any other wrong answer.
            return oc.B.F1 == 0x0000000001010101L ? 8 : 7;
        }

        return 0;
    }
}
