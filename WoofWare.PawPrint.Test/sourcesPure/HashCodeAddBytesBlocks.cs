using System;

// `HashCode.AddBytes` has two regimes: below sixteen remaining bytes it consumes four bytes at a
// time, and at or above sixteen it flushes the queue and runs a sixteen-byte block loop
// (HashCode.cs:359). The block loop opens by rounding its end pointer down to a whole block with
// `Unsafe.Subtract(ref end, Unsafe.ByteOffset(ref pos, ref end) % 16)` — the `(ref T, IntPtr)`
// overload — so nothing at or above sixteen bytes could be hashed until that overload existed.
//
// The hash value itself mixes in a per-process seed, so only invariances are comparable across
// runtimes: equal inputs hash equal, and a single perturbed byte changes the hash. Both regimes
// and the boundary between them are covered by sweeping every length rather than sampling.
public class TestHashCodeAddBytesBlocks
{
    private struct TwoLongs
    {
        public long A;
        public long B;
    }

    private struct ThreeLongs
    {
        public long A;
        public long B;
        public long C;
    }

    private struct FourLongs
    {
        public long A;
        public long B;
        public long C;
        public long D;
    }

    private static int HashOf(byte[] bytes)
    {
        HashCode h = new HashCode();
        h.AddBytes(bytes);
        return h.ToHashCode();
    }

    // Every length from empty through two-and-a-half blocks: the four-byte regime, the block
    // regime, and every tail length a block loop can leave behind.
    public static int Test1()
    {
        for (int len = 0; len <= 40; len++)
        {
            byte[] p = new byte[len];
            byte[] q = new byte[len];
            for (int i = 0; i < len; i++)
            {
                p[i] = (byte)(i + 1);
                q[i] = (byte)(i + 1);
            }

            if (HashOf(p) != HashOf(q))
                return 1;

            // Perturbing any one byte must change the hash. The last byte is the one a block
            // loop that miscounted its final block would drop.
            for (int i = 0; i < len; i++)
            {
                byte saved = q[i];
                q[i] = (byte)(saved + 1);
                if (HashOf(p) == HashOf(q))
                    return 2;
                q[i] = saved;
            }

            // Length is part of the hash, so a shorter prefix must not collide with the whole.
            if (len > 0)
            {
                byte[] shorter = new byte[len - 1];
                Array.Copy(p, shorter, len - 1);
                if (HashOf(p) == HashOf(shorter))
                    return 3;
            }
        }
        return 0;
    }

    // The seed is per-process, not per-call, so repeating the same input inside one run must
    // give the same answer. A length in the block regime with a partial tail is the shape where
    // leftover state between calls would show up.
    public static int Test2()
    {
        byte[] p = new byte[37];
        for (int i = 0; i < p.Length; i++)
            p[i] = (byte)(i * 7 + 3);

        int first = HashOf(p);
        for (int trial = 0; trial < 4; trial++)
        {
            if (HashOf(p) != first)
                return 4;
        }
        return 0;
    }

    // `ValueType.GetHashCode` on a bit-comparable struct hashes the whole instance through
    // `AddBytes`, so a struct of sixteen bytes or more exercises the block loop. These are all
    // tightly packed, which is what keeps them on the whole-image path rather than the
    // first-field one.
    public static int Test3()
    {
        TwoLongs a = new TwoLongs { A = 1, B = 2 };
        TwoLongs b = new TwoLongs { A = 1, B = 2 };
        if (a.GetHashCode() != b.GetHashCode())
            return 5;
        if (a.GetHashCode() == new TwoLongs { A = 1, B = 3 }.GetHashCode())
            return 6;
        if (a.GetHashCode() == new TwoLongs { A = 2, B = 2 }.GetHashCode())
            return 7;

        ThreeLongs c = new ThreeLongs { A = 1, B = 2, C = 3 };
        if (c.GetHashCode() != new ThreeLongs { A = 1, B = 2, C = 3 }.GetHashCode())
            return 8;
        // The third field lands in the tail the block loop leaves behind.
        if (c.GetHashCode() == new ThreeLongs { A = 1, B = 2, C = 4 }.GetHashCode())
            return 9;

        FourLongs d = new FourLongs { A = 1, B = 2, C = 3, D = 4 };
        if (d.GetHashCode() != new FourLongs { A = 1, B = 2, C = 3, D = 4 }.GetHashCode())
            return 10;
        // Two whole blocks: the second one must actually run.
        if (d.GetHashCode() == new FourLongs { A = 1, B = 2, C = 3, D = 5 }.GetHashCode())
            return 11;
        if (d.GetHashCode() == new FourLongs { A = 1, B = 2, C = 4, D = 4 }.GetHashCode())
            return 12;
        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        return 0;
    }
}
