using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class ValueTypeByteViewRegression
{
    [StructLayout(LayoutKind.Sequential)]
    struct Pair
    {
        public int A;
        public int B;
    }

    static unsafe int TestArrayOfStructByteView()
    {
        Pair[] pairs = new Pair[3];
        pairs[0].A = unchecked((int)0xAAAABBBB);
        pairs[0].B = unchecked((int)0xCCCCDDDD);
        pairs[1].A = 0x11112222;
        pairs[1].B = 0x33334444;
        pairs[2].A = 0x55556666;
        pairs[2].B = 0x77778888;

        ref byte first = ref Unsafe.As<Pair, byte>(ref pairs[0]);
        ref byte straddlingPair0Fields = ref Unsafe.Add(ref first, 2);
        Unsafe.WriteUnaligned<int>(ref straddlingPair0Fields, 0x11223344);

        if (pairs[0].A != 0x3344BBBB) return 1;
        if (pairs[0].B != unchecked((int)0xCCCC1122)) return 2;
        if (pairs[1].A != 0x11112222) return 3;
        if (pairs[1].B != 0x33334444) return 4;
        if (pairs[2].A != 0x55556666) return 5;
        if (pairs[2].B != 0x77778888) return 6;

        ref byte pair1BByte1 = ref Unsafe.Add(ref first, sizeof(Pair) + 5);
        Unsafe.WriteUnaligned<short>(ref pair1BByte1, 0x7F6E);

        if (pairs[0].A != 0x3344BBBB) return 7;
        if (pairs[0].B != unchecked((int)0xCCCC1122)) return 8;
        if (pairs[1].A != 0x11112222) return 9;
        if (pairs[1].B != 0x337F6E44) return 10;
        if (pairs[2].A != 0x55556666) return 11;
        if (pairs[2].B != 0x77778888) return 12;

        ref byte pair0ToPair1Boundary = ref Unsafe.Add(ref first, sizeof(Pair) - 2);
        Unsafe.WriteUnaligned<int>(ref pair0ToPair1Boundary, 0x55667788);

        if (pairs[0].A != 0x3344BBBB) return 13;
        if (pairs[0].B != 0x77881122) return 14;
        if (pairs[1].A != 0x11115566) return 15;
        if (pairs[1].B != 0x337F6E44) return 16;
        if (pairs[2].A != 0x55556666) return 17;
        if (pairs[2].B != 0x77778888) return 18;

        ref byte pair0BStart = ref Unsafe.Add(ref first, sizeof(int));
        long pair0BAndPair1A = Unsafe.ReadUnaligned<long>(ref pair0BStart);
        if (pair0BAndPair1A != 0x1111556677881122L) return 19;

        ref byte pair2Bytes = ref Unsafe.Add(ref first, 2 * sizeof(Pair));
        int pair2A = Unsafe.ReadUnaligned<int>(ref pair2Bytes);
        if (pair2A != 0x55556666) return 20;

        ref Pair pair1ViaBytes = ref Unsafe.As<byte, Pair>(ref Unsafe.Add(ref first, sizeof(Pair)));
        if (!Unsafe.AreSame(ref pairs[1], ref pair1ViaBytes)) return 21;

        return 0;
    }

    public static unsafe int Main(string[] argv)
    {
        return TestArrayOfStructByteView();
    }
}
