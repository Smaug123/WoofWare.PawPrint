using System;
using System.Runtime.CompilerServices;

namespace UnsafeIsAddressLessThanCrossesArrayElementTest
{
    struct Pair
    {
        public int X;
        public int Y;
    }

    // Ordering byrefs into one array by element index is only right while each byref stays
    // inside the element its root names. A byte cursor that follows a field selection is
    // bounded relative to the field, not the element, so it can reach the next element:
    // `a[0].Y` viewed as bytes and advanced 4 is `a[1]`, and neither is below the other.
    //
    // `Unsafe.IsAddressLessThan` is `clt.un` on two byrefs; `IsAddressGreaterThanOrEqualTo`
    // is its negation, spelled out in IL. Nothing here reaches `cgt.un`: `IsAddressGreaterThan`
    // is a runtime intrinsic PawPrint does not yet provide, and C# emits `cgt.un` on byrefs
    // through nothing else.
    class Program
    {
        static ref byte BytesFromY(ref Pair p, int displacement)
        {
            return ref Unsafe.AddByteOffset(ref Unsafe.As<int, byte>(ref p.Y), (nint)displacement);
        }

        // The crossing itself: one address reached from two elements.
        static int Test1()
        {
            Pair[] a = new Pair[3];
            ref byte fromElement0 = ref BytesFromY(ref a[0], 4);
            ref byte element1 = ref Unsafe.As<Pair, byte>(ref a[1]);

            if (Unsafe.IsAddressLessThan(ref fromElement0, ref element1))
                return 1;
            if (Unsafe.IsAddressLessThan(ref element1, ref fromElement0))
                return 2;
            if (!Unsafe.IsAddressGreaterThanOrEqualTo(ref fromElement0, ref element1))
                return 3;
            if (!Unsafe.IsAddressGreaterThanOrEqualTo(ref element1, ref fromElement0))
                return 4;

            return 0;
        }

        // Short of the crossing, the cursor is still inside element 0 and below element 1.
        static int Test2()
        {
            Pair[] a = new Pair[3];
            ref byte insideElement0 = ref BytesFromY(ref a[0], 2);
            ref byte element1 = ref Unsafe.As<Pair, byte>(ref a[1]);

            if (!Unsafe.IsAddressLessThan(ref insideElement0, ref element1))
                return 5;
            if (Unsafe.IsAddressLessThan(ref element1, ref insideElement0))
                return 6;

            return 0;
        }

        // Past the crossing, the cursor is inside element 1: above its start and its first
        // field, below its second field and below element 2.
        static int Test3()
        {
            Pair[] a = new Pair[3];
            ref byte insideElement1 = ref BytesFromY(ref a[0], 6);
            ref byte element1 = ref Unsafe.As<Pair, byte>(ref a[1]);
            ref byte element1X = ref Unsafe.As<int, byte>(ref a[1].X);
            ref byte element1Y = ref Unsafe.As<int, byte>(ref a[1].Y);
            ref byte element2 = ref Unsafe.As<Pair, byte>(ref a[2]);

            if (Unsafe.IsAddressLessThan(ref insideElement1, ref element1))
                return 7;
            if (!Unsafe.IsAddressLessThan(ref element1, ref insideElement1))
                return 8;
            if (!Unsafe.IsAddressLessThan(ref element1X, ref insideElement1))
                return 9;
            if (!Unsafe.IsAddressLessThan(ref insideElement1, ref element1Y))
                return 10;
            if (!Unsafe.IsAddressLessThan(ref insideElement1, ref element2))
                return 11;
            if (Unsafe.IsAddressLessThan(ref element2, ref insideElement1))
                return 12;

            return 0;
        }

        // A whole element's worth of displacement lands exactly on the same field of the next
        // element, and that pair is ordered against the elements on either side of it.
        static int Test4()
        {
            Pair[] a = new Pair[3];
            ref byte fromElement0 = ref BytesFromY(ref a[0], 8);
            ref byte element1Y = ref Unsafe.As<int, byte>(ref a[1].Y);
            ref byte element1 = ref Unsafe.As<Pair, byte>(ref a[1]);
            ref byte element2 = ref Unsafe.As<Pair, byte>(ref a[2]);

            if (Unsafe.IsAddressLessThan(ref fromElement0, ref element1Y))
                return 13;
            if (Unsafe.IsAddressLessThan(ref element1Y, ref fromElement0))
                return 14;
            if (!Unsafe.IsAddressLessThan(ref element1, ref fromElement0))
                return 15;
            if (!Unsafe.IsAddressLessThan(ref fromElement0, ref element2))
                return 16;

            return 0;
        }

        // Two fields of one element, and a field of one element against a bare byref to the
        // next: index order alone settles the second, and field layout settles the first.
        static int Test5()
        {
            Pair[] a = new Pair[3];

            if (!Unsafe.IsAddressLessThan(ref a[1].X, ref a[1].Y))
                return 17;
            if (Unsafe.IsAddressLessThan(ref a[1].Y, ref a[1].X))
                return 18;
            if (!Unsafe.IsAddressLessThan(ref a[0].Y, ref a[1].X))
                return 19;
            if (Unsafe.IsAddressLessThan(ref a[1].X, ref a[0].Y))
                return 20;

            return 0;
        }

        // The sentinel-loop shape, but walking a byte cursor from inside the first element
        // in element-sized steps, so that every step lands on the same field of the next
        // element. The loop must visit exactly the elements that remain.
        static int Test6()
        {
            Pair[] a = new Pair[4];
            for (int i = 0; i < a.Length; i++)
            {
                a[i].X = 100 + i;
                a[i].Y = 200 + i;
            }

            ref byte cursor = ref BytesFromY(ref a[0], 0);
            ref byte end = ref Unsafe.As<int, byte>(ref a[a.Length - 1].Y);

            int visited = 0;
            int sum = 0;
            while (Unsafe.IsAddressLessThan(ref cursor, ref end))
            {
                sum += Unsafe.As<byte, int>(ref cursor);
                visited++;
                cursor = ref Unsafe.AddByteOffset(ref cursor, (nint)Unsafe.SizeOf<Pair>());
            }

            if (visited != 3)
                return 21;
            if (sum != 200 + 201 + 202)
                return 22;

            return 0;
        }

        static int Main(string[] args)
        {
            int r = Test1();
            if (r != 0) return r;
            r = Test2();
            if (r != 0) return r;
            r = Test3();
            if (r != 0) return r;
            r = Test4();
            if (r != 0) return r;
            r = Test5();
            if (r != 0) return r;
            r = Test6();
            if (r != 0) return r;
            return 0;
        }
    }
}
