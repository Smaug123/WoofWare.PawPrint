using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestUnsafeIsAddressLessThan
{
    // Irreflexivity: no address is strictly below itself, whatever the storage.
    public static int Test1()
    {
        int[] a = new int[4];
        if (Unsafe.IsAddressLessThan(ref a[0], ref a[0]))
            return 1;

        int local = 0;
        if (Unsafe.IsAddressLessThan(ref local, ref local))
            return 2;

        if (Unsafe.IsAddressLessThan(ref Unsafe.NullRef<int>(), ref Unsafe.NullRef<int>()))
            return 3;

        return 0;
    }

    // Array elements occupy ascending addresses.
    public static int Test2()
    {
        int[] a = new int[4];
        if (!Unsafe.IsAddressLessThan(ref a[0], ref a[1]))
            return 4;
        if (Unsafe.IsAddressLessThan(ref a[1], ref a[0]))
            return 5;
        if (!Unsafe.IsAddressLessThan(ref a[0], ref a[3]))
            return 6;
        if (Unsafe.IsAddressLessThan(ref a[3], ref a[2]))
            return 7;
        return 0;
    }

    // Trichotomy over every ordered pair of elements of one array: exactly one
    // of `left < right`, `right < left`, `AreSame` holds, and the order agrees
    // with the element indices.
    public static int Test3()
    {
        int[] a = new int[5];
        for (int i = 0; i < a.Length; i++)
        {
            for (int j = 0; j < a.Length; j++)
            {
                bool lt = Unsafe.IsAddressLessThan(ref a[i], ref a[j]);
                bool gt = Unsafe.IsAddressLessThan(ref a[j], ref a[i]);
                bool same = Unsafe.AreSame(ref a[i], ref a[j]);

                int trueCount = (lt ? 1 : 0) + (gt ? 1 : 0) + (same ? 1 : 0);
                if (trueCount != 1)
                    return 8;
                if (lt != (i < j))
                    return 9;
            }
        }

        return 0;
    }

    // The order agrees with the sign of `Unsafe.ByteOffset`, which is defined
    // as `addr(right) - addr(left)`.
    public static int Test4()
    {
        int[] a = new int[5];
        for (int i = 0; i < a.Length; i++)
        {
            for (int j = 0; j < a.Length; j++)
            {
                long delta = (long)Unsafe.ByteOffset(ref a[i], ref a[j]);
                if (Unsafe.IsAddressLessThan(ref a[i], ref a[j]) != (delta > 0L))
                    return 10;
            }
        }

        return 0;
    }

    // A null byref is address zero, and the comparison is unsigned, so it sits
    // strictly below every live address and nothing sits below it.
    public static int Test5()
    {
        int value = 42;

        if (!Unsafe.IsAddressLessThan(ref Unsafe.NullRef<int>(), ref value))
            return 11;
        if (Unsafe.IsAddressLessThan(ref value, ref Unsafe.NullRef<int>()))
            return 12;

        int[] a = new int[1];
        if (!Unsafe.IsAddressLessThan(ref Unsafe.NullRef<int>(), ref a[0]))
            return 13;
        if (Unsafe.IsAddressLessThan(ref a[0], ref Unsafe.NullRef<int>()))
            return 14;

        return 0;
    }

    // `Unsafe.IsAddressGreaterThanOrEqualTo` is ordinary managed code layered
    // directly on `IsAddressLessThan`, so it is the cheapest check that the
    // intrinsic answers correctly when reached from the BCL rather than from
    // the guest's own call site.
    public static int Test6()
    {
        int[] a = new int[3];

        if (Unsafe.IsAddressGreaterThanOrEqualTo(ref a[0], ref a[1]))
            return 15;
        if (!Unsafe.IsAddressGreaterThanOrEqualTo(ref a[1], ref a[0]))
            return 16;
        if (!Unsafe.IsAddressGreaterThanOrEqualTo(ref a[1], ref a[1]))
            return 17;

        return 0;
    }

    // Byrefs reached by pointer arithmetic and by reinterpretation order the
    // same way as the byrefs they were derived from: `Unsafe.As` preserves the
    // address, `Unsafe.Add` advances it by whole elements.
    public static int Test7()
    {
        int[] a = new int[4];
        ref int p = ref a[0];
        ref int q = ref Unsafe.Add(ref p, 2);

        if (!Unsafe.IsAddressLessThan(ref p, ref q))
            return 18;
        if (Unsafe.IsAddressLessThan(ref q, ref p))
            return 19;

        ref uint u = ref Unsafe.As<int, uint>(ref p);
        if (Unsafe.IsAddressLessThan(ref u, ref Unsafe.As<int, uint>(ref p)))
            return 20;
        if (Unsafe.IsAddressLessThan(ref Unsafe.As<int, uint>(ref p), ref u))
            return 21;
        if (!Unsafe.IsAddressLessThan(ref u, ref Unsafe.As<int, uint>(ref q)))
            return 22;

        // A byte view of a later element is still above a byte view of an
        // earlier one.
        ref byte firstByte = ref Unsafe.As<int, byte>(ref p);
        ref byte laterByte = ref Unsafe.Add(ref firstByte, 5);
        if (!Unsafe.IsAddressLessThan(ref firstByte, ref laterByte))
            return 23;
        if (Unsafe.IsAddressLessThan(ref laterByte, ref firstByte))
            return 24;

        return 0;
    }

    // Characters of one string occupy ascending addresses. This is the shape
    // BCL string scanners use: a byref to the start of the character data,
    // walked forward and compared against the end.
    public static int Test8()
    {
        string s = "abcd";
        ReadOnlySpan<char> span = s;
        ref char first = ref MemoryMarshal.GetReference(span);
        ref char third = ref Unsafe.Add(ref first, 2);

        if (!Unsafe.IsAddressLessThan(ref first, ref third))
            return 25;
        if (Unsafe.IsAddressLessThan(ref third, ref first))
            return 26;
        if (Unsafe.IsAddressLessThan(ref first, ref first))
            return 27;

        return 0;
    }

    // The end-sentinel loop shape: walk a byref forward one element at a time,
    // comparing against a byref one past the last element. The loop must run
    // exactly `Length` times.
    public static int Test9()
    {
        int[] a = { 10, 20, 30, 40 };
        ref int current = ref a[0];
        ref int end = ref Unsafe.Add(ref a[0], a.Length);

        int visited = 0;
        int sum = 0;
        while (Unsafe.IsAddressLessThan(ref current, ref end))
        {
            sum += current;
            visited++;
            current = ref Unsafe.Add(ref current, 1);
        }

        if (visited != 4)
            return 28;
        if (sum != 100)
            return 29;

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
        r = Test4();
        if (r != 0) return r;
        r = Test5();
        if (r != 0) return r;
        r = Test6();
        if (r != 0) return r;
        r = Test7();
        if (r != 0) return r;
        r = Test8();
        if (r != 0) return r;
        r = Test9();
        if (r != 0) return r;
        return 0;
    }
}
