using System.Threading;

class Program
{
    static int Main(string[] args)
    {
        int i = 10;
        if (Interlocked.Add(ref i, 5) != 15 || i != 15) return 1;
        if (Interlocked.Add(ref i, -20) != -5 || i != -5) return 2;

        int inc = 0;
        if (Interlocked.Increment(ref inc) != 1 || inc != 1) return 3;
        if (Interlocked.Decrement(ref inc) != 0 || inc != 0) return 4;

        int intWrap = int.MaxValue;
        if (Interlocked.Add(ref intWrap, 1) != int.MinValue || intWrap != int.MinValue) return 5;

        long l = 100L;
        if (Interlocked.Add(ref l, 23L) != 123L || l != 123L) return 6;
        if (Interlocked.Add(ref l, -200L) != -77L || l != -77L) return 7;

        long longWrap = long.MaxValue;
        if (Interlocked.Add(ref longWrap, 1L) != long.MinValue || longWrap != long.MinValue) return 8;

        uint u = uint.MaxValue;
        if (Interlocked.Add(ref u, 2U) != 1U || u != 1U) return 9;

        ulong ul = ulong.MaxValue;
        if (Interlocked.Add(ref ul, 2UL) != 1UL || ul != 1UL) return 10;

        return 0;
    }
}
