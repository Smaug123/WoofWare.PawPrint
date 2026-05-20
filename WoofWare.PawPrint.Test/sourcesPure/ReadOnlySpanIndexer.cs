using System;

class Program
{
    private static ReadOnlySpan<uint> UInts => [10u, 20u, 30u, 40u];
    private static ReadOnlySpan<byte> Bytes => [1, 2, 3, 4];

    static int Main(string[] args)
    {
        ReadOnlySpan<uint> uints = UInts;
        if (uints[0] != 10u) return 1;
        if (uints[2] != 30u) return 2;

        ReadOnlySpan<byte> bytes = Bytes;
        if (bytes[3] != 4) return 3;

        return 0;
    }
}
