using System;
using System.Runtime.CompilerServices;

internal static class Program
{
    private static int Main()
    {
        byte[] first = new byte[16];
        byte[] second = new byte[16];

        // Spans over distinct heap objects never overlap, whichever way round
        // the allocator happened to place them. Overlaps lowers to unsigned
        // comparisons of Unsafe.ByteOffset against both a positive bound and a
        // negated (i.e. negative) one, so this exercises both signs.
        ReadOnlySpan<byte> a = first;
        ReadOnlySpan<byte> b = second;
        if (a.Overlaps(b))
        {
            return 1;
        }

        if (b.Overlaps(a))
        {
            return 2;
        }

        // The same comparisons spelled out directly, one per operand order and
        // direction. Two byte[16]s are distinct objects, each larger than 16
        // bytes including its header, so the byte offset between their first
        // elements has magnitude above 16 on every real layout; each check
        // below then has a single correct answer regardless of which object
        // the allocator placed first.
        nint offset = Unsafe.ByteOffset(ref first[0], ref second[0]);
        nuint negativeBound = unchecked((nuint)(-16));

        if ((nuint)offset > negativeBound)
        {
            return 3;
        }

        if (!(negativeBound > (nuint)offset))
        {
            return 4;
        }

        if (!((nuint)offset < negativeBound))
        {
            return 5;
        }

        if (negativeBound < (nuint)offset)
        {
            return 6;
        }

        return 0;
    }
}
