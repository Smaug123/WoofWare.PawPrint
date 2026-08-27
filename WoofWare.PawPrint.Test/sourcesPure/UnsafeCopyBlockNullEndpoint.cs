using System;
using System.Runtime.CompilerServices;

public unsafe class Program
{
    // `Unsafe.CopyBlock` and `Unsafe.CopyBlockUnaligned` are `[Intrinsic]`; the JIT replaces all
    // four overloads with `cpblk`, so their null-endpoint behaviour is the opcode's. A null on
    // either side with a nonzero count raises a NullReferenceException the guest can catch, and a
    // null with a zero count is legal and copies nothing.
    //
    // The other `Unsafe.CopyBlock` guests (`UnsafeCopyBlockProvenance.cs`,
    // `BulkMoveAcrossStructPadding.cs`) only ever pass live endpoints, so this is the one that
    // reaches the fault.

    // The control: an ordinary copy through the same overload, so a runtime that faulted on
    // everything could not pass this file.
    private static int CopiesWhenNeitherEndIsNull()
    {
        byte[] src = new byte[4] { 1, 2, 3, 4 };
        byte[] dst = new byte[4];
        Unsafe.CopyBlock(ref dst[0], ref src[0], 4);

        for (int i = 0; i < 4; i++)
        {
            if (dst[i] != i + 1) return 1;
        }

        return 0;
    }

    private static int NullByrefSource()
    {
        byte[] dst = new byte[4];
        try
        {
            Unsafe.CopyBlock(ref dst[0], ref Unsafe.NullRef<byte>(), 1);
            return 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    private static int NullByrefDestination()
    {
        byte[] src = new byte[4] { 1, 2, 3, 4 };
        try
        {
            Unsafe.CopyBlock(ref Unsafe.NullRef<byte>(), ref src[0], 1);
            return 3;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    private static int NullPointerSource()
    {
        byte[] dst = new byte[4];
        fixed (byte* d = dst)
        {
            try
            {
                Unsafe.CopyBlock(d, null, 1);
                return 4;
            }
            catch (NullReferenceException)
            {
                return 0;
            }
        }
    }

    private static int NullPointerDestination()
    {
        byte[] src = new byte[4] { 1, 2, 3, 4 };
        fixed (byte* s = src)
        {
            try
            {
                Unsafe.CopyBlock(null, s, 1);
                return 5;
            }
            catch (NullReferenceException)
            {
                return 0;
            }
        }
    }

    // The unaligned overload lowers to the same instruction, so it answers the same way.
    private static int UnalignedOverloadFaultsToo()
    {
        byte[] dst = new byte[4];
        try
        {
            Unsafe.CopyBlockUnaligned(ref dst[0], ref Unsafe.NullRef<byte>(), 1);
            return 6;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A count larger than the interpreter could ever copy still faults on the endpoint rather
    // than on the count: the fault is on the first byte, so it happens before any question of how
    // big the range is. The count is also above int.MaxValue, where reading the operand as signed
    // rather than unsigned would make it negative.
    private static int NullEndpointOutranksAnImpossibleCount()
    {
        byte[] src = new byte[4] { 1, 2, 3, 4 };
        try
        {
            Unsafe.CopyBlock(ref Unsafe.NullRef<byte>(), ref src[0], 0x80000000u);
            return 9;
        }
        catch (NullReferenceException)
        {
            // and on the source side too
        }

        byte[] dst = new byte[4];
        try
        {
            Unsafe.CopyBlock(ref dst[0], ref Unsafe.NullRef<byte>(), 0x80000000u);
            return 10;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A null source wins over a destination that is not a real address at all. A copy reads
    // before it writes, so the read from null faults first and the destination is never touched.
    // Measured on real .NET: this raises a catchable NullReferenceException, while the mirror
    // image (null destination, unmapped source) instead dies with an uncatchable
    // AccessViolationException — which is why that direction is not a row here, and why the
    // source endpoint has to be the one examined first.
    private static unsafe int NullSourceBeatsAnUnmappedDestination()
    {
        try
        {
            Unsafe.CopyBlock((void*)0x100000000UL, null, 1);
            return 11;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A zero count must not dereference either endpoint, so none of these faults, and the
    // destination is left alone.
    private static int NullWithZeroCountIsLegal()
    {
        byte[] arr = new byte[2] { 7, 8 };
        try
        {
            Unsafe.CopyBlock(ref Unsafe.NullRef<byte>(), ref Unsafe.NullRef<byte>(), 0);
            Unsafe.CopyBlock(ref arr[0], ref Unsafe.NullRef<byte>(), 0);
            Unsafe.CopyBlock(ref Unsafe.NullRef<byte>(), ref arr[0], 0);
            Unsafe.CopyBlock(null, null, 0);
        }
        catch (NullReferenceException)
        {
            return 7;
        }

        if (arr[0] != 7 || arr[1] != 8) return 8;
        return 0;
    }

    public static int Main(string[] args)
    {
        int r = CopiesWhenNeitherEndIsNull();
        if (r != 0) return r;
        r = NullByrefSource();
        if (r != 0) return r;
        r = NullByrefDestination();
        if (r != 0) return r;
        r = NullPointerSource();
        if (r != 0) return r;
        r = NullPointerDestination();
        if (r != 0) return r;
        r = UnalignedOverloadFaultsToo();
        if (r != 0) return r;
        r = NullEndpointOutranksAnImpossibleCount();
        if (r != 0) return r;
        r = NullSourceBeatsAnUnmappedDestination();
        if (r != 0) return r;
        r = NullWithZeroCountIsLegal();
        if (r != 0) return r;
        return 0;
    }
}
