using System;
using System.Buffers;
using System.Text.Unicode;

// `Utf8.FromUtf16` measures how far it got by subtracting pointers it took with `fixed` from
// pointers it advanced, so its `charsRead` and `bytesWritten` are the distance between a byte
// cursor and a plain array-element byref. The destination span starts partway into its array,
// so neither operand is at index zero.
//
// Returns 0 on success, or the number of the first check that failed.
public class Program
{
    public static int Main (string[] args)
    {
        char[] src = { 'a', 'b' };
        byte[] dst = new byte[16];

        OperationStatus status = Utf8.FromUtf16 (src, new Span<byte> (dst, 13, 3), out int charsRead, out int bytesWritten);

        if (status != OperationStatus.Done)
            return 1;
        if (charsRead != 2)
            return 2;
        if (bytesWritten != 2)
            return 3;
        if (dst[13] != (byte) 'a' || dst[14] != (byte) 'b')
            return 4;
        if (dst[12] != 0 || dst[15] != 0)
            return 5;

        // A destination too short for the input stops partway, and says so.
        char[] longer = { 'x', 'y', 'z', 'w' };
        byte[] small = new byte[8];

        status = Utf8.FromUtf16 (longer, new Span<byte> (small, 6, 2), out charsRead, out bytesWritten);

        if (status != OperationStatus.DestinationTooSmall)
            return 6;
        if (charsRead != 2)
            return 7;
        if (bytesWritten != 2)
            return 8;
        if (small[6] != (byte) 'x' || small[7] != (byte) 'y')
            return 9;

        return 0;
    }
}
