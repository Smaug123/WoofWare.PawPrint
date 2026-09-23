using System;
using System.Buffers;
using System.Text.Unicode;

// `Utf8.FromUtf16` measures how far it got by subtracting pointers it took with `fixed` from
// pointers it advanced, so its `charsRead` and `bytesWritten` are the distance between a byte
// cursor and a plain array-element byref. The destination spans start partway into their arrays,
// so neither operand is at index zero, and the last case does the same to the source.
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

        // A source span that starts partway into its array too, and a two-byte character, so the
        // two counts differ (three chars, four bytes) rather than coinciding as they do for ASCII.
        char[] accented = { 'q', 'd', '\u00e9', 'y', 'q' };
        byte[] wide = new byte[12];

        status = Utf8.FromUtf16 (new ReadOnlySpan<char> (accented, 1, 3), new Span<byte> (wide, 2, 8), out charsRead, out bytesWritten);

        if (status != OperationStatus.Done)
            return 10;
        if (charsRead != 3)
            return 11;
        if (bytesWritten != 4)
            return 12;
        if (wide[2] != (byte) 'd' || wide[3] != 0xC3 || wide[4] != 0xA9 || wide[5] != (byte) 'y')
            return 13;
        if (wide[1] != 0 || wide[6] != 0)
            return 14;

        return 0;
    }
}
