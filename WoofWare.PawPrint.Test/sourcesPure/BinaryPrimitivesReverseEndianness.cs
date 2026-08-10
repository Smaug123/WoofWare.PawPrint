using System;
using System.Buffers.Binary;

public class BinaryPrimitivesReverseEndiannessTests
{
    public static int TestUInt16()
    {
        if (BinaryPrimitives.ReverseEndianness((ushort)0x1234) != 0x3412) return 1;
        if (BinaryPrimitives.ReverseEndianness((ushort)0x0000) != 0x0000) return 2;
        if (BinaryPrimitives.ReverseEndianness(ushort.MaxValue) != ushort.MaxValue) return 3;
        // The one-byte-set cases pin down which half moves where.
        if (BinaryPrimitives.ReverseEndianness((ushort)0x00FF) != 0xFF00) return 4;
        if (BinaryPrimitives.ReverseEndianness((ushort)0xFF00) != 0x00FF) return 5;
        // The BCL body is `(ushort)((value >> 8) + (value << 8))` and relies on the cast
        // discarding bits 16..31 of the widened `int` intermediate; a swap that failed to
        // truncate would return 0x00010100 here rather than 0x0100.
        if (BinaryPrimitives.ReverseEndianness((ushort)0x0001) != 0x0100) return 6;
        return 0;
    }

    public static int TestInt16()
    {
        if (BinaryPrimitives.ReverseEndianness((short)0x1234) != 0x3412) return 1;
        if (BinaryPrimitives.ReverseEndianness((short)0) != 0) return 2;
        // The signed overload casts through the unsigned one, so a value whose reversal
        // sets the top bit must come back negative rather than saturating or throwing.
        if (BinaryPrimitives.ReverseEndianness((short)0x00FF) != unchecked((short)0xFF00)) return 3;
        if (BinaryPrimitives.ReverseEndianness(unchecked((short)0xFF00)) != 0x00FF) return 4;
        if (BinaryPrimitives.ReverseEndianness((short)-1) != -1) return 5;
        if (BinaryPrimitives.ReverseEndianness(short.MinValue) != 0x0080) return 6;
        if (BinaryPrimitives.ReverseEndianness((short)0x0080) != short.MinValue) return 7;
        return 0;
    }

    public static int TestUInt32()
    {
        if (BinaryPrimitives.ReverseEndianness(0x12345678u) != 0x78563412u) return 1;
        if (BinaryPrimitives.ReverseEndianness(0u) != 0u) return 2;
        if (BinaryPrimitives.ReverseEndianness(uint.MaxValue) != uint.MaxValue) return 3;
        // Each single byte in isolation: the BCL body masks into two halves and rotates
        // each in a different direction, so a byte from each half is worth checking.
        if (BinaryPrimitives.ReverseEndianness(0x000000FFu) != 0xFF000000u) return 4;
        if (BinaryPrimitives.ReverseEndianness(0x0000FF00u) != 0x00FF0000u) return 5;
        if (BinaryPrimitives.ReverseEndianness(0x00FF0000u) != 0x0000FF00u) return 6;
        if (BinaryPrimitives.ReverseEndianness(0xFF000000u) != 0x000000FFu) return 7;
        return 0;
    }

    public static int TestInt32()
    {
        if (BinaryPrimitives.ReverseEndianness(0x12345678) != 0x78563412) return 1;
        if (BinaryPrimitives.ReverseEndianness(0) != 0) return 2;
        if (BinaryPrimitives.ReverseEndianness(-1) != -1) return 3;
        if (BinaryPrimitives.ReverseEndianness(int.MinValue) != 0x00000080) return 4;
        if (BinaryPrimitives.ReverseEndianness(0x00000080) != int.MinValue) return 5;
        if (BinaryPrimitives.ReverseEndianness(1) != 0x01000000) return 6;
        return 0;
    }

    public static int TestUInt64()
    {
        if (BinaryPrimitives.ReverseEndianness(0x0123456789ABCDEFul) != 0xEFCDAB8967452301ul) return 1;
        if (BinaryPrimitives.ReverseEndianness(0ul) != 0ul) return 2;
        if (BinaryPrimitives.ReverseEndianness(ulong.MaxValue) != ulong.MaxValue) return 3;
        // The 64-bit body decomposes into two 32-bit reversals and swaps the halves, so a
        // value confined to one half must end up entirely in the other.
        if (BinaryPrimitives.ReverseEndianness(0x00000000FFFFFFFFul) != 0xFFFFFFFF00000000ul) return 4;
        if (BinaryPrimitives.ReverseEndianness(0xFFFFFFFF00000000ul) != 0x00000000FFFFFFFFul) return 5;
        if (BinaryPrimitives.ReverseEndianness(1ul) != 0x0100000000000000ul) return 6;
        return 0;
    }

    public static int TestInt64()
    {
        if (BinaryPrimitives.ReverseEndianness(0x0123456789ABCDEFL) != unchecked((long)0xEFCDAB8967452301UL)) return 1;
        if (BinaryPrimitives.ReverseEndianness(0L) != 0L) return 2;
        if (BinaryPrimitives.ReverseEndianness(-1L) != -1L) return 3;
        if (BinaryPrimitives.ReverseEndianness(long.MinValue) != 0x0000000000000080L) return 4;
        if (BinaryPrimitives.ReverseEndianness(0x0000000000000080L) != long.MinValue) return 5;
        return 0;
    }

    public static int TestNonIntrinsicWrappers()
    {
        // sbyte/byte/nint/nuint are not [Intrinsic]; they are here because they delegate
        // to the overloads that are, and so would break alongside them. (The `char`
        // overload is `internal`, so a guest cannot reach it.)
        // Int128/UInt128 are excluded deliberately: they too delegate here, but pull in
        // 128-bit arithmetic that has nothing to do with byte swapping.
        if (BinaryPrimitives.ReverseEndianness((sbyte)-3) != -3) return 1;
        if (BinaryPrimitives.ReverseEndianness((byte)0xAB) != 0xAB) return 2;

        // nint/nuint forward to the 64-bit overload on a 64-bit process and the 32-bit one
        // otherwise, so derive the expectation from the pointer width rather than assuming.
        if (IntPtr.Size == 8)
        {
            if (BinaryPrimitives.ReverseEndianness((nuint)0x0123456789ABCDEFul) != (nuint)0xEFCDAB8967452301ul) return 3;
            if (BinaryPrimitives.ReverseEndianness((nint)1) != (nint)0x0100000000000000L) return 4;
        }
        else
        {
            if (BinaryPrimitives.ReverseEndianness((nuint)0x12345678u) != (nuint)0x78563412u) return 5;
            if (BinaryPrimitives.ReverseEndianness((nint)1) != (nint)0x01000000) return 6;
        }

        return 0;
    }

    public static int TestInvolution()
    {
        // Reversal is its own inverse at every width; a swap that dropped or duplicated a
        // byte would survive the fixed cases above only by coincidence, but cannot survive
        // this over a set of values with a single byte moving through every position.
        for (int shift = 0; shift < 16; shift += 8)
        {
            ushort v16 = (ushort)(0xA5u << shift);
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(v16)) != v16) return 1;
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(unchecked((short)v16))) != unchecked((short)v16)) return 2;
        }

        for (int shift = 0; shift < 32; shift += 8)
        {
            uint v32 = 0xA5u << shift;
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(v32)) != v32) return 3;
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(unchecked((int)v32))) != unchecked((int)v32)) return 4;
        }

        for (int shift = 0; shift < 64; shift += 8)
        {
            ulong v64 = 0xA5ul << shift;
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(v64)) != v64) return 5;
            if (BinaryPrimitives.ReverseEndianness(BinaryPrimitives.ReverseEndianness(unchecked((long)v64))) != unchecked((long)v64)) return 6;
        }

        return 0;
    }

    public static int TestBigEndianReadWrite()
    {
        // The motivating caller: on a little-endian host the big-endian accessors are
        // exactly a ReverseEndianness plus an unaligned write, and this is the shape
        // System.Reflection.Emit's ILGenerator uses to write a two-byte opcode.
        byte[] buffer = new byte[8];

        BinaryPrimitives.WriteUInt16BigEndian(buffer, 0xFE09);
        if (buffer[0] != 0xFE) return 1;
        if (buffer[1] != 0x09) return 2;
        if (BinaryPrimitives.ReadUInt16BigEndian(buffer) != 0xFE09) return 3;

        BinaryPrimitives.WriteInt16BigEndian(buffer, unchecked((short)0xFE09));
        if (buffer[0] != 0xFE) return 4;
        if (buffer[1] != 0x09) return 5;
        if (BinaryPrimitives.ReadInt16BigEndian(buffer) != unchecked((short)0xFE09)) return 6;

        BinaryPrimitives.WriteUInt32BigEndian(buffer, 0x11223344u);
        if (buffer[0] != 0x11) return 7;
        if (buffer[3] != 0x44) return 8;
        if (BinaryPrimitives.ReadUInt32BigEndian(buffer) != 0x11223344u) return 9;

        BinaryPrimitives.WriteUInt64BigEndian(buffer, 0x0123456789ABCDEFul);
        if (buffer[0] != 0x01) return 10;
        if (buffer[7] != 0xEF) return 11;
        if (BinaryPrimitives.ReadUInt64BigEndian(buffer) != 0x0123456789ABCDEFul) return 12;

        // The little-endian accessors are the identity on a little-endian host, so they
        // pin down that the big-endian ones above really are reversing something.
        BinaryPrimitives.WriteUInt16LittleEndian(buffer, 0xFE09);
        if (buffer[0] != (BitConverter.IsLittleEndian ? 0x09 : 0xFE)) return 13;
        if (buffer[1] != (BitConverter.IsLittleEndian ? 0xFE : 0x09)) return 14;

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        // Failure codes stay inside a single byte: a process exit code is truncated to its
        // low 8 bits, and a code congruent to 0 would be indistinguishable from success.
        int result;

        result = BinaryPrimitivesReverseEndiannessTests.TestUInt16();
        if (result != 0) return result;

        result = BinaryPrimitivesReverseEndiannessTests.TestInt16();
        if (result != 0) return 20 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestUInt32();
        if (result != 0) return 40 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestInt32();
        if (result != 0) return 60 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestUInt64();
        if (result != 0) return 80 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestInt64();
        if (result != 0) return 100 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestNonIntrinsicWrappers();
        if (result != 0) return 120 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestInvolution();
        if (result != 0) return 140 + result;

        result = BinaryPrimitivesReverseEndiannessTests.TestBigEndianReadWrite();
        if (result != 0) return 160 + result;

        return 0;
    }
}
