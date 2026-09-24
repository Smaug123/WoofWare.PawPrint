using System;
using System.Runtime.InteropServices;

public class MarshalSizeOfByValArrayTest
{
    // Each struct brackets its array with a byte on either side, so the size shows both the
    // array's alignment (the padding after Head) and its width (where Tail lands).

    // A primitive element keeps its own width whatever ArraySubType says:
    // 1 (Head) padded to 4, 4 * 4 (A), 1 (Tail), padded to 4 = 24, not the 8 an I1 element gives.
    [StructLayout(LayoutKind.Sequential)]
    struct IntAsI1
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 4, ArraySubType = UnmanagedType.I1)]
        public int[] A;
        public byte Tail;
    }

    // No ArraySubType at all: 1 padded to 8, 3 * 8, 1, padded to 8 = 40.
    [StructLayout(LayoutKind.Sequential)]
    struct LongWithoutSubType
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)]
        public long[] A;
        public byte Tail;
    }

    // A bool element is a four-byte BOOL: 1 padded to 4, 2 * 4, 1, padded to 4 = 16.
    [StructLayout(LayoutKind.Sequential)]
    struct BoolDefault
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)]
        public bool[] A;
        public byte Tail;
    }

    // ... unless ArraySubType asks for a one-byte one: 1 + 2 * 1 + 1 = 4.
    [StructLayout(LayoutKind.Sequential)]
    struct BoolAsU1
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2, ArraySubType = UnmanagedType.U1)]
        public bool[] A;
        public byte Tail;
    }

    // A char element follows the struct's CharSet when ArraySubType names no width:
    // 1 padded to 2, 3 * 2, 1, padded to 2 = 10.
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct UnicodeCharAsR8
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 3, ArraySubType = UnmanagedType.R8)]
        public char[] A;
        public byte Tail;
    }

    // A DateTime element is an eight-byte OLE Automation date: 1 padded to 8, 2 * 8, 1, padded to 8 = 32.
    [StructLayout(LayoutKind.Sequential)]
    struct Dates
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)]
        public DateTime[] A;
        public byte Tail;
    }

    // A string element has no four-byte integer form, so the struct cannot be marshalled.
    [StructLayout(LayoutKind.Sequential)]
    struct StringAsI4
    {
        public byte Head;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 2, ArraySubType = UnmanagedType.I4)]
        public string[] A;
        public byte Tail;
    }

    public static int Main(string[] argv)
    {
        if (Marshal.SizeOf(typeof(IntAsI1)) != 24) return 1;
        if (Marshal.SizeOf(typeof(LongWithoutSubType)) != 40) return 2;
        if (Marshal.SizeOf(typeof(BoolDefault)) != 16) return 3;
        if (Marshal.SizeOf(typeof(BoolAsU1)) != 4) return 4;
        if (Marshal.SizeOf(typeof(UnicodeCharAsR8)) != 10) return 5;
        if (Marshal.SizeOf(typeof(Dates)) != 32) return 6;

        try
        {
            Marshal.SizeOf(typeof(StringAsI4));
            return 7;
        }
        catch (ArgumentException)
        {
        }

        return 0;
    }
}
