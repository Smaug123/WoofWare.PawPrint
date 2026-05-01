using System.Runtime.InteropServices;

public class FixedBufferPointerArithmetic
{
    [StructLayout(LayoutKind.Sequential)]
    unsafe struct ByteBufferStruct
    {
        public int Header;
        public fixed byte Buffer[64];
        public int Footer;
    }

    [StructLayout(LayoutKind.Sequential)]
    unsafe struct NestedFixedStruct
    {
        public fixed int IntArray[4];
        public fixed double DoubleArray[2];
    }

    public static unsafe int Main(string[] argv)
    {
        var byteBuffer = new ByteBufferStruct();
        byteBuffer.Header = 0x12345678;
        byteBuffer.Footer = unchecked((int)0xDEADBEEF);

        for (int i = 0; i < 64; i++)
        {
            byteBuffer.Buffer[i] = (byte)(i + 1);
        }

        if (byteBuffer.Header != 0x12345678) return 1;
        if (byteBuffer.Footer != unchecked((int)0xDEADBEEF)) return 2;
        if (byteBuffer.Buffer[0] != 1) return 3;
        if (byteBuffer.Buffer[63] != 64) return 4;

        byte* bytePtr = byteBuffer.Buffer;
        bytePtr[17] = 200;
        if (byteBuffer.Buffer[17] != 200) return 5;

        var nested = new NestedFixedStruct();
        nested.IntArray[0] = 100;
        nested.IntArray[3] = 400;
        nested.DoubleArray[0] = 1.5;
        nested.DoubleArray[1] = 2.5;

        if (nested.IntArray[0] != 100) return 6;
        if (nested.IntArray[3] != 400) return 7;
        if (nested.DoubleArray[0] != 1.5) return 8;
        if (nested.DoubleArray[1] != 2.5) return 9;

        return 0;
    }
}
