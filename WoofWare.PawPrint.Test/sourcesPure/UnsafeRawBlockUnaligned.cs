using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestUnsafeRawBlockUnaligned
{
    [StructLayout(LayoutKind.Sequential, Size = 16)]
    struct Bytes16
    {
    }

    [StructLayout(LayoutKind.Sequential, Size = 64)]
    struct Bytes64
    {
    }

    static int CheckCopy(byte[] src, int srcOffset, byte[] dest, int destOffset, int length, int failureBase)
    {
        for (int i = 0; i < dest.Length; i++)
        {
            byte expected = 0xCC;
            if (i >= destOffset && i < destOffset + length)
            {
                expected = src[srcOffset + i - destOffset];
            }

            if (dest[i] != expected)
            {
                return failureBase + i;
            }
        }

        return 0;
    }

    public static int TestBlock16()
    {
        byte[] src = new byte[32];
        byte[] dest = new byte[32];

        for (int i = 0; i < src.Length; i++)
        {
            src[i] = (byte)(0x20 + i);
            dest[i] = 0xCC;
        }

        Bytes16 block = Unsafe.ReadUnaligned<Bytes16>(ref src[3]);
        Unsafe.WriteUnaligned<Bytes16>(ref dest[5], block);

        return CheckCopy(src, 3, dest, 5, 16, 100);
    }

    public static int TestBlock64()
    {
        byte[] src = new byte[96];
        byte[] dest = new byte[96];

        for (int i = 0; i < src.Length; i++)
        {
            src[i] = (byte)(0x80 + i);
            dest[i] = 0xCC;
        }

        Bytes64 block = Unsafe.ReadUnaligned<Bytes64>(ref src[7]);
        Unsafe.WriteUnaligned<Bytes64>(ref dest[11], block);

        return CheckCopy(src, 7, dest, 11, 64, 200);
    }

    public static int Main(string[] argv)
    {
        int result = TestBlock16();
        if (result != 0)
        {
            return result;
        }

        result = TestBlock64();
        if (result != 0)
        {
            return result;
        }

        return 0;
    }
}
