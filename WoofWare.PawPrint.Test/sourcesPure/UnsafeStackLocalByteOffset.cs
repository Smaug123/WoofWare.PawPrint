using System.Runtime.CompilerServices;

public class Program
{
    public static int Main(string[] args)
    {
        char delimiter = '.';
        ref byte first = ref Unsafe.As<char, byte>(ref delimiter);
        ref byte second = ref Unsafe.Add(ref first, 1);

        if ((long)Unsafe.ByteOffset(ref first, ref first) != 0)
        {
            return 1;
        }

        if ((long)Unsafe.ByteOffset(ref first, ref second) != 1)
        {
            return 2;
        }

        if ((long)Unsafe.ByteOffset(ref second, ref first) != -1)
        {
            return 3;
        }

        byte[] bytes = new byte[] { 1 };
        long localToArray = (long)Unsafe.ByteOffset(ref first, ref bytes[0]);
        long arrayToLocal = (long)Unsafe.ByteOffset(ref bytes[0], ref first);

        if (localToArray + arrayToLocal != 0)
        {
            return 4;
        }

        if (localToArray == 0)
        {
            return 5;
        }

        int argumentResult = CheckArgumentByteOffset('.');

        if (argumentResult != 0)
        {
            return argumentResult;
        }

        return 0;
    }

    private static int CheckArgumentByteOffset(char delimiter)
    {
        ref byte first = ref Unsafe.As<char, byte>(ref delimiter);
        ref byte second = ref Unsafe.Add(ref first, 1);

        if ((long)Unsafe.ByteOffset(ref first, ref first) != 0)
        {
            return 6;
        }

        if ((long)Unsafe.ByteOffset(ref first, ref second) != 1)
        {
            return 7;
        }

        if ((long)Unsafe.ByteOffset(ref second, ref first) != -1)
        {
            return 8;
        }

        byte[] bytes = new byte[] { 1 };
        long argumentToArray = (long)Unsafe.ByteOffset(ref first, ref bytes[0]);
        long arrayToArgument = (long)Unsafe.ByteOffset(ref bytes[0], ref first);

        if (argumentToArray + arrayToArgument != 0)
        {
            return 9;
        }

        if (argumentToArray == 0)
        {
            return 10;
        }

        return 0;
    }
}
