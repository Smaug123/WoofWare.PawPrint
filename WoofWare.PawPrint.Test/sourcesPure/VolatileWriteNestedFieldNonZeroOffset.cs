using System.Runtime.CompilerServices;
using System.Threading;

public class TestVolatileWriteNestedFieldNonZeroOffset
{
    private struct Inner
    {
        public int X;
        public int Y;
    }

    private struct Outer
    {
        public Inner I;
    }

    public static int Main(string[] argv)
    {
        int[] arr = new int[2];
        ref Outer view = ref Unsafe.As<int, Outer>(ref arr[0]);
        Volatile.Write(ref view.I.Y, 456);
        return arr[1] == 456 ? 0 : 1;
    }
}
