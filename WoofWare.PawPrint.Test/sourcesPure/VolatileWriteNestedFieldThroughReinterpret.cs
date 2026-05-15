using System.Runtime.CompilerServices;
using System.Threading;

public class TestVolatileWriteNestedFieldThroughReinterpret
{
    private struct Inner
    {
        public int X;
    }

    private struct Outer
    {
        public Inner I;
    }

    public static int Main(string[] argv)
    {
        int[] arr = new int[1];
        ref Outer view = ref Unsafe.As<int, Outer>(ref arr[0]);
        Volatile.Write(ref view.I.X, 123);
        return arr[0] == 123 ? 0 : 1;
    }
}
