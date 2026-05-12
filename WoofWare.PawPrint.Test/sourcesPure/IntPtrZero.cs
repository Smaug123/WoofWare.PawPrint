namespace IntPtrZero
{
    class Program
    {
        static int Main(string[] args)
        {
            System.IntPtr zero = System.IntPtr.Zero;
            if (zero != default(System.IntPtr)) return 1;
            if (zero != (System.IntPtr)0) return 2;
            if (zero.ToInt64() != 0L) return 3;
            return 0;
        }
    }
}
