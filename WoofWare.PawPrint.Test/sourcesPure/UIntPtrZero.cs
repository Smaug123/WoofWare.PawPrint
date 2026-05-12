namespace UIntPtrZero
{
    class Program
    {
        static int Main(string[] args)
        {
            System.UIntPtr zero = System.UIntPtr.Zero;
            if (zero != default(System.UIntPtr)) return 1;
            if (zero != (System.UIntPtr)0) return 2;
            if (zero.ToUInt64() != 0UL) return 3;
            return 0;
        }
    }
}
