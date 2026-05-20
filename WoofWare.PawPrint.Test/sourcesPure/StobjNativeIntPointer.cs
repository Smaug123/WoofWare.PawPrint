struct Pair
{
    public int A;
    public int B;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        Pair target = new Pair
        {
            A = 5,
            B = 6,
        };

        Pair replacement = new Pair
        {
            A = 50,
            B = 60,
        };

        Pair* targetPtr = &target;
        nint rawAddress = (nint)targetPtr;
        *(Pair*)rawAddress = replacement;

        return target.A == 50 && target.B == 60 ? 0 : 1;
    }
}
