struct Pair
{
    public int A;
    public int B;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[16];
        Pair* pair = (Pair*)bytes;

        *pair = new Pair
        {
            A = 1,
            B = 2,
        };

        bool firstFieldWritten = bytes[0] == 1 && bytes[1] == 0 && bytes[2] == 0 && bytes[3] == 0;
        bool secondFieldWritten = bytes[4] == 2 && bytes[5] == 0 && bytes[6] == 0 && bytes[7] == 0;

        return firstFieldWritten && secondFieldWritten ? 0 : 1;
    }
}
