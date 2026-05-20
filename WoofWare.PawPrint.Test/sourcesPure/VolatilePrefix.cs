class Program
{
    private static volatile int value;

    static int Main(string[] args)
    {
        value = 41;
        value++;

        return value == 42 ? 0 : 1;
    }
}
