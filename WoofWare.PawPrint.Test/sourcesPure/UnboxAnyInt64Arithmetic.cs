public class Program
{
    public static int Main(string[] args)
    {
        object boxed = 41L;
        long x = (long)boxed + 1L;
        return x == 42L ? 0 : 1;
    }
}
