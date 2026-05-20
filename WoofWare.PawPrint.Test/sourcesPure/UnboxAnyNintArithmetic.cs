public class Program
{
    public static int Main(string[] args)
    {
        object boxed = (nint)41;
        nint x = (nint)boxed + 1;
        return x == 42 ? 0 : 1;
    }
}
