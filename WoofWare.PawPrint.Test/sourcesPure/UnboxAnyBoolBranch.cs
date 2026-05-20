public class Program
{
    public static int Main(string[] args)
    {
        object boxed = true;
        if ((bool)boxed)
        {
            return 0;
        }
        return 1;
    }
}
