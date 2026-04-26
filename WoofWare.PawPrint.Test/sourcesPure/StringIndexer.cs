public class Program
{
    public static int Main(string[] args)
    {
        string value = "abc";

        if (value[0] != 'a')
        {
            return 1;
        }

        if (value[1] != 'b')
        {
            return 2;
        }

        if (value[2] != 'c')
        {
            return 3;
        }

        return 0;
    }
}
