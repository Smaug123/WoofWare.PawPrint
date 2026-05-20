public class MulOvfUn
{
    public static int Main(string[] args)
    {
        uint a = 1234u;
        uint b = 56u;
        uint result = checked(a * b);

        if (result != 69104u)
        {
            return 1;
        }

        return 0;
    }
}
