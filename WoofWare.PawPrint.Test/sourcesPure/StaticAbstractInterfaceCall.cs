using System.Numerics;

public class Program
{
    private static bool IsNegative<T>(T value) where T : INumberBase<T>
    {
        return T.IsNegative(value);
    }

    public static int Main(string[] args)
    {
        if (IsNegative(3))
        {
            return 1;
        }

        if (!IsNegative(-1))
        {
            return 2;
        }

        return 0;
    }
}
