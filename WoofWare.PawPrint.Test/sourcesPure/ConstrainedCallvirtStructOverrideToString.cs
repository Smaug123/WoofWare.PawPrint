public struct CustomText
{
    public override string ToString()
    {
        return "custom";
    }
}

public class Program
{
    private static string Describe<T>(T value) where T : struct
    {
        return value.ToString();
    }

    public static int Main(string[] args)
    {
        return Describe(new CustomText()) == "custom" ? 0 : 1;
    }
}
