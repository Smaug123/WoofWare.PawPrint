public class Program
{
    public enum Color
    {
        Red = 1,
        Green = 2,
        Blue = 42
    }

    public static int Main(string[] args)
    {
        Color myColor = Color.Blue;

        // Box enum value
        object boxed = myColor;

        // Cast to System.Enum
        System.Enum enumValue = (System.Enum)boxed;

        // Cast back to specific enum
        Color unboxed = (Color)enumValue;

        return (int)unboxed;
    }
}
