public class Program
{
    public static int Main(string[] args)
    {
        int[] numbers = new int[] { 1, 2, 3, 4, 5 };

        // Cast array to System.Array - should succeed
        System.Array array = (System.Array)numbers;

        return array.Length;
    }
}
