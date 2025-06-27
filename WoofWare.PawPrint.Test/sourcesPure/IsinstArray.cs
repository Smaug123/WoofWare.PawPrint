public class Program
{
    public static int Main(string[] args)
    {
        string[] names = new string[] { "Alice", "Bob", "Charlie" };

        // Check if array is System.Array
        if (names is System.Array)
        {
            return 42;
        }

        return 0;
    }
}
