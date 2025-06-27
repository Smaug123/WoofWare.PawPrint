public class Program
{
    public struct Counter
    {
        public int Count;

        public Counter(int count)
        {
            Count = count;
        }
    }

    public static int Main(string[] args)
    {
        Counter counter = new Counter(42);

        // Box the value type
        object boxed = counter;

        // Check if boxed value is System.ValueType
        if (boxed is System.ValueType)
        {
            return 42;
        }

        return 0;
    }
}
