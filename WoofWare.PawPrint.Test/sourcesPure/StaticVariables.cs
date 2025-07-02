public class GenericCounter<T>
{
    private static int count = 0;

    public static void Increment()
    {
        count++;
    }

    public static int GetCount()
    {
        return count;
    }

    public static void Reset()
    {
        count = 0;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        // Test that different generic instantiations have separate static variables

        // Initial state should be 0 for all
        if (GenericCounter<int>.GetCount() != 0) return 1;
        if (GenericCounter<string>.GetCount() != 0) return 2;

        // Increment int version 3 times
        GenericCounter<int>.Increment();
        GenericCounter<int>.Increment();
        GenericCounter<int>.Increment();

        // Increment string version 2 times
        GenericCounter<string>.Increment();
        GenericCounter<string>.Increment();

        // Verify counts are independent
        if (GenericCounter<int>.GetCount() != 3) return 3;
        if (GenericCounter<string>.GetCount() != 2) return 4;

        // Reset int version only
        GenericCounter<int>.Reset();

        // Verify reset only affected int version
        if (GenericCounter<int>.GetCount() != 0) return 5;
        if (GenericCounter<string>.GetCount() != 2) return 6;

        // Test passes - static variables are isolated per generic instantiation
        return 0;
    }
}
