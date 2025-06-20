public delegate bool MyFilter(object item, object criteria);

public class DelegateDemo
{
    // This static field initialization will generate the exact IL pattern:
    public static readonly MyFilter FilterField = FilterImpl;

    // The static method that the delegate points to
    private static bool FilterImpl(object item, object criteria)
    {
        return true;
    }

    public static int Main(string[] argv)
    {
        // Force static constructor to run
        var filter = FilterField;

        // Test the delegate
        bool result = filter("test item", "criterion");
        if (result)
        {
            return 8;
        }
        return 5;
    }
}
