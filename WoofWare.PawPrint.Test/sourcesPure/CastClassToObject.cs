public class Program
{
    public class CustomClass
    {
        public int Id { get; set; }
    }

    public static int Main(string[] args)
    {
        CustomClass custom = new CustomClass { Id = 42 };

        // Everything can be cast to System.Object
        System.Object obj = (System.Object)custom;

        // Verify it's the same object
        return obj != null && obj == custom ? 42 : 0;
    }
}
