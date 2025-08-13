public class Program
{
    public class Cat
    {
        public string Name { get; set; }
    }

    public class Dog
    {
        public string Name { get; set; }
    }

    public static int Main(string[] args)
    {
        try
        {
            object cat = new Cat { Name = "Whiskers" };

            // Invalid cast - should throw InvalidCastException
            Dog dog = (Dog)cat;

            // Should not reach here
            return 0;
        }
        catch (System.InvalidCastException)
        {
            // Expected exception caught
            return 42;
        }
    }
}
