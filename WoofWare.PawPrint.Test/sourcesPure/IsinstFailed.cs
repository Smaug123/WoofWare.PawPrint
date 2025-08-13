public class Program
{
    public class Bird
    {
        public bool CanFly { get; set; }
    }

    public class Fish
    {
        public bool CanSwim { get; set; }
    }

    public static int Main(string[] args)
    {
        Bird sparrow = new Bird { CanFly = true };

        // Cast to object first to bypass compile-time checking
        object obj = sparrow;

        // This should fail at runtime and return null (not throw)
        Fish fish = obj as Fish;

        return fish == null ? 42 : 0;
    }
}
