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

        // This should fail and return null (not throw)
        Fish fish = sparrow as Fish;

        return fish == null ? 42 : 0;
    }
}
