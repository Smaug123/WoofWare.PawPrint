public class Program
{
    public interface IAnimal
    {
        string Name { get; set; }
    }

    public class Bird : IAnimal
    {
        public string Name { get; set; }
        public bool CanFly { get; set; }
    }

    public class Fish : IAnimal
    {
        public string Name { get; set; }
        public bool CanSwim { get; set; }
    }

    public static int Main(string[] args)
    {
        IAnimal animal = new Bird { Name = "Sparrow", CanFly = true };

        // This should fail at runtime and return null (not throw)
        // because the actual object is Bird, not Fish
        Fish fish = animal as Fish;

        return fish == null ? 42 : 0;
    }
}
