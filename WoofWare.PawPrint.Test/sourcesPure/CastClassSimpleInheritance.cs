public class Program
{
    public class Animal
    {
        public int Age { get; set; }
    }

    public class Dog : Animal
    {
        public string Name { get; set; }
    }

    public static int Main(string[] args)
    {
        Dog myDog = new Dog { Age = 5, Name = "Rex" };

        // Cast to base class - should succeed
        Animal animal = (Animal)myDog;

        return animal.Age;
    }
}
