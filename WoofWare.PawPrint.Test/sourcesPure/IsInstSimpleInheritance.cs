public class Program
{
    public class Vehicle
    {
        public int Wheels { get; set; }
    }

    public class Car : Vehicle
    {
        public string Model { get; set; }
    }

    public static int Main(string[] args)
    {
        Car myCar = new Car { Wheels = 4, Model = "Tesla" };

        // 'is' operator uses isinst instruction
        if (myCar is Vehicle)
        {
            return 42;
        }

        return 0;
    }
}
