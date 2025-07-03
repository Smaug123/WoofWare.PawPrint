public class Program
{
    public abstract class Shape
    {
        public abstract double GetArea();
    }

    public class Circle : Shape
    {
        public double Radius { get; set; }

        public override double GetArea()
        {
            return 3.14 * Radius * Radius;
        }
    }

    public class Square : Shape
    {
        public double Side { get; set; }

        public override double GetArea()
        {
            return Side * Side;
        }
    }

    public static int Main(string[] args)
    {
        Shape shape = new Circle { Radius = 10 };

        // Pattern matching uses isinst
        if (shape is Circle circle)
        {
            return (int)circle.Radius;
        }

        return 0;
    }
}
