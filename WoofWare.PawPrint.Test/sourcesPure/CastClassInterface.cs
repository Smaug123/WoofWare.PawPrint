public class Program
{
    public interface ICalculator
    {
        int Calculate(int x, int y);
    }

    public class Adder : ICalculator
    {
        public int Calculate(int x, int y)
        {
            return x + y;
        }
    }

    public static int Main(string[] args)
    {
        Adder adder = new Adder();

        // Cast to interface - should succeed
        ICalculator calc = (ICalculator)adder;

        return calc.Calculate(10, 32);
    }
}
