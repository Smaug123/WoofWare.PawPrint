using System.Linq;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int[] array = new[] { 1, 2, 3 };

            if (array.Sum() != 6)
            {
                return 1;
            }

            return 0;
        }
    }
}
