using System;
using System.Linq;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int[] array = new { 10, 20, 30 };

            if (array.Sum() != 60)
            {
                return 1;
            }

            return 0;
        }
    }
}
