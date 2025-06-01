using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int x = 3;
            try
            {
                return x;
            }
            finally
            {
                x = x + 1;
            }
        }
    }
}
