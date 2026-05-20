using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            int x = -1;
            try
            {
                throw new Exception("hello");
            }
            catch
            {
                x += 1;
            }

            return x;
        }
    }
}
