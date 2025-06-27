using System;

namespace HelloWorldApp
{
    class Program
    {
        static int ReallyMain(string[] args)
        {
            return 10;
        }

        static int Main(string[] args)
        {
            try
            {
                return ReallyMain(args);
            }
            catch
            {
                throw;
            }
        }
    }
}
