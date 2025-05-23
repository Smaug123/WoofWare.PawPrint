using System;

namespace HelloWorldApp
{
    class Program
    {
        static int ReallyMain(string[] args)
        {
            return 0;
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
