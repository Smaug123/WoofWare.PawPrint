using System;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            object locker = new object();
            lock (locker)
            {
                return 1;
            }
        }
    }
}
