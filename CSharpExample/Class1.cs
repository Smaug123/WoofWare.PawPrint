using System;
using System.IO;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            object locker = new FileInfo("hi");
            lock (locker)
            {
                return 1;
            }
        }
    }
}
