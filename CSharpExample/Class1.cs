using System;
using System.IO;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            object locker = new FileInfo("hi");
            bool lockTaken = false;
            try
            {
                Monitor.Enter(locker, ref lockTaken);
                return 1;
            }
            finally
            {
                if (lockTaken)
                    Monitor.Exit(locker);
            }
        }
    }
}
