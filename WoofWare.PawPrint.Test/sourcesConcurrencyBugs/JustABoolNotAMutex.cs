using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static bool flag;
        private static bool working;

        static void Worker()
        {
            for (int i = 0; i < 200; i++)
            {
                while (flag) {}

                flag = true;

                if (working)
                {
                    throw new Exception("failed!");
                }
                working = true;
                working = false;

                flag = false;
            }
        }

        static int Main(string[] args)
        {
            flag = false;
            Thread t1 = new Thread(Worker);
            Thread t2 = new Thread(Worker);
            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();
            return 0;
        }
    }
}
