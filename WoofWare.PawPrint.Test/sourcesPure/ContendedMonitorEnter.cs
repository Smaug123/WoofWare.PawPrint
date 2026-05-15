using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker = new object();
        static int shared = 0;

        static void Worker()
        {
            lock (locker)
            {
                shared = 99;
            }
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            lock (locker)
            {
                t.Start();
                shared = 7;
            }
            t.Join();
            return shared;
        }
    }
}
