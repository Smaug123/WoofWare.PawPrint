using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object locker = new object();
        static int produced = 0;

        static void Worker()
        {
            lock (locker)
            {
                produced = 42;
                Monitor.Pulse(locker);
            }
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            lock (locker)
            {
                t.Start();
                while (produced == 0)
                {
                    Monitor.Wait(locker);
                }
            }
            t.Join();
            return produced;
        }
    }
}
