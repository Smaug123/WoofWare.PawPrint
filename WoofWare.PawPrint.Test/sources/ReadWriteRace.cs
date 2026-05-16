using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int x;

        static void Worker()
        {
            x = 1;
        }

        static int Main(string[] args)
        {
            Thread t = new Thread(Worker);
            t.Start();
            int observed = x;
            t.Join();
            return observed;
        }
    }
}
