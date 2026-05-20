using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            Thread t1 = Thread.CurrentThread;
            if (t1 == null) return 1;

            Thread t2 = Thread.CurrentThread;
            if (!object.ReferenceEquals(t1, t2)) return 2;

            return 0;
        }
    }
}
