using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            Thread t = new Thread(() => { });
            if (t == null) return 1;

            int id = t.ManagedThreadId;
            if (id <= 0) return 2;

            if (id == Thread.CurrentThread.ManagedThreadId) return 3;

            return 0;
        }
    }
}
