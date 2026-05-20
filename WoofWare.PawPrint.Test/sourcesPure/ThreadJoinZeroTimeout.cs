using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static int Main(string[] args)
        {
            Thread t = new Thread(() => { });
            t.Start();
            // Blocking Join drives the worker to completion.
            t.Join();

            // After the worker has terminated, Join(0) should return true immediately
            // (non-blocking poll; target already Terminated).
            if (!t.Join(0))
            {
                return 1;
            }

            return 0;
        }
    }
}
