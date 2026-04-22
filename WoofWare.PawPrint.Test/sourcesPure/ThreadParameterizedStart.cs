using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        static object? captured = null;

        static void Worker(object? o)
        {
            captured = o;
        }

        static int Main(string[] args)
        {
            object marker = new object();
            Thread t = new Thread(Worker);
            t.Start(marker);
            t.Join();
            // ReferenceEquals avoids needing an Equals override; the object we captured
            // must be the exact instance we passed to Start.
            return object.ReferenceEquals(captured, marker) ? 0 : 1;
        }
    }
}
