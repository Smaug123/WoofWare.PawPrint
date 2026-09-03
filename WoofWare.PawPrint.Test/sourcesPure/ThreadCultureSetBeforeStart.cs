using System.Globalization;
using System.Threading;

namespace HelloWorldApp
{
    // Setting `Thread.CurrentCulture` on a thread that has not started yet parks the culture in
    // the thread's StartHelper, and the new thread installs it as its own current culture before
    // running the delegate. Identity is what is asserted: the worker's current culture must be
    // the very object handed to the unstarted thread, and the starter's must not become it.
    class Program
    {
        static CultureInfo? handedToThread = null;
        static bool workerSawIt = false;
        static bool workerThreadPropertySawIt = false;

        static void Worker()
        {
            workerSawIt = object.ReferenceEquals(CultureInfo.CurrentCulture, handedToThread);
            workerThreadPropertySawIt = object.ReferenceEquals(Thread.CurrentThread.CurrentCulture, handedToThread);
        }

        static int Main(string[] args)
        {
            // A fresh object equal to the invariant culture, so that identity rather than value
            // is what distinguishes "the culture we handed over" from "the default".
            CultureInfo culture = (CultureInfo)CultureInfo.InvariantCulture.Clone();
            handedToThread = culture;
            Thread t = new Thread(Worker);
            t.CurrentCulture = culture;
            t.Start();
            t.Join();
            if (!workerSawIt)
            {
                return 1;
            }
            if (!workerThreadPropertySawIt)
            {
                return 2;
            }
            if (object.ReferenceEquals(CultureInfo.CurrentCulture, culture))
            {
                return 3;
            }
            return 0;
        }
    }
}
