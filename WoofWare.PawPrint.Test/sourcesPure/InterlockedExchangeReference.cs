using System.Threading;

namespace InterlockedExchangeReference
{
    class Marker
    {
    }

    class Program
    {
        static int Main(string[] args)
        {
            Marker a = new Marker();
            Marker b = new Marker();
            Marker location = null;

            // null -> a: returns null, location is a.
            Marker old = Interlocked.Exchange(ref location, a);
            if (!ReferenceEquals(old, null) || !ReferenceEquals(location, a))
            {
                return 1;
            }

            // a -> b: returns a, location is b. Unlike CompareExchange, no comparand check.
            old = Interlocked.Exchange(ref location, b);
            if (!ReferenceEquals(old, a) || !ReferenceEquals(location, b))
            {
                return 2;
            }

            // b -> null: returns b, location is null.
            old = Interlocked.Exchange(ref location, null);
            if (!ReferenceEquals(old, b) || !ReferenceEquals(location, null))
            {
                return 3;
            }

            // null -> null: returns null, location stays null.
            old = Interlocked.Exchange(ref location, null);
            if (!ReferenceEquals(old, null) || !ReferenceEquals(location, null))
            {
                return 4;
            }

            return 0;
        }
    }
}
