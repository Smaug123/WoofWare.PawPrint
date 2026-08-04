using System;
using System.Collections.Generic;

namespace ArrayGetInterfacesAssignable
{
    class Program
    {
        // IsAssignableFrom answers a *larger* set than GetInterfaces() reports, because it
        // admits covariance. The two must stay consistent with each other: everything
        // GetInterfaces() reports must be assignable from the array, but not conversely.
        static int Main(string[] args)
        {
            if (!typeof(ICollection<int>).IsAssignableFrom(typeof(int[])))
            {
                return 1;
            }

            if (!typeof(IList<int>).IsAssignableFrom(typeof(int[])))
            {
                return 2;
            }

            // Covariance: assignable, though not reported by GetInterfaces().
            if (!typeof(IList<object>).IsAssignableFrom(typeof(string[])))
            {
                return 3;
            }

            if (!typeof(IReadOnlyList<object>).IsAssignableFrom(typeof(string[])))
            {
                return 4;
            }

            // No variance for value-typed elements.
            if (typeof(IList<object>).IsAssignableFrom(typeof(int[])))
            {
                return 5;
            }

            // Multi-dimensional arrays implement none of the generic interfaces.
            if (typeof(IList<int>).IsAssignableFrom(typeof(int[,])))
            {
                return 6;
            }

            if (typeof(IEnumerable<int>).IsAssignableFrom(typeof(int[,])))
            {
                return 7;
            }

            // ... but do implement the non-generic ones inherited from System.Array.
            if (!typeof(System.Collections.IList).IsAssignableFrom(typeof(int[,])))
            {
                return 8;
            }

            // Consistency: every interface reported for an SZ array is assignable from it.
            Type[] ifaces = typeof(string[]).GetInterfaces();
            for (int i = 0; i < ifaces.Length; i++)
            {
                if (!ifaces[i].IsAssignableFrom(typeof(string[])))
                {
                    return 9;
                }
            }

            // The same, for a multi-dimensional array.
            Type[] rank2 = typeof(int[,]).GetInterfaces();
            for (int i = 0; i < rank2.Length; i++)
            {
                if (!rank2[i].IsAssignableFrom(typeof(int[,])))
                {
                    return 10;
                }
            }

            return 0;
        }
    }
}
