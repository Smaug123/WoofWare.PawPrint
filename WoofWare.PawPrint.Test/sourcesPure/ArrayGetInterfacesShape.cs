using System;
using System.Collections.Generic;

namespace ArrayGetInterfacesShape
{
    class Program
    {
        // Counts how many of `arrayType`'s reported interfaces are exactly `wanted`.
        // Deliberately compares closed types directly rather than going through
        // GetGenericTypeDefinition(), which PawPrint does not implement yet; type identity
        // is the stronger assertion anyway.
        static int CountOf(Type arrayType, Type wanted)
        {
            Type[] ifaces = arrayType.GetInterfaces();
            int found = 0;

            for (int i = 0; i < ifaces.Length; i++)
            {
                if (ifaces[i] == wanted)
                {
                    found++;
                }
            }

            return found;
        }

        // The generic interfaces an SZ array reports are instantiated at the array's *exact*
        // element type, and each appears exactly once. Duplicates are the failure mode if the
        // five were synthesised host-side *and* appended by PopulateInterfaces.
        //
        // This is distinct from the set of interfaces the array is assignable to, which is
        // larger under covariance: string[] is an IList<object>, but does not report
        // IList<object> here.
        static int Main(string[] args)
        {
            if (CountOf(typeof(int[]), typeof(IList<int>)) != 1)
            {
                return 1;
            }

            if (CountOf(typeof(int[]), typeof(ICollection<int>)) != 1)
            {
                return 2;
            }

            if (CountOf(typeof(int[]), typeof(IEnumerable<int>)) != 1)
            {
                return 3;
            }

            if (CountOf(typeof(int[]), typeof(IReadOnlyList<int>)) != 1)
            {
                return 4;
            }

            if (CountOf(typeof(int[]), typeof(IReadOnlyCollection<int>)) != 1)
            {
                return 5;
            }

            // Not instantiated at some other element type.
            if (CountOf(typeof(int[]), typeof(IList<object>)) != 0)
            {
                return 6;
            }

            if (CountOf(typeof(string[]), typeof(IList<string>)) != 1)
            {
                return 7;
            }

            if (CountOf(typeof(int[][]), typeof(IList<int[]>)) != 1)
            {
                return 8;
            }

            // Covariantly-reachable instantiations are absent despite being assignable.
            if (CountOf(typeof(string[]), typeof(IList<object>)) != 0)
            {
                return 9;
            }

            if (CountOf(typeof(string[]), typeof(IEnumerable<object>)) != 0)
            {
                return 10;
            }

            // A multi-dimensional array reports no generic interface at all.
            Type[] rank2 = typeof(int[,]).GetInterfaces();
            for (int i = 0; i < rank2.Length; i++)
            {
                if (rank2[i].IsGenericType)
                {
                    return 11;
                }
            }

            // Every reported interface really is an interface, and none is duplicated.
            Type[] ifaces = typeof(int[]).GetInterfaces();
            for (int i = 0; i < ifaces.Length; i++)
            {
                if (!ifaces[i].IsInterface)
                {
                    return 12;
                }

                for (int j = i + 1; j < ifaces.Length; j++)
                {
                    if (ifaces[i] == ifaces[j])
                    {
                        return 13;
                    }
                }
            }

            // The non-generic collection interfaces inherited from System.Array are present
            // alongside the generic ones, and are distinct from them.
            if (CountOf(typeof(int[]), typeof(System.Collections.IList)) != 1)
            {
                return 14;
            }

            if (CountOf(typeof(int[]), typeof(ICloneable)) != 1)
            {
                return 15;
            }

            return 0;
        }
    }
}
