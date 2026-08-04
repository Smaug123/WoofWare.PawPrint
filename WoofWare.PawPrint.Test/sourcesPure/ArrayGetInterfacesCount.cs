using System;

namespace ArrayGetInterfacesCount
{
    enum Colour
    {
        Red,
    }

    struct Pair
    {
        public int A;
        public int B;
    }

    class Program
    {
        // System.Array itself implements six interfaces (ICloneable, IList, ICollection,
        // IEnumerable, IStructuralComparable, IStructuralEquatable), and an array's
        // MethodTable inherits that map verbatim (array.cpp: "Inherit top level class's
        // interface map"). An SZ array additionally reports the five implicit generic
        // interfaces, which RuntimeTypeCache.PopulateInterfaces appends in managed code.
        // A multi-dimensional array is not SZ, so it reports only the inherited six.
        static int Main(string[] args)
        {
            if (typeof(Array).GetInterfaces().Length != 6)
            {
                return 1;
            }

            // Reference element, primitive element, enum element, custom struct element,
            // and an element that is itself an array: all SZ, so all 11.
            if (typeof(int[]).GetInterfaces().Length != 11)
            {
                return 2;
            }

            if (typeof(string[]).GetInterfaces().Length != 11)
            {
                return 3;
            }

            if (typeof(object[]).GetInterfaces().Length != 11)
            {
                return 4;
            }

            if (typeof(Colour[]).GetInterfaces().Length != 11)
            {
                return 5;
            }

            if (typeof(Pair[]).GetInterfaces().Length != 11)
            {
                return 6;
            }

            if (typeof(int[][]).GetInterfaces().Length != 11)
            {
                return 7;
            }

            // Multi-dimensional arrays get no implicit generic interfaces, at any rank.
            if (typeof(int[,]).GetInterfaces().Length != 6)
            {
                return 8;
            }

            if (typeof(int[,,]).GetInterfaces().Length != 6)
            {
                return 9;
            }

            // The instance path agrees with the typeof path.
            if (new int[3].GetType().GetInterfaces().Length != 11)
            {
                return 10;
            }

            // What matters is the rank of the *outermost* array. C# reads the leftmost
            // bracket group as the outer one, so int[][,] is an SZ array of rank-2 arrays
            // (11), while int[,][] is a rank-2 array of SZ arrays (6).
            if (typeof(int[][,]).GetInterfaces().Length != 11)
            {
                return 11;
            }

            if (typeof(int[,][]).GetInterfaces().Length != 6)
            {
                return 12;
            }

            return 0;
        }
    }
}
