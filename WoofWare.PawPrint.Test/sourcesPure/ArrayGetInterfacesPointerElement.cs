using System;

namespace ArrayGetInterfacesPointerElement
{
    class Program
    {
        // An SZ array whose element type is a pointer gets *no* implicit generic interfaces:
        // PopulateInterfaces gates them on `!arrayType.IsPointer` (RuntimeType.CoreCLR.cs:1046),
        // because IList<int*> is not expressible — a pointer cannot be a generic argument.
        // So int*[] reports only the six inherited from System.Array, exactly like a
        // multi-dimensional array does.
        static unsafe int Main(string[] args)
        {
            if (typeof(int*[]).GetInterfaces().Length != 6)
            {
                return 1;
            }

            // A pointer type itself is a TypeDesc: no MethodTable, so no interfaces at all.
            if (typeof(int*).GetInterfaces().Length != 0)
            {
                return 2;
            }

            // The element type round-trips.
            if (typeof(int*[]).GetElementType() != typeof(int*))
            {
                return 3;
            }

            return 0;
        }
    }
}
