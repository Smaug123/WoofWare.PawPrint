using System;

namespace RuntimeTypeGetInterfaceByName
{
    interface IBase { }

    interface IDerived : IBase { }

    interface IUnrelated { }

    interface IGen<T> { }

    class Foo : IDerived { }

    class Bar : Foo, IUnrelated, IGen<int> { }

    class Program
    {
        // Type.GetInterface(name) is the only caller of RuntimeTypeHandle.GetUtf8Name that
        // does not involve arrays: PopulateInterfaces consults the name filter, which calls
        // GetUtf8Name on each candidate interface, only when the caller supplied a name.
        static int Main(string[] args)
        {
            // Directly implemented.
            if (typeof(Bar).GetInterface("IUnrelated") == null)
            {
                return 1;
            }

            // Inherited from the base class, and transitively extended.
            if (typeof(Bar).GetInterface("IDerived") == null)
            {
                return 2;
            }

            if (typeof(Bar).GetInterface("IBase") == null)
            {
                return 3;
            }

            // A miss is null, not an exception.
            if (typeof(Bar).GetInterface("INotThere") != null)
            {
                return 4;
            }

            // The one-argument overload is case-sensitive. (The ignoreCase overload is not
            // covered here: it bottoms out in the MdUtf8String_EqualsCaseInsensitive QCall,
            // which PawPrint does not implement yet.)
            if (typeof(Bar).GetInterface("ibase") != null)
            {
                return 5;
            }

            // The name is the metadata name, so a generic interface is found under its
            // arity-mangled name and not under its unmangled one.
            if (typeof(Bar).GetInterface("IGen`1") == null)
            {
                return 7;
            }

            if (typeof(Bar).GetInterface("IGen") != null)
            {
                return 11;
            }

            // That name round-trips against every interface GetInterfaces() reports.
            Type[] ifaces = typeof(Bar).GetInterfaces();
            if (ifaces.Length != 4)
            {
                return 12;
            }

            for (int i = 0; i < ifaces.Length; i++)
            {
                Type found = typeof(Bar).GetInterface(ifaces[i].Name);
                if (found == null)
                {
                    return 8;
                }

                if (found != ifaces[i])
                {
                    return 9;
                }
            }

            // A type with no interfaces at all still answers null rather than faulting.
            if (typeof(Program).GetInterface("IBase") != null)
            {
                return 10;
            }

            return 0;
        }
    }
}
