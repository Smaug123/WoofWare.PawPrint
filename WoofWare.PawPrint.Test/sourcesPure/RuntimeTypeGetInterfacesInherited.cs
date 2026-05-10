namespace RuntimeTypeGetInterfacesInherited
{
    interface IBase { }

    interface IDerived : IBase { }

    class Foo : IDerived { }

    class Bar : Foo { }

    class Program
    {
        static int Main(string[] args)
        {
            // Bar : Foo : IDerived : IBase, so Bar.GetInterfaces() should yield 2 entries.
            int count = typeof(Bar).GetInterfaces().Length;
            return count == 2 ? 0 : 1;
        }
    }
}
