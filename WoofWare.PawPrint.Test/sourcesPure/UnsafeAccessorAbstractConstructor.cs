using System;
using System.Runtime.CompilerServices;

// An abstract class's constructor reached by `Constructor`. The lookup binds it -- an abstract class
// declares constructors like any other -- and the synthesised body is a `newobj`, which cannot
// allocate an abstract class. Measured on real .NET 10: `InvalidOperationException`
// ("Instances of abstract classes cannot be created."), with the `COR_E_INVALIDOPERATION` HResult.
public class TestUnsafeAccessorAbstractConstructor
{
    private abstract class Abstract
    {
        private Abstract()
        {
        }

        protected Abstract(int x)
        {
        }
    }

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Abstract NewAbstract();

    private static int Run()
    {
        try
        {
            NewAbstract();
            return 1;
        }
        catch (InvalidOperationException e)
        {
            if (e.HResult != unchecked((int) 0x80131509)) return 2;
        }

        return 0;
    }

    public static int Main() => Run();
}
