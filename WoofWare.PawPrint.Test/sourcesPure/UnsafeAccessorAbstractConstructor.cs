using System;
using System.Runtime.CompilerServices;

// An abstract class's constructor reached by `Constructor`. The lookup binds it -- an abstract class
// declares constructors like any other -- and the synthesised body is a `newobj`, which the JIT
// refuses for an abstract class (`CEEInfo::getNewHelper`). Measured on real .NET 10:
// `InvalidOperationException` ("Instances of abstract classes cannot be created."), with the
// `COR_E_INVALIDOPERATION` HResult, raised before the accessor's declaring type is initialised.
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

    private static class Counter
    {
        public static int CctorRuns;
    }

    private static class Accessors
    {
        static Accessors()
        {
            Counter.CctorRuns++;
        }

        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern Abstract NewAbstract();

        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        public static extern Abstract NewAbstractWithArgument(int x);
    }

    private static int Check(int code, Action a)
    {
        try
        {
            a();
            return code;
        }
        catch (InvalidOperationException e)
        {
            if (e.HResult != unchecked((int) 0x80131509)) return code + 1;
            if (e.Message != "Instances of abstract classes cannot be created.") return code + 2;
            if (e.GetType() != typeof(InvalidOperationException)) return code + 3;
        }

        return 0;
    }

    private static int Run()
    {
        int r;

        r = Check(10, () => Accessors.NewAbstract());
        if (r != 0) return r;

        r = Check(20, () => Accessors.NewAbstractWithArgument(3));
        if (r != 0) return r;

        // The JIT refuses the stub before its prologue could run the declaring type's initialiser.
        if (Counter.CctorRuns != 0) return 30;

        return 0;
    }

    public static int Main() => Run();
}
