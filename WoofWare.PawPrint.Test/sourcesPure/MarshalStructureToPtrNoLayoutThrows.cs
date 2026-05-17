using System;
using System.Runtime.InteropServices;

public class Program
{
    class PlainClass
    {
        public int X;
    }

    public static int Main(string[] args)
    {
        // CoreCLR's `MarshalNative_TryGetStructMarshalStub` (marshalnative.cpp:99) returns
        // FALSE for any type whose `HasLayout()` is false — i.e. whose `TypeAttributes`
        // carry `LayoutKind.Auto` (the default for reference types). The managed
        // `Marshal.StructureToPtr` overload turns that FALSE into an `ArgumentException`
        // (resource `Argument_MustHaveLayoutOrBeBlittable`). This test exercises two
        // no-layout shapes: `System.Object` itself, and an ordinary class without
        // `[StructLayout]`. Both reach the QCall via `RuntimeHelpers.GetMethodTable(structure)`
        // and must surface the same `ArgumentException` rather than tripping any host TODO.
        IntPtr ptr = Marshal.AllocHGlobal(8);
        try
        {
            try
            {
                Marshal.StructureToPtr(new object(), ptr, false);
                return 1;
            }
            catch (ArgumentException)
            {
                // Expected
            }

            try
            {
                Marshal.StructureToPtr(new PlainClass(), ptr, false);
                return 2;
            }
            catch (ArgumentException)
            {
                // Expected
            }

            return 0;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
    }
}
