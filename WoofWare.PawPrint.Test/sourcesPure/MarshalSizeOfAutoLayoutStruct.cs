using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Auto)]
    struct AutoLayoutStruct
    {
        public int A;
        public int B;
    }

    public static int Main(string[] args)
    {
        // CoreCLR rejects `Marshal.SizeOf<T>()` for any top-level `T` whose `TypeAttributes`
        // carry `LayoutKind.Auto`: `IsStructMarshalable` in `fieldmarshaler.cpp` returns false
        // because `HasLayout()` is false for AutoLayout, and `MarshalNative_SizeOfHelper` then
        // throws `ArgumentException` (resource `Argument_MustHaveLayoutOrBeBlittable`).
        //
        // The point of this test is that even a struct with two plain `int` fields that LOOKS
        // blittable is still rejected by the runtime when its declared layout kind is Auto.
        // PawPrint must reproduce the rejection rather than silently laying the fields out.
        try
        {
            Marshal.SizeOf(typeof(AutoLayoutStruct));
            return 1;
        }
        catch (ArgumentException)
        {
            return 0;
        }
    }
}
