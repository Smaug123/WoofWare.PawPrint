// `Unsafe.ReadUnaligned<T>(void*)` / `WriteUnaligned<T>(void*, T)` through a null pointer throw
// `NullReferenceException`, as their `ref byte` overloads do (`UnsafeUnalignedNullRef.cs`). Both
// are `[Intrinsic]`, and the IL the JIT substitutes is `ldarg.0; unaligned. 1; ldobj !!T; ret` and
// `ldarg.0; ldarg.1; unaligned. 1; stobj !!T; ret`, with no explicit null check, so the access at
// address 0 faults and the runtime translates the fault into `NullReferenceException`.
//
// Measured on .NET 10 under default tiering, `DOTNET_TieredCompilation=0` and
// `DOTNET_JITMinOpts=1`: all three raise it for every row below.
//
// A store cannot be elided, whereas a read whose result is unused could in principle be, so each
// read parks its result in a static.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    private static int SinkInt;
    private static long SinkLong;

    // Opaque, so the null cannot be constant-folded into the call.
    private static unsafe void* NullPointer()
    {
        return null;
    }

    public static unsafe int Main(string[] args)
    {
        try
        {
            Unsafe.WriteUnaligned<int>(NullPointer(), 42);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            SinkInt = Unsafe.ReadUnaligned<int>(NullPointer());
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            SinkLong = Unsafe.ReadUnaligned<long>((void*)0);
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        // Non-null pointers must still round-trip.
        byte[] buffer = new byte[8];
        fixed (byte* p = buffer)
        {
            Unsafe.WriteUnaligned<int>(p + 1, 0x12345678);
            if (Unsafe.ReadUnaligned<int>(p + 1) != 0x12345678)
            {
                return 4;
            }
        }

        return 0;
    }
}
