// `Unsafe.ReadUnaligned`/`WriteUnaligned` through a null byref throw `NullReferenceException`.
// Their CoreLib bodies are replaced by `ldarg.0; unaligned. 1; ldobj !!T; ret` and the
// symmetric `stobj`, with no explicit null check — the access at address 0 faults and the
// runtime translates it into `NullReferenceException`.
//
// The write is the load-bearing half: a store cannot be elided, whereas a read whose result
// is unused could in principle be. The read below therefore parks its result in a static.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    private static int Sink;

    public static int Main(string[] args)
    {
        try
        {
            Unsafe.WriteUnaligned<int>(ref Unsafe.NullRef<byte>(), 42);
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Sink = Unsafe.ReadUnaligned<int>(ref Unsafe.NullRef<byte>());
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        // Non-null byrefs must still round-trip.
        byte[] buffer = new byte[8];
        Unsafe.WriteUnaligned<int>(ref buffer[1], 0x12345678);
        if (Unsafe.ReadUnaligned<int>(ref buffer[1]) != 0x12345678)
        {
            return 3;
        }

        return 0;
    }
}
