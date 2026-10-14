using System;
using System.Runtime.InteropServices;

// An exception stored through an `Exception*` into native memory, and read back through it: the
// shape of the out-slot CoreLib's [UnmanagedCallersOnly] entry points hand what they catch to.
public static class Program
{
#pragma warning disable CS8500 // a pointer to a managed type
    static unsafe void Fill(Exception* pException)
    {
        try
        {
            throw new InvalidOperationException("stored");
        }
        catch (Exception ex)
        {
            *pException = ex;
        }
    }

    public static unsafe int Main(string[] argv)
    {
        Exception* slot = (Exception*)NativeMemory.AllocZeroed((nuint)sizeof(IntPtr));
        Fill(slot);
        Exception stored = *slot;
        NativeMemory.Free(slot);

        if (stored == null) return 1;
        if (stored.GetType() != typeof(InvalidOperationException)) return 2;
        if (stored.Message != "stored") return 3;
        return 0;
    }
#pragma warning restore CS8500
}
