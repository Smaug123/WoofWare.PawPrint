using System;

public class Program
{
    // `Span<T>.CopyTo(self)` lowers to `Buffer.Memmove<T>` which calls
    // `SpanHelpers.Memmove(ref byte, ref byte, nuint)` with identical src
    // and dest. CoreCLR short-circuits that case
    // (SpanHelpers.ByteMemOps.cs:230) without copying. PawPrint's intercept
    // must do the same: walking byte-by-byte through provenance-carrying
    // `NativeInt` cells (e.g. `TypeHandlePtr` from
    // `typeof(int).TypeHandle.Value`) would otherwise hit the byte
    // serialisation rejection in `validateByteAddressableCell`, even
    // though the no-op self-copy preserves all observable state.
    public static int Main(string[] args)
    {
        IntPtr handle = typeof(int).TypeHandle.Value;

        IntPtr[] buf = new IntPtr[1];
        buf[0] = handle;

        Span<IntPtr> span = buf;
        span.CopyTo(span);

        if (buf[0] != handle) return 1;
        if (buf[0] != typeof(int).TypeHandle.Value) return 2;
        return 0;
    }
}
