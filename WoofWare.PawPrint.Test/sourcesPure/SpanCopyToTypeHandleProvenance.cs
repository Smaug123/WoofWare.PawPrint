using System;

public class Program
{
    // `IntPtr handle = typeof(int).TypeHandle.Value` carries non-`Verbatim`
    // `NativeIntSource.TypeHandlePtr` provenance under PawPrint. The byte
    // model cannot serialise this provenance via `CliNumericType.ToBytes`,
    // so any copy that walks the source cell byte-by-byte will crash.
    //
    // `Span<T>.CopyTo` lowers to `Buffer.Memmove<T>` and for value-type T
    // with no managed references that calls `SpanHelpers.Memmove(ref byte,
    // ref byte, nuint)` (see Buffer.cs:152 and Span.cs:325 in dotnet/runtime).
    // PawPrint intercepts that intrinsic and dispatches it through
    // `CellAwareMemOps.copy` so whole-cell ranges anchored on cell-aware roots
    // (here, both array element byrefs) move through `readManagedByref` /
    // `writeManagedByrefWithBase`, preserving the `TypeHandlePtr` provenance.
    //
    // The third assertion (re-reading via `typeof(int).TypeHandle.Value`)
    // distinguishes a real cell-preserving move from a numeric coincidence
    // that happened to pass the prior equality check: comparing against a
    // freshly produced `TypeHandlePtr` would tunnel through the byte-walk
    // path if the cell had been silently rewritten as raw bits.
    struct WithTypeHandle
    {
        public int Id;
        public IntPtr Handle;
    }

    public static int Main(string[] args)
    {
        IntPtr handle = typeof(int).TypeHandle.Value;

        WithTypeHandle[] src = new WithTypeHandle[1];
        src[0].Id = 7;
        src[0].Handle = handle;

        WithTypeHandle[] dst = new WithTypeHandle[1];
        ((ReadOnlySpan<WithTypeHandle>)src).CopyTo(dst);

        if (dst[0].Id != 7) return 1;
        if (dst[0].Handle != handle) return 2;
        if (dst[0].Handle != typeof(int).TypeHandle.Value) return 3;
        return 0;
    }
}
