using System;
using System.Runtime.CompilerServices;

public unsafe class Program
{
    // `Unsafe.CopyBlock` lowers to `cpblk` (ECMA-335 III.3.30). Under
    // PawPrint that drives the shared cell-aware copy primitive
    // (`CellAwareMemOps.copy` with `CpblkForward`), which detects
    // cell-aligned ranges anchored on cell-aware roots (arrays,
    // heap-value byrefs, heap-object-field byrefs) and moves whole typed
    // cells through `readManagedByref` / `writeManagedByrefWithBase`.
    //
    // The previous implementation walked byte-by-byte through
    // `readManagedByrefBytesAs` / `writeManagedByrefBytesOrTypedCell`,
    // which fails for cells whose `NativeIntSource` provenance is not
    // `Verbatim` (e.g. `TypeHandlePtr` from `typeof(T).TypeHandle.Value`)
    // — `CliNumericType.ToBytes` cannot serialise those provenances. This
    // test pins that behaviour: copying a `TypeHandlePtr`-tagged
    // `IntPtr` cell wholesale must succeed and the destination cell must
    // recover the same `RuntimeTypeHandle` value.

    private static int TestArrayIntPtrTypeHandle()
    {
        IntPtr[] arr = new IntPtr[2];
        IntPtr handle = typeof(int).TypeHandle.Value;
        arr[0] = handle;
        arr[1] = IntPtr.Zero;

        fixed (IntPtr* p = arr)
        {
            Unsafe.CopyBlock(p + 1, p, (uint)sizeof(IntPtr));
        }

        if (arr[0] != handle) return 1;
        if (arr[1] != handle) return 2;
        // Verify the destination cell still behaves as a real handle, not
        // as a numeric coincidence: comparing it back through the typed
        // `RuntimeTypeHandle` API would tunnel through the byte-walk path
        // if the cell-aware move had silently rewritten the cell.
        if (arr[1] != typeof(int).TypeHandle.Value) return 3;
        return 0;
    }

    // The `ref byte` overload of `Unsafe.CopyBlock` lands on the same
    // intrinsic dispatch in PawPrint, but the byrefs flow in as
    // `ConcreteByref byte` rather than `ConcretePointer byte`. Exercise
    // it explicitly so a future divergence between the two argument
    // shapes can't silently regress provenance preservation.
    private static int TestArrayIntPtrTypeHandleByrefOverload()
    {
        IntPtr[] arr = new IntPtr[2];
        IntPtr handle = typeof(long).TypeHandle.Value;
        arr[0] = handle;
        arr[1] = IntPtr.Zero;

        ref byte destByte = ref Unsafe.As<IntPtr, byte>(ref arr[1]);
        ref byte srcByte = ref Unsafe.As<IntPtr, byte>(ref arr[0]);
        Unsafe.CopyBlock(ref destByte, ref srcByte, (uint)sizeof(IntPtr));

        if (arr[0] != handle) return 10;
        if (arr[1] != handle) return 11;
        if (arr[1] != typeof(long).TypeHandle.Value) return 12;
        return 0;
    }

    public static int Main(string[] args)
    {
        int r = TestArrayIntPtrTypeHandle();
        if (r != 0) return r;
        r = TestArrayIntPtrTypeHandleByrefOverload();
        if (r != 0) return r;
        return 0;
    }
}
