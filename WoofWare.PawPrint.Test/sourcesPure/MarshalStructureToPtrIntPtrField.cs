using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct WithIntPtrs
    {
        public int Id;
        public IntPtr Handle;
        public UIntPtr UHandle;
    }

    public static int Main(string[] args)
    {
        // CoreCLR's `MarshalNative_TryGetStructMarshalStub` (marshalnative.cpp:99)
        // treats `IntPtr` / `UIntPtr` fields as blittable: it memmoves the
        // integer-width bits regardless of the value's provenance. PawPrint's
        // strict-numeric classifier rejects all `NativeInt` today because its
        // byte model can't render non-`Verbatim` provenance (function pointers,
        // type handles, etc.). The central `CliByteAddressability` classifier
        // is the right gate: it already accepts `Verbatim` and null managed-
        // pointer provenance, which is exactly the case CoreCLR memmoves.
        //
        // This test exercises the loosened classifier on the case it's meant
        // to cover: handle-typed fields whose values come from integer
        // literals (Verbatim provenance) flow through the blittable path
        // intact.
        IntPtr handle = (IntPtr)0x1234567890ABCDEFL;
        UIntPtr uhandle = (UIntPtr)0xFEDCBA9876543210UL;
        var s = new WithIntPtrs { Id = 7, Handle = handle, UHandle = uhandle };
        int size = Marshal.SizeOf<WithIntPtrs>();
        // On a 64-bit host the layout is: int Id @ 0, pad @ 4..7,
        // IntPtr Handle @ 8..15, UIntPtr UHandle @ 16..23. On a 32-bit
        // host it would be 4 + 4 + 4 = 12 with no padding. PawPrint only
        // targets 64-bit hosts so we hardcode the 64-bit expectation.
        if (IntPtr.Size != 8) return 100;
        if (size != 24) return 101;
        IntPtr ptr = Marshal.AllocHGlobal(size);
        try
        {
            Marshal.StructureToPtr(s, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 7) return 1;
            if (Marshal.ReadInt64(ptr, 8) != 0x1234567890ABCDEFL) return 2;
            if (unchecked((ulong)Marshal.ReadInt64(ptr, 16)) != 0xFEDCBA9876543210UL) return 3;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
