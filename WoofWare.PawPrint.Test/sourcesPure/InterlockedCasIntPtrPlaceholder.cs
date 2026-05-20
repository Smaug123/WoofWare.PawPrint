using System;
using System.Runtime.CompilerServices;
using System.Threading;

// Regression for the `Interlocked.CompareExchange(ref IntPtr, ...)` intrinsic
// when the comparand is produced via the `Unsafe.AsRef<T>((void*)bits)`
// placeholder idiom. PawPrint represents such a value as
// `ManagedPointer (NativeIntPlaceholder bits)`, while a slot initialised by
// `new IntPtr(bits)` holds `Verbatim bits`. CEQ semantics must normalise the
// placeholder to its bit pattern so the two compare equal.
public unsafe class Program
{
    private IntPtr m_slot;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static IntPtr PlaceholderComparand(long bits) =>
        (IntPtr)Unsafe.AsPointer(ref Unsafe.AsRef<byte>((void*)bits));

    static int Main(string[] args)
    {
        var p = new Program();

        // Slot starts at Verbatim 0x1234; comparand is the same bit pattern
        // routed through the placeholder byref. Without normalisation in
        // `equalsForCli` these compare unequal and the CAS fails to write.
        p.m_slot = new IntPtr(0x1234);
        IntPtr comparand = PlaceholderComparand(0x1234);
        IntPtr prev1 = Interlocked.CompareExchange(ref p.m_slot, new IntPtr(0x9999), comparand);
        if (prev1 != new IntPtr(0x1234)) return 1;
        if (p.m_slot != new IntPtr(0x9999)) return 2;

        // Placeholder comparand with bits differing from the slot must NOT
        // match: the slot stays untouched and the previous value is returned.
        IntPtr mismatch = PlaceholderComparand(0x4321);
        IntPtr prev2 = Interlocked.CompareExchange(ref p.m_slot, new IntPtr(0xDEAD), mismatch);
        if (prev2 != new IntPtr(0x9999)) return 3;
        if (p.m_slot != new IntPtr(0x9999)) return 4;

        return 0;
    }
}
