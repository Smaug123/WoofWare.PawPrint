using System;
using System.Runtime.CompilerServices;
using System.Threading;

// Exercises the Interlocked.CompareExchange(ref IntPtr, IntPtr, IntPtr) overload.
// The shipped IL wrapper does Unsafe.As<IntPtr,long> + Conv_I8 and delegates to the
// Int64 overload, which would destroy the interpreter's NativeIntSource provenance.
// We intercept the (ref IntPtr, ...) overload as an intrinsic to preserve provenance.
public class Program
{
    private IntPtr m_slot;

    [MethodImpl(MethodImplOptions.NoInlining)]
    IntPtr CasSlot(IntPtr value, IntPtr comparand) =>
        Interlocked.CompareExchange(ref m_slot, value, comparand);

    static int Main(string[] args)
    {
        var p = new Program();

        // m_slot starts at IntPtr.Zero.
        // Mismatching comparand: value must NOT be written; previous (Zero) is returned.
        IntPtr prev1 = p.CasSlot(new IntPtr(100), new IntPtr(42));
        if (prev1 != IntPtr.Zero) return 1;
        if (p.m_slot != IntPtr.Zero) return 2;

        // Matching comparand (Zero): value IS written; previous (Zero) is returned.
        IntPtr prev2 = p.CasSlot(new IntPtr(100), IntPtr.Zero);
        if (prev2 != IntPtr.Zero) return 3;
        if (p.m_slot != new IntPtr(100)) return 4;

        // Matching comparand (100): value IS written; previous (100) is returned.
        IntPtr prev3 = p.CasSlot(new IntPtr(200), new IntPtr(100));
        if (prev3 != new IntPtr(100)) return 5;
        if (p.m_slot != new IntPtr(200)) return 6;

        // Mismatching comparand again: value must NOT be written; previous (200) is returned.
        IntPtr prev4 = p.CasSlot(new IntPtr(300), new IntPtr(999));
        if (prev4 != new IntPtr(200)) return 7;
        if (p.m_slot != new IntPtr(200)) return 8;

        return 0;
    }
}
