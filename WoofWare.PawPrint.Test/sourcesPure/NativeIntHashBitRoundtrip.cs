using System;

class Program
{
    // Exercises the NativeIntSource `OpaqueHashBits` round-trip: widen a
    // pointer-shaped nuint to ulong (`conv.u8`, producing
    // `Int64Source.OpaqueHashBits`), bit-mix in the int64 domain, then narrow
    // back to nuint (`conv.u`, which lands in
    // `NativeIntSource.OpaqueHashBits`), then re-widen to ulong (`conv.u8`,
    // routing through `Int64Source.widenedNativeInt`'s normalisation back to
    // `Int64Source.OpaqueHashBits`) and finally narrow to int32 (`conv.i4`,
    // the cast-cache "bucket index" form).
    //
    // This is the operation the BCL's
    // `BitOperations.RotateLeft(nuint, int)` inlines as
    // `(nuint)RotateLeft((ulong)value, offset)` — the final `(nuint)` cast
    // is exactly the round-trip this test covers in isolation, without
    // requiring `BitOperations.RotateLeft` or anything downstream.
    static int Main(string[] args)
    {
        IntPtr h = typeof(int).TypeHandle.Value;
        ulong widened = (ulong)h;
        ulong rotated = (widened << 16) | (widened >> 48);
        nuint narrowed = (nuint)rotated;
        ulong rewidened = (ulong)narrowed;
        int bucket = (int)(rewidened & 0xFFFFUL);
        if (bucket < 0 || bucket > 0xFFFF)
        {
            return 1;
        }

        // Exercise the native-int comparison arms on `OpaqueHashBits` in
        // both their unsigned (`cgt.un`/`clt.un`, the lowering for nuint `>`/`<`)
        // and signed forms. The rotated bits are non-zero by construction
        // (the original handle is non-null), so all of these comparisons have
        // a deterministic answer.
        if (!(narrowed > (nuint)0))
        {
            return 2;
        }

        if (narrowed < (nuint)0)
        {
            return 3;
        }

        nint signedNarrowed = (nint)rotated;
        // Compare against an obviously-smaller signed nint to drive signed clt/cgt.
        if (signedNarrowed == (nint)0)
        {
            return 4;
        }

        // `UIntPtr.Zero` and `IntPtr.Zero` lower to `cliTypeZeroOf`'s
        // `NativeIntSource.ManagedPointer ManagedPointerSource.Null`, not
        // to `Verbatim 0L`, so this exercises the OpaqueHashBits-vs-Null
        // unsigned comparison arms — distinct from the `(nuint)0` literal
        // path above (which lowers to `Verbatim 0L`).
        if (!(narrowed > UIntPtr.Zero))
        {
            return 5;
        }

        if (narrowed < UIntPtr.Zero)
        {
            return 6;
        }

        if (UIntPtr.Zero > narrowed)
        {
            return 7;
        }

        if (!(UIntPtr.Zero < narrowed))
        {
            return 8;
        }

        // Signed comparison against `IntPtr.Zero` exercises
        // `NativeIntSource.isLess` with OpaqueHashBits on one side and
        // `ManagedPointer Null` on the other. The signed direction of the
        // answer depends on the sign of the rotated bit pattern, which can
        // legitimately be negative when interpreted as signed nint, so check
        // a property both signs satisfy: the value is consistently ordered
        // against IntPtr.Zero in opposing directions.
        bool gtZero = signedNarrowed > IntPtr.Zero;
        bool ltZero = signedNarrowed < IntPtr.Zero;
        bool eqZero = signedNarrowed == IntPtr.Zero;
        // Trichotomy: exactly one of (>, <, ==) zero must hold.
        if ((gtZero ? 1 : 0) + (ltZero ? 1 : 0) + (eqZero ? 1 : 0) != 1)
        {
            return 9;
        }

        return 0;
    }
}
