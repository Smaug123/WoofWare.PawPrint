using System.Runtime.CompilerServices;

namespace UnsafeAsRefVoidPointer
{
    // Exercises the `Unsafe.AsRef<T>(void* source)` overload. BCL code (notably
    // MemoryMarshal.GetNonNullPinnableReference for empty spans) fabricates a
    // non-null managed reference via `Unsafe.AsRef<T>((void*)1)` so the
    // following `fixed` pins to a non-null pointer. The resulting ref must
    // never be dereferenced; only IsNullRef, AreSame, and round-tripping back
    // through `Unsafe.AsPointer` (which `fixed` does) are legitimate.
    public class Program
    {
        public static unsafe int Main(string[] args)
        {
            // (void*)0 → ref must be the null managed pointer.
            ref int nullRef = ref Unsafe.AsRef<int>((void*)0);
            if (!Unsafe.IsNullRef(in nullRef)) return 1;

            // (void*)1 → ref must be non-null (the contract that
            // GetNonNullPinnableReference relies on for empty spans).
            ref int oneRef = ref Unsafe.AsRef<int>((void*)1);
            if (Unsafe.IsNullRef(in oneRef)) return 2;

            // Two calls with the same bit pattern produce structurally equal
            // managed references.
            ref int oneRefAgain = ref Unsafe.AsRef<int>((void*)1);
            if (!Unsafe.AreSame(ref oneRef, ref oneRefAgain)) return 3;

            // Distinct bit patterns produce distinct managed references.
            ref int twoRef = ref Unsafe.AsRef<int>((void*)2);
            if (Unsafe.AreSame(ref oneRef, ref twoRef)) return 4;

            // Pointer arithmetic on a placeholder advances the bit pattern.
            // BCL code computes end pointers for empty spans this way.
            // Build `(byte*)(void*)1 + 8` from the literal — the addition
            // happens on the placeholder's bit pattern.
            byte* startBytes = (byte*)(void*)1;
            byte* advancedBytes = startBytes + 8;
            ref int advanced = ref Unsafe.AsRef<int>(advancedBytes);
            if (Unsafe.IsNullRef(in advanced)) return 5;
            if (Unsafe.AreSame(ref oneRef, ref advanced)) return 6;

            // Two independently constructed placeholders with the same final
            // bits compare equal under AreSame.
            byte* otherStart = (byte*)(void*)1;
            byte* otherAdvanced = otherStart + 8;
            ref int advancedAgain = ref Unsafe.AsRef<int>(otherAdvanced);
            if (!Unsafe.AreSame(ref advanced, ref advancedAgain)) return 7;

            // Arithmetic that lands back on zero must normalise to the null
            // managed pointer so `Unsafe.IsNullRef` agrees with the CLR's
            // bit-pattern definition.
            byte* normalisedBytes = startBytes - 1;
            ref int normalised = ref Unsafe.AsRef<int>(normalisedBytes);
            if (!Unsafe.IsNullRef(in normalised)) return 8;

            // `Unsafe.AsPointer` must round-trip the placeholder back to the
            // original bit pattern. Casting that pointer to IntPtr/nint must
            // give the original literal so downstream code can compare it
            // against the magic constant it threaded through.
            void* roundTrip = Unsafe.AsPointer(ref oneRef);
            if ((nint)roundTrip != 1) return 9;

            void* advancedRoundTrip = Unsafe.AsPointer(ref advanced);
            if ((nint)advancedRoundTrip != 9) return 10;

            // `Unsafe.ByteOffset` between two placeholders is the bit
            // difference, matching the IL `sub` semantics.
            ref byte byteOne = ref Unsafe.AsRef<byte>((void*)1);
            ref byte byteNine = ref Unsafe.AsRef<byte>((void*)9);
            nint delta = Unsafe.ByteOffset(ref byteOne, ref byteNine);
            if (delta != 8) return 11;

            nint reverseDelta = Unsafe.ByteOffset(ref byteNine, ref byteOne);
            if (reverseDelta != -8) return 12;

            // Null is bit pattern 0; ByteOffset with Null on one side is
            // still well-defined as bit subtraction.
            ref byte byteZero = ref Unsafe.AsRef<byte>((void*)0);
            nint zeroDelta = Unsafe.ByteOffset(ref byteZero, ref byteOne);
            if (zeroDelta != 1) return 13;

            return 0;
        }
    }
}
