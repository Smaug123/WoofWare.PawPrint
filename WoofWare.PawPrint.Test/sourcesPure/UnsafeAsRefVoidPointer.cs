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

            return 0;
        }
    }
}
