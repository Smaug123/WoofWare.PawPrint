using System.Runtime.CompilerServices;

// Tests cgt.un (compare-greater-than, unsigned/unordered) with object references.
// By extracting the comparison into a NoInlining helper that returns bool,
// the compiler must materialise the cgt.un result into a 0/1 value
// rather than folding it into a branch.

public class CgtUn
{
    // obj != null compiles to: ldarg.0; ldnull; cgt.un
    // The NoInlining attribute prevents the JIT/compiler from folding this into a branch.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool IsNonNull(object o)
    {
        return o != null;
    }

    // null != obj also compiles to cgt.un (with reversed operands or ceq+ldc.i4.0+ceq).
    // We test the materialised boolean result.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool IsNull(object o)
    {
        return o == null;
    }

    public static int Main(string[] argv)
    {
        object obj = new object();

        // Non-null object: IsNonNull should be true
        if (!IsNonNull(obj)) return 1;

        // Null: IsNonNull should be false
        if (IsNonNull(null)) return 2;

        // Null: IsNull should be true
        if (!IsNull(null)) return 3;

        // Non-null: IsNull should be false
        if (IsNull(obj)) return 4;

        return 0;
    }
}
