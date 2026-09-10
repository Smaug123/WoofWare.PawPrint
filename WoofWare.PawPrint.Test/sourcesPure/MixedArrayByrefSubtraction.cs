using System;
using System.Runtime.CompilerServices;

// Subtracting two byrefs into one array where only one of them carries a byte view.
//
// `subManagedPtrs` has both mixed-direction arms for stack-memory, native-memory and
// string roots, and neither of them for array roots, so this pair reaches the catch-all
// refusal. The two spellings below differ only in how the byref got onto the native-int
// stack: `Unsafe.AsPointer` transports a plain array-element byref, while `fixed` goes
// through `conv.u`, which anchors a byte view on it.
//
// Returns 0 on success, or the number of the first check that failed.
public class Program
{
    public static unsafe int Main (string[] args)
    {
        int[] a = new int[4] { 10, 20, 30, 40 };

        int* unanchored = (int*) Unsafe.AsPointer (ref a[0]);

        fixed (int* anchored = a)
        {
            // 1: the same address by two routes, so the distance between them is zero.
            if (anchored - unanchored != 0)
                return 1;

            // 2: and the distance is measured in elements, as C# pointer subtraction is.
            if ((anchored + 2) - unanchored != 2)
                return 2;

            // 3: the other direction of the same pair.
            if (unanchored - (anchored + 3) != -3)
                return 3;
        }

        return 0;
    }
}
