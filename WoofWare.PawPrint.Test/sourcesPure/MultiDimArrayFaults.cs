using System;

// The runtime-synthesized `Get`/`Set`/`Address` members of a multi-dimensional array are reached
// from a plain `call`, so what they fault with is a *callee* fault and not the `call` opcode's.
// PawPrint had no guest exercising any of those fault paths, which is how a change that routed
// them through the opcode-fault checker passed the whole suite while turning every one of them
// into an interpreter crash.
//
// Returns 0 when all six faulted as they should. Otherwise it returns a bitmask of the ones that
// did *not*, so a partial regression says which path broke rather than only that something did.
// Every value stays under 128, where an exit code would start colliding with signal termination.
public class Program
{
    public static int Main(string[] args)
    {
        int faulted = 0;

        // Get, index past the end.
        int[,] a = new int[1, 1];
        try { _ = a[1, 0]; }
        catch (IndexOutOfRangeException) { faulted |= 1; }

        // Get, negative index. A separate bit: the bounds check is two-sided, and a regression
        // that only screened the upper end would still pass the case above.
        try { _ = a[-1, 0]; }
        catch (IndexOutOfRangeException) { faulted |= 2; }

        // Set, index past the end.
        try { a[0, 1] = 5; }
        catch (IndexOutOfRangeException) { faulted |= 4; }

        // Address, index past the end. Taken through a `ref` local, which is what compiles to the
        // synthesized `Address` member rather than to `Get`.
        try { ref int slot = ref a[0, 1]; slot = 7; }
        catch (IndexOutOfRangeException) { faulted |= 8; }

        // Get on a null array.
        int[,] nil = null;
        try { _ = nil[0, 0]; }
        catch (NullReferenceException) { faulted |= 16; }

        // Set on a null array.
        try { nil[0, 0] = 1; }
        catch (NullReferenceException) { faulted |= 32; }

        // A store and a load that must *not* fault, so this cannot pass by everything throwing.
        a[0, 0] = 9;
        if (a[0, 0] != 9)
        {
            return 64;
        }

        return 63 & ~faulted;
    }
}
