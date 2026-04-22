public class Program
{
    private enum E
    {
        Zero = 0,
        One = 1,
        Two = 2,
    }

    public static int Main(string[] args)
    {
        // Store to an enum local via ldc.i4 + stloc; subsequent loads must still
        // compare and cast correctly. Before the EvalStack pop-side rewrap fix,
        // stloc into an enum local silently degraded storage from ValueType to
        // a bare Numeric, which other code paths tolerated only coincidentally.
        E a = E.Two;
        E b = E.Two;
        E c = E.One;

        if (a != b) return 1;
        if (a == c) return 2;
        if ((int)a != 2) return 3;
        if ((int)c != 1) return 4;

        // Mutate in place and re-observe.
        a = E.Zero;
        if (a == b) return 5;
        if ((int)a != 0) return 6;

        return 0;
    }
}
