// `unbox.any` with a rank>1 array token. These concretize to a different structural
// handle from SZ-arrays (rank is part of the type identity), so they are worth pinning
// separately from UnboxAnyArrayTarget.cs.

public class TestUnboxAnyMultiDimArrayTarget
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        int[,] grid = new int[2, 3];
        grid[0, 0] = 1;
        grid[1, 2] = 7;

        object boxed = grid;

        int[,] back = Cast<int[,]>(boxed);

        if (back == null) return 1;
        if (back.Rank != 2) return 2;
        if (back.Length != 6) return 3;
        if (back[0, 0] != 1) return 4;
        if (back[1, 2] != 7) return 5;
        if (!object.ReferenceEquals(back, grid)) return 6;

        // A rank-3 token must not accept a rank-2 operand.
        bool threw = false;
        try
        {
            int[,,] _ = Cast<int[,,]>(boxed);
        }
        catch (System.InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 7;

        return 0;
    }
}
