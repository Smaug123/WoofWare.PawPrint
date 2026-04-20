public class ArrayTypeTokenHelper { }

public class ArrayTypeToken
{
    public static int Main(string[] argv)
    {
        int result = 0;

        // Loading typeof(X[]) where X is a user-defined type triggers
        // resolveTypeFromDefn with OneDimensionalArrayLowerBoundZero.
        // This test verifies the interpreter can resolve such type tokens.
        var t = typeof(ArrayTypeTokenHelper[]);
        if (t == null)
        {
            result |= 1;
        }

        return result;
    }
}
