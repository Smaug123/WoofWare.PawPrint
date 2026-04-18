using System;

public class Program
{
    private static bool IsNull(object o)
    {
        return o == null;
    }

    public static int Main(string[] args)
    {
        // Boxing a null Nullable<T> must produce a null reference.
        // Bug: our box instruction creates a non-null heap object.
        int? nullVal = null;
        if (!IsNull((object)nullVal)) return 1;

        // Boxing a non-null Nullable<T> should produce a boxed T, not Nullable<T>.
        int? nonNullVal = 42;
        object boxed = (object)nonNullVal;
        if (IsNull(boxed)) return 2;

        return 0;
    }
}
