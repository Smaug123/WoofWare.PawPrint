public class ArrayTypeTokenHelper { }

public class ArrayTypeToken
{
    public static int Main(string[] argv)
    {
        int result = 0;

        // typeof(X[]) must not resolve to the same type as typeof(X).
        // This catches a bug where resolveTypeFromDefn collapsed array
        // wrappers, returning the element type's identity instead.
        var arrayType = typeof(ArrayTypeTokenHelper[]);
        var elementType = typeof(ArrayTypeTokenHelper);

        if (arrayType == elementType)
        {
            result |= 1;
        }

        if (arrayType == null)
        {
            result |= 2;
        }

        // typeof(X*) must not resolve to the same type as typeof(X).
        unsafe
        {
            var pointerType = typeof(int*);
            var intType = typeof(int);

            if (pointerType == intType)
            {
                result |= 4;
            }

            if (pointerType == null)
            {
                result |= 8;
            }
        }

        // Byref (ref) and pinned types cannot appear in typeof() in C#,
        // so they are not directly testable here. They only arise in
        // method/field signatures and local variable signatures respectively.

        // Array rank must be part of type identity: int[,] and int[,,] are
        // distinct types, and both differ from the szarray int[].
        var twoDim = typeof(int[,]);
        var threeDim = typeof(int[,,]);
        var szArray = typeof(int[]);

        if (twoDim == null)
        {
            result |= 16;
        }

        if (threeDim == null)
        {
            result |= 32;
        }

        if (twoDim == threeDim)
        {
            result |= 64;
        }

        if (twoDim == szArray)
        {
            result |= 128;
        }

        if (threeDim == szArray)
        {
            result |= 256;
        }

        return result;
    }
}
