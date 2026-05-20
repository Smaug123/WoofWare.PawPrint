using System;

public class Program
{
    // The C# compiler emits `box !!T` here. When T is bound to a reference type
    // (e.g. an szarray), the box token concretizes to a structural handle —
    // OneDimArrayZero — that AllConcreteTypes.lookup rightly does not store.
    // ECMA-335 III.4.1: box of a reference-type token is a no-op; the value
    // already on the stack is left unchanged.
    private static object BoxIt<T>(T value)
    {
        return (object)value;
    }

    public static int Main(string[] args)
    {
        int[] arr = { 1, 2, 3 };

        object boxed = BoxIt<int[]>(arr);
        if (!ReferenceEquals(boxed, arr)) return 1;

        // Multi-dim array — box of ConcreteTypeHandle.Array.
        int[,] mat = new int[2, 2];
        object boxedMat = BoxIt<int[,]>(mat);
        if (!ReferenceEquals(boxedMat, mat)) return 2;

        // Sanity: boxing a value type still works through the same generic helper.
        object boxedInt = BoxIt<int>(7);
        if (boxedInt is not int i || i != 7) return 3;

        return 0;
    }
}
