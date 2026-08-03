// ECMA-335 III.4.33: when the `unbox.any` type token denotes a reference type, the
// instruction has exactly the semantics of `castclass`. Array types are reference
// types, so `unbox.any int32[]` must pass an `int32[]` operand through unchanged.
//
// C# emits `unbox.any !!T` for `(T)o` in a generic method, so `Cast<int[]>` is the
// shortest route to an `unbox.any` whose type token concretizes to an array handle
// rather than to a nominal TypeDef.

public class TestUnboxAnyArrayTarget
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        object boxed = new int[] { 1, 2, 3 };

        int[] arr = Cast<int[]>(boxed);

        if (arr == null) return 1;
        if (arr.Length != 3) return 2;
        if (arr[0] != 1) return 3;
        if (arr[1] != 2) return 4;
        if (arr[2] != 3) return 5;

        // `unbox.any` on a reference type pushes the *same* object reference, not a copy.
        if (!object.ReferenceEquals(arr, boxed)) return 6;

        // Mutating through the cast reference is visible through the original.
        arr[0] = 42;
        if (((int[]) boxed)[0] != 42) return 7;

        return 0;
    }
}
