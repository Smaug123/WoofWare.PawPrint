// Companion to UnboxAnyArrayTarget.cs, covering the other half of the same opcode:
// here the *type token* is an ordinary nominal reference type but the *operand* is an
// array object. `unbox.any` must consult the array side of the managed heap to find
// the operand's runtime type, exactly as `castclass` does.
//
// `int[]` derives from `System.Array` and implements the non-generic
// `System.Collections.IList`, so both casts succeed; `System.String` is unrelated, so
// that cast raises InvalidCastException.

using System;
using System.Collections;

public class TestUnboxAnyArrayOperandRefTarget
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        object boxed = new int[] { 1, 2, 3 };

        Array asArray = Cast<Array>(boxed);
        if (asArray == null) return 1;
        if (asArray.Length != 3) return 2;

        IList asList = Cast<IList>(boxed);
        if (asList == null) return 3;

        object asObject = Cast<object>(boxed);
        if (!object.ReferenceEquals(asObject, boxed)) return 4;

        bool threw = false;
        try
        {
            string _ = Cast<string>(boxed);
        }
        catch (InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 5;

        return 0;
    }
}
