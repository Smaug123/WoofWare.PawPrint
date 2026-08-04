using System;
using System.Runtime.CompilerServices;

public class StelemTypeReference
{
    // TimeSpan is a value type defined in System.Private.CoreLib, so `stelem TimeSpan`
    // in this assembly encodes its element-type token as a bare TypeReference
    // (not a TypeDefinition -- TimeSpan isn't declared here -- and not a
    // TypeSpecification, since TimeSpan is non-generic). The general `stelem <type>`
    // form (as opposed to a specialised stelem.i4/stelem.ref/etc) is what Roslyn
    // emits for any non-primitive value-type array element. (Guid was tried first,
    // but its `==` reaches a SIMD bitcast path PawPrint doesn't support yet, which
    // is an unrelated gap -- TimeSpan's equality is a plain field comparison.)
    [MethodImpl(MethodImplOptions.NoInlining)]
    static TimeSpan[] MakeArray (int len)
    {
        return new TimeSpan[len];
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void SetElement (TimeSpan[] arr, int index, TimeSpan value)
    {
        arr[index] = value;
    }

    public static int Main (string[] argv)
    {
        TimeSpan[] arr = MakeArray (3);

        TimeSpan t0 = new TimeSpan (1, 2, 3);
        TimeSpan t1 = new TimeSpan (4, 5, 6);
        TimeSpan t2 = new TimeSpan (7, 8, 9);

        SetElement (arr, 0, t0);
        SetElement (arr, 1, t1);
        SetElement (arr, 2, t2);

        if (arr[0] != t0) return 1;
        if (arr[1] != t1) return 2;
        if (arr[2] != t2) return 3;

        // Overwrite an already-populated slot too.
        SetElement (arr, 1, t2);
        if (arr[1] != t2) return 4;

        return 0;
    }
}
