using System;

// The static constructor below has locals that interleave differently-shaped types:
// a mutable generic value type, a reference, a primitive, and a non-generic value type.
// `.locals init` must zero each slot according to *that* slot's declared type, so the
// concretized per-slot type array has to preserve the declaration order of the method's
// local signature.
//
// There are four declared locals rather than three on purpose. The failure this pins is a
// reversal of the per-slot type array, and reversing an odd-length list leaves the middle
// element at its own index; with three locals the middle slot would be typed correctly even
// under the bug. Four locals means no slot maps to itself, so every one of them detects the
// reversal. Don't reduce the count.
public struct Accumulator<T>
{
    public object Payload;
    public int Count;

    // Reads Payload through the `this` byref before any store, so a default (all-zero)
    // instance must present as a value type rather than as a null object reference.
    public void Add(T item)
    {
        if (Payload == null)
        {
            Payload = new object();
        }

        Count = Count + 1;
    }
}

public struct Offset
{
    public int Value;

    public void Bump()
    {
        Value = Value + 3;
    }
}

public static class CctorLocalOrdering
{
    static readonly int Result;

    // An explicit static constructor, so this body *is* the type's `.cctor` and its locals
    // go through the cctor local-variable concretization path.
    static CctorLocalOrdering()
    {
        Accumulator<char> acc = default;
        string refLocal = "x";
        int intLocal = 7;
        Offset offset = default;

        acc.Add('a');
        acc.Add('b');
        offset.Bump();

        Result = acc.Count + intLocal + refLocal.Length + offset.Value;
    }

    public static int Main(string[] argv)
    {
        // 2 (acc.Count) + 7 (intLocal) + 1 (refLocal.Length) + 3 (offset.Value) = 13.
        if (Result != 13)
        {
            return 1;
        }

        return 0;
    }
}
