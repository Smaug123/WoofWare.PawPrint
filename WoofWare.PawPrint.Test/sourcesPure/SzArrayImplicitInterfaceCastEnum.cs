// ECMA-335 / CoreCLR: when an SZ-array's element type is a value type, the
// element-compatibility rule (`CanCastParam`) reduces to "normalized
// integer width matches" — enums normalize to their underlying integer.
// So `MyEnum : int []` is element-compatible with `IList<int>`,
// `IList<uint>`, and `IList<OtherEnum>` where OtherEnum also has underlying
// int. Mismatched widths (enum on `byte` vs `int`) must answer false.

using System.Collections.Generic;

public enum IntKind : int
{
    Zero,
    One,
}

public enum UintKindMirror : uint
{
    Zero,
    One,
}

public enum ByteKind : byte
{
    Zero,
    One,
}

public class TestSzArrayImplicitInterfaceCastEnum
{
    public static int Main(string[] argv)
    {
        object payload = new IntKind[] { IntKind.One };

        // Enum → underlying int.
        IList<int> asInt = (IList<int>) payload;
        if (asInt == null) return 1;

        // Enum → underlying uint partner (same normalized width).
        IEnumerable<uint> asUint = (IEnumerable<uint>) payload;
        if (asUint == null) return 2;

        // Enum → sibling enum sharing the same normalized width.
        IReadOnlyList<UintKindMirror> asMirror = (IReadOnlyList<UintKindMirror>) payload;
        if (asMirror == null) return 3;

        // isinst partners agree.
        if (!(payload is IList<int>)) return 4;
        if (!(payload is IEnumerable<uint>)) return 5;
        if (!(payload is IReadOnlyList<UintKindMirror>)) return 6;

        // Width mismatch: ByteKind[]'s element normalises to SByte; not
        // compatible with IList<int>.
        object widthMismatch = new ByteKind[] { ByteKind.One };
        if (widthMismatch is IList<int>) return 7;

        bool threw = false;
        try
        {
            IList<int> _ = (IList<int>) widthMismatch;
        }
        catch (System.InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 8;

        return 0;
    }
}
