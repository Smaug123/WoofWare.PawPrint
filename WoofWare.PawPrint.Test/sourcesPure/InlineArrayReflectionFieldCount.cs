using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// An `[InlineArray(N)]` type has exactly *one* FieldDesc however large N is: the N-1 repeats are
// storage, not fields (`MethodTableBuilder::PlaceInstanceFields` multiplies the instance size and
// never adds a FieldDesc, methodtablebuilder.cpp:8612). PawPrint's value storage is field-cell
// based, so it has to materialise a cell per slot; this test pins down that those synthesised
// cells stay on the storage side and never leak into metadata-driven reflection.
//
// Split out from `InlineArrayLayout.cs` so that a gap in reflection cannot hold the layout coverage
// hostage.
public class TestInlineArrayReflectionFieldCount
{
    [InlineArray(4)] private struct BufInt { private int _item; }
    [InlineArray(1)] private struct One { private int _item; }

    private const BindingFlags Instance =
        BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

    public static int Main(string[] argv)
    {
        FieldInfo[] many = typeof(BufInt).GetFields(Instance);
        if (many.Length != 1) return 1;
        if (many[0].Name != "_item") return 2;
        if (many[0].FieldType != typeof(int)) return 3;

        FieldInfo[] one = typeof(One).GetFields(Instance);
        if (one.Length != 1) return 4;
        if (one[0].Name != "_item") return 5;

        return 0;
    }
}
