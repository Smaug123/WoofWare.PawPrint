// ECMA-335 III.4.32 (`unbox`), as distinct from III.4.33 (`unbox.any`): it pushes a managed
// pointer *into* the boxed object rather than a copy of the object's contents.
//
// C# reaches this opcode by reading a field off a cast result: `((Point) o).X` compiles to
// `unbox Point; ldfld Point::X`, because the field read needs an address. A *method* call on a
// cast result does not reach it -- Roslyn spills through `unbox.any; stloc; ldloca` so that a
// mutating method cannot write back into the box -- which is also why nothing here observes the
// aliasing directly; see UnaryMetadataObjectOps.executeUnbox for why the pointer aliases anyway.
//
// The type test is shared with the value-type arm of `unbox.any` (CoreCLR routes both through
// `CastHelpers.Unbox_Helper`), so the negatives below pin that `unbox` agrees with it.

using System;

public struct Point
{
    public int X;
    public int Y;
}

public struct Outer
{
    public Point Inner;
    public int Tag;
}

public readonly struct ReadOnlyPoint
{
    public readonly int X;

    public ReadOnlyPoint(int x)
    {
        X = x;
    }
}

public struct WithRef
{
    public string S;
    public int N;
}

public struct Wrapper<T>
{
    public T Item;
}

public class TestUnboxFieldAccess
{
    public static int Main(string[] argv)
    {
        object boxedPoint = new Point
        {
            X = 3,
            Y = 4,
        };

        if (((Point) boxedPoint).X != 3) return 1;
        if (((Point) boxedPoint).Y != 4) return 2;

        // Reading the same box twice through two separate `unbox` instructions agrees.
        if (((Point) boxedPoint).X + ((Point) boxedPoint).Y != 7) return 3;

        // A projection chain on top of the byref: `unbox Outer; ldflda Outer::Inner; ldfld Point::Y`.
        object boxedOuter = new Outer
        {
            Inner = new Point
            {
                X = 10,
                Y = 20,
            },
            Tag = 30,
        };

        if (((Outer) boxedOuter).Inner.X != 10) return 4;
        if (((Outer) boxedOuter).Inner.Y != 20) return 5;
        if (((Outer) boxedOuter).Tag != 30) return 6;

        // readonly structs take the same path.
        object boxedReadOnly = new ReadOnlyPoint(11);
        if (((ReadOnlyPoint) boxedReadOnly).X != 11) return 7;

        // A payload containing a GC reference reads back through the byref as an objref, not
        // as bytes.
        object boxedWithRef = new WithRef
        {
            S = "hello",
            N = 12,
        };

        if (((WithRef) boxedWithRef).S != "hello") return 8;
        if (((WithRef) boxedWithRef).N != 12) return 9;

        // Generic value types: the token is a TypeSpec rather than a TypeDef.
        object boxedWrapper = new Wrapper<int>
        {
            Item = 13,
        };

        if (((Wrapper<int>) boxedWrapper).Item != 13) return 10;

        object boxedWrapperOfString = new Wrapper<string>
        {
            Item = "world",
        };

        if (((Wrapper<string>) boxedWrapperOfString).Item != "world") return 11;

        // Wrong boxed type is an InvalidCastException, exactly as for `unbox.any`.
        if (!ThrowsInvalidCast((object) new ReadOnlyPoint(1))) return 12;
        if (!ThrowsInvalidCast((object) 1)) return 13;
        if (!ThrowsInvalidCast("not a Point")) return 14;
        if (!ThrowsInvalidCast(new int[1])) return 15;

        // Null is a NullReferenceException, not an InvalidCastException: `unbox` has no
        // Nullable-shaped escape hatch the way `unbox.any` does.
        if (!ThrowsNullReference(null)) return 16;

        // Mutating the box through a normal write is visible to a later `unbox`, so the
        // pointer really is re-read each time rather than cached from the box's creation.
        boxedPoint = new Point
        {
            X = 5,
            Y = 6,
        };

        if (((Point) boxedPoint).X != 5) return 17;

        // A real BCL user of the instruction, so this exercises it against CoreLib's own IL and
        // not just against this assembly's: `System.Index.Equals(object)` is compiled as
        // `isinst Index; unbox Index; ldfld Index::_value` (it is the only bare `unbox` in all of
        // System.Private.CoreLib).
        Index index = new Index(3);
        if (!index.Equals((object) new Index(3))) return 18;
        if (index.Equals((object) new Index(4))) return 19;

        // The `isinst` guard means a non-Index never reaches the `unbox`.
        if (index.Equals((object) "not an index")) return 20;
        if (index.Equals((object) null)) return 21;

        return 0;
    }

    // The field read must be *used*, or Roslyn drops the `ldfld` and degrades the whole
    // expression to `unbox.any; pop` -- which would leave these negatives testing the wrong
    // instruction. Storing to a static keeps the read alive.
    private static int sink;

    private static bool ThrowsInvalidCast(object o)
    {
        try
        {
            sink = ((Point) o).X;
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    private static bool ThrowsNullReference(object o)
    {
        try
        {
            sink = ((Point) o).X;
            return false;
        }
        catch (NullReferenceException)
        {
            return true;
        }
    }
}
