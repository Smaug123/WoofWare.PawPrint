// `unbox.any Nullable<T>` where `T` is a genuine multi-field user struct rather than a primitive.
// `box` stores a primitive in a synthetic single-field struct but stores a multi-field struct
// directly, so the two take different arms when the value is read back out; this pins the
// multi-field arm, including that every field survives the round trip.

using System;

public struct Point
{
    public int X;
    public long Y;
    public bool Flag;

    public Point(int x, long y, bool flag)
    {
        X = x;
        Y = y;
        Flag = flag;
    }
}

public class TestUnboxAnyNullableStruct
{
    private static T Cast<T>(object o)
    {
        return (T) o;
    }

    public static int Main(string[] argv)
    {
        object boxed = new Point(3, 4000000000L, true);

        Point? p = (Point?) boxed;
        if (!p.HasValue) return 1;
        if (p.Value.X != 3) return 2;
        if (p.Value.Y != 4000000000L) return 3;
        if (!p.Value.Flag) return 4;

        // Same through the `unbox.any !!T` token form.
        Point? q = Cast<Point?>(boxed);
        if (!q.HasValue) return 5;
        if (q.Value.X != 3) return 6;
        if (q.Value.Y != 4000000000L) return 7;
        if (!q.Value.Flag) return 8;

        // Null still yields a zeroed Nullable, and the payload is default(Point).
        Point? none = (Point?) (object) null;
        if (none.HasValue) return 9;

        Point zero = none.GetValueOrDefault();
        if (zero.X != 0) return 10;
        if (zero.Y != 0L) return 11;
        if (zero.Flag) return 12;

        // A different struct of the same shape is still a different type.
        bool threw = false;
        try
        {
            Point _ = (Point) (object) 3;
        }
        catch (InvalidCastException)
        {
            threw = true;
        }

        if (!threw) return 13;

        return 0;
    }
}
