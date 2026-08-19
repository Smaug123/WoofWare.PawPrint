using System;

// ECMA-335 II.23.3 encodes an SZARRAY-valued attribute argument as a 4-byte element count
// followed by that many bare `Elem`s. The count 0xFFFFFFFF is the null-array sentinel, which is
// a different thing from a zero-length array, and both are spelled here.
//
// The element type comes from the constructor's parameter, not from the blob, so an enum-typed
// element is written as a bare value of the enum's underlying type — the same width problem the
// scalar case has, one level down.

public enum Level : short
{
    Low = 1,
    High = 4321,
}

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
public sealed class ArraysAttribute : Attribute
{
    // A trailing scalar after the arrays, so a wrong element width or a miscounted array
    // desynchronises the blob cursor and corrupts this too rather than failing silently.
    public ArraysAttribute(byte[] bytes, int[] ints, string[] strings, Level[] levels, int tail)
    {
        Bytes = bytes;
        Ints = ints;
        Strings = strings;
        Levels = levels;
        Tail = tail;
    }

    public byte[] Bytes { get; }
    public int[] Ints { get; }
    public string[] Strings { get; }
    public Level[] Levels { get; }
    public int Tail { get; }
}

[Arrays(new byte[] { 2, 1 }, new int[] { -1, 2147483647 }, new string[] { "a", null, "" }, new Level[] { Level.Low, Level.High }, 4242)]
[Arrays(null, new int[0], null, null, 99)]
public sealed class Decorated
{
}

public class CustomAttributeArrayArg
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    public static int Main()
    {
        object[] attrs = typeof(Decorated).GetCustomAttributes(typeof(ArraysAttribute), false);
        Check(attrs.Length == 2);
        if (attrs.Length != 2)
        {
            return firstFailure;
        }

        // Attribute order within a decorated type is not something this test should pin, so pick
        // the two applications apart by a value only one of them carries.
        ArraysAttribute populated = null;
        ArraysAttribute degenerate = null;
        for (int i = 0; i < attrs.Length; i++)
        {
            ArraysAttribute a = (ArraysAttribute)attrs[i];
            if (a.Tail == 4242)
            {
                populated = a;
            }
            else if (a.Tail == 99)
            {
                degenerate = a;
            }
        }

        Check(populated != null);
        Check(degenerate != null);
        if (populated == null || degenerate == null)
        {
            return firstFailure;
        }

        Check(populated.Bytes != null);
        Check(populated.Bytes.Length == 2);
        Check(populated.Bytes[0] == 2);
        Check(populated.Bytes[1] == 1);

        Check(populated.Ints != null);
        Check(populated.Ints.Length == 2);
        Check(populated.Ints[0] == -1);
        Check(populated.Ints[1] == 2147483647);

        Check(populated.Strings != null);
        Check(populated.Strings.Length == 3);
        Check(populated.Strings[0] == "a");
        Check(populated.Strings[1] == null);
        Check(populated.Strings[2] != null && populated.Strings[2].Length == 0);

        Check(populated.Levels != null);
        Check(populated.Levels.Length == 2);
        Check(populated.Levels[0] == Level.Low);
        Check(populated.Levels[1] == Level.High);

        // The declared element type survives: the array really is a byte[], not an int[] that
        // happens to hold the right numbers.
        Check(populated.Bytes.GetType() == typeof(byte[]));
        Check(populated.Levels.GetType() == typeof(Level[]));

        // The null sentinel and the empty array are distinct encodings and must stay distinct.
        Check(degenerate.Bytes == null);
        Check(degenerate.Strings == null);
        Check(degenerate.Levels == null);
        Check(degenerate.Ints != null);
        Check(degenerate.Ints.Length == 0);

        return firstFailure;
    }
}
