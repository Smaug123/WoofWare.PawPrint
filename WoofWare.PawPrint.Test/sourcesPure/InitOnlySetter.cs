using System;

// A C# `init` accessor is emitted as `set_X` whose return type is
// `void modreq(System.Runtime.CompilerServices.IsExternalInit)`. Nothing about the
// modifier changes what the method returns, so `ret` in such a setter must be
// accepted as a void return.

public sealed class Config
{
    public int Width { get; init; }
    public string Name { get; init; }

    // An ordinary void setter alongside, so a failure is attributable to the
    // modifier rather than to property setters in general.
    public int Height { get; set; }
}

public readonly struct Sized
{
    public int Side { get; init; }
}

public record Pair(int First, int Second);

public static class Program
{
    public static int Main()
    {
        Config c = new Config
        {
            Width = 7,
            Name = "hello",
            Height = 9,
        };

        if (c.Width != 7)
        {
            return 1;
        }

        if (c.Name != "hello")
        {
            return 2;
        }

        if (c.Height != 9)
        {
            return 3;
        }

        // A value type's init setter: the same modifier, but the setter's `this`
        // is a byref rather than an object reference.
        Sized s = new Sized
        {
            Side = 5,
        };

        if (s.Side != 5)
        {
            return 4;
        }

        // A positional record: the primary constructor assigns through
        // compiler-generated init accessors.
        Pair p = new Pair(11, 13);

        if (p.First != 11)
        {
            return 5;
        }

        if (p.Second != 13)
        {
            return 6;
        }

        // `with` on a record runs the copy constructor and then another init
        // setter, on an instance that already holds a value.
        Pair q = p with
        {
            Second = 17,
        };

        if (q.First != 11)
        {
            return 7;
        }

        if (q.Second != 17)
        {
            return 8;
        }

        if (p.Second != 13)
        {
            return 9;
        }

        return 0;
    }
}
