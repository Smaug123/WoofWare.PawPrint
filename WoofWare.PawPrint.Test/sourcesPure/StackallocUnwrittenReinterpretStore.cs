using System.Runtime.CompilerServices;

// A value read from a `stackalloc` slot nothing wrote is stored through a byref that views a
// struct as another struct of the same layout (`Unsafe.As<TFrom, TTo>`), as a whole struct and
// as one field, and is then overwritten or dropped without ever being used. The exit code depends
// only on bytes that were written, so it is the same on every runtime whatever the stack held.

[module: SkipLocalsInit]

struct One
{
    public int X;
}

struct OtherOne
{
    public int Y;
}

struct Two
{
    public int X;
    public int Y;
}

struct OtherTwo
{
    public int P;
    public int Q;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        int* unwritten = stackalloc int[4];

        // A one-field struct, through a field of the view.
        One one = default;
        Unsafe.As<One, OtherOne>(ref one).Y = unwritten[0];
        one.X = 3;
        if (one.X != 3) return 1;

        // A one-field struct, as the whole of the view.
        One whole = default;
        OtherOne replacement;
        replacement.Y = unwritten[1];
        Unsafe.As<One, OtherOne>(ref whole) = replacement;
        whole.X = 4;
        if (whole.X != 4) return 2;

        // A two-field struct, through one field of the view: the other field keeps what was
        // written to it.
        Two two = default;
        two.Y = 5;
        Unsafe.As<Two, OtherTwo>(ref two).P = unwritten[2];
        if (two.Y != 5) return 3;
        two.X = 6;
        if (two.X != 6) return 4;

        // A two-field struct, as the whole of the view, half of which nothing wrote.
        Two halves = default;
        OtherTwo half;
        half.P = unwritten[3];
        half.Q = 7;
        Unsafe.As<Two, OtherTwo>(ref halves) = half;
        if (halves.Y != 7) return 5;
        halves.X = 8;
        if (halves.X != 8) return 6;

        return one.X + whole.X + two.Y + two.X + halves.Y + halves.X - 33;
    }
}
