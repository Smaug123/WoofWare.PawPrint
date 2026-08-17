using System;

// A `ref readonly` return is emitted as `T& modreq(System.Runtime.InteropServices.InAttribute)` --
// the other way a return type carries a custom modifier, and the one where the modifier decorates a
// type that really does come back to the caller. It must stay value-returning.

public sealed class Cells
{
    private int[] values = new int[] { 10, 20, 30 };

    public ref readonly int At (int index)
    {
        return ref values[index];
    }

    public void Set (int index, int value)
    {
        values[index] = value;
    }
}

// The generic shape, which is what every one of the framework's `ref readonly` returns actually is
// (`!0&` rather than a concrete element type).
public sealed class Box<T>
{
    private T[] items;

    public Box (T[] items)
    {
        this.items = items;
    }

    public ref readonly T ItemRef (int index)
    {
        return ref items[index];
    }
}

public static class Program
{
    public static int Main ()
    {
        Cells cells = new Cells ();

        if (cells.At (0) != 10)
        {
            return 1;
        }

        if (cells.At (2) != 30)
        {
            return 2;
        }

        // A reference, not a copy: the write must be visible through it. Reading through the
        // returned reference *after* the mutation is what distinguishes the two.
        ref readonly int slot = ref cells.At (1);
        cells.Set (1, 99);

        if (slot != 99)
        {
            return 3;
        }

        Box<int> box = new Box<int> (new int[] { 4, 5 });

        if (box.ItemRef (1) != 5)
        {
            return 4;
        }

        return 0;
    }
}
