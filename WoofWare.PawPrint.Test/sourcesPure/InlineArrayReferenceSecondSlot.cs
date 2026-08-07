using System;
using System.Runtime.CompilerServices;

// Element 1 of an `[InlineArray(2)]` over a *reference* element: the intersection of the two gaps
// its siblings isolate. `InlineArraySecondSlot.cs` covers indexing past slot 0 with a primitive
// element (which the N-slot layout alone fixes, because the byte view then lands inside the
// aggregate cell); `InlineArrayOfReferenceType*.cs` cover slot 0 with a reference element (which
// needs the byref reinterpret to resolve to the field cell, because object references have no byte
// rendering). Slot 1 of a reference element needs both at once: the byref is
// `[ReinterpretAs T; ByteOffset 8]`, and the storage it lands on is not byte-addressable.
//
// All three roots are exercised because the write dispatcher picks its writer from the root: a
// local falls to the structural writer, whereas a class field and an array element are routed to
// the bytes-or-typed-cell writer unless the transparent-slot classifier redirects them.
public class TestInlineArrayReferenceSecondSlot
{
    [InlineArray(2)]
    private struct Pair<T>
    {
        private T _item;

        // Ordinary `ldfld` of the declared field: this must still see slot 0, so it doubles as a
        // check that the synthesised slot-1 cell has not displaced the metadata field's identity.
        public T First => _item;
    }

    private sealed class Holder
    {
        public Pair<string> Buffer;
    }

    public static int Main(string[] argv)
    {
        // Root: a local.
        Pair<string> local = default;
        local[0] = "zero";
        local[1] = "one";
        if (local[0] != "zero") return 1;
        if (local[1] != "one") return 2;
        if (local.First != "zero") return 3;

        // Writing one slot must not disturb the other.
        local[1] = null;
        if (local[0] != "zero") return 4;
        if (local[1] != null) return 5;

        // Root: a field of a heap-allocated class.
        Holder holder = new Holder();
        holder.Buffer[0] = "a";
        holder.Buffer[1] = "b";
        if (holder.Buffer[0] != "a") return 6;
        if (holder.Buffer[1] != "b") return 7;
        if (holder.Buffer.First != "a") return 8;

        // Root: an element of a heap-allocated array. Slots of *different* elements must be
        // independent too, which needs the element stride to be the whole two-slot run.
        Pair<string>[] arr = new Pair<string>[2];
        arr[0][0] = "p";
        arr[0][1] = "q";
        arr[1][1] = "r";
        if (arr[0][0] != "p" || arr[0][1] != "q") return 9;
        if (arr[1][0] != null || arr[1][1] != "r") return 10;

        // Copying the aggregate by value copies both slots.
        Pair<string> copy = local;
        copy[1] = "changed";
        if (local[1] != null) return 11;
        if (copy[0] != "zero" || copy[1] != "changed") return 12;

        return 0;
    }
}
