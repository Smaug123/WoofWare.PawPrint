using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace ByteViewWriteCrossesContainerTest
{
    struct Pair
    {
        public int X;
        public int Y;
    }

    class Holder
    {
        public long A;
        public long B;
        public long C;
    }

    // Writing through a byref that has walked out of the storage its root names.
    //
    // The read direction of this is covered by `AreSameProjectionCrossesArrayElement.cs` and
    // `GCMemoryInfoSpanProperties.cs`, but reads cannot reach the write path: it is a separate
    // mirror of the same cell walk, and both of those guests build their spans with
    // `CreateReadOnlySpan`. Without this file, half of the change would be untested and the
    // suite would still be green.
    //
    // Both container kinds appear, because they route to different writers. `Pair[]` steps out
    // of an array *element* into the array; `Holder` steps out of a class *field* into the
    // object. The two are separate arms and a fix for one does not imply the other.
    //
    // Both are deliberately rooted somewhere other than the start of their container — element
    // *1* of the array, field *B* of the object. Where the access lands is the sum of where the
    // root sits in the container and where the access sits in the root, and at a zero-offset
    // root the first term vanishes: an implementation that dropped it entirely would still be
    // right. Rooting at zero was the accidental property of every other guest that reaches this
    // code, and it left that term unexercised end to end.
    class Program
    {
        static int Main(string[] args)
        {
            // (1) Array container. `a[1].Y` displaced four bytes is `a[2].X` — the field
            // projection is what stops the cursor folding into the element index, so the write
            // has to leave element 1 to land.
            Pair[] a = new Pair[3];

            ref byte intoElement2 = ref Unsafe.AddByteOffset (ref Unsafe.As<int, byte> (ref a[1].Y), (nint) 4);
            intoElement2 = 42;

            if (a[2].X != 42)
            {
                return 1;
            }

            // It left the field it started from rather than wrapping back into it: without this
            // the check above would also pass for an implementation that wrote to `a[1].Y` and
            // happened to read it back through the same aliasing byref.
            if (a[1].Y != 0)
            {
                return 2;
            }

            // And it landed in element 2 rather than element 1, which is where a write that
            // forgot the root's own position in the array would go.
            if (a[1].X != 0)
            {
                return 3;
            }

            // (2) Heap-object container. This is the shape CoreLib itself uses for
            // `GCMemoryInfo.GenerationInfo`: a span over a run of sibling fields, taken from a
            // byref to one of them.
            Holder h = new Holder ();
            Span<long> fields = MemoryMarshal.CreateSpan (ref h.B, 2);

            fields[1] = 7;

            if (h.C != 7)
            {
                return 4;
            }

            if (h.B != 0)
            {
                return 5;
            }

            // Index 0 still writes the field the byref was actually rooted at, so the step-out
            // has not displaced the whole span by one element.
            fields[0] = 11;

            if (h.B != 11)
            {
                return 6;
            }

            if (h.C != 7)
            {
                return 7;
            }

            // `A` precedes the byref's root, so nothing here should have touched it. A write
            // that ignored the field's offset within the object would have landed on it.
            if (h.A != 0)
            {
                return 8;
            }

            return 0;
        }
    }
}
