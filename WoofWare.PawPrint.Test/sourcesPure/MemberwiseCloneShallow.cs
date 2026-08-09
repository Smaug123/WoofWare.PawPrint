// `Object.MemberwiseClone` is `[Intrinsic]`, so PawPrint routes it to `Intrinsics.call` rather
// than running the managed body (which is `AllocateUninitializedClone` plus a raw byte copy of the
// object payload). The two shapes that reach it from C#:
//
//   - a reference type cloning itself — the method is `protected internal`, so a type can only
//     clone instances of itself;
//   - a boxed value type, via `RuntimeHelpers.GetObjectValue`, whose body clones anything that is
//     a value type but not primitive.
//
// An array cannot reach it: `Array.Clone`'s body *is* `MemberwiseClone()`, but PawPrint intercepts
// `Array.Clone` itself (see `ArrayClone.cs`), and nothing can derive from `Array`.
//
// The contract under test is that the copy is *shallow*: a distinct object of the same runtime
// type, with field values copied, and reference-typed fields naming the very same objects rather
// than copies of them.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    class Node
    {
        public int X;
        public string Name;
        public Node Next;

        public Node ShallowCopy()
        {
            return (Node) MemberwiseClone();
        }
    }

    // Mutating a boxed value type in place needs a method call on the box itself: interface
    // dispatch on a boxed value type receives a managed pointer to the box's own payload as
    // `this`, whereas `(Pair) boxed` unboxes into a *copy* and writes to that instead.
    interface IMutablePair
    {
        void SetA(int value);
        int GetA();
    }

    struct Pair : IMutablePair
    {
        public int A;
        public int B;

        public void SetA(int value)
        {
            A = value;
        }

        public int GetA()
        {
            return A;
        }
    }

    public static int Main(string[] args)
    {
        Node shared = new Node { X = 99 };
        Node original = new Node { X = 1, Name = "hello", Next = shared };
        Node clone = original.ShallowCopy();

        // A distinct object, of the same runtime type.
        if (ReferenceEquals(clone, original)) return 1;
        if (clone.GetType() != typeof (Node)) return 2;

        // Value fields are copied.
        if (clone.X != 1) return 3;

        // Reference fields name the same objects: this is the "shallow" in shallow copy, and
        // `Next` is a freshly allocated object rather than an interned literal, so the check
        // cannot pass by accident.
        if (!ReferenceEquals(clone.Next, shared)) return 4;
        if (!ReferenceEquals(clone.Name, original.Name)) return 5;

        // The two objects have independent storage.
        clone.X = 42;
        if (original.X != 1) return 6;

        original.Name = "changed";
        if (clone.Name != "hello") return 7;

        // A boxed value type: distinct box, same contents.
        object boxed = new Pair { A = 3, B = 4 };
        object copy = RuntimeHelpers.GetObjectValue(boxed);
        if (ReferenceEquals(copy, boxed)) return 8;

        Pair unboxed = (Pair) copy;
        if (unboxed.A != 3) return 9;
        if (unboxed.B != 4) return 10;

        // Mutating one box must not disturb the other. The write has to go through the box, so
        // it is made by interface dispatch; the read-back immediately below is not decoration but
        // the guard that keeps the independence assertion honest, since a write that quietly
        // landed on a copy instead would leave the final check passing whatever the clone did.
        ((IMutablePair) boxed).SetA(77);
        if (((IMutablePair) boxed).GetA() != 77) return 11;

        if (((IMutablePair) copy).GetA() != 3) return 12;

        return 0;
    }
}
