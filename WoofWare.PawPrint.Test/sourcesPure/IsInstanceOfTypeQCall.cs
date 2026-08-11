using System;
using System.Collections.Generic;

interface IShape { }

class Shape : IShape { }

class Square : Shape { }

class Unrelated { }

public class Program
{
    public static int Main(string[] args)
    {
        // Every call here flows through RuntimeType.IsInstanceOfType →
        // CastHelpers.IsInstanceOfAny → CastCache.TryGet. PawPrint's cast cache is a
        // permanently-empty sentinel, so the lookup always reports MaybeCast and the call
        // falls through to IsInstanceOfAny_NoCacheLookup → the IsInstanceOf_NoCacheLookup
        // QCall this test is for — except where the exact-MethodTable-identity check in
        // IsInstanceOfAny short-circuits first.

        // Control: exact type identity. `mt == toTypeHnd`, so this answers true *without*
        // reaching the QCall. It is here so a regression in the short-circuit is visible
        // separately from a regression in the QCall.
        if (!typeof(Shape).IsInstanceOfType(new Shape())) return 1;

        // Interface implemented by the object's own type.
        if (!typeof(IShape).IsInstanceOfType(new Shape())) return 2;

        // Interface implemented by a base class.
        if (!typeof(IShape).IsInstanceOfType(new Square())) return 3;

        // Base class of the object's type.
        if (!typeof(Shape).IsInstanceOfType(new Square())) return 4;

        // Derived type: an instance of the base is not an instance of the derived type.
        if (typeof(Square).IsInstanceOfType(new Shape())) return 5;

        // Wholly unrelated reference types.
        if (typeof(Unrelated).IsInstanceOfType(new Shape())) return 6;
        if (typeof(Shape).IsInstanceOfType("a string")) return 7;

        // Everything is an object.
        if (!typeof(object).IsInstanceOfType(new Square())) return 8;
        if (!typeof(object).IsInstanceOfType(new int[1])) return 9;

        // Array covariance: Square[] is a Shape[].
        if (!typeof(Shape[]).IsInstanceOfType(new Square[1])) return 10;
        if (typeof(Square[]).IsInstanceOfType(new Shape[1])) return 11;

        // An SZ array implicitly implements the generic collection interfaces.
        if (!typeof(IList<Square>).IsInstanceOfType(new Square[1])) return 12;

        // Generic variance: IEnumerable<out T>.
        if (!typeof(IEnumerable<object>).IsInstanceOfType(new List<string>())) return 13;
        if (typeof(IEnumerable<string>).IsInstanceOfType(new List<object>())) return 14;

        // Invariant generic: List<T> is not variant, so no relation holds.
        if (typeof(List<object>).IsInstanceOfType(new List<string>())) return 15;

        // Object castability disagrees with type castability on T -> Nullable<T>: a boxed
        // int and an int? share a boxed representation, so CoreCLR answers true here (the
        // Nullable::IsNullableForType branch, checked before anything else and never cached).
        if (!typeof(int?).IsInstanceOfType(5)) return 16;
        if (typeof(long?).IsInstanceOfType(5)) return 17;

        // Value types with no relation.
        if (typeof(int).IsInstanceOfType("a string")) return 18;

        // Null is never an instance of anything; short-circuits before the QCall.
        if (typeof(object).IsInstanceOfType(null)) return 19;

        return 0;
    }
}
