using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace IsReferenceOrContainsReferencesGeneric
{
    struct Box<T>
    {
        public T Value;
    }

    struct Pair<TFirst, TSecond>
    {
        public TFirst First;
        public TSecond Second;
    }

    struct ArrayHolder<T>
    {
        // The field is an array of T, so this is reference-containing whatever T is.
        public T[] Values;
    }

    struct NonGenericHolder
    {
        public Box<int> Inner;
        public int Tag;
    }

    struct NonGenericRefHolder
    {
        public Box<string> Inner;
        public int Tag;
    }

    // A generic struct that recurses through a generic instantiation of itself at a
    // *different* argument, so a naive memo keyed on the type definition alone would
    // conflate the two.
    struct Outer<T>
    {
        public Box<Box<T>> Nested;
    }

    class Program
    {
        static bool Check<T>()
        {
            return RuntimeHelpers.IsReferenceOrContainsReferences<T>();
        }

        static int Main(string[] args)
        {
            // Controls: the non-generic cases that already worked.
            if (Check<int>()) return 1;
            if (!Check<string>()) return 2;
            if (!Check<object>()) return 3;

            // A generic value type instantiated at a value type.
            if (Check<Box<int>>()) return 4;
            // A generic value type instantiated at a reference type.
            if (!Check<Box<string>>()) return 5;

            // Two generic parameters, only the second of which is a reference.
            if (Check<Pair<int, long>>()) return 6;
            if (!Check<Pair<int, object>>()) return 7;
            if (!Check<Pair<string, int>>()) return 8;

            // Nesting a generic value type inside another one.
            if (Check<Box<Box<int>>>()) return 9;
            if (!Check<Box<Box<string>>>()) return 10;
            if (Check<Outer<int>>()) return 11;
            if (!Check<Outer<object>>()) return 12;

            // The generic argument is irrelevant: an array field is always a reference.
            if (!Check<ArrayHolder<int>>()) return 13;
            if (!Check<ArrayHolder<string>>()) return 14;

            // A non-generic struct whose field is a generic value type.
            if (Check<NonGenericHolder>()) return 15;
            if (!Check<NonGenericRefHolder>()) return 16;

            // A generic value type from CoreLib rather than from the guest assembly.
            if (Check<KeyValuePair<int, int>>()) return 17;
            if (!Check<KeyValuePair<int, string>>()) return 18;
            if (!Check<KeyValuePair<string, int>>()) return 19;

            // Nullable<T> is a generic value type whose reference-ness follows T.
            if (Check<int?>()) return 20;

            return 0;
        }
    }
}
