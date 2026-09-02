using System;
using System.Runtime.CompilerServices;

namespace OpenGenericMethodTableFlagsNominalField
{
    struct Local
    {
        public int A;
        public int B;
    }

    struct LocalRef
    {
        public object O;
    }

    // A generic struct with a non-generic struct field spelled as a bare TypeDef (Local) and one
    // spelled as a bare TypeRef into CoreLib (Guid), next to a field of the parameter type.
    struct Box<T>
    {
        public Local L;
        public Guid Id;
        public T Value;
    }

    struct RefBox<T>
    {
        public LocalRef L;
        public T Value;
    }

    // Reading the MethodTable of the *open* definition walks its fields with the field types'
    // own instantiations in hand: Box<T>'s argument list has one entry, while Local and Guid
    // have none of their own.
    struct Holder<T>
    {
        public Box<T> Inner;
    }

    struct RefHolder<T>
    {
        public RefBox<T> Inner;
    }

    class Program
    {
        static int Main(string[] args)
        {
            Type open = typeof(Holder<>);
            if (!open.IsValueType) return 1;
            if (open.IsEnum) return 2;
            if (open.IsPrimitive) return 3;

            Type openRef = typeof(RefHolder<>);
            if (!openRef.IsValueType) return 4;
            if (openRef.IsPrimitive) return 5;

            if (!typeof(Holder<int>).IsValueType) return 6;
            if (RuntimeHelpers.IsReferenceOrContainsReferences<Holder<int>>()) return 7;
            if (!RuntimeHelpers.IsReferenceOrContainsReferences<Holder<string>>()) return 8;
            if (!RuntimeHelpers.IsReferenceOrContainsReferences<RefHolder<int>>()) return 9;

            return 0;
        }
    }
}
