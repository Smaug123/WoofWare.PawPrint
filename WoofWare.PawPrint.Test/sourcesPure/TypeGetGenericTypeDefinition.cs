using System;
using System.Collections.Generic;

// `Type.GetGenericTypeDefinition()`. Only the closed-instantiation cases reach the
// `RuntimeTypeHandle_GetGenericTypeDefinition` QCall: `RuntimeType.GetGenericTypeDefinition`
// throws when `!IsGenericType`, and `RuntimeTypeCache.GetGenericTypeDefinition` short-circuits
// to `this` when `IsGenericTypeDefinition`. The non-reaching cases are kept here as controls,
// so a regression in either gate shows up as a wrong exit code rather than as a missing test.

namespace TypeGetGenericTypeDefinitionTest
{
    class Box<T> { }

    class Pair<T, U> { }

    struct ValuePair<T, U> { }

    class Outer<T>
    {
        // A type nested in a generic implicitly redeclares its enclosing type's parameters, so
        // `Outer<int>.Inner` is itself a closed generic whose definition is `Outer<>.Inner`.
        internal class Inner { }
    }

    class Program
    {
        static int Main(string[] args)
        {
            // Base case: a closed instantiation in the guest assembly.
            if (typeof(Box<int>).GetGenericTypeDefinition() != typeof(Box<>)) return 1;
            // `Type.op_Equality` is reference equality for RuntimeType operands, but assert it
            // explicitly: the QCall must hand back the canonical RuntimeType, not a fresh one.
            if (!ReferenceEquals(typeof(Box<int>).GetGenericTypeDefinition(), typeof(Box<>))) return 2;

            // Distinct instantiations of the same definition agree.
            if (typeof(Box<string>).GetGenericTypeDefinition() != typeof(Box<>)) return 3;

            // Two type parameters, reference and value class.
            if (typeof(Pair<string, int>).GetGenericTypeDefinition() != typeof(Pair<,>)) return 4;
            if (typeof(ValuePair<string, int>).GetGenericTypeDefinition() != typeof(ValuePair<,>)) return 5;

            // A generic argument that is itself a closed generic: the definition is unaffected.
            if (typeof(Box<Box<int>>).GetGenericTypeDefinition() != typeof(Box<>)) return 6;

            // Nested in a generic: the definition is the nested open type, not the enclosing one.
            if (typeof(Outer<int>.Inner).GetGenericTypeDefinition() != typeof(Outer<>.Inner)) return 7;

            // Cross-assembly: definitions living in CoreLib, as a class, an interface, and a struct.
            if (typeof(List<int>).GetGenericTypeDefinition() != typeof(List<>)) return 8;
            if (typeof(IEnumerable<int>).GetGenericTypeDefinition() != typeof(IEnumerable<>)) return 9;
            if (typeof(KeyValuePair<int, string>).GetGenericTypeDefinition() != typeof(KeyValuePair<,>)) return 10;

            // The shape `ComparerHelpers.CreateDefaultComparer` tests for.
            if (typeof(int?).GetGenericTypeDefinition() != typeof(Nullable<>)) return 11;

            // Control: an open definition short-circuits in managed code and returns itself.
            if (typeof(Box<>).GetGenericTypeDefinition() != typeof(Box<>)) return 12;

            // Control: no QCall for a non-generic type; the managed guard throws first.
            try
            {
                typeof(int).GetGenericTypeDefinition();
                return 13;
            }
            catch (InvalidOperationException)
            {
            }

            // Control: an array of a closed generic is itself not generic.
            try
            {
                typeof(Box<int>[]).GetGenericTypeDefinition();
                return 14;
            }
            catch (InvalidOperationException)
            {
            }

            // The RuntimeTypeCache memoises the definition; a second call must agree.
            Type first = typeof(Pair<string, int>).GetGenericTypeDefinition();
            Type second = typeof(Pair<string, int>).GetGenericTypeDefinition();
            if (!ReferenceEquals(first, second)) return 15;

            return 0;
        }
    }
}
