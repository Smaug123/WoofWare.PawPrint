using System;
using System.Collections.Generic;

class Base<T> { }

class Pair<A, B> { }

// The parent of each definition below is its extends clause with the definition's own type
// variables substituted in: an open construction wherever the clause mentions one, and a closed
// type wherever it does not.
class Derived<T> : Base<T> { }

// Substitution is by position, so the arguments must come out in the order the clause spells them.
class Swapped<A, B> : Pair<B, A> { }

class Nested<T> : Base<List<T>> { }

class OverArray<T> : Base<T[]> { }

class Partial<A, B> : Base<A> { }

// The parent is `Partial<int, T>`, whose arguments mix a closed type and a variable, and whose own
// parent `Base<int>` mentions only the closed one -- so walking two levels up leaves the open world.
class FromPartial<T> : Partial<int, T> { }

class ClosedBase<T> : Base<int> { }

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    static int Main(string[] args)
    {
        Type derivedT = typeof(Derived<>).GetGenericArguments()[0];
        Type derivedParent = typeof(Derived<>).BaseType;
        Check(derivedParent.GetGenericTypeDefinition() == typeof(Base<>));
        Check(!derivedParent.IsGenericTypeDefinition);
        Check(derivedParent.ContainsGenericParameters);
        Check(derivedParent.GetGenericArguments()[0] == derivedT);
        // One type, one `Type` object, however many times it is asked for.
        Check(ReferenceEquals(derivedParent, typeof(Derived<>).BaseType));
        Check(derivedParent.BaseType == typeof(object));

        Type[] swappedFormals = typeof(Swapped<,>).GetGenericArguments();
        Type[] swappedParentArgs = typeof(Swapped<,>).BaseType.GetGenericArguments();
        Check(swappedParentArgs[0] == swappedFormals[1]);
        Check(swappedParentArgs[1] == swappedFormals[0]);

        Type nestedArg = typeof(Nested<>).BaseType.GetGenericArguments()[0];
        Check(nestedArg.GetGenericTypeDefinition() == typeof(List<>));
        Check(!nestedArg.IsGenericTypeDefinition);
        Check(nestedArg.GetGenericArguments()[0] == typeof(Nested<>).GetGenericArguments()[0]);

        Type arrayArg = typeof(OverArray<>).BaseType.GetGenericArguments()[0];
        Check(arrayArg.IsArray);
        Check(arrayArg.GetElementType() == typeof(OverArray<>).GetGenericArguments()[0]);

        Check(typeof(Partial<,>).BaseType.GetGenericArguments()[0] == typeof(Partial<,>).GetGenericArguments()[0]);

        Type fromPartialParent = typeof(FromPartial<>).BaseType;
        Check(fromPartialParent.GetGenericTypeDefinition() == typeof(Partial<,>));
        Check(fromPartialParent.GetGenericArguments()[0] == typeof(int));
        Check(fromPartialParent.GetGenericArguments()[1] == typeof(FromPartial<>).GetGenericArguments()[0]);
        Check(fromPartialParent.BaseType == typeof(Base<int>));
        Check(!fromPartialParent.BaseType.ContainsGenericParameters);

        // Control: a base mentioning no variable is the closed type.
        Check(typeof(ClosedBase<>).BaseType == typeof(Base<int>));

        return firstFailure;
    }
}
