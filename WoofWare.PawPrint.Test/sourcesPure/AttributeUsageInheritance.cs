using System;

// `GetCustomAttributes(..., inherit: true)` consults the attribute type's own [AttributeUsage]
// through a QCall that parses the raw blob. Each attribute below fixes a different (Inherited,
// AllowMultiple) pair, and each is observed twice, because neither bit is observable alone:
// Inherited decides whether a base class's application is seen at all, and AllowMultiple only
// matters once the derived type carries one of its own.

// No named args at all: the blob carries a zero named-arg count, and both defaults stand
// (Inherited true, AllowMultiple false).
[AttributeUsage(AttributeTargets.Class)]
sealed class DefaultUsageAttribute : Attribute { }

[AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = true)]
sealed class InheritedSingleAttribute : Attribute { }

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = true)]
sealed class InheritedMultiAttribute : Attribute { }

// Written Inherited-first, where the two above are written AllowMultiple-first: the blob records
// named args in source order, so between them these pin that matching is by name rather than by
// position.
[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = true)]
sealed class NotInheritedMultiAttribute : Attribute { }

[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
sealed class NotInheritedSingleAttribute : Attribute { }

[DefaultUsage, InheritedSingle, InheritedMulti, NotInheritedMulti, NotInheritedSingle]
class Base { }

// Inherits every application from Base and adds none of its own.
class DerivedBare : Base { }

// Carries its own copy of each, so AllowMultiple decides whether Base's is kept alongside.
[DefaultUsage, InheritedSingle, InheritedMulti, NotInheritedMulti, NotInheritedSingle]
class DerivedDecorated : Base { }

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

    static void CheckPair(Type attr, int expectedBare, int expectedBoth)
    {
        Check(typeof(DerivedBare).GetCustomAttributes(attr, true).Length == expectedBare);
        Check(typeof(DerivedDecorated).GetCustomAttributes(attr, true).Length == expectedBoth);
    }

    static int Main()
    {
        CheckPair(typeof(DefaultUsageAttribute), 1, 1);
        CheckPair(typeof(InheritedSingleAttribute), 1, 1);
        CheckPair(typeof(InheritedMultiAttribute), 1, 2);
        CheckPair(typeof(NotInheritedMultiAttribute), 0, 1);
        CheckPair(typeof(NotInheritedSingleAttribute), 0, 1);
        return firstFailure;
    }
}
