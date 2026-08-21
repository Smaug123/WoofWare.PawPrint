using System;

// A type is identified by its assembly as well as its name. A guest assembly may declare its own
// `System.Double`, and a field of that type is an ordinary value type — so a `GetHashCode` override
// on it must be reached, not bypassed by treating the field as the corelib primitive it is named
// after.
namespace System
{
    public struct Double
    {
        // An `int` rather than a `double` so that a mis-binding to corelib's `System.Double`,
        // which has no such field, is a compile error rather than a silently different test.
        public int V;

        // A constant, so that every strategy which reads the field's bytes — at any width, at any
        // offset — disagrees with asking the override.
        public override int GetHashCode() => 99;

        public override bool Equals(object obj) => obj is Double other && other.V == V;
    }
}

public class Program
{
    private struct LookalikeThenInt
    {
        public System.Double D;
        public int A;
    }

    public static int Main(string[] args)
    {
        // ValueTypeOverride: the override is asked and returns the same answer for every value, so
        // neither `D.V` nor `A` can move the hash.
        LookalikeThenInt one = new LookalikeThenInt { D = new System.Double { V = 1 }, A = 1 };
        LookalikeThenInt two = new LookalikeThenInt { D = new System.Double { V = 2 }, A = 2 };
        if (one.GetHashCode() != two.GetHashCode())
        {
            return 1;
        }

        LookalikeThenInt negative = new LookalikeThenInt { D = new System.Double { V = -1 }, A = 7 };
        if (one.GetHashCode() != negative.GetHashCode())
        {
            return 2;
        }

        return 0;
    }
}
