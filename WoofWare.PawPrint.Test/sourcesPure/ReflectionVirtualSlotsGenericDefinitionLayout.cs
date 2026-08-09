using System;
using System.Reflection;

// Vtable slots are laid out on the *generic definition*, not on any instantiation. At the definition
// level `A<T>.M(T)` and `B<T>.M(string)` are different methods occupying different slots, so
// `C<T>.M(T)` overrides `A<T>.M(T)` and `B<T>.M(string)` keeps its own. Reflecting over `C<string>`
// must therefore still report `B.M`, even though at that instantiation the two inherited signatures
// have become textually identical.
//
// PawPrint matches overrides against base slots by concretising both signatures, i.e. after
// substituting the declaring types' generic arguments — so at `T = string` the candidate matches
// both inherited slots and the "most derived wins" rule picks B's, reporting `A.M` as the survivor
// instead of `B.M`. That is caught rather than answered: `vtableOfClosed` fails with a TODO naming
// this shape.
//
// Fixing it means computing the layout over the generic definition's base chain with generic
// parameters kept symbolic, which is the same capability `RuntimeTypeHandle.GetNumVirtuals` is
// missing for open generic type definitions. Un-park when that lands.
public class Program
{
    public class A3<T>
    {
        public virtual string M (T x)
        {
            return "a";
        }
    }

    public class B3<T> : A3<T>
    {
        public virtual string M (string x)
        {
            return "b";
        }
    }

    public class C3<T> : B3<T>
    {
        public override string M (T x)
        {
            return "c";
        }
    }

    private const BindingFlags All =
        BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

    private static int CountOwnedBy (Type t, string name, string owner)
    {
        int n = 0;

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name && m.DeclaringType.Name == owner)
                n++;
        }

        return n;
    }

    public static int Main (string[] args)
    {
        // The override lands on A3's slot, so A3's declaration is deduped away and B3's survives.
        if (CountOwnedBy (typeof (C3<string>), "M", "C3`1") != 1)
            return 1;

        if (CountOwnedBy (typeof (C3<string>), "M", "B3`1") != 1)
            return 2;

        if (CountOwnedBy (typeof (C3<string>), "M", "A3`1") != 0)
            return 3;

        return 0;
    }
}
