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
//
// The file covers three shapes, all of which need that capability:
//
//  1. `A3`/`B3`/`C3`: two inherited signatures that are distinct at the definition level and
//     coincide once T is substituted.
//  2. `Ha`/`Hb`/`Hc`: a `new virtual` shadow whose signature mentions the parameter. This tie IS
//     genuine and most-derived would be right, but PawPrint cannot tell it apart from (3) with a
//     closed walk, so it refuses rather than guesses.
//  3. `Ka`/`Kb`/`Kc`: why it cannot. `Kb<T> : Ka<string>` pins Ka's parameter, so the two inherited
//     signatures are *both* raw `[!0]` and yet denote different things -- a raw `!0` is scoped to
//     the type that wrote it. Measured: .NET reports `Kc`/`Kb`; trusting the syntactic equality of
//     the raw signatures picks the other slot and reports `Kc`/`Ka`.
//
// The sibling `Ga`/`Gb`/`Gc` case in `ReflectionVirtualMethodSlots.cs` passes and stays there: no
// signature mentions the parameter, so no substitution can have changed anything.
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

    public class Ha<T>
    {
        public virtual string M (T x)
        {
            return "ha";
        }
    }

    public class Hb<T> : Ha<T>
    {
        public new virtual string M (T x)
        {
            return "hb";
        }
    }

    public class Hc<T> : Hb<T>
    {
        public override string M (T x)
        {
            return "hc";
        }
    }

    public class Ka<T>
    {
        public virtual string M (T x)
        {
            return "ka";
        }
    }

    // Non-identity base instantiation: Ka's parameter is pinned to string here, so Ka's `!0` and
    // Kb's `!0` denote different things even though both are written `!0`.
    public class Kb<T> : Ka<string>
    {
        public virtual string M (T x)
        {
            return "kb";
        }
    }

    public class Kc<T> : Kb<T>
    {
        public override string M (string x)
        {
            return "kc";
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

        // A genuine most-derived tie whose signatures mention the parameter.
        if (CountOwnedBy (typeof (Hc<int>), "M", "Hc`1") != 1)
            return 4;

        if (CountOwnedBy (typeof (Hc<int>), "M", "Ha`1") != 1)
            return 5;

        if (CountOwnedBy (typeof (Hc<int>), "M", "Hb`1") != 0)
            return 6;

        // Both inherited signatures are raw `[!0]`, but Kb pinned Ka's parameter, so the override
        // lands on Ka's slot and Kb's survives.
        if (CountOwnedBy (typeof (Kc<string>), "M", "Kc`1") != 1)
            return 7;

        if (CountOwnedBy (typeof (Kc<string>), "M", "Kb`1") != 1)
            return 8;

        if (CountOwnedBy (typeof (Kc<string>), "M", "Ka`1") != 0)
            return 9;

        return 0;
    }
}
