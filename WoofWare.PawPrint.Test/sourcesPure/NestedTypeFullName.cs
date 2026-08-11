using System;
using System.Collections.Generic;

class Enclosing
{
    public class Inner { }

    public class Deeper
    {
        public class Innermost { }
    }
}

namespace Space
{
    class Held
    {
        public class Nested { }
    }
}

public class Program
{
    public static int Main(string[] args)
    {
        // The CLR joins a nesting chain with '+', not '.': a '.' would be ambiguous with the
        // namespace separator. This is TypeNameBuilder::AddNestedName, behind both FullName
        // and ToString().
        if (typeof(Enclosing.Inner).FullName != "Enclosing+Inner") return 1;
        if (typeof(Enclosing.Inner).ToString() != "Enclosing+Inner") return 2;

        // Name is the row's own name, with no chain at all.
        if (typeof(Enclosing.Inner).Name != "Inner") return 3;

        // Chains of more than one link keep a '+' at every step.
        if (typeof(Enclosing.Deeper.Innermost).FullName != "Enclosing+Deeper+Innermost") return 4;

        // The namespace stays '.'-joined and sits in front of the outermost link only.
        if (typeof(Space.Held.Nested).FullName != "Space.Held+Nested") return 5;
        if (typeof(Space.Held).FullName != "Space.Held") return 6;

        // A non-nested type is unaffected.
        if (typeof(Program).FullName != "Program") return 7;
        if (typeof(string).FullName != "System.String") return 8;

        // Nested types are also reachable as generic arguments, which is where a wrong
        // separator shows up in exception messages rather than in reflection output.
        if (typeof(List<Enclosing.Inner>).ToString() != "System.Collections.Generic.List`1[Enclosing+Inner]") return 9;

        return 0;
    }
}
