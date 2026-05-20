using System.Collections.Generic;

class Derived<T> : List<T> { }

public class Program
{
    public static int Main(string[] args)
    {
        // Open-generic *target* handles in CoreCLR are identity-only: even if a closed
        // instantiation would be assignable, asking whether the open definition of one
        // type is assignable from the open definition of another is always false unless
        // the two definitions are the same. Concretely, every List<T> derives from
        // IEnumerable<T>, but `typeof(IEnumerable<>).IsAssignableFrom(typeof(List<>))`
        // is false because the question being asked is whether the type token "the open
        // definition of IEnumerable<>" can be assigned a value of "the open definition
        // of List<>", and open definitions are not instantiable types.
        if (typeof(IEnumerable<>).IsAssignableFrom(typeof(List<>))) return 1;

        // Identity does succeed: an open definition is assignable from itself.
        if (!typeof(List<>).IsAssignableFrom(typeof(List<>))) return 2;

        // Even a derived open definition is not assignable to its open base.
        if (typeof(List<>).IsAssignableFrom(typeof(Derived<>))) return 3;

        return 0;
    }
}
