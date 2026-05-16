class GenericBase<T> { }

class ConstBase<T> : GenericBase<int> { }

public class Program
{
    public static int Main(string[] args)
    {
        // The source is the open definition `ConstBase<>`; the target is the closed
        // generic instantiation `GenericBase<int>`. `ConstBase<T>`'s base is
        // `GenericBase<int>`, which contains no unbound generic parameter and can be
        // materialised directly as a `ConcreteTypeHandle`. The cast oracle then
        // delegates to the Closed/Closed branch, which succeeds. Exercises that the
        // closed-generic target case isn't blanket-rejected by the open-source walk.
        if (!typeof(GenericBase<int>).IsAssignableFrom(typeof(ConstBase<>))) return 1;

        // And the negative direction: `ConstBase<>`'s base is `GenericBase<int>`, not
        // `GenericBase<string>`, so the cast must reject.
        if (typeof(GenericBase<string>).IsAssignableFrom(typeof(ConstBase<>))) return 2;
        return 0;
    }
}
