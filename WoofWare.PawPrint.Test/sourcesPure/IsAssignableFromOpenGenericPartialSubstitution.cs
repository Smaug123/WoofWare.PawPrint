class IFooBox<T> { }

class FooBox<X, Y> : IFooBox<X> { }

class FixedFooBox<T> : FooBox<int, T> { }

public class Program
{
    public static int Main(string[] args)
    {
        // `FixedFooBox<T>` inherits from `FooBox<int, T>`, which itself inherits from
        // `IFooBox<X>` (where X is FooBox's first generic parameter). The cast oracle has
        // to follow this chain while preserving the partial substitution: walking from
        // `FixedFooBox<>` to `FooBox<int, T>` binds FooBox's X to `int` and FooBox's Y to
        // T (still open). When the walk then inspects FooBox's parent class
        // `IFooBox<X>`, the X reference must substitute to `int`, yielding the closed
        // `IFooBox<int>`, which matches the target. Without substitution threading the
        // walk would strip `FooBox<int, T>` to `FooBox<,>` and then walk an unsubstituted
        // `IFooBox<X>` that never closes — incorrectly returning false.
        if (!typeof(IFooBox<int>).IsAssignableFrom(typeof(FixedFooBox<>))) return 1;

        // Negative direction: the binding is FooBox's X = int, not string. Asking
        // `IFooBox<string>` must reject the same chain.
        if (typeof(IFooBox<string>).IsAssignableFrom(typeof(FixedFooBox<>))) return 2;

        return 0;
    }
}
