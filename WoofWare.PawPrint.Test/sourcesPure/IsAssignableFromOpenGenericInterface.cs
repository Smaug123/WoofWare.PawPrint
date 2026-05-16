using System;

class DisposableThing<T> : IDisposable
{
    public void Dispose() { }
}

public class Program
{
    public static int Main(string[] args)
    {
        // The source is the open generic definition `DisposableThing<>` (TypeDesc-shaped
        // RuntimeTypeHandleTarget). The cast oracle has to walk the source's interface
        // list, recognise that the `IDisposable` edge contains no unbound generic
        // parameter, materialise it as a closed `ConcreteTypeHandle`, and delegate the
        // edge check to the existing Closed/Closed oracle. Exercises the
        // materialise-and-check branch of the open-source walk.
        if (!typeof(IDisposable).IsAssignableFrom(typeof(DisposableThing<>))) return 1;
        return 0;
    }
}
