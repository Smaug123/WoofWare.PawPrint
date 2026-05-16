interface IFoo<T> { }

public class Program
{
    public static int Main(string[] args)
    {
        // `IFoo<T>` is an open generic interface with NO non-generic parent interfaces:
        // its metadata `extends` clause is empty and `ImplementedInterfaces` is empty.
        // The open-source cast walk must still recognise that every reference type
        // (including interfaces) is assignable to `System.Object`, mirroring the
        // closed cast oracle's `walkBase` fallback when the inheritance chain runs out.
        // Without that fallback the walk would emit false here even though CoreCLR
        // returns true.
        if (!typeof(object).IsAssignableFrom(typeof(IFoo<>))) return 1;
        return 0;
    }
}
