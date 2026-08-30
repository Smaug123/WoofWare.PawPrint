using System.Runtime.CompilerServices;

// `[UnsafeAccessorType("...")]` names a parameter's or the return's type by (optionally
// assembly-qualified) string rather than in the signature, which is how an accessor reaches a type
// its own assembly cannot reference at all. Every one of CoreLib's own `[UnsafeAccessor]`
// declarations is of this shape -- `ResourceReader` reaching `BinaryFormatter`, `AppDomain`
// reaching `GenericPrincipal`, `AssemblyName` reaching `MetadataReader`.
//
// An unqualified name means the accessor's own assembly, which is what lets this be a single-file
// guest; the mechanism is the same either way.
public class TestUnsafeAccessorTypeByName
{
    private static int Run()
    {
        object h = NewHidden();
        if (h == null) return 1;
        if (h.GetType().Name != "Hidden") return 2;
        if (Twice(h) != 42) return 3;
        if (Thrice(null, 5) != 15) return 4;

        Value(h) = 5;
        if (Twice(h) != 10) return 5;

        return 0;
    }

    // `Main` only delegates. An accessor that pushed the wrong number of arguments would leave the
    // extra one on its *caller's* evaluation stack, and the entry frame is never checked for a
    // clean stack on return -- it has nowhere to return to -- so the leak would go unnoticed if the
    // accessors were called from `Main` itself.
    public static int Main() => Run();

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    [return: UnsafeAccessorType("Hidden")]
    private static extern object NewHidden();

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
    private static extern int Twice([UnsafeAccessorType("Hidden")] object h);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Thrice")]
    private static extern int Thrice([UnsafeAccessorType("Hidden")] object h, int x);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_v")]
    private static extern ref int Value([UnsafeAccessorType("Hidden")] object h);
}

internal class Hidden
{
    private int _v = 21;

    private Hidden()
    {
    }

    private int Twice() => _v * 2;

    private static int Thrice(int x) => x * 3;
}
