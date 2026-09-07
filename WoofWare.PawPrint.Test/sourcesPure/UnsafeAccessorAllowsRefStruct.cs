using System;
using System.Runtime.CompilerServices;

// An accessor whose own type parameter says `allows ref struct`, over targets whose parameter does
// and does not. A target that carries `allows ref struct` accepts a byref-like argument and an
// ordinary one alike; a target that does not still accepts the ordinary one. (Instantiating the
// latter with a byref-like type is the violation `sourcesPure/UnsafeAccessorRefStructConstraintViolation.cs`
// pins.) Measured on real .NET 10.
public class TestUnsafeAccessorAllowsRefStruct
{
    private class Target
    {
        private static U Id<U>(U u) => u;
    }

    // The same target, whose type parameter *does* carry the anti-constraint. The byref-like
    // argument the other target refuses is exactly what this one accepts, so the two together are
    // what make `allows ref struct` decide the answer rather than "is the argument byref-like".
    private class Permissive
    {
        private static int First<U>(U u)
            where U : allows ref struct
            => 1;
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Id")]
    private static extern U Id<U>(Target t, U u)
        where U : allows ref struct;

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "First")]
    private static extern int First<U>(Permissive p, U u)
        where U : allows ref struct;

    private static int Run()
    {
        // An ordinary type argument satisfies the target's parameter, so this reaches the target.
        if (Id<int>(null, 4) != 4) return 1;

        // The permissive target accepts both.
        if (First<int>(null, 4) != 1) return 4;

        Span<int> permitted = stackalloc int[1];
        if (First<Span<int>>(null, permitted) != 1) return 5;

        return 0;
    }

    public static int Main() => Run();
}
