using System;
using System.Runtime.CompilerServices;

// An accessor whose own type parameter says `allows ref struct`, over a target parameter that does
// not. The two are not interchangeable: the *absence* of `allows ref struct` (`gpAllowByRefLike`)
// is itself a constraint, so the target refuses a byref-like argument that the accessor accepts.
//
// Measured on real .NET 10: instantiating with `Span<int>` raises `VerificationException`
// ("type argument 'System.Span`1[System.Int32]' violates the constraint of type parameter 'U'"),
// while instantiating with an ordinary type runs the target -- and a target that *does* carry
// `allows ref struct` accepts both.
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

        try
        {
            Span<int> span = stackalloc int[1];
            span[0] = 7;
            Span<int> got = Id<Span<int>>(null, span);
            return got[0] == 7 ? 2 : 3;
        }
        catch (System.Security.VerificationException)
        {
        }

        // The permissive target accepts both, so the refusal above is about the target's own
        // `allows ref struct` and not about the argument being byref-like.
        if (First<int>(null, 4) != 1) return 4;

        Span<int> permitted = stackalloc int[1];
        if (First<Span<int>>(null, permitted) != 1) return 5;

        return 0;
    }

    public static int Main() => Run();
}
