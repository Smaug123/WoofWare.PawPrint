using System;
using System.Runtime.CompilerServices;

// An accessor whose own type parameter says `allows ref struct`, over a target parameter that does
// not. The two are not interchangeable: the *absence* of `allows ref struct` (`gpAllowByRefLike`)
// is itself a constraint, so the target refuses a byref-like argument that the accessor accepts.
//
// Measured on real .NET 10: instantiating with `Span<int>` raises `VerificationException`
// ("type argument 'System.Span`1[System.Int32]' violates the constraint of type parameter 'U'"),
// while instantiating with an ordinary type runs the target.
public class TestUnsafeAccessorAllowsRefStruct
{
    private class Target
    {
        private static U Id<U>(U u) => u;
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Id")]
    private static extern U Id<U>(Target t, U u)
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

        return 0;
    }

    public static int Main() => Run();
}
