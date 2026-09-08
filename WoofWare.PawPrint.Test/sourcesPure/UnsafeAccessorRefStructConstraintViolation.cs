using System;
using System.Runtime.CompilerServices;

// An accessor whose own type parameter says `allows ref struct`, over a target parameter that does
// not, instantiated with a byref-like type. The *absence* of `allows ref struct` (`gpAllowByRefLike`)
// is itself a constraint, so the target refuses an argument the accessor accepts. Measured on real
// .NET 10: `VerificationException` ("type argument 'System.Span`1[System.Int32]' violates the
// constraint of type parameter 'U'"), with the `COR_E_VERIFICATION` HResult.
// `sourcesPure/UnsafeAccessorAllowsRefStruct.cs` holds the instantiations that bind.
public class TestUnsafeAccessorRefStructConstraintViolation
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
        try
        {
            Span<int> span = stackalloc int[1];
            span[0] = 7;
            Span<int> got = Id<Span<int>>(null, span);
            return got[0] == 7 ? 1 : 2;
        }
        catch (System.Security.VerificationException e)
        {
            if (e.HResult != unchecked((int) 0x8013150D)) return 3;
        }

        return 0;
    }

    public static int Main() => Run();
}
