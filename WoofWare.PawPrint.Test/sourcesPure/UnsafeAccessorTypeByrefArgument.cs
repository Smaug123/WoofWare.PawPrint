using System.Runtime.CompilerServices;

// A byref parameter retyped by `[UnsafeAccessorType]`: the declaration says `ref object`, and the
// stub (`EmitTypeCheck`, unsafeaccessors.cpp) type-checks what the byref holds, copies it into a
// local of the named type, passes the target a byref to that local, and writes the local back
// through the caller's byref after the target returns -- unless the parameter is `in`. So the
// target's assignment reaches the caller, but through a different byref than the caller passed.
public class TestUnsafeAccessorTypeByrefArgument
{
    private static int Run()
    {
        object slot = new Hidden(1);
        Replace(null, ref slot, 7);
        if (slot is not Hidden h) return 1;
        if (h.V != 7) return 2;

        object made = null;
        Make(null, out made);
        if (made is not Hidden m || m.V != 9) return 3;

        return 0;
    }

    public static int Main() => Run();

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Replace")]
    private static extern void Replace(
        [UnsafeAccessorType("Hidden")] object owner,
        [UnsafeAccessorType("Hidden&")] ref object slot,
        int v);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Make")]
    private static extern void Make([UnsafeAccessorType("Hidden")] object owner, [UnsafeAccessorType("Hidden&")] out object made);
}

internal class Hidden
{
    internal int V;

    internal Hidden(int v)
    {
        V = v;
    }

    private static void Replace(ref Hidden slot, int v) => slot = new Hidden(v);

    private static void Make(out Hidden made) => made = new Hidden(9);
}
