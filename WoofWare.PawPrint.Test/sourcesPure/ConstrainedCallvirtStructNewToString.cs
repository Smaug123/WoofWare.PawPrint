// Regression test for tOverridesMethod treating C# `new` members as overrides.
// A `public new string ToString()` on a struct adds a hiding method but does not
// occupy the inherited virtual slot (either it's non-virtual or carries the NewSlot
// layout). At runtime, `constrained. !!T callvirt object::ToString()` must take
// ECMA III.2.1 case 3 (box + virtual dispatch), which resolves to ValueType.ToString
// — NOT the hidden method. If tOverridesMethod incorrectly classifies `new` members
// as overrides, the interpreter takes the unimplemented case-2 path instead.

public struct ShadowedStruct
{
    public int Value;

    // `new` (not `override`) — this hides but does not override object.ToString.
    // In IL, this method is not marked `virtual`, so it does not occupy the base slot.
    public new string ToString () => "shadow";
}

public class Program
{
    // Compiler emits `constrained. !!T callvirt object::ToString()`.
    private static string CallToString<T> (T value) where T : struct
    {
        return value.ToString ();
    }

    public static int Main (string[] args)
    {
        var s = new ShadowedStruct { Value = 7 };
        // The runtime boxes to ValueType and calls the inherited virtual ToString,
        // which yields the type's full name — NOT "shadow" — because the `new`
        // member does not participate in vtable dispatch.
        return CallToString (s) == "shadow" ? 1 : 0;
    }
}
