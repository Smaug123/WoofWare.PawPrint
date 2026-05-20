// Regression test for tOverridesMethod mis-classification in the constrained. handler.
// Previously the predicate treated any same-name, same-arity method on the struct as an
// override of the callvirt target. So a struct declaring `bool Equals(MyStruct)` was flagged
// as overriding `object.Equals(object)`, pushing the code down the unimplemented case-2
// branch instead of ECMA case 3 (box, then dispatch on the boxed receiver).

public struct MyStruct
{
    public int Value;

    // Strongly-typed overload; NOT an override of object.Equals(object). The raw parameter
    // signature is `MyStruct`, not `object`, so a full-signature comparison must distinguish
    // this from the target method and take case 3.
    public bool Equals(MyStruct other) => Value == other.Value;
}

public class Program
{
    // No IEquatable constraint, so C# resolves `value.Equals(other)` against `object.Equals(object)`
    // and emits `constrained. !!T callvirt object::Equals(object)`.
    private static bool CallEquals<T>(T value, object other) where T : struct
    {
        return value.Equals(other);
    }

    public static int Main(string[] args)
    {
        var a = new MyStruct { Value = 42 };
        var b = new MyStruct { Value = 42 };
        return CallEquals(a, (object) b) ? 0 : 1;
    }
}
