using System;

public class Program
{
    private enum TestEnum
    {
        None = 0,
        A = 1,
        B = 2
    }

    private static bool HasFlagViaEnum(Enum value, Enum flag)
    {
        return value.HasFlag(flag);
    }

    public static int Main(string[] args)
    {
        TestEnum value = TestEnum.B;

        // Passing a concrete enum to an Enum-typed parameter forces boxing and then
        // exercises System.Enum.HasFlag on the boxed receiver.
        if (!HasFlagViaEnum(value, TestEnum.B)) return 1;

        // Direct enum instance calls go via constrained callvirt and should still behave
        // like the real runtime.
        if (value.ToString() != "B") return 2;

        // The nominal framework types System.Enum and System.ValueType are not themselves
        // value types, even though user-defined enums are.
        if (typeof(Enum).IsValueType) return 3;
        if (typeof(ValueType).IsValueType) return 4;
        if (!typeof(TestEnum).IsValueType) return 5;

        return 0;
    }
}
