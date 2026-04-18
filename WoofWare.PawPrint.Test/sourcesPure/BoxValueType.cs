using System;

public class Program
{
    private enum TestEnum
    {
        None = 0,
        A = 1,
        B = 2,
        AB = 3
    }

    private static bool HasFlagViaEnum(Enum value, Enum flag)
    {
        return value.HasFlag(flag);
    }

    private static bool IsNotNull(object o)
    {
        return o != null;
    }

    public static int Main(string[] args)
    {
        // Boxing an enum to System.Enum via method parameter
        if (!HasFlagViaEnum(TestEnum.AB, TestEnum.A)) return 1;
        if (!HasFlagViaEnum(TestEnum.AB, TestEnum.B)) return 2;
        if (HasFlagViaEnum(TestEnum.A, TestEnum.B)) return 3;
        if (!HasFlagViaEnum(TestEnum.None, TestEnum.None)) return 4;
        if (HasFlagViaEnum(TestEnum.None, TestEnum.A)) return 5;

        // Boxing an int to object
        int x = 42;
        if (!IsNotNull((object)x)) return 6;

        // Boxing an enum to object
        TestEnum e = TestEnum.B;
        if (!IsNotNull((object)e)) return 7;

        return 0;
    }
}
