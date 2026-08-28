using System;
using System.Runtime.CompilerServices;

// An array target is legal -- modern CoreCLR gives arrays MethodTables rather than TypeDescs, so
// `ValidateTargetType` lets one through -- but the only member that binds on one is its
// constructor. Everything else is a lookup that finds nothing, and is reported as such rather than
// as a malformed accessor: `Get` on `int[,]` is a *missing method*, not a `BadImageFormatException`.
//
// Measured on real .NET 10, including how the target is named: `System.Int32[,]`, `System.Int32[]`,
// `System.String[]`. The constructor is the half PawPrint cannot yet answer, and is parked as
// `sourcesPure/UnsafeAccessorArrayConstructor.cs`.
public class TestUnsafeAccessorArrayMemberLookup
{
    // `Get` is real on a multi-dimensional array -- the runtime provides it -- and still does not
    // bind, so this is about which members are candidates rather than about the name existing.
    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Get")]
    private static extern int RankTwoGet(int[,] a, int i, int j);

    [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuch")]
    private static extern int SzArrayMethod(int[] a);

    // `_numComponents` is the field CoreCLR's own `RawArrayData` names for an array's length, so
    // this is a plausible guess at an array's internals rather than a nonsense name.
    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_numComponents")]
    private static extern ref int SzArrayField(int[] a);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "NoSuch")]
    private static extern ref int RankTwoStaticField(int[,] a);

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "NoSuch")]
    private static extern int ReferenceArrayStaticMethod(string[] a);

    private static int Check<TExpected>(int code, string expectedName, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected e)
        {
            return e.Message.Contains(expectedName) ? 0 : code + 100;
        }
    }

    private static int Run()
    {
        int r;

        r = Check<MissingMethodException>(1, "System.Int32[,].Get", () => RankTwoGet(new int[2, 2], 0, 0));
        if (r != 0) return r;

        r = Check<MissingMethodException>(2, "System.Int32[].NoSuch", () => SzArrayMethod(new int[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(3, "System.Int32[]._numComponents", () => SzArrayField(new int[1]));
        if (r != 0) return r;

        r = Check<MissingFieldException>(4, "System.Int32[,].NoSuch", () => RankTwoStaticField(null));
        if (r != 0) return r;

        r = Check<MissingMethodException>(5, "System.String[].NoSuch", () => ReferenceArrayStaticMethod(null));
        if (r != 0) return r;

        return 0;
    }

    public static int Main() => Run();
}
