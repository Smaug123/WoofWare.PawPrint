using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct Phantom<T> { public byte A; public int B; }

    [StructLayout(LayoutKind.Sequential)]
    struct Holds<T> { public byte A; public T V; }

    [StructLayout(LayoutKind.Sequential)]
    struct HoldsString<T> { public byte A; [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 4)] public T S; public int B; }

    const string CannotMarshalSuffix =
        "' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed.";

    static int CannotMarshal(Type t, string name, string typeName, int failure)
    {
        try
        {
            Marshal.OffsetOf(t, name);
            return failure;
        }
        catch (ArgumentException e)
        {
            if (e.GetType() != typeof(ArgumentException)) return failure + 1;
            if (e.Message != "Type '" + typeName + CannotMarshalSuffix) return failure + 2;
            return 0;
        }
    }

    public static int Main(string[] args)
    {
        // An instantiation over a reference type is answered for its canonical form, over
        // `System.__Canon`. That changes nothing when no field mentions `T`...
        if (Marshal.OffsetOf<Phantom<string>>("B") != (IntPtr)4) return 1;

        int r;
        // ...but a `T` field is a `__Canon`, which has no native form, even where `string`
        // would have had one.
        if ((r = CannotMarshal(typeof(Holds<string>), "A", "Program+Holds`1[System.__Canon]", 10)) != 0) return r;
        if ((r = CannotMarshal(typeof(HoldsString<string>), "B", "Program+HoldsString`1[System.__Canon]", 20)) != 0) return r;

        return 0;
    }
}
