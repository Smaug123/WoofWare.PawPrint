using System;

public class GetDeclaringTypeNonNested
{
    public static int Main(string[] argv)
    {
        // A top-level type has no DeclaringType. The QCall returns IntPtr.Zero,
        // which the managed wrapper maps to null.
        if (typeof(string).DeclaringType is not null) return 1;
        if (typeof(GetDeclaringTypeNonNested).DeclaringType is not null) return 2;
        if (typeof(int).DeclaringType is not null) return 3;

        // Top-level open generic: also no DeclaringType.
        if (typeof(System.Collections.Generic.List<>).DeclaringType is not null) return 4;

        return 0;
    }
}
