using System;
using System.Runtime.InteropServices;

public class Program
{
    // `bool` marshals as a 4-byte Win32 BOOL, and `char` at the width its declaring type's
    // `CharSet` gives it: one byte under the default (Ansi), two under Unicode. So each of these
    // differs from its managed layout, and `OffsetOf` answers from the native one.
    [StructLayout(LayoutKind.Sequential)]
    struct WithBool { public byte A; public bool B; public byte C; }

    [StructLayout(LayoutKind.Sequential)]
    struct WithChar { public byte A; public char C; public byte B; }

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    struct WithCharUnicode { public byte A; public char C; public byte B; }

    public static int Main(string[] args)
    {
        if (Marshal.OffsetOf<WithBool>("B") != (IntPtr)4) return 1;
        if (Marshal.OffsetOf<WithBool>("C") != (IntPtr)8) return 2;

        if (Marshal.OffsetOf<WithChar>("C") != (IntPtr)1) return 3;
        if (Marshal.OffsetOf<WithChar>("B") != (IntPtr)2) return 4;

        if (Marshal.OffsetOf<WithCharUnicode>("C") != (IntPtr)2) return 5;
        if (Marshal.OffsetOf<WithCharUnicode>("B") != (IntPtr)4) return 6;

        // `Nullable<T>` leads with a `bool hasValue`.
        if (Marshal.OffsetOf<int?>("value") != (IntPtr)4) return 7;

        return 0;
    }
}
