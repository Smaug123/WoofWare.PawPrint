using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// A union whose int view is read from `stackalloc` memory in which only byte 0 was written. Its
// byte view is that written byte; the int view is moved into a local and overwritten before it is
// used, so the exit code is the same whatever the stack held.

[module: SkipLocalsInit]

[StructLayout(LayoutKind.Explicit)]
struct Union
{
    [FieldOffset(0)] public int Whole;
    [FieldOffset(0)] public byte Low;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 42;
        Union union = Unsafe.Read<Union>(bytes);
        if (union.Low != 42) return 1;

        int whole = union.Whole;
        whole = 7;
        return whole == 7 ? 0 : 2;
    }
}
