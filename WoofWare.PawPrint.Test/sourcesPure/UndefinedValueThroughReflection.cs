using System;
using System.Reflection;
using System.Runtime.CompilerServices;

// Values read from `stackalloc` memory nothing wrote, handed through reflection: returned from a
// method `MethodInfo.Invoke` calls, and read from a field by `FieldInfo.GetValue`. Reflection only
// moves them into a box, and nothing reads them back, so the exit code is the same whatever the
// stack held.

[module: SkipLocalsInit]

unsafe class Program
{
    static int Unwritten;

    static int ReturnsUnwritten()
    {
        int* source = stackalloc int[1];
        return *source;
    }

    static int Main(string[] args)
    {
        object returned = typeof(Program)
            .GetMethod(nameof(ReturnsUnwritten), BindingFlags.NonPublic | BindingFlags.Static)!
            .Invoke(null, null)!;
        if (returned.GetType() != typeof(int)) return 1;

        int* source = stackalloc int[1];
        Unwritten = *source;
        object read = typeof(Program).GetField(nameof(Unwritten), BindingFlags.NonPublic | BindingFlags.Static)!.GetValue(null)!;
        if (read.GetType() != typeof(int)) return 2;

        return 0;
    }
}
