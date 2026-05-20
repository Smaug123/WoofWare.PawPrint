using System.Numerics;

class Program
{
    static bool Check<T>() where T : struct => Vector<T>.IsSupported;

    static int Main(string[] args)
    {
        // We don't assert a particular value; Check<int>() is `true` on real .NET
        // (int is a primitive supported by Vector) and `false` on PawPrint's
        // scalar-only virtual CPU. The test exists to make sure the call does
        // not crash on PawPrint via the unimplemented-JIT-intrinsic path.
        bool _ = Check<int>();
        return 0;
    }
}
