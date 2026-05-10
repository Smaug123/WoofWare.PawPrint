using System;

// Exercises `ldftn` against a method defined in another assembly (System.Private.CoreLib).
// In-assembly `ldftn` targets resolve to MethodDef tokens, but a delegate constructed
// against a BCL method like `Math.Max` produces a MemberReference token, which is the
// case `UnaryMetadataTokenOps.executeLdftn` must also handle.

class Program
{
    static int Main(string[] args)
    {
        Func<int, int, int> max = Math.Max;
        if (max is null) return 1;
        if (max(2, 5) != 5) return 2;
        if (max(-3, -10) != -3) return 3;
        return 0;
    }
}
