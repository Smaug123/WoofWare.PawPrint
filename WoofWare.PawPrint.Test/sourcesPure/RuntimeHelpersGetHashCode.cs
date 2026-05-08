using System;
using System.Runtime.CompilerServices;

class Program
{
    static int HashViaMethod(object o) => RuntimeHelpers.GetHashCode(o);

    static int Main(string[] args)
    {
        // Null returns 0.
        if (RuntimeHelpers.GetHashCode(null) != 0)
        {
            return 1;
        }

        // Same reference, two calls -> equal hashes.
        object a = new object();
        if (RuntimeHelpers.GetHashCode(a) != RuntimeHelpers.GetHashCode(a))
        {
            return 2;
        }

        // Distinct allocations -> distinct hashes.
        object b = new object();
        if (RuntimeHelpers.GetHashCode(a) == RuntimeHelpers.GetHashCode(b))
        {
            return 3;
        }

        // Boxing the same int twice produces two distinct boxes with distinct identity hashes.
        // (Object.GetHashCode would give equal hashes here because Int32 overrides it; only
        // RuntimeHelpers.GetHashCode is identity-based.)
        object box1 = (object)5;
        object box2 = (object)5;
        if (RuntimeHelpers.GetHashCode(box1) == RuntimeHelpers.GetHashCode(box2))
        {
            return 4;
        }

        // For a plain `new object()`, Object.GetHashCode flows through to
        // RuntimeHelpers.GetHashCode, so the values must agree. (This would NOT hold for
        // String, Int32 boxes, etc., which override GetHashCode.)
        object c = new object();
        if (c.GetHashCode() != RuntimeHelpers.GetHashCode(c))
        {
            return 5;
        }

        // Hash is stable across method calls.
        if (RuntimeHelpers.GetHashCode(a) != HashViaMethod(a))
        {
            return 6;
        }

        return 0;
    }
}
