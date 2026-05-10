using System;
using System.Runtime;

public class Program
{
    public static int Main(string[] args)
    {
        // Construction: InternalAlloc (object?, object?) -> IntPtr.
        object target = new object();
        object dependent = new object();

        DependentHandle handle = new DependentHandle(target, dependent);

        if (!handle.IsAllocated) return 1;

        // Target getter -> InternalGetTarget (in DEBUG) or *(object*)handle (RELEASE).
        if (!ReferenceEquals(handle.Target, target)) return 2;

        // Dependent getter -> InternalGetDependent.
        if (!ReferenceEquals(handle.Dependent, dependent)) return 3;

        // Atomic pair getter -> InternalGetTargetAndDependent.
        var pair = handle.TargetAndDependent;
        if (!ReferenceEquals(pair.Target, target)) return 4;
        if (!ReferenceEquals(pair.Dependent, dependent)) return 5;

        // Replace dependent -> InternalSetDependent.
        object newDependent = new object();
        handle.Dependent = newDependent;
        if (!ReferenceEquals(handle.Dependent, newDependent)) return 6;
        if (!ReferenceEquals(handle.Target, target)) return 7;

        // Null target -> InternalSetTargetToNull. Per the spec, the dependent must
        // also become unobservable through both the direct getter and the atomic
        // pair getter once the target is cleared, even if PawPrint has no GC to
        // actually collect it.
        handle.Target = null;
        if (handle.Target is not null) return 8;
        var clearedPair = handle.TargetAndDependent;
        if (clearedPair.Target is not null) return 9;
        if (clearedPair.Dependent is not null) return 10;
        // Direct Dependent getter must also return null once target is cleared,
        // matching CoreCLR's InternalGetDependent ("dependent tracked only if target non-null").
        if (handle.Dependent is not null) return 11;

        // Free -> InternalFree.
        handle.Dispose();
        if (handle.IsAllocated) return 12;

        // Null target at construction time: the dependent must be unobservable from
        // the very start, since the target is null.
        object initialDependent = new object();
        DependentHandle nullTargetHandle = new DependentHandle(null, initialDependent);
        if (!nullTargetHandle.IsAllocated) return 13;
        if (nullTargetHandle.Target is not null) return 14;
        if (nullTargetHandle.Dependent is not null) return 15;
        var nullTargetPair = nullTargetHandle.TargetAndDependent;
        if (nullTargetPair.Target is not null) return 16;
        if (nullTargetPair.Dependent is not null) return 17;
        nullTargetHandle.Dispose();

        // Null target/dependent at construction time should also work.
        DependentHandle nullsHandle = new DependentHandle(null, null);
        if (!nullsHandle.IsAllocated) return 18;
        if (nullsHandle.Target is not null) return 19;
        if (nullsHandle.Dependent is not null) return 20;
        nullsHandle.Dispose();

        return 0;
    }
}
