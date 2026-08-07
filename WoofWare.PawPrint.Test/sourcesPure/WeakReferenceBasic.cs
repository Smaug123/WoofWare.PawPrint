using System;

public class Program
{
    private sealed class Box
    {
        public int Value;
    }

    public static int Main(string[] args)
    {
        // Non-track-resurrection: WeakReference.Create stores the handle untagged,
        // and get_Target masks off TracksResurrectionBit (`handle & ~1`) before
        // dereferencing.
        Box target = new Box { Value = 17 };
        WeakReference weak = new WeakReference(target);

        if (weak.TrackResurrection) return 1;
        if (!weak.IsAlive) return 2;

        object? got = weak.Target;
        if (!ReferenceEquals(got, target)) return 3;

        // Track-resurrection: Create ORs TracksResurrectionBit into the handle,
        // so every read has to strip a tag bit that is actually set.
        WeakReference weakTracked = new WeakReference(target, true);
        if (!weakTracked.TrackResurrection) return 4;
        if (!weakTracked.IsAlive) return 5;
        if (!ReferenceEquals(weakTracked.Target, target)) return 6;

        // Setting the target goes through InternalSet on the untagged handle.
        Box other = new Box { Value = 23 };
        weak.Target = other;
        if (!ReferenceEquals(weak.Target, other)) return 7;
        if (weak.TrackResurrection) return 8;

        weakTracked.Target = other;
        if (!ReferenceEquals(weakTracked.Target, other)) return 9;
        if (!weakTracked.TrackResurrection) return 10;

        // Generic form: WeakReference<T>.TryGetTarget goes through the same
        // tagged-handle masking.
        WeakReference<Box> generic = new WeakReference<Box>(target);
        if (!generic.TryGetTarget(out Box? fromGeneric)) return 11;
        if (!ReferenceEquals(fromGeneric, target)) return 12;

        generic.SetTarget(other);
        if (!generic.TryGetTarget(out Box? fromGenericAgain)) return 13;
        if (!ReferenceEquals(fromGenericAgain, other)) return 14;

        WeakReference<Box> genericTracked = new WeakReference<Box>(target, true);
        if (!genericTracked.TryGetTarget(out Box? fromTracked)) return 15;
        if (!ReferenceEquals(fromTracked, target)) return 16;

        return 0;
    }
}
