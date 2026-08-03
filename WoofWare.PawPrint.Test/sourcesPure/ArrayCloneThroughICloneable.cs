using System;

// Array.Clone reached through the ICloneable interface rather than through a
// direct `callvirt instance object Array::Clone()` call site.
public class TestArrayCloneThroughICloneable
{
    public static int Main(string[] argv)
    {
        int[] original = new int[] { 10, 20, 30 };
        ICloneable cloneable = original;
        int[] clone = (int[])cloneable.Clone();

        if (ReferenceEquals(clone, original)) return 1;
        if (clone.Length != 3) return 2;
        if (clone[0] != 10 || clone[1] != 20 || clone[2] != 30) return 3;

        clone[1] = 200;
        if (original[1] != 20) return 4;
        if (clone[1] != 200) return 5;

        return 0;
    }
}
