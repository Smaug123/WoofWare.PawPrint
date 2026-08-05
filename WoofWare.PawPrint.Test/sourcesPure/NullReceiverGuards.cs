// Instance methods that PawPrint implements as intrinsics still get `callvirt`'s own null
// check first, so a null receiver raises `NullReferenceException` before the intrinsic body
// is consulted at all.
//
// This is why the null-receiver arms inside those intrinsics (`Object.GetType`,
// `Array.Clone`) are reachable only from hand-written IL that uses a non-virtual `call`,
// which performs no null check. There is no ilasm in this repo, so those arms cannot be
// covered end to end; this test pins the guard that makes them unreachable, so that if the
// guard ever moves, something fails here rather than silently starting to route null
// receivers into the intrinsics.

using System;

public class Program
{
    // Opaque, so the null is not visible to the compiler as a constant.
    private static object NullObject()
    {
        return null;
    }

    private static Array NullArray()
    {
        return null;
    }

    public static int Main(string[] args)
    {
        try
        {
            Type t = NullObject().GetType();
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            object clone = NullArray().Clone();
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        // Through the interface, which is also a callvirt.
        try
        {
            ICloneable c = NullArray();
            object clone = c.Clone();
            return 3;
        }
        catch (NullReferenceException)
        {
        }

        // Non-null receivers still reach the intrinsics.
        if (new int[] { 1, 2 }.GetType() != typeof(int[]))
        {
            return 4;
        }

        int[] source = new int[] { 7, 8, 9 };
        int[] copied = (int[]) source.Clone();

        if (copied.Length != 3 || copied[2] != 9)
        {
            return 5;
        }

        return 0;
    }
}
