using System;
using System.IO;

// `Path.GetFullPath` on an already-rooted path, which skips the current-directory lookup
// entirely and goes straight to `PathInternal.RemoveRelativeSegments`. That segment-collapsing
// walk is the part of `GetFullPath` reachable today; the relative-path entry point needs
// `SystemNative_GetCwd` and is parked separately as `PathGetFullPathRelative.cs`.
public class TestPathGetFullPathRooted
{
    public static int Main(string[] argv)
    {
        // ".." collapses the preceding segment.
        string full = Path.GetFullPath("/a/b/../c");
        if (full != "/a/c") return 1;

        // "." segments are dropped.
        string full2 = Path.GetFullPath("/./x/./y");
        if (full2 != "/x/y") return 2;

        return 0;
    }
}
