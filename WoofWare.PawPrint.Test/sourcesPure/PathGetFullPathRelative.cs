using System;
using System.IO;

// `Path.GetFullPath` on a genuinely *relative* path, which must first resolve the process's
// current directory before it can collapse segments. On Unix that is `Interop.Sys.GetCwd()`,
// whose `SystemNative_GetCwd` PInvoke has no handler registered in PawPrint's NativeDispatch.
// The already-rooted paths, which skip that lookup, are covered by the passing sibling
// `PathGetFullPathRooted.cs`.
public class TestPathGetFullPathRelative
{
    public static int Main(string[] argv)
    {
        // Deliberately assert only what holds regardless of what the current directory is:
        // that the result is rooted, and ends with the relative path we asked for.
        string full = Path.GetFullPath("a/b");

        if (!Path.IsPathRooted(full)) return 1;
        if (!full.EndsWith("a/b")) return 2;

        return 0;
    }
}
