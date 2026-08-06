using System;

// The simplest `String.Split(char)` shape: short enough that the separator search stays on
// CoreLib's scalar loop rather than reaching `MakeSeparatorListVectorized`. The siblings in
// this directory each take a structurally different branch of `String.Manipulation.cs`.
public class TestStringSplitBasic
{
    public static int Main(string[] argv)
    {
        string[] parts = "a/b/c".Split('/');

        if (parts.Length != 3) return 1;
        if (parts[0] != "a") return 2;
        if (parts[1] != "b") return 3;
        if (parts[2] != "c") return 4;

        return 0;
    }
}
