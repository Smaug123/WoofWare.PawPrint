using System;

// Exercises the string[]-separator overload (SplitInternal(string?, string?[]?, ...) ->
// MakeSeparatorListAny(source, ReadOnlySpan<string?>, ...)), which tracks separator
// *lengths* alongside indices (a second ValueListBuilder<int>) since separators can have
// different lengths -- a structurally different path from the single-char/single-string
// separator overloads already covered elsewhere. Also exercises the `count` parameter,
// which limits how many elements MakeSeparatorListAny/SplitWithoutPostProcessing produce.
public class TestStringSplitStringArrayAndCount
{
    public static int Main(string[] argv)
    {
        string[] seps = new string[] { "::", "->", "," };
        string[] parts = "a::b->c,d::e".Split(seps, StringSplitOptions.None);

        if (parts.Length != 5) return 1;
        if (parts[0] != "a") return 2;
        if (parts[4] != "e") return 3;

        string[] limited = "a,b,c,d,e".Split(',', 3);
        if (limited.Length != 3) return 4;
        if (limited[0] != "a") return 5;
        if (limited[1] != "b") return 6;
        if (limited[2] != "c,d,e") return 7;

        return 0;
    }
}
