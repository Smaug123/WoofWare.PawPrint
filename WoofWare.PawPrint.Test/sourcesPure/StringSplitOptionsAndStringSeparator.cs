using System;

// Exercises the string-separator overload (SplitInternal(string, ...) / MakeSeparatorList)
// and StringSplitOptions.RemoveEmptyEntries | TrimEntries, which take a different path
// through String.Manipulation.cs than the single-char overload.
public class TestStringSplitOptionsAndStringSeparator
{
    public static int Main(string[] argv)
    {
        string[] parts1 = "a::b::c".Split("::");
        if (parts1.Length != 3) return 1;
        if (parts1[1] != "b") return 2;

        string[] parts2 = " a / b //  c ".Split('/', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (parts2.Length != 3) return 3;
        if (parts2[0] != "a") return 4;
        if (parts2[1] != "b") return 5;
        if (parts2[2] != "c") return 6;

        return 0;
    }
}
